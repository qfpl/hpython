{-# language DataKinds #-}
{-# options_ghc -ddump-to-file -ddump-simpl  #-}
module Main where

import Criterion.Main

import Control.Monad ((<=<))
import Control.DeepSeq (rnf)
import Data.Validate (Validate(..))
import System.Exit (exitFailure)

import qualified Data.Text as Text
import qualified Data.Text.IO as StrictText

import Language.Python.Internal.Parse
import Language.Python.Internal.Render (showModule)
import Language.Python.Internal.Lexer
  (SrcInfo, Nested, IndentedLine, LogicalLine, logicalLines, nested, indentation, tokenize, initialSrcInfo)
import Language.Python.Internal.Token (PyToken)
import Language.Python.Internal.Syntax.IR (fromIR)
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax

doTokenize :: Text.Text -> IO [PyToken SrcInfo]
doTokenize str = do
  let res = tokenize str
  case res of
    Left err -> print err *> exitFailure
    Right a -> pure a

doIndentation :: Show a => [LogicalLine a] -> IO [IndentedLine a]
doIndentation lls = do
  let res = indentation lls
  case res of
    Left err -> print err *> exitFailure
    Right a -> pure a

doNested :: [IndentedLine a] -> IO (Nested a)
doNested ils = do
  let res = nested ils
  case res of
    Left err -> print err *> exitFailure
    Right a -> pure a

doParse :: Show ann => ann -> Parser ann a -> Nested ann -> IO a
doParse initial pa input = do
  let res = runParser initial pa input
  case res of
    Left err -> print err *> exitFailure
    Right a -> pure a

doFromIR :: Show e => (a -> Validate e b) -> a -> IO b
doFromIR f a = do
  let res = f a
  case res of
    Failure err -> print err *> exitFailure
    Success a -> pure a

doToPython
  :: Show ann
  => Parser SrcInfo a
  -> (a -> Validate [SyntaxError v ann] b)
  -> Text.Text
  -> IO b
doToPython pa f =
  doFromIR f <=<
  doParse (initialSrcInfo "test") pa <=<
  doNested <=<
  doIndentation <=<
  pure . logicalLines <=<
  doTokenize

tokensOnly :: String -> IO ()
tokensOnly name = do
  file <- StrictText.readFile name
  py <- doTokenize file
  pure $! seq (last py) ()

parseCheckPrint :: FilePath -> IO ()
parseCheckPrint name = do
  file <- StrictText.readFile name
  py <- doToPython module_ fromIR file
  case runValidateIndentation $ validateModuleIndentation py of
    Failure errs ->
      print (errs :: [IndentationError '[] SrcInfo]) *> exitFailure
    Success res ->
      case runValidateSyntax initialSyntaxContext [] (validateModuleSyntax res) of
        Failure errs' ->
          print (errs' :: [SyntaxError '[Indentation] SrcInfo]) *> exitFailure
        Success _ -> pure $! rnf (showModule py)

main :: IO ()
main =
  defaultMain
  [ bench "9000 lines of correct python tokens" $ nfIO (tokensOnly "./benchmarks/pypy.py")
  , bench "9000 lines of correct python" $ nfIO (parseCheckPrint "./benchmarks/pypy.py")
  ]
