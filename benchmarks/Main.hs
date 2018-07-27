{-# language DataKinds #-}
module Main where

import Criterion.Main

import Control.Monad ((<=<))
import Control.DeepSeq (rnf)
import Data.Validate (Validate(..))
import System.Exit (exitFailure)
import Text.Megaparsec (SourcePos, initialPos)

import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as LazyText

import Language.Python.Internal.Parse
import Language.Python.Internal.Render (showModule)
import Language.Python.Internal.Lexer
  (Nested, IndentedLine, LogicalLine, logicalLines, nested, indentation, tokenize)
import Language.Python.Internal.Token (PyToken)
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error

doTokenize :: Lazy.Text -> IO [PyToken SourcePos]
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

doToPython :: Parser SourcePos a -> Lazy.Text -> IO a
doToPython pa =
  doParse (initialPos "test") pa <=<
  doNested <=<
  doIndentation <=<
  pure . logicalLines <=<
  doTokenize

tokensOnly :: String -> IO ()
tokensOnly name = do
  file <- LazyText.readFile name
  py <- doTokenize file
  pure $! seq (last py) ()

parseCheckPrint :: FilePath -> IO ()
parseCheckPrint name = do
  file <- LazyText.readFile name
  py <- doToPython module_ file
  case runValidateIndentation $ validateModuleIndentation py of
    Failure errs ->
      print (errs :: [IndentationError '[] SourcePos]) *> exitFailure
    Success res ->
      case runValidateSyntax initialSyntaxContext [] (validateModuleSyntax res) of
        Failure errs' ->
          print (errs' :: [SyntaxError '[Indentation] SourcePos]) *> exitFailure
        Success _ -> pure $! rnf (showModule py)

main :: IO ()
main =
  defaultMain
  [ bench "9000 lines of correct python tokens" $ nfIO (tokensOnly "./benchmarks/pypy.py")
  , bench "9000 lines of correct python" $ nfIO (parseCheckPrint "./benchmarks/pypy.py")
  ]
