{-# language DataKinds #-}
module Main where

import Criterion.Main

import Control.Monad ((<=<))
import Control.DeepSeq (rnf)
import System.Exit (exitFailure)

import Data.Validate (Validate(..))
import Language.Python.Internal.Parse
import Language.Python.Internal.Render (showModule)
import Language.Python.Internal.Lexer
  (Nested, IndentedLine, LogicalLine, logicalLines, nested, indentation, tokenize)
import Language.Python.Internal.Token (PyToken)
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error

import qualified Text.Trifecta as Trifecta

doTokenize :: String -> IO [PyToken Trifecta.Caret]
doTokenize str = do
  let res = tokenize str
  case res of
    Trifecta.Failure err -> print err *> exitFailure
    Trifecta.Success a -> pure a

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

doToPython :: Parser Trifecta.Caret a -> String -> IO a
doToPython pa =
  doParse (Trifecta.Caret mempty mempty) pa <=<
  doNested <=<
  doIndentation <=<
  pure . logicalLines <=<
  doTokenize

parseCheckPrint :: String -> IO ()
parseCheckPrint name = do
  file <- readFile name
  py <- doToPython module_ file
  case runValidateIndentation $ validateModuleIndentation py of
    Failure errs ->
      print (errs :: [IndentationError '[] Trifecta.Caret]) *> exitFailure
    Success res ->
      case runValidateSyntax initialSyntaxContext [] (validateModuleSyntax res) of
        Failure errs' ->
          print (errs' :: [SyntaxError '[Indentation] Trifecta.Caret]) *> exitFailure
        Success _ -> pure $! rnf (showModule py)
          

main :: IO ()
main =
  defaultMain
  [ bench "9000 lines of correct python" $ nfIO (parseCheckPrint "./benchmarks/pypy.py")
  ]
