{-# language DataKinds #-}
{-# options_ghc -ddump-to-file -ddump-simpl  #-}
module Main where

import Criterion.Main

import Control.DeepSeq (rnf)
import Data.Validate (Validate(..))
import System.Exit (exitFailure)

import qualified Data.Text.IO as StrictText

import Language.Python.Parse (parseModule)
import Language.Python.Internal.Render (showModule)
import Language.Python.Internal.Lexer (SrcInfo)
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax

parseCheckPrint :: FilePath -> IO ()
parseCheckPrint name = do
  file <- StrictText.readFile name
  py <-
    case parseModule name file of
      Failure e -> print e *> exitFailure
      Success a -> pure a
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
  [ bench "9000 lines of correct python" $ nfIO (parseCheckPrint "./benchmarks/pypy.py")
  ]
