{-# language DataKinds #-}
{-# options_ghc -ddump-to-file -ddump-simpl  #-}
module Main where

import Criterion.Main

import Data.Validate (Validate(..))
import System.Exit (exitFailure)

import qualified Data.Text.IO as StrictText

import Language.Python.Parse (parseModule)
import Language.Python.Internal.Lexer (SrcInfo, tokenize)
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax

parseCheckSeq :: FilePath -> IO ()
parseCheckSeq name = do
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
        Success a -> pure $ seq a ()

tokenizeSeq :: FilePath -> IO ()
tokenizeSeq name = do
  file <- StrictText.readFile name
  case tokenize name file of
    Left e -> print e *> exitFailure
    Right a -> pure $ seq (length a) ()

main :: IO ()
main =
  defaultMain
  [ bench "tokenize 9000 lines of correct python" $
    nfIO (tokenizeSeq "./benchmarks/pypy.py")
  , bench "9000 lines of correct python" $
    nfIO (parseCheckSeq "./benchmarks/pypy.py")
  ]
