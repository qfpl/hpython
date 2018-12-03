{-# language DataKinds #-}
{-# options_ghc -ddump-to-file -ddump-simpl  #-}
module Main where

import Criterion.Main

import Data.List.NonEmpty (NonEmpty)
import Data.Validation (Validation(..))
import System.Exit (exitFailure)

import qualified Data.Text.IO as StrictText

import Language.Python.Parse (parseModule)
import Language.Python.Parse.Error (ParseError)
import Language.Python.Internal.Lexer (SrcInfo, tokenize)
import Language.Python.Validate

parseCheckSeq :: FilePath -> IO ()
parseCheckSeq name = do
  file <- StrictText.readFile name
  py <-
    case parseModule name file of
      Failure e -> print (e :: NonEmpty (ParseError SrcInfo)) *> exitFailure
      Success a -> pure a
  case runValidateIndentation $ validateModuleIndentation py of
    Failure errs ->
      print (errs :: NonEmpty (IndentationError SrcInfo)) *> exitFailure
    Success res ->
      case runValidateSyntax initialSyntaxContext [] (validateModuleSyntax res) of
        Failure errs' ->
          print (errs' :: (NonEmpty (SyntaxError '[Indentation] SrcInfo))) *> exitFailure
        Success a -> pure $ seq a ()

tokenizeSeq :: FilePath -> IO ()
tokenizeSeq name = do
  file <- StrictText.readFile name
  case tokenize name file of
    Left e -> print (e :: (ParseError SrcInfo)) *> exitFailure
    Right a -> pure $ seq (length a) ()

main :: IO ()
main =
  defaultMain
  [ bench "tokenize 9000 lines of correct python" $
    nfIO (tokenizeSeq "./benchmarks/pypy.py")
  , bench "9000 lines of correct python" $
    nfIO (parseCheckSeq "./benchmarks/pypy.py")
  ]
