{-# options_ghc -fno-warn-unused-do-bind #-}
{-# language DataKinds, TypeOperators, FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad.State
import Data.List.NonEmpty
import Data.Functor (($>))
import Data.Text (Text)
import Data.Validation (Validation(..), validation)
import qualified Data.Text.IO as StrictText
import System.Directory
import System.Exit
import System.Process

import Language.Python.Internal.Syntax
import Language.Python.Optics.Validated (unvalidated)
import Language.Python.Parse (parseStatement, parseExpr, parseExprList)
import Language.Python.Render
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Module
import Language.Python.Syntax.Statement
import Language.Python.Validate

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Generators.General as General
import qualified Generators.Correct as Correct

runPython3 :: (MonadTest m, MonadIO m) => FilePath -> Bool -> Text -> m ()
runPython3 path shouldSucceed str = do
  () <- liftIO $ StrictText.writeFile path str
  (ec, sto, ste) <-
    liftIO $ readProcessWithExitCode "python3.5" ["-m", "py_compile", path] ""
  annotateShow str
  annotateShow shouldSucceed
  annotateShow ec
  annotate sto
  annotate ste
  case (shouldSucceed, ec) of
    -- Validation is sound but not complete
    --
    -- If the python repl would fail, then validation should fail
    -- If validation succeeds, then the python repl should succeed
    (True, ExitSuccess) -> success
    (True, ExitFailure{}) -> failure
      -- | "SyntaxError" `isInfixOf` last (lines ste) -> failure
      -- | otherwise -> success
    (False, ExitSuccess) -> success
    (False, ExitFailure{}) -> success

syntax_expr :: FilePath -> Property
syntax_expr path =
  property $ do
    ex <- forAll $ Gen.resize 300 General.genExpr
    let rex = showExpr ex
    shouldSucceed <-
      case validateExprIndentation' ex of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateExprSyntax' res of
            Failure errs'' -> annotateShow errs'' $> False
            Success _ -> pure True
    annotateShow rex
    runPython3
      path
      shouldSucceed
      rex

syntax_statement :: FilePath -> Property
syntax_statement path =
  property $ do
    st <- forAll $ Gen.resize 300 General.genStatement
    let rst = showStatement st
    shouldSucceed <-
      case validateStatementIndentation' st of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateStatementSyntax' res of
            Failure errs'' -> annotateShow errs'' $> False
            Success _ -> pure True
    annotateShow rst
    runPython3 path shouldSucceed rst

syntax_modul :: FilePath -> Property
syntax_modul path =
  property $ do
    st <- forAll $ Gen.resize 300 General.genModule
    let rst = showModule st
    shouldSucceed <-
      case validateModuleIndentation' st of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateModuleSyntax' res of
            Failure errs'' -> annotateShow errs'' $> False
            Success _ -> pure True
    annotateShow rst
    runPython3 path shouldSucceed rst

goodExpr :: FilePath -> Expr '[] () -> PropertyT IO ()
goodExpr path ex =
  case validateExprIndentation' ex of
    Failure errs -> annotateShow errs *> failure
    Success res ->
      case validateExprSyntax' res of
        Failure errs' -> do
          annotateShow errs'
          annotateShow $ showExpr ex
          failure
        Success res' -> runPython3 path True (showExpr res')

correct_syntax_expr :: FilePath -> Property
correct_syntax_expr path =
  property $ do
    ex <- forAll $ evalStateT Correct.genExpr Correct.initialGenState
    goodExpr path ex

correct_syntax_statement :: FilePath -> Property
correct_syntax_statement path =
  property $ do
    st <- forAll $ evalStateT Correct.genStatement Correct.initialGenState
    annotateShow $ showStatement st
    case validateStatementIndentation' st of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateStatementSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success _ -> runPython3 path True $ showStatement st

string_correct :: FilePath -> Property
string_correct path =
  property $ do
    str <- forAll $ Gen.list (Range.constant 0 100) Gen.unicode
    qt <- forAll $ Gen.element [SingleQuote, DoubleQuote]
    st <- forAll $ Gen.element [ShortString, LongString]
    let ex = String () . pure $ StringLiteral () Nothing st qt (fromHaskellString str) []
    goodExpr path ex

    let ex' = showExpr ex
    py <-
      validation (\e -> annotateShow e *> failure) pure $
      parseExpr "test" ex'

    goodExpr path $ () <$ py
    ex' === showExpr py

validateExprSyntax'
  :: Expr '[Indentation] a
  -> Validation (NonEmpty (SyntaxError '[Indentation] a)) (Expr '[Syntax, Indentation] a)
validateExprSyntax' = runValidateSyntax initialSyntaxContext [] . validateExprSyntax

validateExprIndentation'
  :: Expr '[] a
  -> Validation (NonEmpty (IndentationError '[] a)) (Expr '[Indentation] a)
validateExprIndentation' = runValidateIndentation . validateExprIndentation

validateStatementSyntax'
  :: Statement '[Indentation] a
  -> Validation (NonEmpty (SyntaxError '[Indentation] a)) (Statement '[Syntax, Indentation] a)
validateStatementSyntax' =
  runValidateSyntax initialSyntaxContext [] . validateStatementSyntax

validateStatementIndentation'
  :: Statement '[] a
  -> Validation (NonEmpty (IndentationError '[] a)) (Statement '[Indentation] a)
validateStatementIndentation' = runValidateIndentation . validateStatementIndentation

validateModuleSyntax'
  :: Module '[Indentation] a
  -> Validation (NonEmpty (SyntaxError '[Indentation] a)) (Module '[Syntax, Indentation] a)
validateModuleSyntax' =
  runValidateSyntax initialSyntaxContext [] . validateModuleSyntax

validateModuleIndentation'
  :: Module '[] a
  -> Validation (NonEmpty (IndentationError '[] a)) (Module '[Indentation] a)
validateModuleIndentation' = runValidateIndentation . validateModuleIndentation

expr_printparseprint_print :: Property
expr_printparseprint_print =
  property $ do
    ex <- forAll $ evalStateT Correct.genExprList Correct.initialGenState
    annotateShow $ showExpr ex
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> do
            _ <-
              validation (\e -> annotateShow e *> failure) pure $
              parseExprList "test" (showExpr res')
            showExpr (res' ^. unvalidated) === showExpr (res $> ())

statement_printparseprint_print :: Property
statement_printparseprint_print =
  property $ do
    st <- forAll $ evalStateT Correct.genStatement Correct.initialGenState
    annotateShow $ showStatement st
    case validateStatementIndentation' st of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateStatementSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> do
            py <-
              validation (\e -> annotateShow e *> failure) pure $
              parseStatement "test" (showStatement res')
            annotateShow py
            showStatement (res' ^. unvalidated) ===
              showStatement (py $> ())

main :: IO ()
main =
  do
    result <- checkParallel group
    removeFile file
    when (not result)
      exitFailure
  where
    file = "hedgehog-test.py"
    group =
      Group "main tests"
        [ ("Haskell String to Python String", withTests 500 $ string_correct file)
        , ("Syntax checking for expressions", withTests 500 $ syntax_expr file)
        , ("Syntax checking for statements", withTests 500 $ syntax_statement file)
        , ("Syntax checking for modules", withTests 500 $ syntax_modul file)
        , ("Correct generator for expressions", withTests 500 $ correct_syntax_expr file)
        , ("Correct generator for statements", withTests 500 $ correct_syntax_statement file)
        , ("Print/Parse idempotent expressions", withTests 500 $ expr_printparseprint_print)
        , ("Print/Parse idempotent statements", withTests 500 $ statement_printparseprint_print)
        ]
