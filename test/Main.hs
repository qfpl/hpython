{-# language DataKinds, TypeOperators, FlexibleContexts #-}
module Main where

import Language.Python.Internal.Optics
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Parse (parseStatement, parseExpr)
import Language.Python.Validate.Indentation
import Language.Python.Validate.Syntax

import LexerParser
import Scope
import Roundtrip

import qualified Generators.General as General
import qualified Generators.Correct as Correct

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Functor
import Data.List
import Data.Text (Text)
import Data.Validate (Validate(..), validate)
import System.Directory
import System.Exit
import System.Process

import qualified Data.Text.IO as StrictText

import Hedgehog
import qualified Hedgehog.Gen as Gen

validateExprSyntax'
  :: Expr '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Expr '[Syntax, Indentation] a)
validateExprSyntax' = runValidateSyntax initialSyntaxContext [] . validateExprSyntax

validateExprIndentation'
  :: Expr '[] a
  -> Validate [IndentationError '[] a] (Expr '[Indentation] a)
validateExprIndentation' = runValidateIndentation . validateExprIndentation

validateStatementSyntax'
  :: Statement '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Statement '[Syntax, Indentation] a)
validateStatementSyntax' =
  runValidateSyntax initialSyntaxContext [] . validateStatementSyntax

validateStatementIndentation'
  :: Statement '[] a
  -> Validate [IndentationError '[] a] (Statement '[Indentation] a)
validateStatementIndentation' = runValidateIndentation . validateStatementIndentation

validateModuleSyntax'
  :: Module '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Module '[Syntax, Indentation] a)
validateModuleSyntax' =
  runValidateSyntax initialSyntaxContext [] . validateModuleSyntax

validateModuleIndentation'
  :: Module '[] a
  -> Validate [IndentationError '[] a] (Module '[Indentation] a)
validateModuleIndentation' = runValidateIndentation . validateModuleIndentation

runPython3 :: (MonadTest m, MonadIO m) => FilePath -> Bool -> Text -> m ()
runPython3 path shouldSucceed str = do
  () <- liftIO $ StrictText.writeFile path str
  (ec, sto, ste) <- liftIO $ readProcessWithExitCode "python3" ["-m", "py_compile", path] ""
  annotateShow shouldSucceed
  annotateShow ec
  annotate sto
  annotate ste
  case (shouldSucceed, ec) of
    (True, ExitSuccess) -> success
    (True, ExitFailure{})
      | "SyntaxError" `isInfixOf` last (lines ste) -> failure
      | otherwise -> success
    (False, ExitSuccess) -> failure
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
            Failure [] -> pure True
            Failure errs'' -> annotateShow errs'' $> False
            Success res' -> pure True
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
            Failure [] -> pure True
            Failure errs'' -> annotateShow errs'' $> False
            Success res' -> pure True
    annotateShow rst
    runPython3 path shouldSucceed rst

syntax_module :: FilePath -> Property
syntax_module path =
  property $ do
    st <- forAll $ Gen.resize 300 General.genModule
    let rst = showModule st
    shouldSucceed <-
      case validateModuleIndentation' st of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateModuleSyntax' res of
            Failure [] -> pure True
            Failure errs'' -> annotateShow errs'' $> False
            Success res' -> pure True
    annotateShow rst
    runPython3 path shouldSucceed rst

correct_syntax_expr :: FilePath -> Property
correct_syntax_expr path =
  property $ do
    ex <- forAll $ evalStateT Correct.genExpr Correct.initialGenState
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> runPython3 path True (showExpr ex)

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
          Success res' -> runPython3 path True $ showStatement st

expr_printparseprint_print :: Property
expr_printparseprint_print =
  property $ do
    ex <- forAll $ evalStateT Correct.genExpr Correct.initialGenState
    annotateShow $ showExpr ex
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> do
            py <-
              validate (\e -> annotateShow e *> failure) pure $
              parseExpr "test" (showExpr res')
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
              validate (\e -> annotateShow e *> failure) pure $
              parseStatement "test" (showStatement res')
            annotateShow py
            showStatement (res' ^. unvalidated) ===
              showStatement (py $> ())

main = do
  checkParallel lexerParserTests
  checkParallel scopeTests
  checkParallel roundtripTests
  let file = "hedgehog-test.py"
  check . withTests 200 $ syntax_expr file
  check . withTests 200 $ syntax_statement file
  check . withTests 200 $ syntax_module file
  check . withTests 200 $ correct_syntax_expr file
  check . withTests 200 $ correct_syntax_statement file
  check expr_printparseprint_print
  check . withShrinks 2000 $ statement_printparseprint_print
  removeFile "hedgehog-test.py"
