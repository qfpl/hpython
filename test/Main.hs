{-# language DataKinds, TypeOperators, FlexibleContexts #-}
module Main where

import Language.Python.Internal.Optics
import Language.Python.Internal.Parse
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error
import Scope

import qualified Generators.General as General
import qualified Generators.Correct as Correct

import Control.Lens
import Control.Monad.IO.Class
import Data.Functor
import Data.List
import Data.Validate
import System.Exit
import System.Process
import qualified Text.Trifecta as Trifecta

import Hedgehog
import qualified Hedgehog.Gen as Gen

validateExprSyntax'
  :: Expr '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Expr '[Syntax, Indentation] a)
validateExprSyntax' = validateExprSyntax

validateExprIndentation'
  :: Expr '[] a
  -> Validate [IndentationError '[] a] (Expr '[Indentation] a)
validateExprIndentation' = validateExprIndentation

validateStatementSyntax'
  :: Statement '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Statement '[Syntax, Indentation] a)
validateStatementSyntax' =
  validateStatementSyntax initialSyntaxContext

validateStatementIndentation'
  :: Statement '[] a
  -> Validate [IndentationError '[] a] (Statement '[Indentation] a)
validateStatementIndentation' = validateStatementIndentation

runPython3 :: (MonadTest m, MonadIO m) => Bool -> String -> m ()
runPython3 shouldSucceed str = do
  (ec, sto, ste) <- liftIO $ readProcessWithExitCode "python3" [] str
  annotateShow shouldSucceed
  annotateShow ec
  annotate ste
  case (shouldSucceed, ec) of
    (True, ExitSuccess) -> success
    (True, ExitFailure{})
      | "SyntaxError" `isInfixOf` last (lines ste) -> failure
      | otherwise -> success
    (False, ExitSuccess) -> failure
    (False, ExitFailure{}) -> success

syntax_expr :: Property
syntax_expr =
  property $ do
    ex <- forAll $ Gen.resize 300 General.genExpr
    let rex = renderExpr ex
    shouldSucceed <-
      case validateExprIndentation' ex of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateExprSyntax' res of
            Failure errs' -> annotateShow errs' $> False
            Success res' -> pure True
    annotate rex
    runPython3
      (has (_Success._Success) . fmap validateExprSyntax' $ validateExprIndentation' ex)
      rex

syntax_statement :: Property
syntax_statement =
  property $ do
    st <- forAll $ Gen.resize 300 General.genStatement
    let rst = renderLines $ renderStatement st
    shouldSucceed <-
      case validateStatementIndentation' st of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateStatementSyntax' res of
            Failure errs' -> annotateShow errs' $> False
            Success res' -> pure True
    annotate rst
    runPython3 shouldSucceed rst

expr_printparseprint_print :: Property
expr_printparseprint_print =
  property $ do
    ex <- forAll General.genExpr
    annotate (renderExpr ex)
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' ->
            case Trifecta.parseString expr mempty (renderExpr res') of
              Trifecta.Failure errs'' -> annotateShow errs''
              Trifecta.Success res'' ->
                renderExpr (res' ^. unvalidated) === renderExpr (res'' $> ())

correct_syntax_expr :: Property
correct_syntax_expr =
  property $ do
    ex <- forAll Correct.genExpr
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> runPython3 True (renderExpr ex)

correct_syntax_statement :: Property
correct_syntax_statement =
  property $ do
    st <- forAll $ Correct.genStatement initialSyntaxContext
    annotate $ renderLines (renderStatement st)
    case validateStatementIndentation' st of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateStatementSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> runPython3 True (renderLines $ renderStatement st)

main = do
  check $ withTests 200 syntax_expr
  check $ withTests 200 syntax_statement
  check $ withTests 200 correct_syntax_expr
  check $ withTests 200 correct_syntax_statement
  check expr_printparseprint_print
  checkParallel scopeTests
