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
import Control.Monad.State
import Data.Functor
import Data.List
import Data.Validate
import System.Directory
import System.Exit
import System.Process
import qualified Text.Trifecta as Trifecta

import Hedgehog
import qualified Hedgehog.Gen as Gen

validateExprSyntax'
  :: Expr '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Expr '[Syntax, Indentation] a)
validateExprSyntax' = runValidateSyntax initialSyntaxContext [] . validateExprSyntax

validateExprIndentation'
  :: Expr '[] a
  -> Validate [IndentationError '[] a] (Expr '[Indentation] a)
validateExprIndentation' = validateExprIndentation

validateStatementSyntax'
  :: Statement '[Indentation] a
  -> Validate [SyntaxError '[Indentation] a] (Statement '[Syntax, Indentation] a)
validateStatementSyntax' =
  runValidateSyntax initialSyntaxContext [] . validateStatementSyntax

validateStatementIndentation'
  :: Statement '[] a
  -> Validate [IndentationError '[] a] (Statement '[Indentation] a)
validateStatementIndentation' = validateStatementIndentation

runPython3 :: (MonadTest m, MonadIO m) => FilePath -> Bool -> String -> m ()
runPython3 path shouldSucceed str = do
  () <- liftIO $ writeFile path str
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
    let rex = renderExpr ex
    shouldSucceed <-
      case validateExprIndentation' ex of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateExprSyntax' res of
            Failure errs' ->
              let
                errs'' = filter (hasn't _MissingSpacesIn) errs'
              in
                if null errs''
                then pure True
                else annotateShow errs'' $> False
            Success res' -> pure True
    annotate rex
    runPython3
      path
      shouldSucceed
      rex

syntax_statement :: FilePath -> Property
syntax_statement path =
  property $ do
    st <- forAll $ Gen.resize 300 General.genStatement
    let rst = renderLines $ renderStatement st
    shouldSucceed <-
      case validateStatementIndentation' st of
        Failure errs -> annotateShow errs $> False
        Success res ->
          case validateStatementSyntax' res of
            Failure errs' ->
              let
                errs'' = filter (hasn't _MissingSpacesIn) errs'
              in
                if null errs''
                then pure True
                else annotateShow errs'' $> False
            Success res' -> pure True
    annotate rst
    runPython3 path shouldSucceed rst

correct_syntax_expr :: FilePath -> Property
correct_syntax_expr path =
  property $ do
    ex <- forAll Correct.genExpr
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> runPython3 path True (renderExpr ex)

correct_syntax_statement :: FilePath -> Property
correct_syntax_statement path =
  property $ do
    st <- forAll $ evalStateT (Correct.genStatement initialSyntaxContext) []
    annotate $ renderLines (renderStatement st)
    case validateStatementIndentation' st of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateStatementSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' -> runPython3 path True (renderLines $ renderStatement st)

expr_printparseprint_print :: Property
expr_printparseprint_print =
  property $ do
    ex <- forAll Correct.genExpr
    annotate (renderExpr ex)
    case validateExprIndentation' ex of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateExprSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' ->
            case Trifecta.parseString expr mempty (renderExpr res') of
              Trifecta.Failure errs'' -> annotateShow errs''
              Trifecta.Success res'' -> do
                annotateShow res''
                renderExpr (res' ^. unvalidated) === renderExpr (res'' $> ())

parseStatement = Trifecta.parseString (evalStateT statement []) mempty

statement_printparseprint_print :: Property
statement_printparseprint_print =
  property $ do
    st <- forAll $ evalStateT (Correct.genStatement initialSyntaxContext) []
    annotate (renderLines $ renderStatement st)
    case validateStatementIndentation' st of
      Failure errs -> annotateShow errs *> failure
      Success res ->
        case validateStatementSyntax' res of
          Failure errs' -> annotateShow errs' *> failure
          Success res' ->
            case parseStatement (renderLines $ renderStatement res') of
              Trifecta.Failure errs'' -> annotateShow errs''
              Trifecta.Success res'' -> do
                annotateShow res''
                renderLines (renderStatement (res' ^. unvalidated)) ===
                  renderLines (renderStatement (res'' $> ()))

main = do
  let file = "hedgehog-test.py"
  check . withTests 200 $ syntax_expr file
  check . withTests 200 $ syntax_statement file
  check . withTests 200 $ correct_syntax_expr file
  check . withTests 200 $ correct_syntax_statement file
  check expr_printparseprint_print
  check . withShrinks 2000 $ statement_printparseprint_print
  checkParallel scopeTests
  removeFile "hedgehog-test.py"
