{-# language DataKinds, TypeOperators, FlexibleContexts #-}
module Main where

import Language.Python.Internal.Render
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error
import Generators

import Control.Lens
import Control.Monad.IO.Class
import Data.Functor
import Data.List
import Data.Validate
import System.Exit
import System.Process

import Hedgehog

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
  validateStatementSyntax $
  SyntaxContext {_inLoop = False, _inFunction = False}

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
    ex <- forAll genSizedExpr
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
    st <- forAll genSizedStatement
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

main = do
  check $ withTests 1000 syntax_expr
  check $ withTests 1000 syntax_statement
