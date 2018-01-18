{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
module Language.Python.IR.Checker.ArgumentList where

import Papa
import Data.Separated.Before

import Language.Python.AST.IsArgList (HasName)
import Language.Python.IR.ExprConfig
import Language.Python.IR.SyntaxChecker

import qualified Language.Python.AST.IsArgList as IR (ArgumentError(..))
import qualified Language.Python.IR.ArgumentList as IR
import qualified Language.Python.AST.ArgumentList as Safe

checkArgument
  :: ExprConfig as dctxt
  -> ( val ann
    -> SyntaxChecker ann (checkedVal ann))
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as' ws' dctxt'
     . ExprConfig as' dctxt'
    -> expr ws' ann
    -> SyntaxChecker ann (checkedExpr ws' as' dctxt' ann))
  -> IR.Argument val name expr ann
  -> SyntaxChecker ann (Safe.Argument checkedVal checkedName checkedExpr dctxt ann)
checkArgument ecfg _checkVal _checkName _checkExpr a =
  let ecfg' = ecfg & atomType .~ SNotAssignable in
  case a of
    IR.ArgumentPositional a b ->
      Safe.ArgumentPositional <$>
      _checkVal a <*>
      pure b
    IR.ArgumentKeyword a b c d ->
      Safe.ArgumentKeyword <$>
      _checkName ecfg' a <*>
      pure b <*>
      _checkExpr ecfg' c <*>
      pure d
    IR.ArgumentStar a b c ->
      Safe.ArgumentStar a <$>
      _checkVal b <*>
      pure c
    IR.ArgumentDoublestar a b c ->
      Safe.ArgumentDoublestar a <$>
      _checkVal b <*>
      pure c

checkArgumentList
  :: HasName checkedName
  => ExprConfig as dctxt
  -> ( val ann
    -> SyntaxChecker ann (checkedVal ann))
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as' ws' dctxt'
     . ExprConfig as' dctxt'
    -> expr ws' ann
    -> SyntaxChecker ann (checkedExpr ws' as' dctxt' ann))
  -> IR.ArgumentList val name expr ann
  -> SyntaxChecker ann (Safe.ArgumentList checkedVal checkedName checkedExpr 'NotAssignable dctxt ann)
checkArgumentList cfg _checkVal _checkName _checkExpr a =
  case a of
    IR.ArgumentList h t ws c ann ->
      liftArgumentError ann $
      Safe.mkArgumentList <$>
      checkArgument cfg _checkVal _checkName _checkExpr h <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.before._2)
        (checkArgument cfg _checkVal _checkName _checkExpr)
        t <*>
      pure ws <*>
      pure c <*>
      pure ann
  where
    liftArgumentError ann =
      liftError
      (\case
          IR.KeywordBeforePositional -> KeywordBeforePositional ann
          IR.DuplicateArguments -> DuplicateArguments ann)
