{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
module Language.Python.IR.Checker.ArgumentList where

import Papa
import Data.Functor.Sum

import Language.Python.AST.IsArgList (HasName)
import Language.Python.IR.ExprConfig
import Language.Python.IR.SyntaxChecker

import qualified Language.Python.AST.IsArgList as IR (ArgumentError(..))
import qualified Language.Python.IR.ArgumentList as IR
import qualified Language.Python.AST.ArgumentList as Safe

checkKeywordItem
  :: ExprConfig as dctxt
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as' dctxt' ann))
  -> IR.KeywordItem name expr ann
  -> SyntaxChecker ann (Safe.KeywordItem checkedName checkedExpr 'NotAssignable dctxt ann)
checkKeywordItem cfg _checkName _checkExpr (IR.KeywordItem l r ann) =
  Safe.KeywordItem <$>
  traverseOf (_Wrapped.traverse) (_checkName cfg) l <*>
  traverseOf
    (_Wrapped.traverse)
    (_checkExpr $ cfg & atomType .~ SNotAssignable)
    r <*>
  pure ann

checkKeywordsArguments
  :: ExprConfig as dctxt
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as' dctxt' ann))
  -> IR.KeywordsArguments name expr ann
  -> SyntaxChecker ann (Safe.KeywordsArguments checkedName checkedExpr 'NotAssignable dctxt ann)
checkKeywordsArguments cfg _checkName _checkExpr (IR.KeywordsArguments h t ann) =
  Safe.KeywordsArguments <$>
  (case h of
    InL a -> InL <$> checkKeywordItem cfg _checkName _checkExpr a
    InR a ->
      InR <$>
      traverseOf
        (_Wrapped.traverse)
        (_checkExpr $ cfg & atomType .~ SNotAssignable)
        a) <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (\case
        InL a -> InL <$> checkKeywordItem cfg _checkName _checkExpr a
        InR a ->
          InR <$>
          traverseOf
            (_Wrapped.traverse)
            (_checkExpr $ cfg & atomType .~ SNotAssignable)
            a)
    t <*>
  pure ann

checkPositionalArguments
  :: ExprConfig as dctxt
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as' dctxt' ann))
  -> IR.PositionalArguments expr ann
  -> SyntaxChecker ann (Safe.PositionalArguments checkedExpr 'NotAssignable dctxt ann)
checkPositionalArguments cfg _checkExpr (IR.PositionalArguments h t ann) =
  Safe.PositionalArguments <$>
  traverseOf
    (_Wrapped.traverse)
    (_checkExpr $ cfg & atomType .~ SNotAssignable)
    h <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse._Wrapped.traverse)
    (_checkExpr $ cfg & atomType .~ SNotAssignable)
    t <*>
  pure ann

checkStarredAndKeywords
  :: ExprConfig as dctxt
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as' dctxt' ann))
  -> IR.StarredAndKeywords name expr ann
  -> SyntaxChecker ann (Safe.StarredAndKeywords checkedName checkedExpr 'NotAssignable dctxt ann)
checkStarredAndKeywords cfg _checkName _checkExpr (IR.StarredAndKeywords h t ann) =
  Safe.StarredAndKeywords <$>
  (case h of
     InL a ->
       InL <$>
       traverseOf
         (_Wrapped.traverse)
         (_checkExpr $ cfg & atomType .~ SNotAssignable)
         a
     InR a -> InR <$> checkKeywordItem cfg _checkName _checkExpr a) <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (\case
        InL a ->
          InL <$>
          traverseOf
            (_Wrapped.traverse)
            (_checkExpr $ cfg & atomType .~ SNotAssignable)
            a
        InR a -> InR <$> checkKeywordItem cfg _checkName _checkExpr a)
    t <*>
  pure ann

checkArgumentList
  :: HasName checkedName
  => ExprConfig as dctxt
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as' dctxt' ann))
  -> IR.ArgumentList name expr ann
  -> SyntaxChecker ann (Safe.ArgumentList checkedName checkedExpr 'NotAssignable dctxt ann)
checkArgumentList cfg _checkName _checkExpr a =
  case a of
    IR.ArgumentListAll ps ss ks c ann ->
      liftArgumentError ann $
      Safe.mkArgumentListAll <$>
      checkPositionalArguments cfg _checkExpr ps <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkStarredAndKeywords cfg _checkName _checkExpr)
        ss <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkKeywordsArguments cfg _checkName _checkExpr)
        ks <*>
      pure c <*>
      pure ann
    IR.ArgumentListUnpacking ss ks c ann ->
      liftArgumentError ann $
      Safe.mkArgumentListUnpacking <$>
      checkStarredAndKeywords cfg _checkName _checkExpr ss <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkKeywordsArguments cfg _checkName _checkExpr)
        ks <*>
      pure c <*>
      pure ann
    IR.ArgumentListKeywords ks c ann ->
      liftArgumentError ann $
      Safe.mkArgumentListKeywords <$>
      checkKeywordsArguments cfg _checkName _checkExpr ks <*>
      pure c <*>
      pure ann
  where
    liftArgumentError ann =
      liftError
      (\case
          IR.KeywordBeforePositional -> KeywordBeforePositional ann
          IR.DuplicateArguments -> DuplicateArguments ann)
