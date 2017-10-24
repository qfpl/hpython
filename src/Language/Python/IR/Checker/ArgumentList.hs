{-# language RankNTypes #-}
module Language.Python.IR.Checker.ArgumentList where

import Language.Python.IR.ExprConfig
import Language.Python.IR.SyntaxChecker

import qualified Language.Python.IR.ArgumentList as IR
import qualified Language.Python.AST.ArgumentList as Safe

checkKeywordItem
  :: ExprConfig as dctxt
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as dctxt ann))
  -> IR.KeywordItem name expr ann
  -> SyntaxChecker ann (Safe.KeywordItem checkedName checkedExpr as dctxt ann)
checkKeywordItem cfg _checkName _checkExpr (IR.KeywordItem l r ann) = _

checkKeywordsArguments
  :: ExprConfig as dctxt
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as dctxt ann))
  -> IR.KeywordsArguments name expr a
  -> SyntaxChecker ann (Safe.KeywordsArguments checkedName checkedExpr as dctxt ann)
checkKeywordsArguments cfg _checkName _checkExpr (IR.KeywordsArguments h t ann) = _

checkPositionalArguments
  :: ExprConfig as dctxt
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as dctxt ann))
  -> IR.PositionalArguments expr a
  -> SyntaxChecker ann (Safe.PositionalArguments checkedExpr as dctxt ann)
checkPositionalArguments cfg _checkExpr (IR.PositionalArguments h t ann) = _

checkStarredAndKeywords
  :: ExprConfig as dctxt
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as dctxt ann))
  -> IR.StarredAndKeywords name expr a
  -> SyntaxChecker ann (Safe.StarredAndKeywords checkedName checkedExpr as dctxt ann)
checkStarredAndKeywords cfg _checkName _checkExpr (IR.StarredAndKeywords h t ann) = _

checkArgumentList
  :: ExprConfig as dctxt
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> name ann
    -> SyntaxChecker ann (checkedName ann))
  -> ( forall as dctxt
     . ExprConfig as dctxt
    -> expr ann
    -> SyntaxChecker ann (checkedExpr as dctxt ann))
  -> IR.ArgumentList name expr a
  -> SyntaxChecker ann (Safe.ArgumentList checkedName checkedExpr as dctxt ann)
checkArgumentList cfg _checkName _checkExpr a =
  case a of
    IR.ArgumentListAll ps ss ks ann -> _
    IR.ArgumentListUnpacking ss ks ann -> _
    IR.ArgumentListKeywords ks ann -> _
