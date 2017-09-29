module Language.Python.Statement.IR.Checker where

import Papa
import Data.Functor.Sum

import qualified Language.Python.Statement.AST as Safe
import qualified Language.Python.Statement.IR as IR

import Language.Python.IR.SyntaxChecker
import Language.Python.IR.SyntaxConfig

checkStatement
  :: SyntaxConfig assignable ctxt
  -> IR.Statement ann
  -> SyntaxChecker ann (Safe.Statement ctxt ann)
checkStatement cfg s =
  case s of
    IR.StatementSimple v ann ->
      Safe.StatementSimple <$> checkSimpleStatement cfg v <*> pure ann
    IR.StatementCompound v ann ->
      Safe.StatementCompound <$> checkCompoundStatement cfg v <*> pure ann

checkSimpleStatement
  :: SyntaxConfig assignable ctxt
  -> IR.SimpleStatement ann
  -> SyntaxChecker ann (Safe.SimpleStatement ctxt ann)
checkSimpleStatement cfg (IR.SimpleStatement h t s n ann) =
  Safe.SimpleStatement <$>
  checkSmallStatement cfg h <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkSmallStatement cfg)
    t <*>
  pure s <*>
  pure n <*>
  pure ann

checkCompoundStatement
  :: SyntaxConfig assignable ctxt
  -> IR.CompoundStatement ann
  -> SyntaxChecker ann (Safe.CompoundStatement ctxt ann)
checkCompoundStatement cfg s = _

checkSmallStatement
  :: SyntaxConfig assignable ctxt
  -> IR.SmallStatement ann
  -> SyntaxChecker ann (Safe.SmallStatement ctxt ann)
checkSmallStatement cfg s =
  case s of
    IR.SmallStatementExpr l r ann ->
      Safe.SmallStatementExpr <$>
      checkTestlistStarExpr (cfg & atomType .~ SAssignable) l <*>
      (case r of
         InL a -> InL <$> traverseOf (_Wrapped.traverse) _ a
         InR a -> InR <$> traverseOf (_Wrapped.traverse._Wrapped.traverse) _ a) <*>
      pure ann
    IR.SmallStatementDel v ann -> _
    IR.SmallStatementPass ann -> _
    IR.SmallStatementFlow v ann -> _
    IR.SmallStatementImport v ann -> _
    IR.SmallStatementGlobal h t ann -> _
    IR.SmallStatementNonlocal h t ann -> _
    IR.SmallStatementAssert h t ann -> _

checkTestlistStarExpr
  :: SyntaxConfig assignable ctxt
  -> IR.TestlistStarExpr ann
  -> SyntaxChecker ann (Safe.TestlistStarExpr assignable ctxt ann)
checkTestlistStarExpr cfg s = _
