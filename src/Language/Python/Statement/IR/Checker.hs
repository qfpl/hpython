{-# language DataKinds #-}
{-# language GADTs #-}
module Language.Python.Statement.IR.Checker where

import Papa hiding (Sum)
import Data.Functor.Sum
import Data.Separated.Before

import qualified Language.Python.Statement.AST as Safe
import qualified Language.Python.Statement.IR as IR

import Language.Python.Expr.IR.Checker
import Language.Python.IR.ExprConfig
import Language.Python.IR.SyntaxChecker
import Language.Python.IR.StatementConfig

import Language.Python.Expr.IR as EI
import Language.Python.Expr.AST as EA

checkStatement
  :: ExprConfig assignable ctxt
  -> IR.Statement ann
  -> SyntaxChecker ann (Safe.Statement ctxt ann)
checkStatement cfg s =
  case s of
    IR.StatementSimple v ann ->
      Safe.StatementSimple <$> checkSimpleStatement cfg v <*> pure ann
    IR.StatementCompound v ann ->
      Safe.StatementCompound <$> checkCompoundStatement cfg v <*> pure ann

checkSimpleStatement
  :: ExprConfig assignable ctxt
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
  :: ExprConfig assignable ctxt
  -> IR.CompoundStatement ann
  -> SyntaxChecker ann (Safe.CompoundStatement ctxt ann)
checkCompoundStatement cfg s = _

checkSmallStatement
  :: ExprConfig assignable ctxt
  -> IR.SmallStatement ann
  -> SyntaxChecker ann (Safe.SmallStatement ctxt ann)
checkSmallStatement cfg s =
  case s of
    IR.SmallStatementExpr l r ann ->
      Safe.SmallStatementExpr <$>
      checkTestlistStarExpr (cfg & atomType .~ SAssignable) l <*>
      (case r of
         InL a -> InL <$> traverseOf (_Wrapped.before._2) (yieldOrTestList cfg) a
         InR a -> InR <$> traverseOf (_Wrapped.traverse._Wrapped.traverse) (yieldOrTestlistStarExpr cfg) a) <*>
      pure ann
    IR.SmallStatementDel v ann ->
      Safe.SmallStatementDel <$>
      traverseOf (_Wrapped.traverse) (checkExprList $ cfg & atomType .~ SAssignable) v <*>
      pure ann
    IR.SmallStatementPass ann -> pure $ Safe.SmallStatementPass ann
    IR.SmallStatementFlow v ann ->
      Safe.SmallStatementFlow <$>
      checkFlowStatement cfg v <*>
      pure ann
    IR.SmallStatementImport v ann ->
      Safe.SmallStatementImport <$>
      checkImportStatement cfg v <*>
      pure ann
    IR.SmallStatementGlobal h t ann ->
      pure $ Safe.SmallStatementGlobal h t ann
    IR.SmallStatementNonlocal h t ann ->
      case cfg ^. definitionContext of
        STopLevel -> syntaxError $ NonlocalAtModuleLevel ann
        SFunDef _ -> pure $ Safe.SmallStatementNonlocal h t ann
    IR.SmallStatementAssert h t ann ->
      Safe.SmallStatementAssert <$>
      checkTest (cfg & atomType .~ SNotAssignable) h <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.before._2)
        (checkTest $ cfg & atomType .~ SNotAssignable)
        t <*>
      pure ann
  where
    yieldOrTestList cfg (InL a) = InL <$> checkYieldExpr cfg a
    yieldOrTestList cfg (InR a) = InR <$> checkTestList (cfg & atomType .~ SNotAssignable) a

    yieldOrTestlistStarExpr cfg (InL a) = InL <$> checkYieldExpr cfg a
    yieldOrTestlistStarExpr cfg (InR a) = InR <$> checkTestlistStarExpr (cfg & atomType .~ SNotAssignable) a

checkFlowStatement
  :: ExprConfig assignable ctxt
  -> IR.FlowStatement ann
  -> SyntaxChecker ann (Safe.FlowStatement ctxt ann)
checkFlowStatement cfg s = _

checkImportStatement
  :: ExprConfig assignable ctxt
  -> IR.ImportStatement ann
  -> SyntaxChecker ann (Safe.ImportStatement ann)
checkImportStatement cfg s = _

checkTestlistStarExpr
  :: ExprConfig assignable ctxt
  -> IR.TestlistStarExpr ann
  -> SyntaxChecker ann (Safe.TestlistStarExpr assignable ctxt ann)
checkTestlistStarExpr cfg s = _
