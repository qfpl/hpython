{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
module Language.Python.Statement.IR.Checker where

import Papa hiding (Sum, Product)
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Separated.Before

import qualified Language.Python.Expr.AST as Safe
import qualified Language.Python.Expr.IR as IR

import qualified Language.Python.AST.TestlistStarExpr as Safe
import qualified Language.Python.IR.TestlistStarExpr as IR
import qualified Language.Python.Statement.AST as Safe
import qualified Language.Python.Statement.IR as IR

import Language.Python.AST.IndentedLines
import Language.Python.Expr.IR.Checker
import Language.Python.IR.Checker.ArgsList
import Language.Python.IR.Checker.TestlistStarExpr
import Language.Python.IR.ExprConfig
import Language.Python.IR.SyntaxChecker
import Language.Python.IR.StatementConfig

checkStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.Statement ann
  -> SyntaxChecker ann (Safe.Statement lctxt dctxt ann)
checkStatement ecfg scfg s =
  case s of
    IR.StatementSimple v ann ->
      Safe.StatementSimple <$>
      checkSimpleStatement ecfg scfg v <*>
      pure ann
    IR.StatementCompound v ann ->
      Safe.StatementCompound <$>
      checkCompoundStatement ecfg scfg v <*>
      pure ann

checkSimpleStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.SimpleStatement ann
  -> SyntaxChecker ann (Safe.SimpleStatement lctxt dctxt ann)
checkSimpleStatement ecfg scfg (IR.SimpleStatement h t s n ann) =
  Safe.SimpleStatement <$>
  checkSmallStatement ecfg scfg h <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkSmallStatement ecfg scfg)
    t <*>
  pure s <*>
  pure n <*>
  pure ann

checkCompoundStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.CompoundStatement ann
  -> SyntaxChecker ann (Safe.CompoundStatement lctxt dctxt ann)
checkCompoundStatement ecfg scfg s =
  case s of
    IR.CompoundStatementIf v ann ->
      Safe.CompoundStatementIf <$>
      checkIfStatement ecfg scfg v <*>
      pure ann
    IR.CompoundStatementWhile v ann ->
      Safe.CompoundStatementWhile <$>
      checkWhileStatement ecfg scfg v <*>
      pure ann
    IR.CompoundStatementFor v ann ->
      Safe.CompoundStatementFor <$>
      checkForStatement ecfg scfg v <*>
      pure ann
    IR.CompoundStatementTry v ann ->
      Safe.CompoundStatementTry <$>
      checkTryStatement ecfg scfg v <*>
      pure ann
    IR.CompoundStatementWith v ann ->
      Safe.CompoundStatementWith <$>
      checkWithStatement ecfg scfg v <*>
      pure ann
    IR.CompoundStatementFuncDef v ann ->
      Safe.CompoundStatementFuncDef <$>
      checkFuncDef ecfg (SFunDef SNormal) v <*>
      pure ann
    IR.CompoundStatementClassDef v ann ->
      Safe.CompoundStatementClassDef <$>
      checkClassDef ecfg v <*>
      pure ann
    IR.CompoundStatementDecorated v ann ->
      Safe.CompoundStatementDecorated <$>
      checkDecorated ecfg v <*>
      pure ann
    IR.CompoundStatementAsync v ann ->
      case ecfg ^. definitionContext of
        SFunDef SAsync ->
          Safe.CompoundStatementAsync <$>
          checkAsyncStatement ecfg scfg v <*>
          pure ann
        _ -> syntaxError $ AsyncNotInAsyncFunction ann

checkWhileStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.WhileStatement ann
  -> SyntaxChecker ann (Safe.WhileStatement lctxt dctxt ann)
checkWhileStatement ecfg scfg (IR.WhileStatement c b e ann) =
  Safe.WhileStatement <$>
  traverseOf
    (_Wrapped.traverse)
    (checkTest $ ecfg & atomType .~ SNotAssignable)
    c <*>
  traverseOf
    (_Wrapped.traverse)
    (checkSuite ecfg $ scfg & loopContext .~ SInLoop)
    b <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkSuite ecfg scfg)
    e <*>
  pure ann

checkForStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.ForStatement ann
  -> SyntaxChecker ann (Safe.ForStatement lctxt dctxt ann)
checkForStatement ecfg scfg (IR.ForStatement f i b e ann) =
  Safe.ForStatement <$>
  traverseOf
    (_Wrapped.traverse)
    (checkTestlistStarExpr checkExpr checkStarExpr $ ecfg & atomType .~ SAssignable)
    f <*>
  traverseOf
    (_Wrapped.traverse)
    (checkTestList $ ecfg & atomType .~ SNotAssignable)
    i <*>
  traverseOf
    (_Wrapped.traverse)
    (checkSuite ecfg $ scfg & loopContext .~ SInLoop)
    b <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkSuite ecfg scfg)
    e <*>
  pure ann

checkTryStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.TryStatement ann
  -> SyntaxChecker ann (Safe.TryStatement lctxt dctxt ann)
checkTryStatement ecfg scfg s =
  case s of
    IR.TryStatementExcepts t ex e f ann ->
      Safe.TryStatementExcepts <$>
      traverseOf
        (_Wrapped.traverse)
        (checkSuite ecfg scfg)
        t <*>
      traverseOf
        (_Wrapped.traverse)
        (\(Pair a b) ->
           Pair <$>
           checkExceptClause ecfg a <*>
           traverseOf
             (_Wrapped.traverse)
             (checkSuite ecfg scfg)
             b)
        ex <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkSuite ecfg scfg)
        e <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkSuite ecfg scfg)
        f <*>
      pure ann
    IR.TryStatementFinally t f ann ->
      Safe.TryStatementFinally <$>
      traverseOf
        (_Wrapped.traverse)
        (checkSuite ecfg scfg)
        t <*>
      traverseOf
        (_Wrapped.traverse)
        (checkSuite ecfg scfg)
        f <*>
      pure ann

checkExceptClause
  :: ExprConfig assignable dctxt
  -> IR.ExceptClause ann
  -> SyntaxChecker ann (Safe.ExceptClause dctxt ann)
checkExceptClause ecfg (IR.ExceptClause v ann) =
  Safe.ExceptClause <$>
  traverseOf
    (_Wrapped.traverse)
    (\(Pair a b) ->
      Pair <$>
      (checkTest $ ecfg & atomType .~ SNotAssignable) a <*>
      pure b)
    v <*>
  pure ann

checkWithStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.WithStatement ann
  -> SyntaxChecker ann (Safe.WithStatement lctxt dctxt ann)
checkWithStatement ecfg scfg (IR.WithStatement h t s ann) =
  Safe.WithStatement <$>
  traverseOf
    (_Wrapped.traverse)
    (checkWithItem ecfg)
    h <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkWithItem ecfg)
    t <*>
  traverseOf
    (_Wrapped.traverse)
    (checkSuite ecfg scfg)
    s <*>
  pure ann

checkWithItem
  :: ExprConfig assignable dctxt
  -> IR.WithItem ann
  -> SyntaxChecker ann (Safe.WithItem dctxt ann)
checkWithItem ecfg (IR.WithItem l r ann) =
  Safe.WithItem <$>
  checkTest (ecfg & atomType .~ SNotAssignable) l <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkExpr $ ecfg & atomType .~ SAssignable)
    r <*>
  pure ann

checkFuncDef
  :: ExprConfig assignable dctxt
  -> SDefinitionContext inner
  -> IR.FuncDef ann
  -> SyntaxChecker ann (Safe.FuncDef dctxt inner ann)
checkFuncDef ecfg inner (IR.FuncDef n p t b ann) =
  Safe.FuncDef n <$>
  traverseOf (_Wrapped.traverse) (checkParameters ecfg) p <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkTest $ ecfg & atomType .~ SNotAssignable)
    t <*>
  traverseOf
    (_Wrapped.traverse)
    (checkSuite (ecfg & definitionContext .~ inner) (StatementConfig SNotInLoop))
    b <*>
  pure ann

checkParameters
  :: ExprConfig assignable dctxt
  -> IR.Parameters ann
  -> SyntaxChecker ann (Safe.Parameters dctxt ann)
checkParameters ecfg (IR.Parameters v ann) =
  Safe.Parameters <$>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkArgsList
      (ecfg & atomType .~ SNotAssignable)
      checkTest
      checkTypedArg)
    v <*>
  pure ann

checkTypedArg
  :: ExprConfig assignable dctxt
  -> IR.TypedArg ann
  -> SyntaxChecker ann (Safe.TypedArg ann)
checkTypedArg ecfg (IR.TypedArg v t ann) =
  Safe.TypedArg v <$>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkTest $ ecfg
      & atomType .~ SNotAssignable
      & definitionContext .~ SFunDef SNormal)
  t <*>
  pure ann

checkClassDef
  :: ExprConfig assignable dctxt
  -> IR.ClassDef ann
  -> SyntaxChecker ann (Safe.ClassDef dctxt ann)
checkClassDef ecfg (IR.ClassDef n a b ann) =
  Safe.ClassDef n <$>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse._Wrapped.traverse._Wrapped.traverse)
    (checkArgList checkTest checkCompFor $ ecfg & atomType .~ SNotAssignable)
    a <*>
  traverseOf
    (_Wrapped.traverse)
    (checkSuite ecfg $ StatementConfig SNotInLoop)
    b <*>
  pure ann

checkDecorated
  :: ExprConfig assignable dctxt
  -> IR.Decorated ann
  -> SyntaxChecker ann (Safe.Decorated dctxt ann)
checkDecorated ecfg (IR.Decorated d b ann) =
  Safe.Decorated <$>
  traverseOf (_Wrapped.traverse) (checkDecorator ecfg) d <*>
  (case b of
     InL (InL a) -> InL . InL <$> checkClassDef ecfg a
     InL (InR a) -> InL . InR <$> checkFuncDef ecfg (SFunDef SNormal) a
     InR a -> InR <$> checkAsyncFuncDef ecfg a
     ) <*>
  pure ann

checkAsyncFuncDef
  :: ExprConfig assignable dctxt
  -> IR.AsyncFuncDef ann
  -> SyntaxChecker ann (Safe.AsyncFuncDef dctxt ann)
checkAsyncFuncDef ecfg (IR.AsyncFuncDef v ann) =
  Safe.AsyncFuncDef <$>
  traverseOf
    (_Wrapped.traverse)
    (checkFuncDef ecfg (SFunDef SAsync))
    v <*>
  pure ann

checkDecorator
  :: ExprConfig assignable dctxt
  -> IR.Decorator ann
  -> SyntaxChecker ann (Safe.Decorator dctxt ann)
checkDecorator ecfg (IR.Decorator n a nl ann) =
  Safe.Decorator n <$>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse._Wrapped.traverse)
    (checkArgList checkTest checkCompFor $ ecfg & atomType .~ SNotAssignable)
    a <*>
  pure nl <*>
  pure ann

checkAsyncStatement
  :: ExprConfig assignable ('FunDef 'Async)
  -> StatementConfig lctxt
  -> IR.AsyncStatement ann
  -> SyntaxChecker ann (Safe.AsyncStatement lctxt ('FunDef 'Async) ann)
checkAsyncStatement ecfg scfg (IR.AsyncStatement v ann) =
  Safe.AsyncStatement <$>
  traverseOf
    (_Wrapped.traverse)
    (\a -> case a of
        InL (InL a') -> InL . InL <$> checkFuncDef ecfg (SFunDef SAsync) a'
        InL (InR a') -> InL . InR <$> checkWithStatement ecfg scfg a'
        InR a' -> InR <$> checkForStatement ecfg scfg a')
    v <*>
  pure ann

checkIfStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.IfStatement ann
  -> SyntaxChecker ann (Safe.IfStatement lctxt dctxt ann)
checkIfStatement ecfg scfg (IR.IfStatement c t elif el ann) =
  Safe.IfStatement <$>
  traverseOf
    (_Wrapped.traverse)
    (checkTest $ ecfg & atomType .~ SNotAssignable)
    c <*>
  traverseOf
    (_Wrapped.traverse)
    (checkSuite ecfg scfg)
    t <*>
  traverseOf
    (_Wrapped.traverse)
    (\(Pair a b) ->
       Pair <$>
       traverseOf
         (_Wrapped.traverse)
         (checkTest $ ecfg & atomType .~ SNotAssignable)
         a <*>
       traverseOf
         (_Wrapped.traverse)
         (checkSuite ecfg scfg)
         b)
    elif <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (checkSuite ecfg scfg)
    el <*>
  pure ann

checkSmallStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.SmallStatement ann
  -> SyntaxChecker ann (Safe.SmallStatement lctxt dctxt ann)
checkSmallStatement ecfg scfg s =
  case s of
    IR.SmallStatementExpr l r ann ->
      case (l, r) of
        (IR.TestlistStarExpr a (Compose []) Nothing _, InL b) ->
          Safe.SmallStatementAugAssign <$>
          (case a of
             InL a' -> checkTest (ecfg & atomType .~ SAssignable) a'
             InR _ -> syntaxError $ CannotAssignTo LHSSingleStarExpr ann) <*>
          traverseOf
            (_Wrapped.traverse)
            (yieldOrTestList ecfg)
            b <*>
          pure ann
        (_, InL _) ->
          syntaxError $ AugAssignListLHS ann
        (IR.TestlistStarExpr a (Compose []) Nothing _, InR (Compose [])) ->
          case a of
            InL a' ->
              Safe.SmallStatementExpr <$>
              checkTest (ecfg & atomType .~ SNotAssignable) a' <*>
              pure ann
            InR _ -> syntaxError $ TopLevelUnpacking ann
        (_, InR (Compose as)) ->
          case unsnoc as of
            Just (as', a') ->
              Safe.SmallStatementAssign <$>
              checkTestlistStarExpr
                checkTest
                checkStarExpr
                (ecfg & atomType .~ SAssignable)
                l <*>
              traverseOf
                (_Wrapped.traverse._Wrapped.traverse)
                (\case
                    InL _ -> syntaxError $ CannotAssignTo LHSYieldExpr ann
                    InR a'' ->
                      checkTestlistStarExpr
                        checkTest
                        checkStarExpr
                        (ecfg & atomType .~ SAssignable)
                        a'')
                (Compose as') <*>
              traverseOf (_Wrapped.traverse) (yieldOrTestlistStarExpr ecfg) a' <*>
              pure ann
    IR.SmallStatementDel v ann ->
      Safe.SmallStatementDel <$>
      traverseOf
        (_Wrapped.traverse)
        (checkExprList $ ecfg & atomType .~ SAssignable)
        v <*>
      pure ann
    IR.SmallStatementPass ann -> pure $ Safe.SmallStatementPass ann
    IR.SmallStatementFlow v ann ->
      Safe.SmallStatementFlow <$>
      checkFlowStatement ecfg scfg v <*>
      pure ann
    IR.SmallStatementImport v ann -> pure $ Safe.SmallStatementImport v ann
    IR.SmallStatementGlobal h t ann ->
      pure $ Safe.SmallStatementGlobal h t ann
    IR.SmallStatementNonlocal h t ann ->
      case ecfg ^. definitionContext of
        STopLevel -> syntaxError $ NonlocalAtModuleLevel ann
        SFunDef _ -> pure $ Safe.SmallStatementNonlocal h t ann
    IR.SmallStatementAssert h t ann ->
      Safe.SmallStatementAssert <$>
      traverseOf
        (_Wrapped.traverse)
        (checkTest $ ecfg & atomType .~ SNotAssignable)
        h <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.before._2)
        (checkTest $ ecfg & atomType .~ SNotAssignable)
        t <*>
      pure ann
  where
    yieldOrTestList
      :: ExprConfig as dctxt
      -> Sum IR.YieldExpr IR.TestList ann
      -> SyntaxChecker ann (Sum (Safe.YieldExpr dctxt) (Safe.TestList 'NotAssignable dctxt) ann)
    yieldOrTestList cfg (InL a) =
      case cfg ^. definitionContext of
        SFunDef SNormal -> InL <$> checkYieldExpr cfg a
        _ -> syntaxError $ YieldNotInFunction (a ^. IR.yieldExpr_ann)
    yieldOrTestList cfg (InR a) = InR <$> checkTestList (cfg & atomType .~ SNotAssignable) a

    yieldOrTestlistStarExpr
      :: ExprConfig as dctxt
      -> Sum IR.YieldExpr (IR.TestlistStarExpr IR.Test IR.StarExpr) ann
      -> SyntaxChecker ann (Sum (Safe.YieldExpr dctxt) (Safe.TestlistStarExpr Safe.Test Safe.StarExpr 'NotAssignable dctxt) ann)
    yieldOrTestlistStarExpr cfg (InL a) =
      case cfg ^. definitionContext of
        SFunDef SNormal -> InL <$> checkYieldExpr cfg a
        _ -> syntaxError $ YieldNotInFunction (a ^. IR.yieldExpr_ann)
    yieldOrTestlistStarExpr cfg (InR a) =
      InR <$>
      checkTestlistStarExpr
        checkTest
        checkStarExpr
        (cfg & atomType .~ SNotAssignable)
        a

checkFlowStatement
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.FlowStatement ann
  -> SyntaxChecker ann (Safe.FlowStatement lctxt dctxt ann)
checkFlowStatement ecfg scfg s =
  case s of
    IR.FlowStatementBreak ann ->
      case scfg ^. loopContext of
        SInLoop -> pure $ Safe.FlowStatementBreak ann
        _ -> syntaxError $ BreakOutsideLoop ann
    IR.FlowStatementContinue ann ->
      case scfg ^. loopContext of
        SInLoop -> pure $ Safe.FlowStatementContinue ann
        _ -> syntaxError $ ContinueOutsideLoop ann
    IR.FlowStatementReturn v ann ->
      case ecfg ^. definitionContext of
        SFunDef _ -> 
          Safe.FlowStatementReturn <$>
          traverseOf
            (_Wrapped.traverse._Wrapped.traverse)
            (checkTestList $ ecfg & atomType .~ SNotAssignable)
            v <*>
          pure ann
        _ -> syntaxError $ ReturnOutsideFunction ann
    IR.FlowStatementRaise v ann ->
      Safe.FlowStatementRaise <$>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkRaiseStatement ecfg)
        v <*>
      pure ann
    IR.FlowStatementYield v ann ->
      case ecfg ^. definitionContext of
        SFunDef SNormal ->
          Safe.FlowStatementYield <$>
          checkYieldExpr ecfg v <*>
          pure ann
        SFunDef SAsync -> syntaxError $ YieldInAsyncFunction ann
        STopLevel -> syntaxError $ YieldNotInFunction ann

checkRaiseStatement
  :: ExprConfig assignable dctxt
  -> IR.RaiseStatement ann
  -> SyntaxChecker ann (Safe.RaiseStatement dctxt ann)
checkRaiseStatement ecfg (IR.RaiseStatement l r ann) =
  Safe.RaiseStatement <$>
  checkTest (ecfg & atomType .~ SNotAssignable) l <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse._Wrapped.traverse)
    (checkTest $ ecfg & atomType .~ SNotAssignable)
    r <*>
  pure ann

checkSuite
  :: ExprConfig assignable dctxt
  -> StatementConfig lctxt
  -> IR.Suite ann
  -> SyntaxChecker ann (Safe.Suite lctxt dctxt ann)
checkSuite ecfg scfg s =
  case s of
    IR.SuiteSingle v ann ->
      Safe.SuiteSingle <$>
      checkSimpleStatement ecfg scfg v <*>
      pure ann
    IR.SuiteMulti n ls ann ->
      Safe.SuiteMulti n <$>
      traverseOf
        _Wrapped
        (liftError (IndentationError . ($ ann)) .
         fmap mkIndentedLines .
         traverseOf (traverse._2) (checkStatement ecfg scfg) .
         fmap (view before . getCompose))
        ls <*>
      pure ann
