{-# language DataKinds #-}
module Test.Language.Python.Statement.Gen where

import Prelude (error)
import Papa
import Hedgehog

import Data.Separated.Before
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.Statement.AST
import Language.Python.IR.ExprConfig
import Language.Python.IR.StatementConfig

import Test.Language.Python.Expr.Gen
import Test.Language.Python.Gen.ArgsList
import Test.Language.Python.Gen.ArgumentList
import Test.Language.Python.Gen.Combinators
import Test.Language.Python.Gen.DottedName
import Test.Language.Python.Gen.Identifier
import Test.Language.Python.Gen.IndentedLines
import Test.Language.Python.Gen.TestlistStarExpr
import Test.Language.Python.Statement.AugAssign
import Test.Language.Python.Statement.Gen.Imports

genStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (Statement lc dc ())
genStatement scfg ecfg =
  Gen.choice
    [ StatementSimple <$>
      genSimpleStatement scfg ecfg <*>
      pure ()
    , StatementCompound <$>
      genCompoundStatement scfg ecfg <*>
      pure ()
    ]

genSimpleStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (SimpleStatement lc dc ())
genSimpleStatement scfg ecfg =
  SimpleStatement <$>
  Gen.small (genSmallStatement scfg ecfg) <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $ pure Semicolon)
      (Gen.small $ genSmallStatement scfg ecfg)) <*>
  Gen.maybe (genWhitespaceBefore $ pure Semicolon) <*>
  genWhitespaceBefore genNewlineChar <*>
  pure ()

genFlowStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (FlowStatement lc dc ())
genFlowStatement scfg ecfg =
  case (scfg ^. loopContext, ecfg ^. definitionContext) of
    (SInLoop, SFunDef SNormal) ->
      Gen.choice
        [ genFlowStatementBreak scfg ecfg
        , genFlowStatementContinue scfg ecfg
        , genFlowStatementReturn scfg ecfg
        , genFlowStatementRaise scfg ecfg
        , genFlowStatementYield scfg ecfg
        ]
    (SInLoop, SFunDef SAsync) ->
      Gen.choice
        [ genFlowStatementBreak scfg ecfg
        , genFlowStatementContinue scfg ecfg
        , genFlowStatementReturn scfg ecfg
        , genFlowStatementRaise scfg ecfg
        ]
    (SInLoop, STopLevel) ->
      Gen.choice
        [ genFlowStatementBreak scfg ecfg
        , genFlowStatementContinue scfg ecfg
        , genFlowStatementRaise scfg ecfg
        ]
    (SNotInLoop, SFunDef SNormal) ->
      Gen.choice
        [ genFlowStatementReturn scfg ecfg
        , genFlowStatementRaise scfg ecfg
        , genFlowStatementYield scfg ecfg
        ]
    (SNotInLoop, SFunDef SAsync) ->
      Gen.choice
        [ genFlowStatementReturn scfg ecfg
        , genFlowStatementRaise scfg ecfg
        ]
    (SNotInLoop, STopLevel) ->
      Gen.choice
        [ genFlowStatementRaise scfg ecfg ]
  where
    genFlowStatementBreak
      :: MonadGen m
      => StatementConfig 'InLoop
      -> ExprConfig as dc
      -> m (FlowStatement 'InLoop dc ())
    genFlowStatementBreak _ _ = pure $ FlowStatementBreak ()

    genFlowStatementContinue
      :: MonadGen m
      => StatementConfig 'InLoop
      -> ExprConfig as dc
      -> m (FlowStatement 'InLoop dc ())
    genFlowStatementContinue _ _ = pure $ FlowStatementContinue ()

    genFlowStatementReturn
      :: MonadGen m
      => StatementConfig lc
      -> ExprConfig as ('FunDef b)
      -> m (FlowStatement lc ('FunDef b) ())
    genFlowStatementReturn _ e =
      FlowStatementReturn <$>
      genMaybeF
        (genWhitespaceBefore1F
          (genTestList $ e & atomType .~ SNotAssignable)) <*>
      pure ()

    genFlowStatementRaise
      :: MonadGen m
      => StatementConfig lc
      -> ExprConfig as dc
      -> m (FlowStatement lc dc ())
    genFlowStatementRaise _ e =
      FlowStatementRaise <$>
      genMaybeF (genWhitespaceBefore1F $ genRaiseStatement e) <*>
      pure ()

    genFlowStatementYield
      :: MonadGen m
      => StatementConfig lc
      -> ExprConfig as ('FunDef 'Normal)
      -> m (FlowStatement lc ('FunDef 'Normal) ())
    genFlowStatementYield _ e =
      FlowStatementYield <$>
      genYieldExpr
        (e
           & atomType .~ SNotAssignable
           & definitionContext .~ SFunDef SNormal) <*>
      pure ()

genRaiseStatement :: MonadGen m => ExprConfig as dc -> m (RaiseStatement dc ())
genRaiseStatement ecfg =
  RaiseStatement <$>
  genTest (ecfg & atomType .~ SNotAssignable) <*>
  genMaybeF
    (genBeforeF
      (pure KFrom)
      (genWhitespaceBefore1F
       (genTest $ ecfg & atomType .~ SNotAssignable))) <*>
  pure ()

genSmallStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (SmallStatement lc dc ())
genSmallStatement scfg ecfg =
  case ecfg ^. definitionContext of
    STopLevel ->
      Gen.choice
        [ genSmallStatementExpr
        , genSmallStatementAssign
        , genSmallStatementAugAssign
        , genSmallStatementDel
        , genSmallStatementPass
        , genSmallStatementFlow
        , genSmallStatementImport
        , genSmallStatementGlobal
        , genSmallStatementAssert
        ]
    _ ->
      Gen.choice
        [ genSmallStatementExpr
        , genSmallStatementAssign
        , genSmallStatementAugAssign
        , genSmallStatementDel
        , genSmallStatementPass
        , genSmallStatementFlow
        , genSmallStatementImport
        , genSmallStatementGlobal
        , genSmallStatementNonlocal $ ecfg ^. definitionContext
        , genSmallStatementAssert
        ]
  where
    genSmallStatementExpr =
      SmallStatementExpr <$>
      genTest (ecfg & atomType .~ SNotAssignable) <*>
      pure ()

    genSmallStatementAssign =
      SmallStatementAssign <$>
      genTestlistStarExpr genTest genStarExpr (ecfg & atomType .~ SAssignable) <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure Equals)
          (genTestlistStarExpr genTest genStarExpr (ecfg & atomType .~ SAssignable))) <*>
      genBeforeF
        (genBetweenWhitespace $ pure Equals)
        (Gen.choice $
          (case ecfg ^. definitionContext of
             SFunDef SNormal ->
               [ InL <$> genYieldExpr (ecfg & atomType .~ SNotAssignable) ]
             _ -> []) <>
          [ InR <$> genTestlistStarExpr genTest genStarExpr (ecfg & atomType .~ SNotAssignable) ]) <*>
      pure ()

    genSmallStatementAugAssign =
      SmallStatementAugAssign <$>
      genTest (ecfg & atomType .~ SAssignable) <*>
      genBeforeF
        (genBetweenWhitespace genAugAssign)
        (Gen.choice $
          (case ecfg ^. definitionContext of
             SFunDef SNormal ->
               [ InL <$> genYieldExpr (ecfg & atomType .~ SNotAssignable) ]
             _ -> []) <>
          [ InR <$> genTestList (ecfg & atomType .~ SNotAssignable) ]) <*>
      pure ()

    genSmallStatementDel =
      SmallStatementDel <$>
      genWhitespaceBefore1F (genExprList $ ecfg & atomType .~ SAssignable) <*>
      pure ()

    genSmallStatementPass =
      pure $ SmallStatementPass ()

    genSmallStatementFlow =
      SmallStatementFlow <$>
      genFlowStatement scfg ecfg <*>
      pure ()

    genSmallStatementImport =
      SmallStatementImport <$>
      genImportStatement <*>
      pure ()

    genSmallStatementGlobal =
      SmallStatementGlobal <$>
      genWhitespaceBefore1F genIdentifier <*>
      genListF
        (genBeforeF (genBetweenWhitespace $ pure Comma) genIdentifier) <*>
      pure ()

    genSmallStatementNonlocal :: MonadGen m => SDefinitionContext a -> m (SmallStatement b a ())
    genSmallStatementNonlocal dc =
      case dc of
        STopLevel -> error "impossible"
        SFunDef _ ->
          SmallStatementNonlocal <$>
          genWhitespaceBefore1F genIdentifier <*>
          genListF
            (genBeforeF (genBetweenWhitespace $ pure Comma) genIdentifier) <*>
          pure ()

    genSmallStatementAssert =
      SmallStatementAssert <$>
      genWhitespaceBefore1F (genTest $ ecfg & atomType .~ SNotAssignable) <*>
      genMaybeF
        (genBeforeF
          (genBetweenWhitespace $ pure Comma)
          (genTest $ ecfg & atomType .~ SNotAssignable)) <*>
      pure ()

genCompoundStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (CompoundStatement lc dc ())
genCompoundStatement scfg ecfg =
  Gen.small $
  Gen.choice $
    [ CompoundStatementIf <$> genIfStatement scfg ecfg <*> pure ()
    , CompoundStatementWhile <$> genWhileStatement scfg ecfg <*> pure ()
    , CompoundStatementFor <$> genForStatement scfg ecfg <*> pure ()
    , CompoundStatementTry <$> genTryStatement scfg ecfg <*> pure ()
    , CompoundStatementWith <$> genWithStatement scfg ecfg <*> pure ()
    , CompoundStatementFuncDef <$> genFuncDef ecfg (SFunDef SNormal) <*> pure ()
    , CompoundStatementClassDef <$> genClassDef ecfg <*> pure ()
    , CompoundStatementDecorated <$> genDecorated ecfg <*> pure ()
    ] <>
    (case ecfg ^. definitionContext of
       SFunDef SAsync ->
         [ CompoundStatementAsync <$>
           genAsyncStatement scfg (ecfg & definitionContext .~ SFunDef SAsync) <*>
           pure ()
         ]
       _ -> [])

genIfStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (IfStatement lc dc ())
genIfStatement scfg ecfg =
  IfStatement <$>
  genWhitespaceBefore1F (genTest $ ecfg & atomType .~ SNotAssignable) <*>
  genBeforeF
    (genBetweenWhitespace $ pure Colon)
    (Gen.small $ genSuite scfg ecfg) <*>
  genListF
    (Pair <$>
     genWhitespaceBefore1F (genTest $ ecfg & atomType .~ SNotAssignable) <*>
     genBeforeF
       (genBetweenWhitespace $ pure Colon)
       (Gen.small $ genSuite scfg ecfg)) <*>
  genMaybeF
    (genBeforeF
      (genBetweenWhitespace $ pure Colon)
      (Gen.small $ genSuite scfg ecfg)) <*>
  pure ()

genWhileStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (WhileStatement lc dc ())
genWhileStatement scfg ecfg =
  WhileStatement <$>
  genWhitespaceBefore1F (genTest $ ecfg & atomType .~ SNotAssignable) <*>
  genBeforeF
    (genBetweenWhitespace $ pure Colon)
    (Gen.small $ genSuite (scfg & loopContext .~ SInLoop) ecfg) <*>
  genMaybeF
    (genBeforeF
      (genBetweenWhitespace $ pure Colon)
      (Gen.small $ genSuite scfg ecfg)) <*>
  pure ()

genForStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (ForStatement lc dc ())
genForStatement scfg ecfg =
  ForStatement <$>
  genBetweenWhitespace1F (genTestlistStarExpr genExpr genStarExpr $ ecfg & atomType .~ SAssignable) <*>
  genWhitespaceBefore1F (genTestList $ ecfg & atomType .~ SNotAssignable) <*>
  genBeforeF
    (genBetweenWhitespace $ pure Colon)
    (Gen.small $ genSuite (scfg & loopContext .~ SInLoop) ecfg) <*>
  genMaybeF
    (genBeforeF
      (genBetweenWhitespace $ pure Colon)
      (Gen.small $ genSuite scfg ecfg)) <*>
  pure ()

genTryStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (TryStatement lc dc ())
genTryStatement scfg ecfg =
  Gen.choice
    [ TryStatementExcepts <$>
      genBeforeF
        (genBetweenWhitespace $ pure Colon)
        (Gen.small $ genSuite scfg ecfg) <*>
      genNonEmptyF
        (Pair <$>
         genExceptClause ecfg <*>
         genBeforeF
           (genBetweenWhitespace $ pure Colon)
           (Gen.small $ genSuite scfg ecfg)) <*>
      genMaybeF
        (genBeforeF
          (genBetweenWhitespace $ pure Colon)
          (Gen.small $ genSuite scfg ecfg)) <*>
      genMaybeF
        (genBeforeF
          (genBetweenWhitespace $ pure Colon)
          (Gen.small $ genSuite scfg ecfg)) <*>
      pure ()
    , TryStatementFinally <$>
      genBeforeF
        (genBetweenWhitespace $ pure Colon)
        (Gen.small $ genSuite scfg ecfg) <*>
      genBeforeF
        (genBetweenWhitespace $ pure Colon)
        (Gen.small $ genSuite scfg ecfg) <*>
      pure ()
    ]

genExceptClause
  :: MonadGen m
  => ExprConfig as dc
  -> m (ExceptClause dc ())
genExceptClause ecfg =
  ExceptClause <$>
  genMaybeF
    (genWhitespaceBefore1F $ Pair <$>
     genTest (ecfg & atomType .~ SNotAssignable) <*>
     genMaybeF
       (genBeforeF (genBetweenWhitespace1 $ pure KAs) genIdentifier)) <*>
  pure ()

genWithStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (WithStatement lc dc ())
genWithStatement scfg ecfg =
  WithStatement <$>
  genWhitespaceBefore1F (genWithItem ecfg) <*>
  genListF
    (genBeforeF (genBetweenWhitespace $ pure Comma) (genWithItem ecfg)) <*>
  genBeforeF
    (genBetweenWhitespace $ pure Colon)
    (Gen.small $ genSuite scfg ecfg) <*>
  pure ()

genWithItem
  :: MonadGen m
  => ExprConfig as dc
  -> m (WithItem dc ())
genWithItem ecfg =
  WithItem <$>
  genTest (ecfg & atomType .~ SNotAssignable) <*>
  genMaybeF
    (genBeforeF
      (genBetweenWhitespace1 $ pure KAs)
      (genExpr $ ecfg & atomType .~ SAssignable)) <*>
  pure ()

genFuncDef
  :: MonadGen m
  => ExprConfig as dc
  -> SDefinitionContext inner
  -> m (FuncDef dc inner ())
genFuncDef ecfg inner =
  FuncDef <$>
  genWhitespaceBefore1F genIdentifier <*>
  genWhitespaceBeforeF (genParameters ecfg) <*>
  genMaybeF
    (genBeforeF
       (genBetweenWhitespace $ pure RightArrow)
       (genTest $ ecfg
         & atomType .~ SNotAssignable)) <*>
  genBeforeF
    (genBetweenWhitespace $ pure Colon)
    (Gen.small $ genSuite (StatementConfig SNotInLoop) (ecfg & definitionContext .~ inner)) <*>
  pure ()

genParameters
  :: MonadGen m
  => ExprConfig as dc
  -> m (Parameters dc ())
genParameters ecfg =
  Parameters <$>
  genBetweenWhitespaceF
    (genMaybeF
      (genArgsList
        (ecfg & atomType .~ SNotAssignable)
        (genTypedArg ecfg)
        (genTest $ ecfg & atomType .~ SNotAssignable))) <*>
  pure ()

genTypedArg
  :: MonadGen m
  => ExprConfig as dc
  -> m (TypedArg ())
genTypedArg ecfg =
  TypedArg <$>
  genIdentifier <*>
  genMaybeF
    (genBeforeF
      (genBetweenWhitespace $ pure Colon)
      (genTest $ ecfg
        & atomType .~ SNotAssignable
        & definitionContext .~ SFunDef SNormal)) <*>
  pure ()

genClassDef
  :: MonadGen m
  => ExprConfig as dc
  -> m (ClassDef dc ())
genClassDef ecfg =
  ClassDef <$>
  genWhitespaceBefore1F genIdentifier <*>
  genMaybeF
    (genWhitespaceBeforeF .
     genBetweenWhitespaceF .
     genMaybeF $
     genArgumentList (ecfg & atomType .~ SNotAssignable) genIdentifier genTest) <*>
  genBeforeF
    (genBetweenWhitespace $ pure Colon)
    (Gen.small $ genSuite (StatementConfig SNotInLoop) ecfg) <*>
  pure ()

genDecorated
  :: MonadGen m
  => ExprConfig as dc
  -> m (Decorated dc ())
genDecorated ecfg =
  Decorated <$>
  genNonEmptyF (genDecorator ecfg) <*>
  Gen.choice
    [ Gen.small $ InL . InL <$> genClassDef ecfg
    , Gen.small $ InL . InR <$> genFuncDef ecfg (SFunDef SNormal)
    , Gen.small $ InR <$> genAsyncFuncDef ecfg
    ] <*>
  pure ()

genAsyncFuncDef
  :: MonadGen m
  => ExprConfig as dc
  -> m (AsyncFuncDef dc ())
genAsyncFuncDef ecfg =
  AsyncFuncDef <$>
  genWhitespaceBefore1F
    (Gen.small $
     genFuncDef ecfg (SFunDef SAsync)) <*>
  pure ()

genDecorator
  :: MonadGen m
  => ExprConfig as dc
  -> m (Decorator dc ())
genDecorator ecfg =
  Decorator <$>
  genWhitespaceBeforeF genDottedName <*>
  genMaybeF
    (genBetweenWhitespaceF
       (genMaybeF $
        genArgumentList (ecfg & atomType .~ SNotAssignable) genIdentifier genTest)) <*>
  genNewlineChar <*>
  pure ()

genAsyncStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as ('FunDef 'Async)
  -> m (AsyncStatement lc ('FunDef 'Async) ())
genAsyncStatement scfg ecfg =
  AsyncStatement <$>
  genWhitespaceBefore1F
    (Gen.choice
      [ Gen.small $ InL . InL <$> genFuncDef ecfg (SFunDef SAsync)
      , Gen.small $ InL . InR <$> genWithStatement scfg ecfg
      , Gen.small $ InR <$> genForStatement scfg ecfg
      ]) <*>
  pure ()

genSuite
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (Suite lc dc ())
genSuite scfg ecfg =
  Gen.choice
    [ SuiteSingle <$> genSimpleStatement scfg ecfg <*> pure ()
    , SuiteMulti <$>
      genNewlineChar <*>
      (Compose <$>
      genIndentedLines (Range.constant 1 10) (genStatement scfg ecfg)) <*>
      pure ()
    ]
