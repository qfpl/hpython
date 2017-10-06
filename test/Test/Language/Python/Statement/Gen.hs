module Test.Language.Python.Statement.Gen where

import Papa
import Hedgehog

import Data.Functor.Product
import Data.Functor.Sum
import qualified Hedgehog.Gen as Gen

import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.Statement.AST
import Language.Python.IR.ExprConfig
import Language.Python.IR.StatementConfig

import Test.Language.Python.Expr.Gen
import Test.Language.Python.Gen.Combinators
import Test.Language.Python.Statement.AugAssign

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

genSmallStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (SmallStatement lc dc ())
genSmallStatement scfg ecfg =
  Gen.choice
    [ SmallStatementExpr <$>
      genTestlistStarExpr (ecfg & atomType .~ SAssignable) <*>
      Gen.choice
        [ InL <$>
          genBeforeF
            (genBetweenWhitespace genAugAssign)
            (Gen.choice
              [ InL <$> genYieldExpr (ecfg & atomType .~ SNotAssignable)
              , InR <$> genTestList (ecfg & atomType .~ SNotAssignable)
              ])
        , InR <$>
          genListF
            (genBeforeF
              (genBetweenWhitespace $ pure Equals)
              (Gen.choice
                [ InL <$> genYieldExpr (ecfg & atomType .~ SNotAssignable)
                , InR <$> genTestlistStarExpr (ecfg & atomType .~ SNotAssignable)
                ]))
        ] <*>
      pure ()
    ]

genTestlistStarExpr
  :: MonadGen m
  => ExprConfig as dc
  -> m (TestlistStarExpr as dc ())
genTestlistStarExpr ecfg =
  TestlistStarExpr <$>
  Gen.choice
    [ InL <$> genTest ecfg
    , InR <$> genStarExpr ecfg
    ] <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $ pure Comma)
      (Gen.choice
        [ InL <$> genTest ecfg
        , InR <$> genStarExpr ecfg
        ])) <*>
  Gen.maybe (genBetweenWhitespace $ pure Comma) <*>
  pure ()

genCompoundStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (CompoundStatement lc dc ())
genCompoundStatement scfg ecfg =
  Gen.small $
  Gen.choice
    [ CompoundStatementIf <$> genIfStatement scfg ecfg <*> pure ()
    , CompoundStatementWhile <$> genWhileStatement scfg ecfg <*> pure ()
    , CompoundStatementFor <$> genForStatement scfg ecfg <*> pure ()
    , CompoundStatementTry <$> genTryStatement scfg ecfg <*> pure ()
    , CompoundStatementWith <$> genWithStatement scfg ecfg <*> pure ()
    , CompoundStatementFuncDef <$> genFuncDef scfg ecfg <*> pure ()
    , CompoundStatementClassDef <$> genClassDef scfg ecfg <*> pure ()
    , CompoundStatementDecorated <$> genDecorated scfg ecfg <*> pure ()
    , CompoundStatementAsync <$> genAsyncStatement scfg ecfg <*> pure ()
    ]

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
  genBetweenWhitespace1F (genExprList $ ecfg & atomType .~ SAssignable) <*>
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
    (Pair <$>
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
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (FuncDef dc ())
genFuncDef scfg ecfg = _

genClassDef
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (ClassDef dc ())
genClassDef scfg ecfg = _

genDecorated
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (Decorated dc ())
genDecorated scfg ecfg = _

genAsyncStatement
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (AsyncStatement lc dc ())
genAsyncStatement scfg ecfg = _

genSuite
  :: MonadGen m
  => StatementConfig lc
  -> ExprConfig as dc
  -> m (Suite lc dc ())
genSuite scfg ecfg = _
