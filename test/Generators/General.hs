{-# language DataKinds, TypeFamilies #-}
{-# language LambdaCase #-}
module Generators.General where

import Control.Applicative
import Control.Lens.Cons
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Wrapped
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax
import Generators.Common

genParam :: MonadGen m => m (Expr '[] ()) -> m (Param '[] ())
genParam genExpr = Gen.sized $ \n ->
  Gen.choice $
    (if n <= 1
     then PositionalParam () <$> genIdent
     else
       Gen.resize (n-1) $
       KeywordParam () <$> genIdent <*> genWhitespaces <*> genExpr) :
    [ StarParam () <$> genWhitespaces <*> genIdent
    , DoubleStarParam () <$> genWhitespaces <*> genIdent
    ]

genArg :: MonadGen m => m (Expr '[] ()) -> m (Arg '[] ())
genArg genExpr = Gen.sized $ \n ->
  if n <= 1
  then -- error "arg for size 1"
    Gen.choice
      [ PositionalArg () <$> genExpr
      , KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr
      ]
  else
    Gen.resize (n-1) $
    Gen.choice
      [ PositionalArg () <$> genExpr
      , KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr
      ]

genInt :: MonadGen m => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant (-2^32) (2^32)) <*> genWhitespaces

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_'])) <*>
  genWhitespaces

genModuleName :: MonadGen m => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne () <$> genIdent ]
  [ ModuleNameMany () <$>
    genIdent <*>
    genWhitespaces <*>
    genModuleName
  ]

genRelativeModuleName :: MonadGen m => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative <$>
    Gen.nonEmpty (Range.constant 1 10) genDot <*>
    genWhitespaces
  , RelativeWithName <$>
    Gen.list (Range.constant 1 10) genDot <*>
    genModuleName
  ]

genImportTargets :: MonadGen m => m (ImportTargets '[] ())
genImportTargets =
  Gen.choice
  [ ImportAll () <$> genWhitespaces
  , ImportSome () <$>
    genSizedCommaSep1 (genImportAs genIdent genIdent)
  , ImportSomeParens () <$>
    genWhitespaces <*>
    genSizedCommaSep1' (genImportAs genIdent genIdent) <*>
    genWhitespaces
  ]

genBlock :: MonadGen m => m (Block '[] ())
genBlock =
  Gen.shrink ((++) <$> shrinkHead <*> shrinkTail) $ do
  indent <- NonEmpty.toList <$> genWhitespaces1
  go indent
  where
    shrinkHead b
      | Just (_, rest) <- b ^? _Wrapped.to toList._Cons
      , not (null rest) = [Block $ NonEmpty.fromList rest]
      | otherwise = []
    shrinkTail b
      | Just (rest, _) <- b ^? _Wrapped.to toList._Snoc
      , not (null rest) = [Block $ NonEmpty.fromList rest]
      | otherwise = []
    go indent =
      Gen.sized $ \n ->
      if n <= 1
        then do
          s1 <-
            Gen.choice
              [ Right <$> genStatement
              , fmap Left $ (,) <$> Gen.maybe genComment <*> genNewline
              ]
          pure . Block $ ((), indent, s1) :| []
        else do
          n' <- Gen.integral (Range.constant 1 (n-1))
          s1 <-
            Gen.resize n' $
            Gen.choice
              [ Right <$> genStatement
              , fmap Left $ (,) <$> Gen.maybe genComment <*> genNewline
              ]
          let n'' = n - n'
          b <- Gen.resize n'' (go indent)
          pure . Block $ NonEmpty.cons ((), indent, s1) (unBlock b)

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: MonadGen m => m (Expr '[] ())
genExpr = genExpr' False

genExpr' :: MonadGen m => Bool -> m (Expr '[] ())
genExpr' isExp = Gen.sized $ \n ->
  if n <= 1
  then
    Gen.choice
    [ genBool
    , if isExp then genSmallInt else genInt
    , Ident () <$> genIdent
    , String () <$>
      Gen.maybe genStringPrefix <*>
      genStringType <*>
      genString <*>
      genWhitespaces
    ]
  else
    Gen.resize (n-1) $
    Gen.choice $
      [ List () <$>
        genWhitespaces <*>
        genSizedCommaSep genExpr <*>
        genWhitespaces
      , Gen.subtermM
          genExpr
          (\a ->
             Deref () <$>
             pure a <*>
             genWhitespaces <*>
             genIdent)
      , Gen.shrink
          (\case
              Call () a _ (CommaSepOne b) _ -> [a, _argExpr b]
              Call () a _ _ _ -> [a]
              _ -> []) $
        Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genExpr
          b <- Gen.resize (n - n') $ genSizedCommaSep (genArg genExpr)
          Call () a <$> genWhitespaces <*> pure b <*> genWhitespaces
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          op <- genOp
          Gen.subterm2
            (Gen.resize n' genExpr)
            (Gen.resize (n - n') (genExpr' $ case op of; Exp{} -> True; _ -> False))
            (\a b -> BinOp () a op b)
      , Gen.subtermM
          (genExpr' isExp)
          (\a -> Parens () <$> genWhitespaces <*> pure a <*> genWhitespaces)
      , genTuple genExpr
      , Not () <$> genWhitespaces <*> genExpr
      ]

genSmallStatement :: MonadGen m => m (SmallStatement '[] ())
genSmallStatement = Gen.sized $ \n ->
  if n <= 1
  then Gen.element $ [Pass (), Break (), Continue ()]
  else
    Gen.resize (n-1) .
      Gen.choice $
        [ Expr () <$> genExpr
        , pure $ Pass ()
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 1 (n-1))
            a <- Gen.resize n' genExpr
            b <- Gen.resize (n - n') genExpr
            Assign () a <$> genWhitespaces <*> genWhitespaces <*> pure b
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            Global () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 genIdent)
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            Del () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 genIdent)
        , pure (Break ())
        , pure (Continue ())
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            Nonlocal () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 genIdent)
        , Return () <$>
          genWhitespaces <*>
          genExpr
        , Import () <$>
          genWhitespaces1 <*>
          genSizedCommaSep1 (genImportAs genModuleName genIdent)
        , From () <$>
          genWhitespaces <*>
          genRelativeModuleName <*>
          genWhitespaces <*>
          genImportTargets
        ]

genCompoundStatement
  :: MonadGen m
  => m (CompoundStatement '[] ())
genCompoundStatement =
  Gen.sized $ \n ->
  Gen.resize (n-1) .
  Gen.choice $
    [ Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 (n-1))
        a <- Gen.resize n' $ genSizedCommaSep (genParam genExpr)
        let paramIdents = a ^.. folded.paramName.identValue
        b <- Gen.resize (n - n') genBlock
        Fundef () <$> genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
    , Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 (n-1))
        n'' <- Gen.integral (Range.constant 0 (n-n'))
        a <- Gen.resize n' genExpr
        b <- Gen.resize (n - n') genBlock
        c <-
          if n - n' - n'' == 0
          then pure Nothing
          else
            fmap Just $
            (,,,) <$>
            genWhitespaces <*>
            genWhitespaces <*>
            genNewline <*>
            Gen.resize (n - n' - n'') genBlock
        If () <$> genWhitespaces <*> pure a <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b <*> pure c
    , Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 (n-1))
        a <- Gen.resize n' genExpr
        b <- Gen.resize (n - n') genBlock
        While () <$> genWhitespaces <*> pure a <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
    , Gen.sized $ \n -> do
        sz <- Gen.integral (Range.constant 1 5)
        n1 <- Gen.integral (Range.constant 1 $ n - 2)
        n2 <- Gen.integral (Range.constant 1 $ n - n1 - 1)
        n3 <- Gen.integral (Range.constant 1 $ n - n2 - n1)
        let remaining = n - n1 - n2 - n3
        (e1, e2) <-
          if remaining > 0
          then do
            n4 <- Gen.integral (Range.constant 0 remaining)
            e1 <- Gen.resize n4 genBlock
            e2 <- Gen.maybe (Gen.resize (remaining - n4) genBlock)
            (,) <$>
              fmap Just
                ((,,,) <$> genWhitespaces <*> genWhitespaces <*> genNewline <*> pure e1) <*>
              maybe
                 (pure Nothing)
                 (\e2' ->
                    fmap Just $
                    (,,,) <$> genWhitespaces <*> genWhitespaces <*> genNewline <*> pure e2')
                 e2
          else pure (Nothing, Nothing)
        TryExcept () <$>
          genWhitespaces <*> genWhitespaces <*> genNewline <*>
          Gen.resize n1 genBlock <*>
          genWhitespaces <*>
          Gen.nonEmpty
            (Range.singleton sz)
            (ExceptAs () <$>
             Gen.resize n2 genExpr <*>
             Gen.maybe ((,) <$> genWhitespaces <*> genIdent)) <*>
          genWhitespaces <*>
          genNewline <*>
          Gen.resize n3 genBlock <*>
          pure e1 <*>
          pure e2
    , Gen.sized $ \n -> do
        n1 <- Gen.integral (Range.constant 1 $ n-1)
        n2 <- Gen.integral (Range.constant 1 n1)
        TryFinally () <$>
          genWhitespaces <*> genWhitespaces <*> genNewline <*>
          Gen.resize n1 genBlock <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*>
          Gen.resize n2 genBlock
    , Gen.sized $ \n -> do
        n1 <- Gen.integral $ Range.constant 0 (n-1)
        ClassDef () <$>
          genWhitespaces1 <*>
          genIdent <*>
          Gen.maybe
            ((,,) <$>
             genWhitespaces <*>
             (if n1 == 0
              then pure Nothing
              else fmap Just . Gen.resize n1 $ genSizedCommaSep1 (genArg genExpr)) <*>
             genWhitespaces) <*>
          genWhitespaces <*> genNewline <*>
          Gen.resize (n - n1 - 1) genBlock
    ] ++
    [ Gen.sized $ \n -> do
        n1 <- Gen.integral $ Range.constant 1 (max 1 $ n-2)
        n2 <- Gen.integral $ Range.constant 1 (max 1 $ n-n1-1)
        n3 <- Gen.integral $ Range.constant 1 (max 1 $ n-n1-n2)
        n4 <- Gen.integral $ Range.constant 0 (max 0 $ n-n1-n2-n3)
        For () <$>
          genWhitespaces <*>
          Gen.resize n1 genExpr <*>
          genWhitespaces <*>
          Gen.resize n2 genExpr <*>
          genWhitespaces <*> genNewline <*>
          Gen.resize n3 genBlock <*>
          if n4 == 0
          then pure Nothing
          else
            Gen.resize n4
              (fmap Just $
               (,,,) <$>
               genWhitespaces <*> genWhitespaces <*>
               genNewline <*> genBlock)
    | n >= 4
    ]

genStatement :: MonadGen m => m (Statement '[] ())
genStatement =
  Gen.sized $ \n ->
  if n < 4
  then
    SmallStatements <$>
    genSmallStatement <*>
    pure [] <*>
    Gen.maybe genWhitespaces <*>
    genNewline
  else
    Gen.scale (subtract 1) $
    Gen.choice
    [ Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 n)
        n'' <- Gen.integral (Range.constant 0 (n-n'))
        SmallStatements <$>
          Gen.resize n' genSmallStatement <*>
          (if n'' == 0
           then pure []
           else
             Gen.list
               (Range.singleton $ unSize n'')
               (Gen.resize ((n-n') `div` n'') $
                (,) <$> genWhitespaces <*> genSmallStatement)) <*>
          Gen.maybe genWhitespaces <*>
          genNewline
    , CompoundStatement <$> genCompoundStatement
    ]

genModule :: MonadGen m => m (Module '[] ())
genModule = Gen.sized $ \n -> do
  num <- Gen.integral (Range.constant 1 n)
  Module <$>
    Gen.list
      (Range.singleton $ unSize num)
      (Gen.resize
         (n `div` num)
         (Gen.choice
          [ fmap Left $ (,,) <$> genWhitespaces <*> Gen.maybe genComment <*> genNewline
          , Right <$> genStatement
          ]))
