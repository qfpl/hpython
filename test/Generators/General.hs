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
  if n <= 1
  then PositionalParam () <$> genIdent
  else
    Gen.resize (n-1) $
    KeywordParam () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genExpr

genArg :: MonadGen m => m (Expr '[] ()) -> m (Arg '[] ())
genArg genExpr = Gen.sized $ \n ->
  if n <= 1
  then -- error "arg for size 1"
    Gen.choice
      [ PositionalArg () <$> genExpr
      , KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genExpr
      ]
  else
    Gen.resize (n-1) $
    Gen.choice
      [ PositionalArg () <$> genExpr
      , KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genExpr
      ]

genInt :: MonadGen m => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant (-2^32) (2^32)) <*> genWhitespaces

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_']))

genModuleName :: MonadGen m => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne () <$> genIdent ]
  [ ModuleNameMany () <$>
    genIdent <*>
    genWhitespaces <*>
    genWhitespaces <*>
    genModuleName
  ]

genRelativeModuleName :: MonadGen m => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative <$>
    Gen.nonEmpty (Range.constant 1 10) genDot
  , RelativeWithName <$>
    Gen.list (Range.constant 1 10) genDot <*>
    genModuleName
  ]

genImportTargets :: MonadGen m => m (ImportTargets '[] ())
genImportTargets =
  Gen.choice
  [ pure ImportAll
  , ImportSome <$>
    genSizedCommaSep1
      ((,) <$> genIdent <*> Gen.maybe (genAs1 genIdent))
  , ImportSomeParens <$>
    genAnyWhitespaces <*>
    genSizedCommaSep1'
      genAnyWhitespaces
      ((,) <$> genIdent <*> Gen.maybe (genAs1 genIdent)) <*>
    genAnyWhitespaces
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
          s1 <- genStatement
          pure . Block $ ((), indent, s1) :| []
        else do
          n' <- Gen.integral (Range.constant 1 (n-1))
          s1 <- Gen.resize n' genStatement
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
    [ Ident () <$> genIdent <*> genWhitespaces
    , if isExp then genSmallInt else genInt
    , genBool
    , String () <$> genStringType <*> genString <*> genWhitespaces
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
             genIdent <*>
             genWhitespaces)
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
          Gen.subtermM2
            (Gen.resize n' genExpr)
            (Gen.resize (n - n') (genExpr' $ case op of; Exp{} -> True; _ -> False))
            (\a b -> BinOp () a <$> pure op <*> genWhitespaces <*> pure b)
      , Gen.subtermM
          genExpr
          (\a -> Parens () <$> genWhitespaces <*> pure a <*> genWhitespaces)
      , genTuple genExpr
      ]

genSmallStatement :: MonadGen m => m (SmallStatement '[] ())
genSmallStatement = Gen.sized $ \n ->
  if n <= 1
  then Gen.element $ [Pass (), Break ()]
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
          genSizedCommaSep1
            ((,) <$> genModuleName <*> Gen.maybe (genAs1 genIdent))
        , From () <$>
          genWhitespaces <*>
          genRelativeModuleName <*>
          genWhitespaces1 <*>
          genWhitespaces <*>
          genImportTargets
        ]

genCompoundStatement
  :: MonadGen m => m (CompoundStatement '[] ())
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
    ]

genStatement :: MonadGen m => m (Statement '[] ())
genStatement =
  Gen.sized $ \n ->
  if n < 4
  then
    SmallStatements <$>
    genSmallStatement <*>
    pure [] <*>
    Gen.maybe ((,) <$> genWhitespaces <*> genWhitespaces) <*>
    genNewline
  else
    Gen.scale (subtract 1) $
    Gen.choice
    [ CompoundStatement <$> genCompoundStatement
    , Gen.sized $ \n -> do
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
                (,,) <$> genWhitespaces <*> genWhitespaces <*> genSmallStatement)) <*>
          Gen.maybe ((,) <$> genWhitespaces <*> genWhitespaces) <*>
          genNewline
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
          [ Left <$> liftA2 (,) genWhitespaces genNewline
          , Right <$> genStatement
          ]))
