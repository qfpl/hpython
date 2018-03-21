{-# language DataKinds #-}
module Generators.General where

import Control.Applicative
import Control.Lens (sumOf, folded, _3, to)
import Data.List.NonEmpty (NonEmpty(..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax
import Generators.Common

genString :: MonadGen m => m String
genString = Gen.list (Range.constant 0 50) (Gen.filter (/='\0') Gen.latin1)

blockSize :: Block v a -> Size
blockSize (Block b) = sumOf (folded._3.to statementSize) b

argSize :: Arg v a -> Size
argSize (PositionalArg _ a) = 1 + exprSize a
argSize (KeywordArg _ _ _ _ a) = 1 + exprSize a

paramSize :: Param v a -> Size
paramSize (PositionalParam _ _) = 1
paramSize (KeywordParam _ _ _ _ a) = 1 + exprSize a

commaSepSize :: (a -> Size) -> CommaSep a -> Size
commaSepSize _ CommaSepNone = 1
commaSepSize f (CommaSepOne a) = 1 + f a
commaSepSize f (CommaSepMany a _ _ c) = 1 + f a + commaSepSize f c

exprSize :: Expr v a -> Size
exprSize (List _ _ a _) = 1 + commaSepSize exprSize a
exprSize (Deref _ a _ _ _) = 1 + exprSize a
exprSize (Call _ a _ b) = 1 + exprSize a + commaSepSize argSize b
exprSize (None _) = 1
exprSize (BinOp _ a _ _ _ b) = 1 + exprSize a + exprSize b
exprSize (Negate _ _ a) = 1 + exprSize a
exprSize (Parens _ _ a _) = 1 + exprSize a
exprSize Ident{} = 1
exprSize Int{} = 1
exprSize Bool{} = 1
exprSize String{} = 1

genSizedArg :: MonadGen m => m (Arg '[] ())
genSizedArg = Gen.sized $ \n ->
  if n <= 1
  then -- error "arg for size 1"
    Gen.choice
      [ PositionalArg () <$> genSizedExpr
      , KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genSizedExpr
      ]
  else
    Gen.resize (n-1) $
    Gen.choice
      [ PositionalArg () <$> genSizedExpr
      , KeywordArg () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genSizedExpr
      ]

genSizedParam :: MonadGen m => m (Param '[] ())
genSizedParam = Gen.sized $ \n ->
  if n <= 1
  then PositionalParam () <$> genIdent
  else
    Gen.resize (n-1) $
    KeywordParam () <$> genIdent <*> genWhitespaces <*> genWhitespaces <*> genSizedExpr

genSizedBlock :: MonadGen m => m (Block '[] ())
genSizedBlock = do
  indent <- NonEmpty.toList <$> genWhitespaces1
  go indent
  where
    go indent =
      Gen.sized $ \n ->
      if n <= 1
        then do
          s1 <- genSizedStatement
          Block <$>
            liftA2 (:|)
              ((,,,) () indent <$> pure s1 <*> Gen.maybe genNewline)
              (pure [])
        else do
          n' <- Gen.integral (Range.constant 1 (n-1))
          s1 <- Gen.resize n' genSizedStatement
          let n'' = n - n'
          b <- Gen.resize n'' (go indent)
          Block <$>
            liftA2 NonEmpty.cons
              ((,,,) () indent <$> pure s1 <*> Gen.maybe genNewline)
              (pure $ unBlock b)

genSizedCommaSep :: MonadGen m => m a -> m (CommaSep a)
genSizedCommaSep ma = Gen.sized $ \n ->
  if n <= 1
  then pure CommaSepNone
  else
    Gen.resize (n-1) $
    Gen.choice
      [ CommaSepOne <$> ma
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' ma
          Gen.subtermM
            (Gen.resize (n - n') $ genSizedCommaSep ma)
            (\b -> CommaSepMany a <$> genWhitespaces <*> genWhitespaces <*> pure b)
      ]

genSizedCommaSep1 :: MonadGen m => m a -> m (CommaSep1 a)
genSizedCommaSep1 ma = Gen.sized $ \n ->
  if n <= 1
  then CommaSepOne1 <$> ma
  else
    Gen.resize (n-1) $
    Gen.choice
      [ CommaSepOne1 <$> ma
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' ma
          Gen.subtermM
            (Gen.resize (n - n') $ genSizedCommaSep1 ma)
            (\b -> CommaSepMany1 a <$> genWhitespaces <*> genWhitespaces <*> pure b)
      ]

genSizedExpr :: MonadGen m => m (Expr '[] ())
genSizedExpr = Gen.sized $ \n ->
  if n <= 1
  then Gen.choice [Ident () <$> genIdent, genInt, genBool, String () <$> genString]
  else
    Gen.resize (n-1) $
    Gen.choice $
      [ List () <$>
        genWhitespaces <*>
        genSizedCommaSep genSizedExpr <*>
        genWhitespaces
      , Gen.subtermM
          genSizedExpr
          (\a ->
             Deref () <$>
             pure a <*>
             genWhitespaces <*>
             genWhitespaces <*>
             genIdent)
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genSizedExpr
          b <- Gen.resize (n - n') $ genSizedCommaSep genSizedArg
          Call () a <$> genWhitespaces <*> pure b
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          Gen.subtermM2
            (Gen.resize n' genSizedExpr)
            (Gen.resize (n - n') genSizedExpr)
            (\a b -> BinOp () a <$> genWhitespaces <*> genOp <*> genWhitespaces <*> pure b)
      , Parens () <$> genWhitespaces <*> genSizedExpr <*> genWhitespaces
      ]

statementSize :: Statement v a -> Size
statementSize (Fundef _ _ _ _ a _ _ _ b) = 1 + commaSepSize paramSize a + blockSize b
statementSize (Return _ _ a) = 1 + exprSize a
statementSize (Expr _ a) = 1 + exprSize a
statementSize (If _ _ a _ _ _ b c) =
  1 + exprSize a + blockSize b + maybe 0 (\(_, _, _, b) -> blockSize b) c
statementSize (While _ _ a _ _ _ b) = 1 + exprSize a + blockSize b
statementSize (Assign _ a _ _ b) = 1 + exprSize a + exprSize b
statementSize Pass{} = 1
statementSize Break{} = 1
statementSize (Global _ _ cs) = Size $ 1 + length cs
statementSize (Nonlocal _ _ cs) = Size $ 1 + length cs
statementSize (Del _ _ cs) = Size $ 1 + length cs

genSizedStatement :: MonadGen m => m (Statement '[] ())
genSizedStatement = Gen.sized $ \n ->
  if n <= 1
  then Gen.element [Pass (), Break ()]
  else
    Gen.resize (n-1) $
    Gen.choice
      [ Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' $ genSizedCommaSep genSizedParam
          b <- Gen.resize (n - n') genSizedBlock
          Fundef () <$> genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
            genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
      , Return () <$> genWhitespaces <*> genSizedExpr
      , Expr () <$> genSizedExpr
      , pure $ Pass ()
      , pure $ Break ()
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genSizedExpr
          b <- Gen.resize (n - n') genSizedExpr
          Assign () a <$> genWhitespaces <*> genWhitespaces <*> pure b
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          n'' <- Gen.integral (Range.constant 0 (n-n'))
          a <- Gen.resize n' genSizedExpr
          b <- Gen.resize (n - n') genSizedBlock
          c <-
            if n - n' - n'' == 0
            then pure Nothing
            else
              fmap Just $
              (,,,) <$>
              genWhitespaces <*>
              genWhitespaces <*>
              genNewline <*>
              Gen.resize (n - n' - n'') genSizedBlock
          If () <$> genWhitespaces <*> pure a <*>
            genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b <*> pure c
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genSizedExpr
          b <- Gen.resize (n - n') genSizedBlock
          While () <$> genWhitespaces <*> pure a <*>
            genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 2 (n-1))
          Global () <$>
            genWhitespaces1 <*>
            Gen.resize n' (genSizedCommaSep1 genIdent)
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 2 (n-1))
          Nonlocal () <$>
            genWhitespaces1 <*>
            Gen.resize n' (genSizedCommaSep1 genIdent)
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 2 (n-1))
          Del () <$>
            genWhitespaces1 <*>
            Gen.resize n' (genSizedCommaSep1 genIdent)
      ]
