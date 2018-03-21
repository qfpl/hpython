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


genBlock :: MonadGen m => m (Block '[] ())
genBlock = do
  indent <- NonEmpty.toList <$> genWhitespaces1
  go indent
  where
    go indent =
      Gen.sized $ \n ->
      if n <= 1
        then do
          s1 <- genStatement
          Block <$>
            liftA2 (:|)
              ((,,,) () indent <$> pure s1 <*> Gen.maybe genNewline)
              (pure [])
        else do
          n' <- Gen.integral (Range.constant 1 (n-1))
          s1 <- Gen.resize n' genStatement
          let n'' = n - n'
          b <- Gen.resize n'' (go indent)
          Block <$>
            liftA2 NonEmpty.cons
              ((,,,) () indent <$> pure s1 <*> Gen.maybe genNewline)
              (pure $ unBlock b)

genExpr :: MonadGen m => m (Expr '[] ())
genExpr = Gen.sized $ \n ->
  if n <= 1
  then Gen.choice [Ident () <$> genIdent, genInt, genBool, String () <$> genString]
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
             genWhitespaces <*>
             genIdent)
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genExpr
          b <- Gen.resize (n - n') $ genSizedCommaSep (genArg genExpr)
          Call () a <$> genWhitespaces <*> pure b
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          Gen.subtermM2
            (Gen.resize n' genExpr)
            (Gen.resize (n - n') genExpr)
            (\a b -> BinOp () a <$> genWhitespaces <*> genOp <*> genWhitespaces <*> pure b)
      , Parens () <$> genWhitespaces <*> genExpr <*> genWhitespaces
      ]

genStatement :: MonadGen m => m (Statement '[] ())
genStatement = Gen.sized $ \n ->
  if n <= 1
  then Gen.element [Pass (), Break ()]
  else
    Gen.resize (n-1) $
    Gen.choice
      [ Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' $ genSizedCommaSep (genParam genExpr)
          b <- Gen.resize (n - n') genBlock
          Fundef () <$> genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
            genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
      , Return () <$> genWhitespaces <*> genExpr
      , Expr () <$> genExpr
      , pure $ Pass ()
      , pure $ Break ()
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genExpr
          b <- Gen.resize (n - n') genExpr
          Assign () a <$> genWhitespaces <*> genWhitespaces <*> pure b
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
