{-# language DataKinds #-}
module Generators.Common where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Applicative
import Data.List.NonEmpty (NonEmpty(..))

import Language.Python.Internal.Syntax

whitespaceSize :: Whitespace -> Size
whitespaceSize Space = 1
whitespaceSize Tab = 1
whitespaceSize (Continued _ ws) = 1 + sum (fmap whitespaceSize ws)

genSmallInt :: MonadGen m => m (Expr '[] ())
genSmallInt = Int () <$> Gen.integral (Range.constant 0 100)

genString :: MonadGen m => m String
genString = Gen.list (Range.constant 0 50) (Gen.filter (/='\0') Gen.latin1)

genNewline :: MonadGen m => m Newline
genNewline = Gen.element [LF, CR, CRLF]

genSizedWhitespace :: MonadGen m => m [Whitespace]
genSizedWhitespace = Gen.sized $ \n ->
  if n == 0
  then pure []
  else if n == 1
  then Gen.element [[Space], [Tab]]
  else
    Gen.resize (n-1) $ do
      w <-
        Gen.choice
          [ pure Space
          , pure Tab
          , do
              n' <- Gen.integral (Range.constant 1 (n-1))
              Gen.resize n' $ Continued <$> genNewline <*> genSizedWhitespace
          ]
      l <- Gen.resize (n - whitespaceSize w) genSizedWhitespace
      pure $ w : l

genWhitespaces :: MonadGen m => m [Whitespace]
genWhitespaces = do
  n <- Gen.integral (Range.constant 0 10)
  Gen.resize n genSizedWhitespace

genWhitespaces1 :: MonadGen m => m (NonEmpty Whitespace)
genWhitespaces1 = do
  n <- Gen.integral (Range.constant 0 9)
  liftA2 (:|) (head <$> Gen.resize 1 genSizedWhitespace) (Gen.resize n genSizedWhitespace)

genNone :: MonadGen m => m (Expr '[] ())
genNone = pure $ None ()

genBool :: MonadGen m => m (Expr '[] ())
genBool = Bool () <$> Gen.bool

genOp :: MonadGen m => m (BinOp ())
genOp = Gen.element $ _opOperator <$> operatorTable

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
