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

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_']))

genNone :: MonadGen m => m (Expr '[] ())
genNone = pure $ None ()

genBool :: MonadGen m => m (Expr '[] ())
genBool = Bool () <$> Gen.bool

genOp :: MonadGen m => m (BinOp ())
genOp = Gen.element $ _opOperator <$> operatorTable

genInt :: MonadGen m => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant (-2^16) (2^16))
