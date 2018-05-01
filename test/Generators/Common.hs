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
whitespaceSize (Newline _) = 1

genSmallInt :: MonadGen m => m (Expr '[] ())
genSmallInt = Int () <$> Gen.integral (Range.constant 0 100) <*> genWhitespaces

genString :: MonadGen m => m String
genString = Gen.list (Range.constant 0 50) (Gen.filter (/='\0') Gen.latin1)

genNewline :: MonadGen m => m Newline
genNewline = Gen.element [LF, CR, CRLF]

genStringType :: MonadGen m => m StringType
genStringType = Gen.element [ShortSingle, ShortDouble, LongSingle, LongDouble]

genAnyWhitespace :: MonadGen m => m Whitespace
genAnyWhitespace = Gen.sized $ \n ->
  if n <= 1
  then Gen.choice [pure Space, pure Tab, Newline <$> genNewline]
  else
    Gen.resize (n-1) $
      Gen.choice
        [ Newline <$> genNewline
        , pure Space
        , pure Tab
        , do
            n' <- Gen.integral (Range.constant 1 (n-1))
            Gen.resize n' $
              Continued <$>
              genNewline <*>
              genSizedWhitespace genAnyWhitespace
        ]

genNormalWhitespace :: MonadGen m => m Whitespace
genNormalWhitespace = Gen.sized $ \n ->
  if n <= 1
  then Gen.element [Space, Tab]
  else
    Gen.resize (n-1) $
    Gen.sized $ \n ->
      Gen.choice
        [ pure Space
        , pure Tab
        , do
            n' <- Gen.integral (Range.constant 1 (n-1))
            Gen.resize n' $
              Continued <$>
              genNewline <*>
              genSizedWhitespace genNormalWhitespace
        ]

genComment :: MonadGen m => m Comment
genComment =
  Comment <$> Gen.list (Range.linear 0 100) (Gen.filter (`notElem` "\0\r\n") Gen.ascii)

genSizedWhitespace :: MonadGen m => m Whitespace -> m [Whitespace]
genSizedWhitespace ws =
  Gen.sized $ \n ->
    if n == 0
    then pure []
    else do
      n' <- Gen.integral (Range.constant 1 (n-1))
      w <- Gen.resize n' ws
      l <- Gen.resize (n - n') (genSizedWhitespace ws)
      pure $ w : l

genWhitespaces :: MonadGen m => m [Whitespace]
genWhitespaces = do
  n <- Gen.integral (Range.constant 0 10)
  Gen.resize n $ genSizedWhitespace genNormalWhitespace

genAnyWhitespaces :: MonadGen m => m [Whitespace]
genAnyWhitespaces = do
  n <- Gen.integral (Range.constant 0 10)
  Gen.resize n $ genSizedWhitespace genAnyWhitespace

genWhitespaces1 :: MonadGen m => m (NonEmpty Whitespace)
genWhitespaces1 = do
  n <- Gen.integral (Range.constant 0 9)
  liftA2
    (:|)
    (head <$> Gen.resize 1 (genSizedWhitespace genNormalWhitespace))
    (Gen.resize n $ genSizedWhitespace genNormalWhitespace)

genNone :: MonadGen m => m (Expr '[] ())
genNone = None () <$> genWhitespaces

genBool :: MonadGen m => m (Expr '[] ())
genBool = Bool () <$> Gen.bool <*> genWhitespaces

genOp :: MonadGen m => m (BinOp ())
genOp = Gen.element $ _opOperator <$> operatorTable

genDot :: MonadGen m => m Dot
genDot = Dot <$> genWhitespaces

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
            (\b -> CommaSepMany a <$> genWhitespaces <*> pure b)
      ]

genTuple :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genTuple expr =
  Tuple () <$>
  expr <*>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' expr)

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
            (\b -> CommaSepMany1 a <$> genWhitespaces <*> pure b)
      ]

genSizedCommaSep1' :: MonadGen m => m a -> m (CommaSep1' a)
genSizedCommaSep1' ma = Gen.sized $ \n ->
  if n <= 1
  then CommaSepOne1' <$> ma <*> Gen.maybe genWhitespaces
  else
    Gen.resize (n-1) $
    Gen.choice
      [ CommaSepOne1' <$> ma <*> Gen.maybe genWhitespaces
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' ma
          Gen.subtermM
            (Gen.resize (n - n') $ genSizedCommaSep1' ma)
            (\b -> CommaSepMany1' a <$> genWhitespaces <*> pure b)
      ]

genImportAs :: MonadGen m => m (e ()) -> m (Ident '[] ()) -> m (ImportAs e '[] ())
genImportAs me genIdent =
  Gen.sized $ \n -> do
    n' <- Gen.integral (Range.constant 1 n)
    let n'' = n - n'
    ImportAs () <$>
      Gen.resize n' me <*>
      (if n'' <= 2
       then pure Nothing
       else fmap Just $ (,) <$> genWhitespaces1 <*> Gen.resize n'' genIdent)
