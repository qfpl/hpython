{-# language DataKinds #-}
module Generators.Common where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax

whitespaceSize :: Whitespace -> Size
whitespaceSize Space = 1
whitespaceSize Tab = 1
whitespaceSize (Continued _ ws) = 1 + sum (fmap whitespaceSize ws)
whitespaceSize (Newline _) = 1

genSmallInt :: MonadGen m => m (Expr '[] ())
genSmallInt =
  Int () <$>
  Gen.integral (Range.constant 0 100) <*>
  genWhitespaces

genString :: MonadGen m => m String
genString = Gen.list (Range.constant 0 50) (Gen.filter (/='\0') Gen.latin1)

genNewline :: MonadGen m => m Newline
genNewline = Gen.element [LF, CR, CRLF]

genStringType :: MonadGen m => m StringType
genStringType = Gen.element [ShortString, LongString]

genQuoteType :: MonadGen m => m QuoteType
genQuoteType = Gen.element [SingleQuote, DoubleQuote]

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

genStringPrefix :: MonadGen m => m StringPrefix
genStringPrefix =
  Gen.element
    [ Prefix_r
    , Prefix_R
    , Prefix_u
    , Prefix_U
    ]

genBytesPrefix :: MonadGen m => m BytesPrefix
genBytesPrefix =
  Gen.element
    [ Prefix_b
    , Prefix_B
    , Prefix_br
    , Prefix_Br
    , Prefix_bR
    , Prefix_BR
    , Prefix_rb
    , Prefix_rB
    , Prefix_Rb
    , Prefix_RB
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
  go n
  where
    go 0 = pure []
    go n =
      Gen.choice
      [ (Space :) <$> go (n-1)
      , (Tab :) <$> go (n-1)
      , fmap pure $ Continued <$> genNewline <*> go (n-1)
      ]

genAnyWhitespaces :: MonadGen m => m [Whitespace]
genAnyWhitespaces = do
  n <- Gen.integral (Range.constant 0 10)
  go n
  where
    go 0 = pure []
    go n =
      Gen.choice
      [ (Space :) <$> go (n-1)
      , (Tab :) <$> go (n-1)
      , fmap pure $ Continued <$> genNewline <*> go (n-1)
      , (:) <$> (Newline <$> genNewline) <*> go (n-1)
      ]

genAnyWhitespaces1 :: MonadGen m => m (NonEmpty Whitespace)
genAnyWhitespaces1 = do
  n <- Gen.integral (Range.constant 1 10)
  go n
  where
    go 1 =
      Gen.choice
      [ pure $ pure Space
      , pure $ pure Tab
      , fmap pure $ Continued <$> genNewline <*> pure []
      , fmap pure $ Newline <$> genNewline
      ]
    go n =
      Gen.choice
      [ (Space `NonEmpty.cons`) <$> go (n-1)
      , (Tab `NonEmpty.cons`) <$> go (n-1)
      , fmap pure $
        Continued <$>
        genNewline <*>
        ((:) <$>
         Gen.choice
           [ pure Space
           , pure Tab
           , Newline <$> genNewline
           ] <*>
         (NonEmpty.toList <$> go (n-1)))
      ]

genWhitespaces1 :: MonadGen m => m (NonEmpty Whitespace)
genWhitespaces1 = do
  n <- Gen.integral (Range.constant 0 9)
  (:|) <$> Gen.element [Space, Tab] <*> genWhitespaces
  where
    go 0 = pure []
    go n =
      Gen.choice
      [ (Space :) <$> go (n-1)
      , (Tab :) <$> go (n-1)
      , fmap pure $ Continued <$> genNewline <*> go (n-1)
      ]

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

genAugAssign :: MonadGen m => m (AugAssign ())
genAugAssign =
  Gen.element
    [ PlusEq
    , MinusEq
    , StarEq
    , AtEq
    , SlashEq
    , PercentEq
    , AmphersandEq
    , PipeEq
    , CaretEq
    , ShiftLeftEq
    , ShiftRightEq
    , DoubleStarEq
    , DoubleSlashEq
    ] <*>
  pure () <*>
  genWhitespaces

genStringLiteral :: MonadGen m => m (StringLiteral ())
genStringLiteral =
  StringLiteral () <$>
  Gen.maybe genStringPrefix <*>
  genQuoteType <*>
  genStringType <*>
  genString <*>
  genWhitespaces

genBytesLiteral :: MonadGen m => m (StringLiteral ())
genBytesLiteral =
  BytesLiteral () <$>
  genBytesPrefix <*>
  genQuoteType <*>
  genStringType <*>
  genString <*>
  genWhitespaces

genDictItem :: MonadGen m => m (Expr v ()) -> m (DictItem v ())
genDictItem ge =
  DictItem () <$>
  ge <*>
  genAnyWhitespaces <*>
  ge
