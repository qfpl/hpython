{-# language DataKinds #-}
module Generators.Common where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Lens.Fold ((^?!))
import Control.Lens.Prism (_Right)
import Control.Monad ((<=<))
import Data.Digit.Enum (enumDecimal)
import Data.Digit.HeXaDeCiMaL
import Data.Digit.Integral
import Data.List.NonEmpty (NonEmpty(..))
import Data.These (These(..))
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax

whitespaceSize :: Whitespace -> Size
whitespaceSize Space = 1
whitespaceSize Tab = 1
whitespaceSize (Continued _ ws) = 1 + sum (fmap whitespaceSize ws)
whitespaceSize (Newline _) = 1

genSuite :: MonadGen m => m (SmallStatement '[] ()) -> m (Block '[] ()) -> m (Suite '[] ())
genSuite gss gb =
  Gen.choice
  [ SuiteMany () <$>
    genWhitespaces <*>
    genNewline <*>
    gb
  , SuiteOne () <$>
    genWhitespaces <*>
    gss <*>
    genNewline
  ]

genUnOp :: MonadGen m => m (UnOp ())
genUnOp =
  Gen.element [Negate (), Positive (), Complement ()] <*>
  genWhitespaces

integralHeXaDeCiMaL'
  :: (MonadGen m, Integral a)
  => a
  -> m HeXDigit
integralHeXaDeCiMaL' 0 = pure HeXDigit0
integralHeXaDeCiMaL' 1 = pure HeXDigit1
integralHeXaDeCiMaL' 2 = pure HeXDigit2
integralHeXaDeCiMaL' 3 = pure HeXDigit3
integralHeXaDeCiMaL' 4 = pure HeXDigit4
integralHeXaDeCiMaL' 5 = pure HeXDigit5
integralHeXaDeCiMaL' 6 = pure HeXDigit6
integralHeXaDeCiMaL' 7 = pure HeXDigit7
integralHeXaDeCiMaL' 8 = pure HeXDigit8
integralHeXaDeCiMaL' 9 = pure HeXDigit9
integralHeXaDeCiMaL' 10 = Gen.element [ HeXDigita, HeXDigitA ]
integralHeXaDeCiMaL' 11 = Gen.element [ HeXDigitb, HeXDigitB ]
integralHeXaDeCiMaL' 12 = Gen.element [ HeXDigitc, HeXDigitC ]
integralHeXaDeCiMaL' 13 = Gen.element [ HeXDigitd, HeXDigitD ]
integralHeXaDeCiMaL' 14 = Gen.element [ HeXDigite, HeXDigitE ]
integralHeXaDeCiMaL' 15 = Gen.element [ HeXDigitf, HeXDigitF ]
integralHeXaDeCiMaL' _ = Gen.discard

integralHeXDigits
  :: (MonadGen m, Integral a)
  => a
  -> m (Either (NonEmpty HeXDigit) (NonEmpty HeXDigit))
integralHeXDigits n =
  if n >= 0
  then Right . NonEmpty.fromList <$> go n []
  else Left . NonEmpty.fromList <$> go (-n - 1) []
  where
    go k =
      let
        (q, r) = quotRem k 16
      in
        (if q == 0 then pure else go q) <=< (\rest -> (: rest) <$> integralHeXaDeCiMaL' r)

genSmallInt :: MonadGen m => m (Expr '[] ())
genSmallInt = do
  n <- Gen.integral (Range.constant 0 100)
  Int () <$>
    Gen.choice
    [ pure $ IntLiteralDec () (integralDecDigits n ^?! _Right)
    , IntLiteralBin () <$> Gen.bool <*> pure (integralBinDigits n ^?! _Right)
    , IntLiteralOct () <$> Gen.bool <*> pure (integralOctDigits n ^?! _Right)
    , IntLiteralHex () <$> Gen.bool <*> ((^?! _Right) <$> integralHeXDigits n)
    ] <*>
    genWhitespaces

genUnit :: MonadGen m => m (Expr '[] ())
genUnit = Unit () <$> genAnyWhitespaces <*> genWhitespaces

genInt :: MonadGen m => m (Expr '[] ())
genInt = do
  n <- Gen.integral (Range.constant (-2^32) (2^32))
  let
    f = if n < 0 then (\a -> UnOp () <$> (Negate () <$> genWhitespaces) <*> a) else id
    n' = if n < 0 then -n - 1 else n
  f $
    Int () <$>
    Gen.choice
      [ pure $ IntLiteralDec () (integralDecDigits n' ^?! _Right)
      , IntLiteralBin () <$> Gen.bool <*> pure (integralBinDigits n' ^?! _Right)
      , IntLiteralOct () <$> Gen.bool <*> pure (integralOctDigits n' ^?! _Right)
      , IntLiteralHex () <$> Gen.bool <*> ((^?! _Right) <$> integralHeXDigits n')
      ] <*>
    genWhitespaces

genSmallFloat :: MonadGen m => m (Expr '[] ())
genSmallFloat =
  Float () <$>
  Gen.choice
    [ FloatLiteralFull () <$>
      genDecs <*>
      Gen.maybe
        (Gen.choice
           [ This <$> genDecs
           , That <$> floatExponent
           , These <$> genDecs <*> floatExponent
           ])
    , FloatLiteralPoint () <$>
      genDecs <*>
      Gen.maybe floatExponent
    ] <*>
  genWhitespaces
  where
    genDecs = Gen.nonEmpty (Range.constant 1 3) (Gen.element enumDecimal)
    floatExponent =
      FloatExponent <$>
      Gen.bool <*>
      Gen.maybe (Gen.element [Pos, Neg]) <*>
      genDecs

genFloat :: MonadGen m => m (Expr '[] ())
genFloat =
  Float () <$>
  Gen.choice
    [ FloatLiteralFull () <$>
      genDecs <*>
      Gen.maybe
        (Gen.choice
           [ This <$> genDecs
           , That <$> floatExponent
           , These <$> genDecs <*> floatExponent
           ])
    , FloatLiteralPoint () <$>
      genDecs <*>
      Gen.maybe floatExponent
    ] <*>
  genWhitespaces
  where
    genDecs = Gen.nonEmpty (Range.constant 1 10) (Gen.element enumDecimal)
    floatExponent =
      FloatExponent <$>
      Gen.bool <*>
      Gen.maybe (Gen.element [Pos, Neg]) <*>
      genDecs

genString :: MonadGen m => m PyChar -> m [PyChar]
genString = Gen.list (Range.constant 0 50)

genNewline' :: MonadGen m => m Newline
genNewline' =
  Gen.element
    [ LF Nothing
    , CR Nothing
    , CRLF Nothing
    ]

genNewline :: MonadGen m => m Newline
genNewline =
  Gen.choice
    [ LF <$> Gen.maybe genComment
    , CR <$> Gen.maybe genComment
    , CRLF <$> Gen.maybe genComment
    ]

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
              genNewline' <*>
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
              genNewline' <*>
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
      , Gen.shrink (\_ -> [[Space]]) . fmap pure $ Continued <$> genNewline' <*> go (n-1)
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
      , Gen.shrink (\_ -> [[Space]]) . fmap pure $ Continued <$> genNewline' <*> go (n-1)
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
      , fmap pure $ Continued <$> genNewline' <*> pure []
      , fmap pure $ Newline <$> genNewline
      ]
    go n =
      Gen.choice
      [ (Space `NonEmpty.cons`) <$> go (n-1)
      , (Tab `NonEmpty.cons`) <$> go (n-1)
      , fmap pure $
        Continued <$>
        genNewline' <*>
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
      , fmap pure $ Continued <$> genNewline' <*> go (n-1)
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
    , AmpersandEq
    , PipeEq
    , CaretEq
    , ShiftLeftEq
    , ShiftRightEq
    , DoubleStarEq
    , DoubleSlashEq
    ] <*>
  pure () <*>
  genWhitespaces

genStringLiteral :: MonadGen m => m PyChar -> m (StringLiteral ())
genStringLiteral gChar =
  StringLiteral () <$>
  Gen.maybe genStringPrefix <*>
  genQuoteType <*>
  genStringType <*>
  genString gChar <*>
  genWhitespaces

genBytesLiteral :: MonadGen m => m PyChar -> m (StringLiteral ())
genBytesLiteral gChar =
  BytesLiteral () <$>
  genBytesPrefix <*>
  genQuoteType <*>
  genStringType <*>
  genString gChar <*>
  genWhitespaces

genDictItem :: MonadGen m => m (Expr v ()) -> m (DictItem v ())
genDictItem ge =
  Gen.choice
  [ DictItem () <$>
    ge <*>
    genAnyWhitespaces <*>
    ge
  , DictUnpack () <$>
    genAnyWhitespaces <*>
    ge
  ]

genHexDigit :: MonadGen m => m HeXDigit
genHexDigit =
  Gen.element
  [ HeXDigit0
  , HeXDigit1
  , HeXDigit2
  , HeXDigit3
  , HeXDigit4
  , HeXDigit5
  , HeXDigit6
  , HeXDigit7
  , HeXDigit8
  , HeXDigit9
  , HeXDigita
  , HeXDigitA
  , HeXDigitb
  , HeXDigitB
  , HeXDigitc
  , HeXDigitC
  , HeXDigitd
  , HeXDigitD
  , HeXDigite
  , HeXDigitE
  , HeXDigitf
  , HeXDigitF
  ]
