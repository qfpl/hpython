{-# options_ghc -fno-warn-type-defaults #-}
{-# language DataKinds #-}
module Generators.Common where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Lens.Fold ((^?!))
import Control.Lens.Prism (_Right)
import Control.Monad ((<=<))
import Data.Digit.Enum (enumOctal, enumDecimal)
import Data.Digit.Hexadecimal.MixedCase
import Data.Digit.Integral
import Data.List.NonEmpty (NonEmpty(..))
import Data.These (These(..))
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Syntax.AugAssign
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comment
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace
import Generators.Sized

genSmallStatement
  :: MonadGen m
  => m (SimpleStatement '[] ())
  -> m (SmallStatement '[] ())
genSmallStatement gss =
  sized2M
    (\a b -> 
      MkSmallStatement a b <$>
      Gen.maybe genWhitespaces <*>
      Gen.maybe genComment <*>
      Gen.maybe genNewline)
    gss
    (sizedList $ (,) <$> genWhitespaces <*> gss)

genSuite
  :: MonadGen m
  => m (SimpleStatement '[] ())
  -> m (Block '[] ())
  -> m (Suite '[] ())
genSuite gss gb =
  Gen.choice
  [ SuiteMany () <$>
    genColon <*>
    Gen.maybe genComment <*>
    genNewline <*>
    gb
  , SuiteOne () <$> genColon <*> genSmallStatement gss
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

genE :: MonadGen m => m E
genE = Gen.element [Ee, EE]

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
      genE <*>
      Gen.maybe (Gen.element [Pos, Neg]) <*>
      genDecs

genImag :: MonadGen m => m (Expr '[] ())
genImag =
  Imag () <$>
  Gen.choice
    [ ImagLiteralInt () <$>
      genDecs <*>
      Gen.bool
    , ImagLiteralFloat () <$>
      genFloatLiteral <*>
      Gen.bool
    ] <*>
  genWhitespaces
  where
    genDecs = Gen.nonEmpty (Range.constant 1 10) (Gen.element enumDecimal)

genFloatLiteral :: MonadGen m => m (FloatLiteral ())
genFloatLiteral =
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
    , FloatLiteralWhole () <$>
      genDecs <*>
      floatExponent
    ]
  where
    genDecs = Gen.nonEmpty (Range.constant 1 10) (Gen.element enumDecimal)
    floatExponent =
      FloatExponent <$>
      genE <*>
      Gen.maybe (Gen.element [Pos, Neg]) <*>
      genDecs

genFloat :: MonadGen m => m (Expr '[] ())
genFloat =
  Float () <$>
  genFloatLiteral <*>
  genWhitespaces

genString :: MonadGen m => m PyChar -> m [PyChar]
genString = Gen.list (Range.constant 0 50)

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
    [ Prefix_u
    , Prefix_U
    ]

genRawStringPrefix :: MonadGen m => m RawStringPrefix
genRawStringPrefix =
  Gen.element
    [ Prefix_r
    , Prefix_R
    ]

genBytesPrefix :: MonadGen m => m BytesPrefix
genBytesPrefix =
  Gen.element
    [ Prefix_b
    , Prefix_B
    ]

genRawBytesPrefix :: MonadGen m => m RawBytesPrefix
genRawBytesPrefix =
  Gen.element
    [ Prefix_br
    , Prefix_Br
    , Prefix_bR
    , Prefix_BR
    , Prefix_rb
    , Prefix_rB
    , Prefix_Rb
    , Prefix_RB
    ]

genComment :: MonadGen m => m (Comment ())
genComment =
  MkComment () <$> Gen.list (Range.linear 0 100) (Gen.filter (`notElem` "\0\r\n") Gen.ascii)

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
  (:|) <$> Gen.element [Space, Tab] <*> genWhitespaces

genNone :: MonadGen m => m (Expr '[] ())
genNone = None () <$> genWhitespaces

genEllipsis :: MonadGen m => m (Expr '[] ())
genEllipsis = Ellipsis () <$> genWhitespaces

genBool :: MonadGen m => m (Expr '[] ())
genBool = Bool () <$> Gen.bool <*> genWhitespaces

genOp :: MonadGen m => m (BinOp ())
genOp = Gen.element $ _opOperator <$> operatorTable

genDot :: MonadGen m => m Dot
genDot = Dot <$> genWhitespaces

genComma :: MonadGen m => m Comma
genComma = Comma <$> genWhitespaces

genColon :: MonadGen m => m Colon
genColon = Colon <$> genWhitespaces

genColonAny :: MonadGen m => m Colon
genColonAny = Colon <$> genAnyWhitespaces

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
            (\b -> CommaSepMany a <$> genComma <*> pure b)
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
            (\b -> CommaSepMany1 a <$> genComma <*> pure b)
      ]

genSizedCommaSep1' :: MonadGen m => m a -> m (CommaSep1' a)
genSizedCommaSep1' ma = Gen.sized $ \n ->
  if n <= 1
  then CommaSepOne1' <$> ma <*> Gen.maybe genComma
  else
    Gen.resize (n-1) $
    Gen.choice
      [ CommaSepOne1' <$> ma <*> Gen.maybe genComma
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' ma
          Gen.subtermM
            (Gen.resize (n - n') $ genSizedCommaSep1' ma)
            (\b -> CommaSepMany1' a <$> genComma <*> pure b)
      ]

genAugAssign :: MonadGen m => m (AugAssign ())
genAugAssign =
  MkAugAssign <$>
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
  genStringType <*>
  genQuoteType <*>
  genString gChar <*>
  genWhitespaces

genBytesLiteral :: MonadGen m => m PyChar -> m (StringLiteral ())
genBytesLiteral gChar =
  BytesLiteral () <$>
  genBytesPrefix <*>
  genStringType <*>
  genQuoteType <*>
  genString gChar <*>
  genWhitespaces

genTupleItem :: MonadGen m => m [Whitespace] -> m (Expr v ()) -> m (TupleItem v ())
genTupleItem ws ge =
  Gen.choice
  [ TupleItem () <$> ge
  , TupleUnpack () <$>
    Gen.list (Range.constant 0 10) ((,) <$> genAnyWhitespaces <*> ws) <*>
    ws <*>
    ge
  ]

genListItem :: MonadGen m => m [Whitespace] -> m (Expr v ()) -> m (ListItem v ())
genListItem ws ge =
  Gen.choice
  [ ListItem () <$>
    ge
  , ListUnpack () <$>
    Gen.list (Range.constant 0 10) ((,) <$> genAnyWhitespaces <*> ws) <*>
    ws <*>
    ge
  ]

genSetItem :: MonadGen m => m [Whitespace] -> m (Expr v ()) -> m (SetItem v ())
genSetItem ws ge =
  Gen.choice
  [ SetItem () <$>
    ge
  , SetUnpack () <$>
    Gen.list (Range.constant 0 10) ((,) <$> genAnyWhitespaces <*> ws) <*>
    ws <*>
    ge
  ]

genDictItem :: MonadGen m => m (Expr v ()) -> m (DictItem v ())
genDictItem ge =
  Gen.choice
  [ DictItem () <$>
    ge <*>
    genColonAny <*>
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

genPyChar :: MonadGen m => m Char -> m PyChar
genPyChar mlit =
  Gen.choice
  [ pure Char_newline
  , Char_octal1 <$>
    Gen.element enumOctal
  , Char_octal2 <$>
    Gen.element enumOctal <*>
    Gen.element enumOctal
  , Char_octal3 <$>
    Gen.element enumOctal <*>
    Gen.element enumOctal <*>
    Gen.element enumOctal
  , Char_hex <$> genHexDigit <*> genHexDigit
  , Char_uni16 <$>
    genHexDigit <*>
    genHexDigit <*>
    genHexDigit <*>
    genHexDigit
  , do
      a <- genHexDigit
      b <- case a of
        HeXDigit1 -> pure HeXDigit0
        _ -> genHexDigit
      Char_uni32 HeXDigit0 HeXDigit0 a b <$>
        genHexDigit <*>
        genHexDigit <*>
        genHexDigit <*>
        genHexDigit
  , pure Char_esc_bslash
  , pure Char_esc_singlequote
  , pure Char_esc_doublequote
  , pure Char_esc_a
  , pure Char_esc_b
  , pure Char_esc_f
  , pure Char_esc_n
  , pure Char_esc_r
  , pure Char_esc_t
  , pure Char_esc_v
  , Char_lit <$> mlit
  ]
