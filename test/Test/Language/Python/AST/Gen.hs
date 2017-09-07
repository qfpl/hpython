{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
module Test.Language.Python.AST.Gen where

import Papa

import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After (After(..))
import Data.Separated.Before (Before(..))
import Data.Separated.Between (Between(..), Between'(..))
import Hedgehog

import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Python.AST as AST
import qualified Language.Python.AST.BytesLiteral as AST
import qualified Language.Python.AST.BytesPrefix as AST
import qualified Language.Python.AST.CompOperator as AST
import qualified Language.Python.AST.Digits as AST
import qualified Language.Python.AST.EscapeSeq as AST
import qualified Language.Python.AST.FactorOperator as AST
import qualified Language.Python.AST.Float as AST
import qualified Language.Python.AST.Identifier as AST
import qualified Language.Python.AST.Imag as AST
import qualified Language.Python.AST.Integer as AST
import qualified Language.Python.AST.Keywords as AST
import qualified Language.Python.AST.LongBytesChar as AST
import qualified Language.Python.AST.LongBytes as AST
import qualified Language.Python.AST.LongStringChar as AST
import qualified Language.Python.AST.LongString as AST
import qualified Language.Python.AST.ShortBytes as AST
import qualified Language.Python.AST.ShortBytesChar as AST
import qualified Language.Python.AST.ShortStringChar as AST
import qualified Language.Python.AST.ShortString as AST
import qualified Language.Python.AST.StringLiteral as AST
import qualified Language.Python.AST.StringPrefix as AST
import qualified Language.Python.AST.Symbols as AST
import qualified Language.Python.AST.TermOperator as AST
import Language.Python.Parser.IR.SyntaxConfig

predNat :: (Ord a, Num a) => a -> a
predNat n = if n <= 0 then n else n - 1

genBefore :: MonadGen m => m s -> m a -> m (Before s a)
genBefore = liftA2 Before

genBeforeF
  :: MonadGen m
  => m s
  -> m (f a)
  -> m (Compose (Before s) f a)
genBeforeF ms = fmap Compose . genBefore ms

genAfter
  :: MonadGen m
  => m s -> m a -> m (After s a)
genAfter = liftA2 After

genAfterF
  :: MonadGen m
  => m s
  -> m (f a)
  -> m (Compose (After s) f a)
genAfterF ms = fmap Compose . genAfter ms

genBetween
  :: MonadGen m
  => m s -> m t -> m a -> m (Between s t a)
genBetween ms mt ma = Between <$> ms <*> ma <*> mt

genBetweenF
  :: MonadGen m
  => m s
  -> m t
  -> m (f a)
  -> m (Compose (Between s t) f a)
genBetweenF ms mt = fmap Compose . genBetween ms mt

genBetween'
  :: MonadGen m
  => m s -> m a -> m (Between' s a)
genBetween' ms ma = Between' <$> genBetween ms ms ma

genNewlineChar
  :: MonadGen m
  => m AST.NewlineChar
genNewlineChar = Gen.element [AST.CR, AST.LF, AST.CRLF]

genWhitespaceChar
  :: MonadGen m
  => m AST.WhitespaceChar
genWhitespaceChar =
  Gen.choice
    [ pure AST.Space
    , pure AST.Tab
    , AST.Continued <$> genNewlineChar
    ]

genListF
  :: MonadGen m
  => m (f a) -> m (Compose [] f a)
genListF ma =
  Compose <$>
  Gen.list (Range.linear 0 10) ma

genNonEmptyF
  :: MonadGen m
  => m (f a) -> m (Compose NonEmpty f a)
genNonEmptyF ma =
  Compose <$>
  Gen.nonEmpty (Range.linear 1 10) ma

genMaybeF
  :: MonadGen m
  => m (f a) -> m (Compose Maybe f a)
genMaybeF ma = Compose <$> Gen.maybe ma

genWhitespace1
  :: MonadGen m
  => m (NonEmpty AST.WhitespaceChar)
genWhitespace1 = Gen.nonEmpty (Range.linear 1 10) genWhitespaceChar

genWhitespaceBefore
  :: MonadGen m
  => m a
  -> m (Before [AST.WhitespaceChar] a)
genWhitespaceBefore ma = Before <$> genWhitespace <*> ma

genWhitespaceBeforeF
  :: MonadGen m
  => m (f a)
  -> m (Compose (Before [AST.WhitespaceChar]) f a)
genWhitespaceBeforeF = fmap Compose . genWhitespaceBefore

genWhitespaceBefore1
  :: MonadGen m
  => m a
  -> m (Before (NonEmpty AST.WhitespaceChar) a)
genWhitespaceBefore1 ma = Before <$> genWhitespace1 <*> ma

genWhitespaceBefore1F
  :: MonadGen m
  => m (f a)
  -> m (Compose (Before (NonEmpty AST.WhitespaceChar)) f a)
genWhitespaceBefore1F = fmap Compose . genWhitespaceBefore1

genWhitespaceAfter
  :: MonadGen m
  => m a
  -> m (After [AST.WhitespaceChar] a)
genWhitespaceAfter ma = After <$> genWhitespace <*> ma

genWhitespaceAfterF
  :: MonadGen m
  => m (f a)
  -> m (Compose (After [AST.WhitespaceChar]) f a)
genWhitespaceAfterF = fmap Compose . genWhitespaceAfter

genWhitespaceAfter1
  :: MonadGen m
  => m a
  -> m (After (NonEmpty AST.WhitespaceChar) a)
genWhitespaceAfter1 ma = After <$> genWhitespace1 <*> ma

genWhitespaceAfter1F
  :: MonadGen m
  => m (f a)
  -> m (Compose (After (NonEmpty AST.WhitespaceChar)) f a)
genWhitespaceAfter1F = fmap Compose . genWhitespaceAfter1

genWhitespace
  :: MonadGen m
  => m [AST.WhitespaceChar]
genWhitespace = Gen.list (Range.linear 0 10) genWhitespaceChar

genBetweenWhitespace
  :: MonadGen m
  => m a
  -> m (Between' [AST.WhitespaceChar] a)
genBetweenWhitespace = genBetween' genWhitespace

genBetweenWhitespaceF
  :: MonadGen m
  => m (f a)
  -> m (Compose (Between' [AST.WhitespaceChar]) f a)
genBetweenWhitespaceF = fmap Compose . genBetweenWhitespace

genBetweenWhitespace1
  :: MonadGen m
  => m a
  -> m (Between' (NonEmpty AST.WhitespaceChar) a)
genBetweenWhitespace1 = genBetween' genWhitespace1

genBetweenWhitespace1F
  :: MonadGen m
  => m (f a)
  -> m (Compose (Between' (NonEmpty AST.WhitespaceChar)) f a)
genBetweenWhitespace1F = fmap Compose . genBetweenWhitespace1

genIfThenElse
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.IfThenElse 'AST.NotAssignable ctxt ())
genIfThenElse cfg =
  AST.IfThenElse <$>
  genBetweenWhitespace1 (pure AST.KIf) <*>
  Gen.small (genOrTest cfg) <*>
  genBetweenWhitespace1 (pure AST.KElse) <*>
  Gen.small (genTest cfg)

genTermOp :: MonadGen m => m AST.TermOperator
genTermOp =
  Gen.element
    [ AST.TermMult
    , AST.TermAt
    , AST.TermFloorDiv
    , AST.TermDiv
    , AST.TermMod
    ]

genStarExpr
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.StarExpr atomType ctxt ())
genStarExpr cfg =
  AST.StarExpr <$>
  genWhitespaceBeforeF
    (Gen.scale predNat . genExpr $ cfg & atomType .~ AST.SAssignable) <*>
  pure ()

genTestlistComp
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.TestlistComp atomType ctxt ())
genTestlistComp cfg =
  case cfg ^. atomType of
    AST.SAssignable ->
      Gen.choice [ Gen.scale predNat $ testlistCompList cfg ]
    AST.SNotAssignable ->
      Gen.choice
        [ Gen.scale predNat $ testlistCompList cfg
        , AST.TestlistCompFor <$>
          Gen.small (genTestOrStar cfg) <*>
          genWhitespaceBeforeF (Gen.small $ genCompFor cfg) <*>
          pure ()
        ]
  where
    testlistCompList cfg' =
      AST.TestlistCompList <$>
      genTestOrStar cfg' <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Comma) .
          Gen.scale predNat $ genTestOrStar cfg') <*>
      Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
      pure ()

    genTestOrStar cfg' =
      Gen.scale predNat $
      Gen.choice [ InL <$> genTest cfg', InR <$> genStarExpr cfg' ]

genTestList
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.TestList atomType ctxt ())
genTestList cfg =
  AST.TestList <$>
    Gen.small (genTest cfg) <*>
    genBeforeF
      (genBetweenWhitespace $ pure AST.Comma)
      (Gen.small $ genTest cfg) <*>
    Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
    pure ()

genYieldArg
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.YieldArg atomType ctxt ())
genYieldArg cfg =
  case cfg ^. atomType of
    AST.SAssignable -> Gen.scale predNat $ Gen.choice [ yieldArgList cfg ]
    AST.SNotAssignable ->
      Gen.scale predNat $
      Gen.choice
        [ yieldArgList cfg
        , AST.YieldArgFrom <$> genWhitespaceBefore1F (genTest cfg) <*> pure ()
        ]
  where
    yieldArgList cfg' =
      Gen.scale predNat $
      AST.YieldArgList <$> genTestList cfg' <*> pure ()

genYieldExpr
  :: MonadGen m
  => SyntaxConfig atomType cfg
  -> m (AST.YieldExpr ())
genYieldExpr cfg =
  Gen.scale predNat $
  AST.YieldExpr <$>
  genMaybeF
    (genWhitespaceBefore1F . genYieldArg $
     cfg
       & atomType .~ AST.SNotAssignable
       & exprContext .~ AST.SFunDef AST.SNormal) <*>
  pure ()

genDictOrSetMaker
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.DictOrSetMaker atomType ctxt ())
genDictOrSetMaker _ = pure AST.DictOrSetMaker

genDigit :: MonadGen m => m AST.Digit
genDigit =
  Gen.element
    [ AST.Digit_0
    , AST.Digit_1
    , AST.Digit_2
    , AST.Digit_3
    , AST.Digit_4
    , AST.Digit_5
    , AST.Digit_6
    , AST.Digit_7
    , AST.Digit_8
    , AST.Digit_9
    ]

genNonZeroDigit :: MonadGen m => m AST.NonZeroDigit
genNonZeroDigit =
  Gen.element
    [ AST.NonZeroDigit_1
    , AST.NonZeroDigit_2
    , AST.NonZeroDigit_3
    , AST.NonZeroDigit_4
    , AST.NonZeroDigit_5
    , AST.NonZeroDigit_6
    , AST.NonZeroDigit_7
    , AST.NonZeroDigit_8
    , AST.NonZeroDigit_9
    ]
    
genOctDigit :: MonadGen m => m AST.OctDigit
genOctDigit =
  Gen.element
    [ AST.OctDigit_0
    , AST.OctDigit_1
    , AST.OctDigit_2
    , AST.OctDigit_3
    , AST.OctDigit_4
    , AST.OctDigit_5
    , AST.OctDigit_6
    , AST.OctDigit_7
    ]
    
genHexDigit :: MonadGen m => m AST.HexDigit
genHexDigit =
  Gen.element
    [ AST.HexDigit_0
    , AST.HexDigit_1
    , AST.HexDigit_2
    , AST.HexDigit_3
    , AST.HexDigit_4
    , AST.HexDigit_5
    , AST.HexDigit_6
    , AST.HexDigit_7
    , AST.HexDigit_8
    , AST.HexDigit_9
    , AST.HexDigit_a
    , AST.HexDigit_A
    , AST.HexDigit_b
    , AST.HexDigit_B
    , AST.HexDigit_c
    , AST.HexDigit_C
    , AST.HexDigit_d
    , AST.HexDigit_D
    , AST.HexDigit_e
    , AST.HexDigit_E
    , AST.HexDigit_f
    , AST.HexDigit_F
    ]

genBinDigit :: MonadGen m => m AST.BinDigit
genBinDigit = Gen.element [AST.BinDigit_0, AST.BinDigit_1]

genInteger :: MonadGen m => m (AST.Integer' ())
genInteger =
  Gen.choice
    [ AST.IntegerDecimal <$>
      Gen.choice
        [ Left <$>
          liftA2 (,) genNonZeroDigit (Gen.list (Range.linear 0 10) genDigit)
        , Right <$> Gen.nonEmpty (Range.linear 1 10) (pure AST.Zero)
        ] <*>
      pure ()
    , AST.IntegerOct <$>
      genBefore
        (Gen.element [Left AST.Char_o, Right AST.Char_O])
        (Gen.nonEmpty (Range.linear 1 10) genOctDigit) <*>
      pure ()
    , AST.IntegerHex <$>
      genBefore
        (Gen.element [Left AST.Char_x, Right AST.Char_X])
        (Gen.nonEmpty (Range.linear 1 10) genHexDigit) <*>
      pure ()
    , AST.IntegerBin <$>
      genBefore
        (Gen.element [Left AST.Char_b, Right AST.Char_B])
        (Gen.nonEmpty (Range.linear 1 10) genBinDigit) <*>
      pure ()
    ]

genImag :: MonadGen m => m (AST.Imag ())
genImag =
  AST.Imag <$>
  genAfterF
    (Gen.choice [pure $ Left AST.Char_j, pure $ Right AST.Char_J])
    (Gen.choice
      [ InL <$> genFloat
      , InR . Const <$> Gen.nonEmpty (Range.linear 1 10) genDigit
      ]) <*>
  pure ()

genFloat :: MonadGen m => m (AST.Float' ())
genFloat =
  Gen.choice
    [ AST.FloatNoDecimal <$>
      someDigits <*>
      Gen.maybe
        (genBefore genE someDigits) <*>
      pure ()
    , AST.FloatDecimalNoBase <$>
      someDigits <*>
      Gen.maybe (genBefore genE someDigits) <*>
      pure ()
    , AST.FloatDecimalBase <$>
      someDigits <*>
      genMaybeF someDigits <*>
      Gen.maybe (genBefore genE someDigits) <*>
      pure ()
    ]
  where
    someDigits = Gen.nonEmpty (Range.linear 0 10) genDigit
    genE = Gen.element [Left AST.Char_e, Right AST.Char_E]

genStringPrefix :: MonadGen m => m AST.StringPrefix
genStringPrefix =
  Gen.element
    [ AST.StringPrefix_r
    , AST.StringPrefix_u
    , AST.StringPrefix_R
    , AST.StringPrefix_U
    ]

genShortStringCharSingle
  :: MonadGen m
  => m (AST.ShortStringChar AST.SingleQuote)
genShortStringCharSingle =
  Gen.just (fmap (^? AST._ShortStringCharSingle) Gen.ascii)

genShortStringCharDouble
  :: MonadGen m
  => m (AST.ShortStringChar AST.DoubleQuote)
genShortStringCharDouble =
  Gen.just (fmap (^? AST._ShortStringCharDouble) Gen.ascii)

genShortString :: MonadGen m => m (AST.ShortString ())
genShortString =
  Gen.choice
    [ AST.ShortStringSingle <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genShortStringCharSingle
          , Right <$> genEscapeSeq
          ]) <*>
      pure ()
    , AST.ShortStringDouble <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genShortStringCharDouble
          , Right <$> genEscapeSeq
          ])<*>
      pure ()
    ]

genLongStringChar
  :: MonadGen m
  => m AST.LongStringChar
genLongStringChar =
  Gen.just (fmap (^? AST._LongStringChar) Gen.ascii)

genLongString :: MonadGen m => m (AST.LongString ())
genLongString =
  Gen.choice
    [ AST.LongStringSingle <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genLongStringChar
          , Right <$> genEscapeSeq
          ]) <*>
      pure ()
    , AST.LongStringDouble <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genLongStringChar
          , Right <$> genEscapeSeq
          ])<*>
      pure ()
    ]

genStringLiteral :: MonadGen m => m (AST.StringLiteral ())
genStringLiteral =
  AST.StringLiteral <$>
  genBeforeF
    (Gen.maybe genStringPrefix)
    (Gen.choice [InL <$> genShortString, InR <$> genLongString]) <*>
  pure ()

genLongBytesChar
  :: MonadGen m
  => m AST.LongBytesChar
genLongBytesChar =
  Gen.just (fmap (^? AST._LongBytesChar) Gen.ascii)

genLongBytes :: MonadGen m => m (AST.LongBytes ())
genLongBytes =
  Gen.choice
    [ AST.LongBytesSingle <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> Gen.ascii
          , Right <$> genEscapeSeq
          ]) <*>
      pure ()
    , AST.LongBytesDouble <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> Gen.ascii
          , Right <$> genEscapeSeq
          ])<*>
      pure ()
    ]

genShortBytesCharSingle
  :: MonadGen m
  => m (AST.ShortBytesChar AST.SingleQuote)
genShortBytesCharSingle =
  Gen.just (fmap (^? AST._ShortBytesCharSingle) Gen.ascii)
  
genShortBytesCharDouble
  :: MonadGen m
  => m (AST.ShortBytesChar AST.DoubleQuote)
genShortBytesCharDouble =
  Gen.just (fmap (^? AST._ShortBytesCharDouble) Gen.ascii)

genEscapeSeq
  :: MonadGen m
  => m AST.EscapeSeq
genEscapeSeq =
  Gen.choice
    [ pure AST.Slash_newline
    , pure AST.Slash_backslash
    , pure AST.Slash_singlequote
    , pure AST.Slash_doublequote
    , pure AST.Slash_a
    , pure AST.Slash_f
    , pure AST.Slash_b
    , pure AST.Slash_n
    , pure AST.Slash_r
    , pure AST.Slash_t
    , pure AST.Slash_v
    , AST.Slash_octal <$> Gen.nonEmpty (Range.linear 1 10) genOctDigit
    , AST.Slash_hex <$> Gen.nonEmpty (Range.linear 1 10) genHexDigit
    ]

genShortBytes :: MonadGen m => m (AST.ShortBytes ())
genShortBytes =
  Gen.choice
    [ AST.ShortBytesSingle <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genShortBytesCharSingle
          , Right <$> genEscapeSeq
          ]) <*>
      pure ()
    , AST.ShortBytesDouble <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genShortBytesCharDouble
          , Right <$> genEscapeSeq
          ])<*>
      pure ()
    ]

genBytesLiteral :: MonadGen m => m (AST.BytesLiteral ())
genBytesLiteral =
  AST.BytesLiteral <$>
  Gen.element
    [ AST.BytesPrefix_b
    , AST.BytesPrefix_B
    , AST.BytesPrefix_br
    , AST.BytesPrefix_Br
    , AST.BytesPrefix_bR
    , AST.BytesPrefix_BR
    , AST.BytesPrefix_rb
    , AST.BytesPrefix_rB
    , AST.BytesPrefix_Rb
    , AST.BytesPrefix_RB
    ] <*>
  Gen.choice [ InL <$> genShortBytes, InR <$> genLongBytes ] <*>
  pure ()

genAtom
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.Atom atomType ctxt ())
genAtom cfg =
  case (cfg ^. atomType, cfg ^. exprContext) of
    (AST.SNotAssignable, AST.SFunDef AST.SNormal) ->
      Gen.recursive Gen.choice
        normalNonRec
        (normalRec cfg ++ [ genAtomParenYield cfg ])
    (AST.SNotAssignable, _) ->
      Gen.recursive Gen.choice
        (normalNonRec ++
          [ genAtomInteger cfg
          , genAtomFloat cfg
          , genAtomString cfg
          , genAtomImag cfg
          , genAtomEllipsis cfg
          , genAtomNone cfg
          , genAtomTrue cfg
          , genAtomFalse cfg
          ])
        (normalRec cfg)
    _ -> Gen.recursive Gen.choice normalNonRec $ normalRec cfg
  where
    normalNonRec = 
      [ AST.AtomIdentifier <$> genIdentifier <*> pure () ]

    normalRec cfg' =
      [ genAtomParenNoYield cfg'
      , genAtomBracket cfg'
      -- , AST.AtomCurly <$>
      -- genBetweenWhitespaceF (genMaybeF genDictOrSetMaker) <*>
      -- pure ()  
      ]

    genAtomBracket cfg' =
      AST.AtomBracket <$>
      genBetweenWhitespaceF (genMaybeF $ genTestlistComp cfg') <*>
      pure ()

    genAtomParenNoYield cfg' =
      AST.AtomParenNoYield <$>
      genBetweenWhitespaceF
        (genMaybeF $ genTestlistComp cfg') <*>
      pure ()

    genAtomInteger _ = AST.AtomInteger <$> genInteger <*> pure ()
    genAtomImag _ =
      AST.AtomImag <$>
      genWhitespaceBeforeF genImag <*>
      pure ()
    genAtomFloat _ = AST.AtomFloat <$> genFloat <*> pure ()

    genAtomString cfg' =
      AST.AtomString <$>
      genStringOrBytes cfg' <*>
      genListF (genWhitespaceBeforeF $ genStringOrBytes cfg') <*>
      pure ()  
    genAtomEllipsis _ = pure $ AST.AtomEllipsis ()
    genAtomNone _ = pure $ AST.AtomNone ()
    genAtomTrue _ = pure $ AST.AtomTrue ()
    genAtomFalse _ = pure $ AST.AtomFalse ()

    genAtomParenYield cfg' = 
      AST.AtomParenYield <$>
      genBetweenWhitespaceF (genYieldExpr cfg') <*>
      pure ()

    genStringOrBytes _ =
      Gen.choice [ InL <$> genStringLiteral, InR <$> genBytesLiteral ]

genVarargsList
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.VarargsList atomType ctxt ())
genVarargsList _ = pure AST.VarargsList

genLambdefNocond
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.LambdefNocond atomType ctxt ())
genLambdefNocond cfg =
  AST.LambdefNocond <$>
  genMaybeF
    (Gen.small . genBetweenF genWhitespace1 genWhitespace $
      genVarargsList cfg) <*>
  genWhitespaceBeforeF (Gen.small $ genTestNocond cfg) <*>
  pure ()

genTestNocond
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.TestNocond atomType ctxt ())
genTestNocond cfg =
  Gen.scale predNat $
  AST.TestNocond <$>
  Gen.choice [ InL <$> genOrTest cfg {-, InR <$> genLambdefNocond -} ] <*>
  pure ()

genCompIf
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.CompIf 'AST.NotAssignable ctxt ())
genCompIf cfg =
  AST.CompIf <$>
  genBetweenWhitespace1 (pure AST.KIf) <*>
  (Gen.small $ genTestNocond cfg) <*>
  genMaybeF
    (Gen.small . genWhitespaceBeforeF $ genCompIter cfg) <*>
  pure ()

genCompIter
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.CompIter 'AST.NotAssignable ctxt ())
genCompIter cfg =
  Gen.scale predNat $
  AST.CompIter <$>
  Gen.choice [ InL <$> genCompFor cfg, InR <$> genCompIf cfg ] <*>
  pure ()

genExprList
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.ExprList atomType ctxt ())
genExprList cfg =
  AST.ExprList <$>
  Gen.small (genSumOrStar cfg) <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $ pure AST.Comma) . Gen.small $
        genSumOrStar cfg) <*>
  pure ()
  where
    genSumOrStar cfg' =
      Gen.choice [InL <$> genExpr cfg, InR <$> genStarExpr cfg']

genCompFor
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.CompFor 'AST.NotAssignable ctxt ())
genCompFor cfg =
  AST.CompFor <$>
  genBeforeF
    (genBetweenWhitespace1 $ pure AST.KFor)
    (genWhitespaceAfter1F . Gen.small . genExprList $
      cfg & atomType .~ AST.SAssignable) <*>
  genWhitespaceBefore1F (Gen.small $ genOrTest cfg) <*>
  genMaybeF (genWhitespaceBeforeF . Gen.small $ genCompIter cfg) <*>
  pure ()

genArgument
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.Argument 'AST.NotAssignable ctxt ())
genArgument cfg =
  Gen.choice
    [ AST.ArgumentFor <$>
      Gen.small (genTest cfg) <*>
      genMaybeF (Gen.small . genWhitespaceBeforeF $ genCompFor cfg) <*>
      pure ()
    , AST.ArgumentDefault <$>
      genWhitespaceAfterF
        (Gen.small . genTest $ cfg & atomType .~ AST.SAssignable) <*>
      genWhitespaceBeforeF (Gen.small $ genTest cfg) <*>
      pure ()
    , AST.ArgumentUnpack <$>
      Gen.element [Left AST.Asterisk, Right AST.DoubleAsterisk] <*>
      genWhitespaceBeforeF (Gen.scale predNat $ genTest cfg) <*>
      pure ()
    ]

genArgList
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.ArgList 'AST.NotAssignable ctxt ())
genArgList cfg =
  AST.ArgList <$>
  Gen.small (genArgument $ cfg & atomType .~ AST.SNotAssignable) <*>
  genListF
    (genBeforeF
       (genBetweenWhitespace $ pure AST.Comma)
       (Gen.small . genArgument $ cfg & atomType .~ AST.SNotAssignable)) <*>
  Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
  pure ()

genSliceOp
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.SliceOp 'AST.NotAssignable ctxt ())
genSliceOp cfg =
  AST.SliceOp <$>
  genMaybeF (Gen.scale predNat . genWhitespaceBeforeF $ genTest cfg) <*>
  pure ()

genSubscript
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.Subscript 'AST.NotAssignable ctxt ())
genSubscript cfg =
  Gen.choice
    [ AST.SubscriptTest <$> Gen.scale predNat (genTest cfg) <*> pure ()
    , AST.SubscriptSlice <$>
      genWhitespaceAfterF (genMaybeF . Gen.small $ genTest cfg) <*>
      genWhitespaceAfter (pure AST.Colon) <*>
      genMaybeF (Gen.small . genWhitespaceAfterF $ genTest cfg) <*>
      genMaybeF (Gen.small . genWhitespaceAfterF $ genSliceOp cfg) <*>
      pure ()
    ]

genSubscriptList
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.SubscriptList 'AST.NotAssignable ctxt ())
genSubscriptList cfg =
  AST.SubscriptList <$>
  Gen.small (genSubscript cfg) <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $ pure AST.Comma)
      (Gen.small $ genSubscript cfg)) <*>
  Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
  pure ()

genIdentifier :: MonadGen m => m (AST.Identifier ())
genIdentifier =
  AST.Identifier <$>
  (T.pack <$> Gen.list
    (Range.linear 1 10)
    (Gen.frequency [(1, Gen.upper), (1, Gen.lower), (26, pure '_')])) <*>
  pure ()

genTrailer
  :: MonadGen m
  => SyntaxConfig 'AST.NotAssignable ctxt
  -> m (AST.Trailer 'AST.NotAssignable ctxt ())
genTrailer cfg =
  Gen.recursive Gen.choice
    [ AST.TrailerAccess <$>
      genWhitespaceBeforeF genIdentifier <*>
      pure ()
    ]
    [ AST.TrailerCall <$>
      genBetweenWhitespaceF (genMaybeF $ genArgList cfg) <*>
      pure ()
    , AST.TrailerSubscript <$>
      genBetweenWhitespaceF (genSubscriptList cfg) <*>
      pure ()
    ]

genAtomExpr
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.AtomExpr atomType ctxt ())
genAtomExpr cfg =
  case (cfg ^. atomType, cfg ^. exprContext) of
    (AST.SNotAssignable, AST.SFunDef AST.SAsync) ->
      Gen.choice
        [ atomExprNoAwait cfg
        , AST.AtomExprAwait <$>
          genWhitespaceAfter1 (pure AST.KAwait) <*>
          Gen.small (genAtom cfg) <*>
          genListF (Gen.small . genWhitespaceBeforeF $ genTrailer cfg) <*>
          pure ()
        ]
    _ -> atomExprNoAwait cfg
  where
    atomExprNoAwait cfg' =
      AST.AtomExprNoAwait <$>
      Gen.small (genAtom cfg') <*>
      genListF
        (Gen.small . genWhitespaceBeforeF . genTrailer $
          cfg' & atomType .~ AST.SNotAssignable) <*>
      pure ()

genPower
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.Power atomType ctxt ())
genPower cfg =
  case cfg ^. atomType of
    AST.SAssignable -> powerOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ powerOne cfg
        , AST.PowerMany <$>
          Gen.small (genAtomExpr cfg) <*>
          genBeforeF
            (Gen.small . genBetweenWhitespace $ pure AST.DoubleAsterisk)
            (Gen.small $ genFactor cfg) <*>
          pure ()
        ]
  where
    powerOne cfg' =
      AST.PowerOne <$>
      Gen.scale predNat (genAtomExpr cfg') <*>
      pure ()

genFactorOp :: MonadGen m => m AST.FactorOperator
genFactorOp =
  Gen.element
    [ AST.FactorNeg
    , AST.FactorPos
    , AST.FactorInv
    ]

genFactor
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.Factor atomType ctxt ())
genFactor cfg =
  case cfg ^. atomType of
    AST.SAssignable -> factorNone cfg
    AST.SNotAssignable ->
      Gen.choice
        [ factorNone cfg
        , AST.FactorOne <$>
          genWhitespaceAfter genFactorOp <*>
          Gen.small (genFactor cfg) <*>
          pure ()
        ]
  where
    factorNone cfg' =
      AST.FactorNone <$>
      Gen.scale predNat (genPower cfg') <*>
      pure ()

genTerm
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.Term atomType ctxt ())
genTerm cfg =
  case cfg ^. atomType of
    AST.SAssignable -> termOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ termOne cfg
        , AST.TermMany <$>
            Gen.small (genFactor cfg) <*>
            genNonEmptyF
              (genBeforeF
                (Gen.small $ genBetweenWhitespace genTermOp)
                (Gen.small $ genFactor cfg)) <*>
            pure ()
        ]
  where
    termOne cfg' =
      AST.TermOne <$>
      Gen.scale predNat (genFactor cfg') <*>
      pure ()

genArithExpr
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.ArithExpr atomType ctxt ())
genArithExpr cfg =
  case cfg ^. atomType of
    AST.SAssignable -> arithExprOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ arithExprOne cfg
        , AST.ArithExprMany <$>
          Gen.small (genTerm cfg) <*>
          genNonEmptyF
            (genBeforeF
              (genBetweenWhitespace $
                Gen.element [Left AST.Plus, Right AST.Minus])
              (Gen.small $ genTerm cfg)) <*>
          pure ()
        ]
  where
    arithExprOne cfg' =
      AST.ArithExprOne <$>
      Gen.scale predNat (genTerm cfg') <*>
      pure ()

genShiftExpr
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.ShiftExpr atomType ctxt ())
genShiftExpr cfg =
  case cfg ^. atomType of
    AST.SAssignable -> shiftExprOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ shiftExprOne cfg
        , AST.ShiftExprMany <$>
          Gen.small (genArithExpr cfg) <*>
          genNonEmptyF
            (genBeforeF
              (genBetweenWhitespace $
                  Gen.element [Left AST.DoubleLT, Right AST.DoubleGT])
              (Gen.small $ genArithExpr cfg)) <*>
          pure ()
        ]
  where
    shiftExprOne cfg' =
      AST.ShiftExprOne <$>
      Gen.scale predNat (genArithExpr cfg') <*>
      pure ()

genAndExpr
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.AndExpr atomType ctxt ())
genAndExpr cfg =
  case cfg ^. atomType of
    AST.SAssignable -> andExprOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ andExprOne cfg
        , AST.AndExprMany <$>
          Gen.small (genShiftExpr cfg) <*>
          genNonEmptyF
            (genBeforeF
              (genBetweenWhitespace $ pure AST.Ampersand)
              (Gen.small $ genShiftExpr cfg)) <*>
          pure ()
        ]
  where
    andExprOne cfg' =
      AST.AndExprOne <$>
      Gen.scale predNat (genShiftExpr cfg') <*>
      pure ()

genXorExpr
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.XorExpr atomType ctxt ())
genXorExpr cfg =
  case cfg ^. atomType of
    AST.SAssignable -> xorExprOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ xorExprOne cfg
        , AST.XorExprMany <$>
          Gen.small (genAndExpr cfg) <*>
          genNonEmptyF
            (genBeforeF
              (genBetweenWhitespace $ pure AST.Caret)
              (Gen.small $ genAndExpr cfg)) <*>
          pure ()
        ]
  where
    xorExprOne cfg' =
      AST.XorExprOne <$>
      Gen.scale predNat (genAndExpr cfg') <*>
      pure ()
genExpr
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.Expr atomType ctxt ())
genExpr cfg =
  case cfg ^. atomType of
    AST.SAssignable -> exprOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ exprOne cfg
        , AST.ExprMany <$>
          Gen.small (genXorExpr cfg) <*>
          genNonEmptyF
            (genBeforeF
              (genBetweenWhitespace $ pure AST.Pipe)
              (Gen.small $ genXorExpr cfg)) <*>
          pure ()
        ]
  where
    exprOne cfg' =
      AST.ExprOne <$>
      Gen.scale predNat (genXorExpr cfg') <*>
      pure ()

genCompOperator :: MonadGen m => m AST.CompOperator
genCompOperator =
  Gen.choice
    [ AST.CompLT <$> genWhitespace <*> genWhitespace
    , AST.CompGT <$> genWhitespace <*> genWhitespace
    , AST.CompEq <$> genWhitespace <*> genWhitespace
    , AST.CompGEq <$> genWhitespace <*> genWhitespace
    , AST.CompLEq <$> genWhitespace <*> genWhitespace
    , AST.CompNEq <$> genWhitespace <*> genWhitespace
    , AST.CompIs <$> genWhitespace1 <*> genWhitespace1
    , AST.CompIn <$> genWhitespace1 <*> genWhitespace1
    , AST.CompIsNot <$> genWhitespace1 <*> genWhitespace1 <*> genWhitespace1
    , AST.CompNotIn <$> genWhitespace1 <*> genWhitespace1 <*> genWhitespace1
    ]

genComparison
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.Comparison atomType ctxt ())
genComparison cfg =
  case cfg ^. atomType of
    AST.SAssignable -> comparisonOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ comparisonOne cfg
        , AST.ComparisonMany <$>
          Gen.small (genExpr cfg) <*>
          genNonEmptyF
            (genBeforeF
              genCompOperator
              (Gen.small $ genExpr cfg)) <*>
          pure ()
        ]
  where
    comparisonOne cfg' =
      AST.ComparisonOne <$>
      Gen.scale predNat (genExpr cfg') <*>
      pure ()

genNotTest
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.NotTest atomType ctxt ())
genNotTest cfg =
  case cfg ^. atomType of
    AST.SAssignable -> notTestOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ notTestOne cfg
        , AST.NotTestMany <$>
          genBeforeF
            (genWhitespaceAfter1 $ pure AST.KNot)
            (Gen.scale predNat $ genNotTest cfg) <*>
          pure ()
        ]
  where
    notTestOne cfg' =
      AST.NotTestOne <$>
      Gen.scale predNat (genComparison cfg') <*>
      pure ()

genAndTest
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.AndTest atomType ctxt ())
genAndTest cfg =
  case cfg ^. atomType of
    AST.SAssignable -> andTestOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ andTestOne cfg
        , AST.AndTestMany <$>
          Gen.small (genNotTest cfg) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' genWhitespace1 $ pure AST.KAnd)
              (Gen.small $ genAndTest cfg)) <*>
          pure ()
        ]
  where
    andTestOne cfg' =
      AST.AndTestOne <$>
      Gen.scale predNat (genNotTest cfg') <*>
      pure ()

genOrTest
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.OrTest atomType ctxt ())
genOrTest cfg =
  case cfg ^. atomType of
    AST.SAssignable -> orTestOne cfg
    AST.SNotAssignable ->
      Gen.choice
        [ orTestOne cfg
        , AST.OrTestMany <$>
          Gen.small (genAndTest cfg) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' genWhitespace1 $ pure AST.KOr)
              (Gen.small $ genAndTest cfg)) <*>
          pure ()
        ]
  where
    orTestOne cfg' =
      AST.OrTestOne <$>
      Gen.scale predNat (genAndTest cfg') <*>
      pure ()

genTest
  :: MonadGen m
  => SyntaxConfig atomType ctxt
  -> m (AST.Test atomType ctxt ())
genTest cfg =
  case cfg ^. atomType of
    AST.SAssignable -> testCondNoIf cfg
    AST.SNotAssignable ->
      Gen.choice
        [ testCondNoIf cfg
        , AST.TestCondIf <$>
          Gen.small (genOrTest cfg) <*>
          genBeforeF
            genWhitespace1
            (Gen.small $ genIfThenElse cfg) <*>
          pure ()
        -- , pure AST.TestLambdef
        ]
  where
    testCondNoIf cfg' =
      AST.TestCondNoIf <$>
      Gen.scale predNat (genOrTest cfg') <*>
      pure ()
