{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
module Test.Language.Python.Expr.Gen where

import Papa

import Data.Functor.Sum
import Hedgehog

import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Language.Python.AST.Identifier as AST
import qualified Language.Python.AST.Keywords as AST
import qualified Language.Python.AST.Symbols as AST
import qualified Language.Python.Expr.AST as AST
import qualified Language.Python.Expr.AST.BytesLiteral as AST
import qualified Language.Python.Expr.AST.BytesPrefix as AST
import qualified Language.Python.Expr.AST.CompOperator as AST
import qualified Language.Python.Expr.AST.Digits as AST
import qualified Language.Python.Expr.AST.EscapeSeq as AST
import qualified Language.Python.Expr.AST.FactorOperator as AST
import qualified Language.Python.Expr.AST.Float as AST
import qualified Language.Python.Expr.AST.Imag as AST
import qualified Language.Python.Expr.AST.Integer as AST
import qualified Language.Python.Expr.AST.LongBytesChar as AST
import qualified Language.Python.Expr.AST.LongBytes as AST
import qualified Language.Python.Expr.AST.LongStringChar as AST
import qualified Language.Python.Expr.AST.LongString as AST
import qualified Language.Python.Expr.AST.ShortBytes as AST
import qualified Language.Python.Expr.AST.ShortBytesChar as AST
import qualified Language.Python.Expr.AST.ShortStringChar as AST
import qualified Language.Python.Expr.AST.ShortString as AST
import qualified Language.Python.Expr.AST.StringLiteral as AST
import qualified Language.Python.Expr.AST.StringPrefix as AST
import qualified Language.Python.Expr.AST.TermOperator as AST
import qualified Language.Python.Expr.AST.StringContent as SC
import Language.Python.IR.ExprConfig

import Test.Language.Python.Gen.ArgList
import Test.Language.Python.Gen.Combinators

genIfThenElse
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.IfThenElse 'NotAssignable ctxt ())
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
  => ExprConfig atomType ctxt
  -> m (AST.StarExpr atomType ctxt ())
genStarExpr cfg =
  AST.StarExpr <$>
  genWhitespaceBeforeF
    (Gen.small . genExpr $ cfg & atomType .~ SAssignable) <*>
  pure ()

genListTestlistComp
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.ListTestlistComp atomType ctxt ())
genListTestlistComp cfg =
  case cfg ^. atomType of
    SAssignable ->
      Gen.choice [ Gen.small $ listTestlistCompList cfg ]
    SNotAssignable ->
      Gen.choice
        [ Gen.small $ listTestlistCompList cfg
        , AST.ListTestlistCompFor <$>
          Gen.small (genTest cfg) <*>
          Gen.small (genCompFor cfg) <*>
          pure ()
        ]
  where
    listTestlistCompList cfg' =
      Gen.choice
        [ AST.ListTestlistCompStarred <$>
          Gen.small (genStarExpr cfg') <*>
          genListF
            (genBeforeF
              (genBetweenWhitespace $ pure AST.Comma) .
              Gen.small $ genTestOrStar cfg') <*>
          Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
          pure ()
        , AST.ListTestlistCompList <$>
          Gen.small (genTest cfg') <*>
          genListF
            (genBeforeF
              (genBetweenWhitespace $ pure AST.Comma) .
              Gen.small $ genTestOrStar cfg') <*>
          Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
          pure ()
        ]

    genTestOrStar cfg' =
      Gen.small $
      Gen.choice [ InL <$> genTest cfg', InR <$> genStarExpr cfg' ]

genTupleTestlistComp
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.TupleTestlistComp atomType ctxt ())
genTupleTestlistComp cfg =
  case cfg ^. atomType of
    SAssignable ->
      Gen.choice [ Gen.small $ tupleTestlistCompList cfg ]
    SNotAssignable ->
      Gen.choice
        [ Gen.small $ tupleTestlistCompList cfg
        , AST.TupleTestlistCompFor <$>
          Gen.small (genTest cfg) <*>
          Gen.small (genCompFor cfg) <*>
          pure ()
        ]
  where
    tupleTestlistCompList cfg' =
      Gen.choice
        [ AST.TupleTestlistCompStarredOne <$>
          Gen.small (genStarExpr cfg') <*>
          genWhitespaceBefore (pure AST.Comma) <*>
          pure ()
        , AST.TupleTestlistCompStarredMany <$>
          Gen.small (genStarExpr cfg') <*>
          genNonEmptyF
            (genBeforeF
              (genBetweenWhitespace $ pure AST.Comma) .
              Gen.small $ genTestOrStar cfg') <*>
          Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
          pure ()
        , AST.TupleTestlistCompList <$>
          Gen.small (genTest cfg') <*>
          genListF
            (genBeforeF
              (genBetweenWhitespace $ pure AST.Comma) .
              Gen.small $ genTestOrStar cfg') <*>
          Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
          pure ()
        ]

    genTestOrStar cfg' =
      Gen.small $
      Gen.choice [ InL <$> genTest cfg', InR <$> genStarExpr cfg' ]

genTestList
  :: MonadGen m
  => ExprConfig atomType ctxt
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
  => ExprConfig atomType ctxt
  -> m (AST.YieldArg atomType ctxt ())
genYieldArg cfg =
  case cfg ^. atomType of
    SAssignable -> Gen.small $ Gen.choice [ yieldArgList cfg ]
    SNotAssignable ->
      Gen.small $
      Gen.choice
        [ yieldArgList cfg
        , AST.YieldArgFrom <$> genWhitespaceBefore1F (genTest cfg) <*> pure ()
        ]
  where
    yieldArgList cfg' =
      AST.YieldArgList <$> genTestList cfg' <*> pure ()

genYieldExpr
  :: MonadGen m
  => ExprConfig 'NotAssignable cfg
  -> m (AST.YieldExpr ())
genYieldExpr cfg =
  Gen.small $
  AST.YieldExpr <$>
  genMaybeF
    (genWhitespaceBefore1F . genYieldArg $
     cfg
       & atomType .~ SNotAssignable
       & definitionContext .~ SFunDef SNormal) <*>
  pure ()

genDictItem
  :: MonadGen m
  => ExprConfig 'NotAssignable cfg
  -> m (AST.DictItem 'NotAssignable cfg ())
genDictItem cfg =
  AST.DictItem <$>
  Gen.small (genTest cfg) <*>
  genBetweenWhitespace (pure AST.Colon) <*>
  Gen.small (genTest cfg) <*>
  pure ()

genDictUnpacking
  :: MonadGen m
  => ExprConfig 'NotAssignable cfg
  -> m (AST.DictUnpacking 'NotAssignable cfg ())
genDictUnpacking cfg =
  AST.DictUnpacking <$>
  genBeforeF
    (genBetweenWhitespace $ pure AST.DoubleAsterisk)
    (Gen.small $ genExpr cfg) <*>
  pure ()

genDictOrSetMaker
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.DictOrSetMaker 'NotAssignable ctxt ())
genDictOrSetMaker cfg =
  Gen.choice
    [ AST.DictOrSetMakerDictComp <$>
      Gen.small (genDictItem cfg) <*>
      Gen.small (genCompFor cfg) <*>
      pure ()
    , AST.DictOrSetMakerDictUnpack <$>
      Gen.small genItemOrUnpacking <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Comma)
          (Gen.small genItemOrUnpacking)) <*>
      Gen.maybe (genBetweenWhitespace $ pure AST.Comma) <*>
      pure ()
    , AST.DictOrSetMakerSetComp <$>
      Gen.small (genTest cfg) <*>
      Gen.small (genCompFor cfg) <*>
      pure ()
    , AST.DictOrSetMakerSetUnpack <$>
      Gen.small genTestOrStar <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Comma)
          (Gen.small genTestOrStar)) <*>
      Gen.maybe (genBetweenWhitespace $ pure AST.Comma) <*>
      pure ()
    ]
  where
    genItemOrUnpacking =
      Gen.choice [ InL <$> genDictItem cfg, InR <$> genDictUnpacking cfg ]

    genTestOrStar =
      Gen.choice [ InL <$> genTest cfg, InR <$> genStarExpr cfg ]

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
      genStringContent (Range.linear 0 20) <*>
      pure ()
    , AST.ShortStringDouble <$>
      genStringContent (Range.linear 0 20) <*>
      pure ()
    ]

genLongStringChar
  :: MonadGen m
  => m AST.LongStringChar
genLongStringChar =
  Gen.just (fmap (^? AST._LongStringChar) Gen.ascii)

genLongStringCharFinalSingle
  :: MonadGen m
  => m (AST.LongStringCharFinal AST.SingleQuote)
genLongStringCharFinalSingle =
  Gen.just (fmap (^? AST._LongStringCharFinalSingle) Gen.ascii)

genLongStringCharFinalDouble
  :: MonadGen m
  => m (AST.LongStringCharFinal AST.DoubleQuote)
genLongStringCharFinalDouble =
  Gen.just (fmap (^? AST._LongStringCharFinalDouble) Gen.ascii)

genLongString :: MonadGen m => m (AST.LongString ())
genLongString =
  Gen.choice
    [ pure $ AST.LongStringSingleEmpty ()
    , pure $ AST.LongStringDoubleEmpty ()
    , AST.LongStringSingle <$>
      genStringContent (Range.linear 0 20) <*>
      pure ()
    , AST.LongStringDouble <$>
      genStringContent (Range.linear 0 20) <*>
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
  
genLongBytesCharFinalSingle
  :: MonadGen m
  => m (AST.LongBytesCharFinal AST.SingleQuote)
genLongBytesCharFinalSingle =
  Gen.just (fmap (^? AST._LongBytesCharFinalSingle) Gen.ascii)
  
genLongBytesCharFinalDouble
  :: MonadGen m
  => m (AST.LongBytesCharFinal AST.DoubleQuote)
genLongBytesCharFinalDouble =
  Gen.just (fmap (^? AST._LongBytesCharFinalDouble) Gen.ascii)

genLongBytes :: MonadGen m => m (AST.LongBytes ())
genLongBytes =
  Gen.choice
    [ pure $ AST.LongBytesSingleEmpty ()
    , pure $ AST.LongBytesDoubleEmpty ()
    , AST.LongBytesSingle <$>
      genStringContent (Range.linear 0 20) <*>
      pure ()
    , AST.LongBytesDouble <$>
      genStringContent (Range.linear 0 20) <*>
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
    , AST.Slash_hex <$> genHexDigit <*> Gen.nonEmpty (Range.linear 1 10) genHexDigit
    ]

genShortBytes :: MonadGen m => m (AST.ShortBytes ())
genShortBytes =
  Gen.choice
    [ AST.ShortBytesSingle <$>
      genStringContent (Range.linear 0 20) <*>
      pure ()
    , AST.ShortBytesDouble <$>
      genStringContent (Range.linear 0 20) <*>
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
  => ExprConfig atomType ctxt
  -> m (AST.Atom atomType ctxt ())
genAtom cfg =
  case (cfg ^. atomType, cfg ^. definitionContext) of
    (SNotAssignable, SFunDef SNormal) ->
      Gen.recursive Gen.choice
        normalNonRec
        (normalRec cfg ++ [ genAtomParenYield cfg, genAtomCurly cfg ])
    (SNotAssignable, _) ->
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
        (normalRec cfg ++ [genAtomCurly cfg])
    _ -> Gen.recursive Gen.choice normalNonRec $ normalRec cfg
  where
    normalNonRec = 
      [ AST.AtomIdentifier <$> genIdentifier <*> pure () ]

    normalRec cfg' =
      [ genAtomParenNoYield cfg'
      , genAtomBracket cfg'
      ]

    genAtomCurly cfg' =
      AST.AtomCurly <$>
      genBetweenWhitespaceF (genMaybeF $ genDictOrSetMaker cfg') <*>
      pure ()  

    genAtomBracket cfg' =
      AST.AtomBracket <$>
      genBetweenWhitespaceF (genMaybeF $ genListTestlistComp cfg') <*>
      pure ()

    genAtomParenNoYield cfg' =
      AST.AtomParenNoYield <$>
      genBetweenWhitespaceF
        (genMaybeF $ genTupleTestlistComp cfg') <*>
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

genLambdefNocond
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.LambdefNocond 'NotAssignable ctxt ())
genLambdefNocond cfg =
  AST.LambdefNocond <$>
  genMaybeF
    (Gen.small . genBetweenF genWhitespace1 genWhitespace $
      genArgsList cfg genIdentifier (genTest cfg)) <*>
  genWhitespaceBeforeF (Gen.small $ genTestNocond cfg) <*>
  pure ()

genTestNocond
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.TestNocond atomType ctxt ())
genTestNocond cfg =
  Gen.small $
  AST.TestNocond <$>
  (case cfg ^. atomType of
    SNotAssignable ->
      Gen.choice [ InL <$> genOrTest cfg, InR <$> genLambdefNocond cfg ]
    SAssignable ->
      Gen.choice [ InL <$> genOrTest cfg ]) <*>
  pure ()

genCompIf
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.CompIf 'NotAssignable ctxt ())
genCompIf cfg =
  AST.CompIf <$>
  genBetweenWhitespace1 (pure AST.KIf) <*>
  Gen.small (genTestNocond cfg) <*>
  genMaybeF
    (Gen.small . genWhitespaceBeforeF $ genCompIter cfg) <*>
  pure ()

genCompIter
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.CompIter 'NotAssignable ctxt ())
genCompIter cfg =
  Gen.small $
  AST.CompIter <$>
  Gen.choice [ InL <$> genCompFor cfg, InR <$> genCompIf cfg ] <*>
  pure ()

genExprList
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.ExprList atomType ctxt ())
genExprList cfg =
  case cfg ^. atomType of
    SAssignable -> 
      Gen.choice
        [ exprListSingleStarredComma cfg
        , exprListSingle cfg
        , exprListMany cfg
        ]
    SNotAssignable ->
      Gen.choice
        [ exprListSingleStarredComma cfg
        , exprListSingleStarredNoComma cfg
        , exprListSingle cfg
        , exprListMany cfg
        ]
  where
    exprListSingleStarredComma cfg' =
     AST.ExprListSingleStarredComma <$>
      Gen.small (genStarExpr cfg') <*>
      genWhitespaceBefore (pure AST.Comma) <*>
      pure ()
    exprListSingleStarredNoComma cfg' =
      AST.ExprListSingleStarredNoComma <$>
      Gen.small (genStarExpr cfg') <*>
      pure ()
    exprListSingle cfg' =
      AST.ExprListSingle <$>
      Gen.small (genExpr cfg') <*>
      Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
      pure ()
    exprListMany cfg' =
      AST.ExprListMany <$>
      Gen.small (genSumOrStar cfg') <*>
      genNonEmptyF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Comma) . Gen.small $
            genSumOrStar cfg') <*>
      Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
      pure ()
    genSumOrStar cfg' =
      Gen.choice [InL <$> genExpr cfg', InR <$> genStarExpr cfg']

genCompFor
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.CompFor 'NotAssignable ctxt ())
genCompFor cfg =
  AST.CompFor <$>
  genBeforeF
    (genBetweenWhitespace1 $ pure AST.KFor)
    (genWhitespaceAfter1F . Gen.small . genExprList $
      cfg & atomType .~ SAssignable) <*>
  genWhitespaceBefore1F (Gen.small $ genOrTest cfg) <*>
  genMaybeF (genWhitespaceBeforeF . Gen.small $ genCompIter cfg) <*>
  pure ()

genArgument
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.Argument 'NotAssignable ctxt ())
genArgument cfg =
  Gen.choice
    [ AST.ArgumentFor <$>
      Gen.small (genTest cfg) <*>
      genMaybeF (Gen.small $ genCompFor cfg) <*>
      pure ()
    , AST.ArgumentDefault <$>
      genWhitespaceAfterF
        (Gen.small . genTest $ cfg & atomType .~ SAssignable) <*>
      genWhitespaceBeforeF (Gen.small $ genTest cfg) <*>
      pure ()
    , AST.ArgumentUnpack <$>
      Gen.element [Left AST.Asterisk, Right AST.DoubleAsterisk] <*>
      genWhitespaceBeforeF (Gen.small $ genTest cfg) <*>
      pure ()
    ]

genArgList
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.ArgList 'NotAssignable ctxt ())
genArgList cfg =
  AST.ArgList <$>
  Gen.small (genArgument $ cfg & atomType .~ SNotAssignable) <*>
  genListF
    (genBeforeF
       (genBetweenWhitespace $ pure AST.Comma)
       (Gen.small . genArgument $ cfg & atomType .~ SNotAssignable)) <*>
  Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
  pure ()

genSliceOp
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.SliceOp 'NotAssignable ctxt ())
genSliceOp cfg =
  AST.SliceOp <$>
  genMaybeF (Gen.small . genWhitespaceBeforeF $ genTest cfg) <*>
  pure ()

genSubscript
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.Subscript 'NotAssignable ctxt ())
genSubscript cfg =
  Gen.choice
    [ AST.SubscriptTest <$> Gen.small (genTest cfg) <*> pure ()
    , AST.SubscriptSlice <$>
      genWhitespaceAfterF (genMaybeF . Gen.small $ genTest cfg) <*>
      genWhitespaceAfter (pure AST.Colon) <*>
      genMaybeF (Gen.small . genWhitespaceAfterF $ genTest cfg) <*>
      genMaybeF (Gen.small . genWhitespaceAfterF $ genSliceOp cfg) <*>
      pure ()
    ]

genSubscriptList
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.SubscriptList 'NotAssignable ctxt ())
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
  => ExprConfig atomType ctxt
  -> m (AST.Trailer atomType ctxt ())
genTrailer cfg =
  case cfg ^. atomType of
    SNotAssignable ->
      Gen.recursive
        Gen.choice
        (commonNonRec cfg) $
        commonRec cfg ++
        [ AST.TrailerCall <$>
          genBetweenWhitespaceF (genMaybeF $ genArgList cfg) <*>
          pure ()
        ]
    SAssignable -> Gen.recursive Gen.choice (commonNonRec cfg) (commonRec cfg)
  where
    commonNonRec _ =
      [ AST.TrailerAccess <$>
        genWhitespaceBeforeF genIdentifier <*>
        pure ()
      ]
    commonRec cfg' =
      [ AST.TrailerSubscript <$>
        genBetweenWhitespaceF (genSubscriptList $ cfg' & atomType .~ SNotAssignable) <*>
        pure ()
      ]

genAtomExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.AtomExpr atomType ctxt ())
genAtomExpr cfg =
  case (cfg ^. atomType, cfg ^. definitionContext) of
    (SNotAssignable, SFunDef SAsync) ->
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
      Gen.small (genAtom $ cfg' & atomType .~ SNotAssignable) <*>
      genListF
        (Gen.small . genWhitespaceBeforeF . genTrailer $ cfg') <*>
      pure ()

genPower
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.Power atomType ctxt ())
genPower cfg =
  case cfg ^. atomType of
    SAssignable -> powerOne cfg
    SNotAssignable ->
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
      Gen.small (genAtomExpr cfg') <*>
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
  => ExprConfig atomType ctxt
  -> m (AST.Factor atomType ctxt ())
genFactor cfg =
  case cfg ^. atomType of
    SAssignable -> factorNone cfg
    SNotAssignable ->
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
      Gen.small (genPower cfg') <*>
      pure ()

genTerm
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.Term atomType ctxt ())
genTerm cfg =
  case cfg ^. atomType of
    SAssignable -> termOne cfg
    SNotAssignable ->
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
      Gen.small (genFactor cfg') <*>
      pure ()

genArithExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.ArithExpr atomType ctxt ())
genArithExpr cfg =
  case cfg ^. atomType of
    SAssignable -> arithExprOne cfg
    SNotAssignable ->
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
      Gen.small (genTerm cfg') <*>
      pure ()

genShiftExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.ShiftExpr atomType ctxt ())
genShiftExpr cfg =
  case cfg ^. atomType of
    SAssignable -> shiftExprOne cfg
    SNotAssignable ->
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
      Gen.small (genArithExpr cfg') <*>
      pure ()

genAndExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.AndExpr atomType ctxt ())
genAndExpr cfg =
  case cfg ^. atomType of
    SAssignable -> andExprOne cfg
    SNotAssignable ->
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
      Gen.small (genShiftExpr cfg') <*>
      pure ()

genXorExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.XorExpr atomType ctxt ())
genXorExpr cfg =
  case cfg ^. atomType of
    SAssignable -> xorExprOne cfg
    SNotAssignable ->
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
      Gen.small (genAndExpr cfg') <*>
      pure ()
genExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.Expr atomType ctxt ())
genExpr cfg =
  case cfg ^. atomType of
    SAssignable -> exprOne cfg
    SNotAssignable ->
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
      Gen.small (genXorExpr cfg') <*>
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
  => ExprConfig atomType ctxt
  -> m (AST.Comparison atomType ctxt ())
genComparison cfg =
  case cfg ^. atomType of
    SAssignable -> comparisonOne cfg
    SNotAssignable ->
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
      Gen.small (genExpr cfg') <*>
      pure ()

genNotTest
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.NotTest atomType ctxt ())
genNotTest cfg =
  case cfg ^. atomType of
    SAssignable -> notTestOne cfg
    SNotAssignable ->
      Gen.choice
        [ notTestOne cfg
        , AST.NotTestMany <$>
          genBeforeF
            (genWhitespaceAfter1 $ pure AST.KNot)
            (Gen.small $ genNotTest cfg) <*>
          pure ()
        ]
  where
    notTestOne cfg' =
      AST.NotTestOne <$>
      Gen.small (genComparison cfg') <*>
      pure ()

genAndTest
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.AndTest atomType ctxt ())
genAndTest cfg =
  case cfg ^. atomType of
    SAssignable -> andTestOne cfg
    SNotAssignable ->
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
      Gen.small (genNotTest cfg') <*>
      pure ()

genOrTest
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.OrTest atomType ctxt ())
genOrTest cfg =
  case cfg ^. atomType of
    SAssignable -> orTestOne cfg
    SNotAssignable ->
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
      Gen.small (genAndTest cfg') <*>
      pure ()

genTest
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m (AST.Test atomType ctxt ())
genTest cfg =
  case cfg ^. atomType of
    SAssignable -> testCondNoIf cfg
    SNotAssignable ->
      Gen.choice
        [ testCondNoIf cfg
        , AST.TestCondIf <$>
          Gen.small (genOrTest cfg) <*>
          genBeforeF
            genWhitespace1
            (Gen.small $ genIfThenElse cfg) <*>
          pure ()
        , Gen.small $
          AST.TestLambdef <$>
          genLambdef cfg <*>
          pure ()
        ]
  where
    testCondNoIf cfg' =
      AST.TestCondNoIf <$>
      Gen.small (genOrTest cfg') <*>
      pure ()

genLambdef
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m (AST.Lambdef 'NotAssignable ctxt ())
genLambdef cfg =
  AST.Lambdef <$>
  genMaybeF (genWhitespaceBefore1F . genArgsList cfg genIdentifier $ genTest cfg) <*>
  genBeforeF
    (genBetweenWhitespace $ pure AST.Colon)
    (genTest cfg) <*>
  pure ()

genStringContent
  :: ( MonadGen m
     , SC.AsChar a
     , SC.StringInside b
     )
  => Range Int
  -> m (SC.StringContent b a)
genStringContent range =
  Gen.just $
    preview SC._StringContent <$>
    Gen.string range (Gen.element [' '..'~'])

genStringContentSingle
  :: ( MonadGen m
     , SC.AsChar a
     )
  => Range Int
  -> m (SC.StringContent AST.SingleQuote a)
genStringContentSingle = genStringContent

genStringContentDouble
  :: ( MonadGen m
     , SC.AsChar a
     )
  => Range Int
  -> m (SC.StringContent AST.DoubleQuote a)
genStringContentDouble = genStringContent
