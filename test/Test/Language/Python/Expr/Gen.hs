{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
module Test.Language.Python.Expr.Gen where

import Papa
import Prelude (div)

import Data.Functor.Compose

import Data.Functor.Sum
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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

import Test.Language.Python.Gen.ArgsList
import Test.Language.Python.Gen.ArgumentList
import Test.Language.Python.Gen.Combinators
import Test.Language.Python.Gen.Identifier
import Test.Language.Python.Gen.TestlistStarExpr

genIfThenElse
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.IfThenElse ws 'NotAssignable ctxt ())
genIfThenElse cfg ws =
  AST.IfThenElse <$>
  genBetween'1 ws (pure AST.KIf) <*>
  Gen.small (genOrTest cfg ws) <*>
  genBetween'1 ws (pure AST.KElse) <*>
  Gen.small (genTest cfg ws)

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
  -> m ws
  -> m (AST.StarExpr ws atomType ctxt ())
genStarExpr cfg ws =
  AST.StarExpr <$>
  genBeforeF (Gen.list (Range.linear 0 10) ws)
    (Gen.small $ genExpr (cfg & atomType .~ SAssignable) ws) <*>
  pure ()

genListTestlistComp
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.ListTestlistComp ws atomType ctxt ())
genListTestlistComp cfg ws =
  case cfg ^. atomType of
    SAssignable ->
      Gen.choice [ Gen.small $ listTestlistCompList cfg ws ]
    SNotAssignable ->
      Gen.choice
        [ Gen.small $ listTestlistCompList cfg ws
        , AST.ListTestlistCompFor <$>
          Gen.small (genTest cfg ws) <*>
          Gen.small (genCompFor cfg ws) <*>
          pure ()
        ]
  where
    listTestlistCompList cfg' ws' =
      Gen.choice
        [ AST.ListTestlistCompStarred <$>
          Gen.small (genStarExpr cfg' ws') <*>
          genListF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) .
              Gen.small $ genTestOrStar cfg' ws') <*>
          Gen.maybe (genBefore (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) <*>
          pure ()
        , AST.ListTestlistCompList <$>
          Gen.small (genTest cfg' ws') <*>
          genListF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) .
              Gen.small $ genTestOrStar cfg' ws') <*>
          Gen.maybe (genBefore (Gen.list (Range.linear 0 10) ws')$ pure AST.Comma) <*>
          pure ()
        ]

    genTestOrStar cfg' ws' =
      Gen.small $
      Gen.choice [ InL <$> genTest cfg' ws', InR <$> genStarExpr cfg' ws' ]

genTupleTestlistComp
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.TupleTestlistComp ws atomType ctxt ())
genTupleTestlistComp cfg ws =
  case cfg ^. atomType of
    SAssignable ->
      Gen.choice [ Gen.small $ tupleTestlistCompList cfg ws ]
    SNotAssignable ->
      Gen.choice
        [ Gen.small $ tupleTestlistCompList cfg ws
        , AST.TupleTestlistCompFor <$>
          Gen.small (genTest cfg ws) <*>
          Gen.small (genCompFor cfg ws) <*>
          pure ()
        ]
  where
    tupleTestlistCompList cfg' ws' =
      Gen.choice
        [ AST.TupleTestlistCompStarredOne <$>
          Gen.small (genStarExpr cfg' ws') <*>
          genBefore (Gen.list (Range.linear 0 10) ws') (pure AST.Comma) <*>
          pure ()
        , AST.TupleTestlistCompStarredMany <$>
          Gen.small (genStarExpr cfg' ws') <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) .
              Gen.small $ genTestOrStar cfg' ws') <*>
          Gen.maybe (genBefore (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) <*>
          pure ()
        , AST.TupleTestlistCompList <$>
          Gen.small (genTest cfg' ws') <*>
          genListF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) .
              Gen.small $ genTestOrStar cfg' ws') <*>
          Gen.maybe (genBefore (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) <*>
          pure ()
        ]

    genTestOrStar cfg' ws' =
      Gen.small $
      Gen.choice [ InL <$> genTest cfg' ws', InR <$> genStarExpr cfg' ws' ]

genTestList
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.TestList ws atomType ctxt ())
genTestList cfg ws =
  AST.TestList <$>
    Gen.small (genTest cfg ws) <*>
    genListF
      (genBeforeF
        (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Comma)
        (Gen.small $ genTest cfg ws)) <*>
    Gen.maybe (genBefore (Gen.list (Range.linear 0 10) ws) $ pure AST.Comma) <*>
    pure ()

genYieldArg
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.YieldArg ws atomType ctxt ())
genYieldArg cfg ws =
  case cfg ^. atomType of
    SAssignable -> Gen.small $ Gen.choice [ yieldArgList cfg ws ]
    SNotAssignable ->
      Gen.small $
      Gen.choice
        [ yieldArgList cfg ws
        , AST.YieldArgFrom <$> genBefore1F ws (genTest cfg ws) <*> pure ()
        ]
  where
    yieldArgList cfg' ws' =
      AST.YieldArgList <$> genTestList cfg' ws' <*> pure ()

genYieldExpr
  :: MonadGen m
  => ExprConfig 'NotAssignable ('FunDef 'Normal)
  -> m ws
  -> m (AST.YieldExpr ws ('FunDef 'Normal) ())
genYieldExpr cfg ws =
  Gen.small $
  AST.YieldExpr <$>
  genMaybeF
    (genBefore1F ws $
     genYieldArg 
       (cfg
         & atomType .~ SNotAssignable
         & definitionContext .~ SFunDef SNormal)
       ws) <*>
  pure ()

genDictItem
  :: MonadGen m
  => ExprConfig 'NotAssignable cfg
  -> m ws
  -> m (AST.DictItem ws 'NotAssignable cfg ())
genDictItem cfg ws =
  AST.DictItem <$>
  Gen.small (genTest cfg ws) <*>
  genBetween' (Gen.list (Range.linear 0 10) ws) (pure AST.Colon) <*>
  Gen.small (genTest cfg ws) <*>
  pure ()

genDictUnpacking
  :: MonadGen m
  => ExprConfig 'NotAssignable cfg
  -> m ws
  -> m (AST.DictUnpacking ws 'NotAssignable cfg ())
genDictUnpacking cfg ws =
  AST.DictUnpacking <$>
  genBeforeF
    (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.DoubleAsterisk)
    (Gen.small $ genExpr cfg ws) <*>
  pure ()

genDictOrSetMaker
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.DictOrSetMaker ws 'NotAssignable ctxt ())
genDictOrSetMaker cfg ws =
  Gen.choice
    [ AST.DictOrSetMakerDictComp <$>
      Gen.small (genDictItem cfg ws) <*>
      Gen.small (genCompFor cfg ws) <*>
      pure ()
    , AST.DictOrSetMakerDictUnpack <$>
      Gen.small genItemOrUnpacking <*>
      genListF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Comma)
          (Gen.small genItemOrUnpacking)) <*>
      Gen.maybe (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Comma) <*>
      pure ()
    , AST.DictOrSetMakerSetComp <$>
      Gen.small (genTest cfg ws) <*>
      Gen.small (genCompFor cfg ws) <*>
      pure ()
    , AST.DictOrSetMakerSetUnpack <$>
      Gen.small genTestOrStar <*>
      genListF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Comma)
          (Gen.small genTestOrStar)) <*>
      Gen.maybe (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Comma) <*>
      pure ()
    ]
  where
    genItemOrUnpacking =
      Gen.choice [ InL <$> genDictItem cfg ws, InR <$> genDictUnpacking cfg ws ]

    genTestOrStar =
      Gen.choice [ InL <$> genTest cfg ws, InR <$> genStarExpr cfg ws ]

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
      (Right <$> Gen.nonEmpty (Range.linear 1 10) (pure AST.Zero)) <*>
      pure ()
    , AST.IntegerDecimal <$>
      (Left <$>
       liftA2 (,) genNonZeroDigit (Gen.list (Range.linear 0 10) genDigit)) <*>
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
      genBefore genE someDigits <*>
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
  -> m ws
  -> m (AST.Atom ws atomType ctxt ())
genAtom cfg ws =
  case cfg ^. atomType of
    SNotAssignable -> Gen.choice [genAtomInteger cfg, genAtomNoInt' cfg]
    _ -> genAtomNoInt' cfg
  where
    genAtomNoInt' cfg' =
      AST.AtomNoInt <$>
      genAtomNoInt cfg' ws <*>
      pure ()
    genAtomInteger _ = AST.AtomInteger <$> genInteger <*> pure ()

genAtomNoInt
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.AtomNoInt ws atomType ctxt ())
genAtomNoInt cfg ws =
  case (cfg ^. atomType, cfg ^. definitionContext) of
    (SNotAssignable, SFunDef SNormal) ->
      Gen.recursive Gen.choice
        normalNonRec
        (normalRec cfg ++ [ genAtomParenYield cfg, genAtomCurly cfg ])
    (SNotAssignable, _) -> 
      Gen.recursive Gen.choice
        (normalNonRec ++
          [ genAtomFloat cfg
          , genAtomString cfg ws
          , genAtomImag cfg ws
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
      genBetween'F
        (Gen.list (Range.linear 0 10) genAnyWhitespaceChar)
        (genMaybeF $ genDictOrSetMaker cfg' genAnyWhitespaceChar) <*>
      pure ()  

    genAtomBracket cfg' =
      AST.AtomBracket <$>
      genBetween'F
        (Gen.list (Range.linear 0 10) genAnyWhitespaceChar)
        (genMaybeF $ genListTestlistComp cfg' genAnyWhitespaceChar) <*>
      pure ()

    genAtomParenNoYield cfg' =
      AST.AtomParenNoYield <$>
      genBetween'F (Gen.list (Range.linear 0 10) genAnyWhitespaceChar)
        (genMaybeF $ genTupleTestlistComp cfg' genAnyWhitespaceChar) <*>
      pure ()

    genAtomImag _ ws' =
      AST.AtomImag <$>
      genBeforeF (Gen.list (Range.linear 0 10) ws') genImag <*>
      pure ()
    genAtomFloat _ = AST.AtomFloat <$> genFloat <*> pure ()

    genAtomString cfg' ws' =
      AST.AtomString <$>
      genStringOrBytes cfg' <*>
      genListF (genBeforeF (Gen.list (Range.linear 0 10) ws') $ genStringOrBytes cfg') <*>
      pure ()  
    genAtomEllipsis _ = pure $ AST.AtomEllipsis ()
    genAtomNone _ = pure $ AST.AtomNone ()
    genAtomTrue _ = pure $ AST.AtomTrue ()
    genAtomFalse _ = pure $ AST.AtomFalse ()

    genAtomParenYield cfg' = 
      AST.AtomParenYield <$>
      genBetween'F
        (Gen.list (Range.linear 0 10) genAnyWhitespaceChar)
        (genYieldExpr cfg' genAnyWhitespaceChar) <*>
      pure ()

    genStringOrBytes _ =
      Gen.choice [ InL <$> genStringLiteral, InR <$> genBytesLiteral ]

genLambdefNocond
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.LambdefNocond ws 'NotAssignable ctxt ())
genLambdefNocond cfg ws =
  AST.LambdefNocond <$>
  genMaybeF
    (Gen.small .
     genBetweenF
       (Gen.nonEmpty (Range.linear 1 10) ws)
       (Gen.list (Range.linear 0 10) ws) $
      genArgsList cfg genIdentifier (genTest cfg ws)) <*>
  genBeforeF (Gen.list (Range.linear 0 10) ws)
    (Gen.small $
     genTestNocond (cfg & definitionContext .~ SFunDef SNormal) ws) <*>
  pure ()

genTestNocond
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.TestNocond ws atomType ctxt ())
genTestNocond cfg ws =
  Gen.small $
  AST.TestNocond <$>
  (case cfg ^. atomType of
    SNotAssignable ->
      Gen.choice [ InL <$> genOrTest cfg ws, InR <$> genLambdefNocond cfg ws ]
    SAssignable ->
      Gen.choice [ InL <$> genOrTest cfg ws ]) <*>
  pure ()

genCompIf
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.CompIf ws 'NotAssignable ctxt ())
genCompIf cfg ws =
  AST.CompIf <$>
  genBetween'1 ws (pure AST.KIf) <*>
  Gen.small (genTestNocond cfg ws) <*>
  genMaybeF
    (Gen.small . genBeforeF (Gen.list (Range.linear 0 10) ws) $ genCompIter cfg ws) <*>
  pure ()

genCompIter
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.CompIter ws 'NotAssignable ctxt ())
genCompIter cfg ws =
  Gen.small $
  AST.CompIter <$>
  Gen.choice [ InL <$> genCompFor cfg ws, InR <$> genCompIf cfg ws ] <*>
  pure ()

genExprList
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.ExprList ws atomType ctxt ())
genExprList cfg ws =
  case cfg ^. atomType of
    SAssignable -> 
      Gen.choice
        [ exprListSingleStarredComma cfg ws
        , exprListSingle cfg ws
        , exprListMany cfg ws
        ]
    SNotAssignable ->
      Gen.choice
        [ exprListSingleStarredComma cfg ws
        , exprListSingleStarredNoComma cfg ws
        , exprListSingle cfg ws
        , exprListMany cfg ws
        ]
  where
    exprListSingleStarredComma cfg' ws' =
     AST.ExprListSingleStarredComma <$>
      Gen.small (genStarExpr cfg' ws') <*>
      genBefore (Gen.list (Range.linear 0 10) ws') (pure AST.Comma) <*>
      pure ()
    exprListSingleStarredNoComma cfg' ws' =
      AST.ExprListSingleStarredNoComma <$>
      Gen.small (genStarExpr cfg' ws') <*>
      pure ()
    exprListSingle cfg' ws' =
      AST.ExprListSingle <$>
      Gen.small (genExpr cfg' ws') <*>
      Gen.maybe (genBefore (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) <*>
      pure ()
    exprListMany cfg' ws' =
      AST.ExprListMany <$>
      Gen.small (genSumOrStar cfg' ws') <*>
      genNonEmptyF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) . Gen.small $
            genSumOrStar cfg' ws') <*>
      Gen.maybe (genBefore (Gen.list (Range.linear 0 10) ws') $ pure AST.Comma) <*>
      pure ()
    genSumOrStar cfg' ws' =
      Gen.choice [InL <$> genExpr cfg' ws', InR <$> genStarExpr cfg' ws']

genCompFor
  :: MonadGen m
  => ExprConfig as ctxt
  -> m ws
  -> m (AST.CompFor ws 'NotAssignable ctxt ())
genCompFor cfg ws =
  AST.CompFor <$>
  genBeforeF
    (genBetween'1 ws $ pure AST.KFor)
    (genAfter1F ws .
     Gen.small $
     genTestlistStarExpr genExpr genStarExpr (cfg & atomType .~ SAssignable) ws) <*>
  genBefore1F ws
    (Gen.small $ genOrTest (cfg & atomType .~ SNotAssignable) ws) <*>
  genMaybeF
    (genBeforeF (Gen.list (Range.linear 0 10) ws) .
     Gen.small $
     genCompIter (cfg & atomType .~ SNotAssignable) ws) <*>
  pure ()

genSliceOp
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.SliceOp ws 'NotAssignable ctxt ())
genSliceOp cfg ws =
  AST.SliceOp <$>
  genMaybeF (Gen.small . genBeforeF (Gen.list (Range.linear 0 10) ws) $ genTest cfg ws) <*>
  pure ()

genSubscript
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.Subscript ws 'NotAssignable ctxt ())
genSubscript cfg ws =
  Gen.choice
    [ AST.SubscriptTest <$> Gen.small (genTest cfg ws) <*> pure ()
    , AST.SubscriptSlice <$>
      genAfterF (Gen.list (Range.linear 0 10) ws) (genMaybeF . Gen.small $ genTest cfg ws) <*>
      genAfter (Gen.list (Range.linear 0 10) ws) (pure AST.Colon) <*>
      genMaybeF
        (Gen.small .
         genAfterF (Gen.list (Range.linear 0 10) ws) $ genTest cfg ws) <*>
      genMaybeF
        (Gen.small .
         genAfterF (Gen.list (Range.linear 0 10) ws) $ genSliceOp cfg ws) <*>
      pure ()
    ]

genSubscriptList
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.SubscriptList ws 'NotAssignable ctxt ())
genSubscriptList cfg ws =
  AST.SubscriptList <$>
  Gen.small (genSubscript cfg ws) <*>
  genListF
    (genBeforeF
      (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Comma)
      (Gen.small $ genSubscript cfg ws)) <*>
  Gen.maybe (genBefore (Gen.list (Range.linear 0 10) ws) $ pure AST.Comma) <*>
  pure ()

genTrailer
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.Trailer ws atomType ctxt ())
genTrailer cfg ws =
  case cfg ^. atomType of
    SNotAssignable ->
      Gen.recursive
        Gen.choice
        (commonNonRec cfg ws) $
        commonRec cfg ++
        [ AST.TrailerCall <$>
          genBetween'F (Gen.list (Range.linear 0 10) genAnyWhitespaceChar)
            (genMaybeF $ genArgumentList cfg genIdentifier genTest) <*>
          pure ()
        ]
    SAssignable ->
      Gen.recursive Gen.choice
        (commonNonRec cfg ws)
        (commonRec cfg)
  where
    commonNonRec _ ws' =
      [ AST.TrailerAccess <$>
        genBeforeF (Gen.list (Range.linear 0 10) ws') genIdentifier <*>
        pure ()
      ]
    commonRec cfg' =
      [ AST.TrailerSubscript <$>
        genBetween'F
          (Gen.list (Range.linear 0 10) genAnyWhitespaceChar)
          (genSubscriptList (cfg' & atomType .~ SNotAssignable) genAnyWhitespaceChar) <*>
        pure ()
      ]

genAtomExprTrailers
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.AtomExprTrailers ws atomType ctxt ())
genAtomExprTrailers cfg ws =
  Gen.recursive Gen.choice
    [ AST.AtomExprTrailersBase <$>
      genAtomNoInt (cfg & atomType .~ SNotAssignable) ws <*>
      genBeforeF (Gen.list (Range.linear 0 10) ws) (genTrailer cfg ws) <*>
      pure ()
    ]
    [ AST.AtomExprTrailersMany <$>
      genAtomExprTrailers (cfg & atomType .~ SNotAssignable) ws <*>
      genBeforeF (Gen.list (Range.linear 0 10) ws) (genTrailer cfg ws) <*>
      pure ()
    ]

genAtomExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.AtomExpr ws atomType ctxt ())
genAtomExpr cfg ws =
  case (cfg ^. atomType, cfg ^. definitionContext) of
    (SNotAssignable, SFunDef SAsync) ->
      Gen.choice $
        atomExprNoAwait cfg ws ++
        [ AST.AtomExprAwaitSingle <$>
          genAfter1 ws (pure AST.KAwait) <*>
          genAtom cfg ws <*>
          pure ()
        , AST.AtomExprAwaitTrailers<$>
          genAfter1 ws (pure AST.KAwait) <*>
          genAtomExprTrailers cfg ws <*>
          pure ()
        ]
    _ -> Gen.choice $ atomExprNoAwait cfg ws
  where
    atomExprNoAwait cfg' ws' =
      [ AST.AtomExprSingle <$>
        genAtom cfg' ws' <*>
        pure ()
      , AST.AtomExprTrailers <$>
        genAtomExprTrailers cfg' ws' <*>
        pure ()
      ]

genPower
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.Power ws atomType ctxt ())
genPower cfg ws =
  case cfg ^. atomType of
    SAssignable -> powerOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ powerOne cfg ws ]
        [ AST.PowerMany <$>
          Gen.small (genAtomExpr cfg ws) <*>
          genBeforeF
            (Gen.small . genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.DoubleAsterisk)
            (Gen.small $ genFactor cfg ws) <*>
          pure ()
        ]
  where
    powerOne cfg' ws' =
      AST.PowerOne <$>
      Gen.small (genAtomExpr cfg' ws') <*>
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
  -> m ws
  -> m (AST.Factor ws atomType ctxt ())
genFactor cfg ws =
  case cfg ^. atomType of
    SAssignable -> factorNone cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ factorNone cfg ws ]
        [ AST.FactorOne <$>
          genAfter (Gen.list (Range.linear 0 10) ws) genFactorOp <*>
          Gen.small (genFactor cfg ws) <*>
          pure ()
        ]
  where
    factorNone cfg' ws' =
      AST.FactorNone <$>
      Gen.small (genPower cfg' ws') <*>
      pure ()

genTerm
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.Term ws atomType ctxt ())
genTerm cfg ws =
  case cfg ^. atomType of
    SAssignable -> termOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ termOne cfg ws ]
        [ AST.TermMany <$>
            Gen.small (genFactor cfg ws) <*>
            genNonEmptyF
              (genBeforeF
                (Gen.small $ genBetween' (Gen.list (Range.linear 0 10) ws) genTermOp)
                (Gen.small $ genFactor cfg ws)) <*>
            pure ()
        ]
  where
    termOne cfg' ws' =
      AST.TermOne <$>
      Gen.small (genFactor cfg' ws') <*>
      pure ()

genArithExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.ArithExpr ws atomType ctxt ())
genArithExpr cfg ws =
  case cfg ^. atomType of
    SAssignable -> arithExprOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ arithExprOne cfg ws ]
        [ AST.ArithExprMany <$>
          Gen.small (genTerm cfg ws) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws) $
                Gen.element [Left AST.Plus, Right AST.Minus])
              (Gen.small $ genTerm cfg ws)) <*>
          pure ()
        ]
  where
    arithExprOne cfg' ws' =
      AST.ArithExprOne <$>
      Gen.small (genTerm cfg' ws') <*>
      pure ()

genShiftExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.ShiftExpr ws atomType ctxt ())
genShiftExpr cfg ws =
  case cfg ^. atomType of
    SAssignable -> shiftExprOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ shiftExprOne cfg ws ]
        [ AST.ShiftExprMany <$>
          Gen.small (genArithExpr cfg ws) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws) $
                  Gen.element [Left AST.DoubleLT, Right AST.DoubleGT])
              (Gen.small $ genArithExpr cfg ws)) <*>
          pure ()
        ]
  where
    shiftExprOne cfg' ws' =
      AST.ShiftExprOne <$>
      Gen.small (genArithExpr cfg' ws') <*>
      pure ()

genAndExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.AndExpr ws atomType ctxt ())
genAndExpr cfg ws =
  case cfg ^. atomType of
    SAssignable -> andExprOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ andExprOne cfg ws ]
        [ AST.AndExprMany <$>
          Gen.small (genShiftExpr cfg ws) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 1 10) ws) $ pure AST.Ampersand)
              (Gen.small $ genShiftExpr cfg ws)) <*>
          pure ()
        ]
  where
    andExprOne cfg' ws' =
      AST.AndExprOne <$>
      Gen.small (genShiftExpr cfg' ws') <*>
      pure ()

genXorExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.XorExpr ws atomType ctxt ())
genXorExpr cfg ws =
  case cfg ^. atomType of
    SAssignable -> xorExprOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ xorExprOne cfg ws ]
        [ AST.XorExprMany <$>
          Gen.small (genAndExpr cfg ws) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Caret)
              (Gen.small $ genAndExpr cfg ws)) <*>
          pure ()
        ]
  where
    xorExprOne cfg' ws' =
      AST.XorExprOne <$>
      Gen.small (genAndExpr cfg' ws') <*>
      pure ()

genExpr
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.Expr ws atomType ctxt ())
genExpr cfg ws =
  case cfg ^. atomType of
    SAssignable -> exprOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ exprOne cfg ws ]
        [ AST.ExprMany <$>
          Gen.small (genXorExpr cfg ws) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Pipe)
              (Gen.small $ genXorExpr cfg ws)) <*>
          pure ()
        ]
  where
    exprOne cfg' ws' =
      AST.ExprOne <$>
      Gen.small (genXorExpr cfg' ws') <*>
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
  -> m ws
  -> m (AST.Comparison ws atomType ctxt ())
genComparison cfg ws =
  case cfg ^. atomType of
    SAssignable -> comparisonOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ comparisonOne cfg ws ]
        [ AST.ComparisonMany <$>
          Gen.small (genExpr cfg ws) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' (Gen.list (Range.linear 0 10) ws) genCompOperator)
              (Gen.small $ genExpr cfg ws)) <*>
          pure ()
        ]
  where
    comparisonOne cfg' ws' =
      AST.ComparisonOne <$>
      Gen.small (genExpr cfg' ws') <*>
      pure ()

genNotTest
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.NotTest ws atomType ctxt ())
genNotTest cfg ws =
  case cfg ^. atomType of
    SAssignable -> notTestOne cfg ws
    SNotAssignable ->
      Gen.recursive
        Gen.choice
        [ notTestOne cfg ws ]
        [ AST.NotTestMany <$>
          genBeforeF
            (genAfter1 ws $ pure AST.KNot)
            (genNotTest cfg ws) <*>
          pure ()
        ]
  where
    notTestOne cfg' ws' =
      AST.NotTestOne <$>
      Gen.small (genComparison cfg' ws') <*>
      pure ()

genAndTest
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.AndTest ws atomType ctxt ())
genAndTest cfg ws = do
  n <- Gen.int (Range.linear 1 10)
  case cfg ^. atomType of
    SAssignable -> andTestOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ andTestOne cfg ws ]
        [ AST.AndTestMany <$>
          Gen.small (genNotTest cfg ws) <*>
          (Compose <$> Gen.nonEmpty (Range.singleton n)
            (genBeforeF
              (genBetween' (Gen.nonEmpty (Range.linear 1 10) ws) $ pure AST.KAnd)
              (Gen.scale (`div` Size n) $ genNotTest cfg ws))) <*>
          pure ()
        ]
  where
    andTestOne cfg' ws' =
      AST.AndTestOne <$>
      Gen.small (genNotTest cfg' ws') <*>
      pure ()

genOrTest
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.OrTest ws atomType ctxt ())
genOrTest cfg ws =
  case cfg ^. atomType of
    SAssignable -> orTestOne cfg ws
    SNotAssignable ->
      Gen.recursive Gen.choice
        [ orTestOne cfg ws ]
        [ AST.OrTestMany <$>
          Gen.small (genAndTest cfg ws) <*>
          genNonEmptyF
            (genBeforeF
              (genBetween' (Gen.nonEmpty (Range.linear 1 10) ws) $ pure AST.KOr)
              (Gen.small $ genAndTest cfg ws)) <*>
          pure ()
        ]
  where
    orTestOne cfg' ws' =
      AST.OrTestOne <$>
      Gen.small (genAndTest cfg' ws') <*>
      pure ()

genTest
  :: MonadGen m
  => ExprConfig atomType ctxt
  -> m ws
  -> m (AST.Test ws atomType ctxt ())
genTest cfg ws =
  case cfg ^. atomType of
    SAssignable -> testCondNoIf cfg ws
    SNotAssignable ->
      Gen.recursive
        Gen.choice
        [ testCondNoIf cfg ws ]
        [ AST.TestCondIf <$>
          Gen.small (genOrTest cfg ws) <*>
          genBeforeF
            (Gen.nonEmpty (Range.linear 1 10) ws)
            (Gen.small $ genIfThenElse cfg ws) <*>
          pure ()
        , Gen.small $
          AST.TestLambdef <$>
          genLambdef cfg ws <*>
          pure ()
        ]
  where
    testCondNoIf cfg' ws =
      AST.TestCondNoIf <$>
      Gen.small (genOrTest cfg' ws) <*>
      pure ()

genLambdef
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (AST.Lambdef ws 'NotAssignable ctxt ())
genLambdef cfg ws =
  AST.Lambdef <$>
  genMaybeF
    (genBefore1F ws .
     genArgsList cfg genIdentifier $ genTest cfg ws) <*>
  genBeforeF
    (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure AST.Colon)
    (Gen.small $ genTest (cfg & definitionContext .~ SFunDef SNormal) ws) <*>
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
