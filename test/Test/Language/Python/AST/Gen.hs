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
import qualified Language.Python.AST.BytesEscapeSeq as AST
import qualified Language.Python.AST.Keywords as AST
import qualified Language.Python.AST.LongBytesChar as AST
import qualified Language.Python.AST.LongStringChar as AST
import qualified Language.Python.AST.ShortBytesChar as AST
import qualified Language.Python.AST.ShortStringChar as AST
import qualified Language.Python.AST.Symbols as AST

genBefore :: MonadGen m => m s -> m a -> m (Before s a)
genBefore = liftA2 Before

genBeforeF
  :: MonadGen m
  => m s
  -> m (f a)
  -> m (Compose (Before s) f a)
genBeforeF ms = fmap Compose . genBefore ms

genAfter :: MonadGen m => m s -> m a -> m (After s a)
genAfter = liftA2 After

genBetween :: MonadGen m => m s -> m t -> m a -> m (Between s t a)
genBetween ms mt ma = Between <$> ms <*> ma <*> mt

genBetweenF
  :: MonadGen m
  => m s
  -> m t
  -> m (f a)
  -> m (Compose (Between s t) f a)
genBetweenF ms mt = fmap Compose . genBetween ms mt

genBetween' :: MonadGen m => m s -> m a -> m (Between' s a)
genBetween' ms ma = Between' <$> genBetween ms ms ma

genNewlineChar :: MonadGen m => m AST.NewlineChar
genNewlineChar = Gen.element [AST.CR, AST.LF, AST.CRLF]

genWhitespaceChar :: MonadGen m => m AST.WhitespaceChar
genWhitespaceChar =
  Gen.choice
    [ pure AST.Space
    , pure AST.Tab
    , AST.Continued <$> genNewlineChar
    ]

genListF :: MonadGen m => m (f a) -> m (Compose [] f a)
genListF ma =
  Compose <$>
  Gen.list (Range.linear 0 10) ma
  
genMaybeF :: MonadGen m => m (f a) -> m (Compose Maybe f a)
genMaybeF ma = Compose <$> Gen.maybe ma
    
genWhitespace1 :: MonadGen m => m (NonEmpty AST.WhitespaceChar)
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

genWhitespace :: MonadGen m => m [AST.WhitespaceChar]
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
    
genIfThenElse :: MonadGen m => m (AST.IfThenElse ())
genIfThenElse =
  AST.IfThenElse <$>
  genBetweenWhitespace1F genOrTest <*>
  genWhitespaceBefore1F genTest

genTermOp :: MonadGen m => m AST.TermOp
genTermOp =
  Gen.element
    [ AST.TermMult
    , AST.TermAt
    , AST.TermFloorDiv
    , AST.TermDiv
    , AST.TermMod
    ]
    
genStarExpr :: MonadGen m => m (AST.StarExpr ())
genStarExpr =
  AST.StarExpr <$>
  genWhitespaceBeforeF genExpr <*>
  pure ()
    
genTestlistComp :: MonadGen m => m (AST.TestlistComp ())
genTestlistComp =
  Gen.choice
    [ AST.TestlistCompFor <$>
      genTestOrStar <*>
      genWhitespaceBeforeF genCompFor <*>
      pure ()
    , AST.TestlistCompList <$>
      genTestOrStar <*>
      genListF
        (genBeforeF (genBetweenWhitespace $ pure AST.Comma) genTestOrStar) <*>
      Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
      pure ()
    ]
  where
    genTestOrStar = Gen.choice [ InL <$> genTest, InR <$> genStarExpr ]

genTestList :: MonadGen m => m (AST.TestList ())
genTestList =
  AST.TestList <$>
    genTest <*>
    genBeforeF (genBetweenWhitespace $ pure AST.Comma) genTest <*>
    Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
    pure ()

genYieldArg :: MonadGen m => m (AST.YieldArg ())
genYieldArg =
  Gen.choice
    [ AST.YieldArgFrom <$> genWhitespaceBefore1F genTest <*> pure ()
    , AST.YieldArgList <$> genTestList <*> pure ()
    ]
    
genYieldExpr :: MonadGen m => m (AST.YieldExpr ())
genYieldExpr =
  AST.YieldExpr <$>
  genMaybeF (genWhitespaceBefore1F genYieldArg) <*>
  pure ()

genDictOrSetMaker :: MonadGen m => m (AST.DictOrSetMaker ())
genDictOrSetMaker = pure AST.DictOrSetMaker

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

genFloat :: MonadGen m => m (AST.Float' ())
genFloat =
  Gen.choice
    [ AST.FloatNoDecimal <$>
      genInteger <*>
      genMaybeF
        (genBeforeF genE genInteger) <*>
      pure ()
    , AST.FloatDecimalNoBase <$>
      genInteger <*>
      genMaybeF (genBeforeF genE genInteger) <*>
      pure ()
    , AST.FloatDecimalBase <$>
      genInteger <*>
      genMaybeF genInteger <*>
      genMaybeF (genBeforeF genE genInteger) <*>
      pure ()
    ]
  where
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

genStringEscapeSeq
  :: MonadGen m
  => m AST.StringEscapeSeq
genStringEscapeSeq = AST.StringEscapeSeq <$> Gen.ascii

genShortString :: MonadGen m => m (AST.ShortString ())
genShortString =
  Gen.choice
    [ AST.ShortStringSingle <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genShortStringCharSingle
          , Right <$> genStringEscapeSeq
          ]) <*>
      pure ()
    , AST.ShortStringDouble <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genShortStringCharDouble
          , Right <$> genStringEscapeSeq
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
          , Right <$> genStringEscapeSeq
          ]) <*>
      pure ()
    , AST.LongStringDouble <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genLongStringChar
          , Right <$> genStringEscapeSeq
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
          [ Left <$> genLongBytesChar
          , Right <$> genBytesEscapeSeq
          ]) <*>
      pure ()
    , AST.LongBytesDouble <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genLongBytesChar
          , Right <$> genBytesEscapeSeq
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

genBytesEscapeSeq
  :: MonadGen m
  => m AST.BytesEscapeSeq
genBytesEscapeSeq =
  Gen.just (fmap (^? AST._BytesEscapeSeq) Gen.ascii)

genShortBytes :: MonadGen m => m (AST.ShortBytes ())
genShortBytes =
  Gen.choice
    [ AST.ShortBytesSingle <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genShortBytesCharSingle
          , Right <$> genBytesEscapeSeq
          ]) <*>
      pure ()
    , AST.ShortBytesDouble <$>
      Gen.list
        (Range.linear 0 200)
        (Gen.choice
          [ Left <$> genShortBytesCharDouble
          , Right <$> genBytesEscapeSeq
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
    
genAtom :: MonadGen m => m (AST.Atom ())
genAtom =
  Gen.recursive Gen.choice
    [ AST.AtomIdentifier <$> genIdentifier <*> pure ()  
    , AST.AtomInteger <$> genInteger <*> pure ()  
    , AST.AtomFloat <$> genFloat <*> pure ()  
    , AST.AtomString <$>
      genStringOrBytes <*>
      genListF (genWhitespaceBeforeF genStringOrBytes) <*>
      pure ()  
    , pure $ AST.AtomEllipsis ()
    , pure $ AST.AtomNone ()
    , pure $ AST.AtomTrue ()
    , pure $ AST.AtomFalse ()
    ]
    [ AST.AtomParen <$>
      genBetweenWhitespaceF
        (genMaybeF $
         Gen.choice [InL <$> genYieldExpr, InR <$> genTestlistComp]) <*>
      pure ()
    , AST.AtomBracket <$>
      genBetweenWhitespaceF (genMaybeF genTestlistComp) <*>
      pure ()  
    -- , AST.AtomCurly <$>
    -- genBetweenWhitespaceF (genMaybeF genDictOrSetMaker) <*>
    -- pure ()  
    ]
  where
    genStringOrBytes =
      Gen.choice [ InL <$> genStringLiteral, InR <$> genBytesLiteral ]

genVarargsList :: MonadGen m => m (AST.VarargsList ())
genVarargsList = pure AST.VarargsList

genLambdefNocond :: MonadGen m => m (AST.LambdefNocond ())
genLambdefNocond =
  AST.LambdefNocond <$>
  genMaybeF (genBetweenF genWhitespace1 genWhitespace genVarargsList) <*>
  genWhitespaceBeforeF genTestNocond <*>
  pure ()

genTestNocond :: MonadGen m => m (AST.TestNocond ())
genTestNocond =
  AST.TestNocond <$>
  Gen.choice [ InL <$> genOrTest, InR <$> genLambdefNocond ] <*>
  pure () 

genCompIf :: MonadGen m => m (AST.CompIf ())
genCompIf =
  AST.CompIf <$>
  genWhitespaceBeforeF genTestNocond <*>
  genMaybeF (genWhitespaceBeforeF genCompIter) <*>
  pure ()

genCompIter :: MonadGen m => m (AST.CompIter ())
genCompIter =
  AST.CompIter <$>
  Gen.choice [ InL <$> genCompFor, InR <$> genCompIf ] <*>
  pure ()

genExprList :: MonadGen m => m (AST.ExprList ())
genExprList =
  AST.ExprList <$>
  genSumOrStar <*>
  genListF
    (genBeforeF (genBetweenWhitespace $ pure AST.Comma) genSumOrStar) <*>
  pure ()
  where
    genSumOrStar =
      Gen.choice [InL <$> genExpr, InR <$> genStarExpr]
    
genCompFor :: MonadGen m => m (AST.CompFor ())
genCompFor =
  AST.CompFor <$>
  genBetweenWhitespaceF genExprList <*>
  genWhitespaceBeforeF genOrTest <*>
  genMaybeF (genWhitespaceBeforeF genCompIter) <*>
  pure ()
  
genArgument :: MonadGen m => m (AST.Argument ())
genArgument =
  Gen.choice
    [ AST.ArgumentFor <$>
      genTest <*>
      genMaybeF (genWhitespaceBeforeF genCompFor) <*>
      pure ()
    , AST.ArgumentDefault <$>
      genWhitespaceAfterF genTest <*>
      genWhitespaceBeforeF genTest <*>
      pure () 
    , AST.ArgumentUnpack <$>
      Gen.element [Left AST.Asterisk, Right AST.DoubleAsterisk] <*>
      genWhitespaceBeforeF genTest <*>
      pure () 
    ]
    
genArgList :: MonadGen m => m (AST.ArgList ())
genArgList =
  AST.ArgList <$>
  genArgument <*>
  genListF
    (genBeforeF (genBetweenWhitespace $ pure AST.Comma) genArgument) <*>
  Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
  pure () 

genSliceOp :: MonadGen m => m (AST.SliceOp ())
genSliceOp =
  AST.SliceOp <$> genMaybeF (genWhitespaceBeforeF genTest) <*> pure ()

genSubscript :: MonadGen m => m (AST.Subscript ())
genSubscript =
  Gen.choice
    [ AST.SubscriptTest <$> genTest <*> pure ()
    , AST.SubscriptSlice <$>
      genMaybeF (genWhitespaceAfterF genTest) <*>
      genMaybeF (genWhitespaceBeforeF genTest) <*>
      genMaybeF (genWhitespaceBeforeF genSliceOp) <*>
      pure ()
    ]

genSubscriptList :: MonadGen m => m (AST.SubscriptList ())
genSubscriptList =
  AST.SubscriptList <$>
  genSubscript <*>
  genMaybeF
    (genBeforeF
      (genBetweenWhitespace $ pure AST.Comma)
      genSubscript) <*>
  Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
  pure ()

genIdentifier :: MonadGen m => m (AST.Identifier ())
genIdentifier =
  AST.Identifier <$>
  (T.pack <$> Gen.list
    (Range.linear 1 10)
    (Gen.frequency [(1, Gen.upper), (1, Gen.lower), (26, pure '_')])) <*>
  pure ()

genTrailer :: MonadGen m => m (AST.Trailer ())
genTrailer =
  Gen.recursive Gen.choice
    [ AST.TrailerAccess <$>
      genWhitespaceBeforeF genIdentifier <*>
      pure ()
    ]
    [ AST.TrailerCall <$>
      genBetweenWhitespaceF (genMaybeF genArgList) <*>
      pure ()
    , AST.TrailerSubscript <$>
      genBetweenWhitespaceF (genMaybeF genSubscriptList) <*>
      pure ()
    ]
    
genAtomExpr :: MonadGen m => m (AST.AtomExpr ())
genAtomExpr =
  AST.AtomExpr <$>
  genMaybeF (genWhitespaceAfter1 $ pure AST.KAwait) <*>
  genAtom <*>
  genListF (genWhitespaceBeforeF genTrailer) <*>
  pure ()
    
genPower :: MonadGen m => m (AST.Power ())
genPower =
  AST.Power <$>
  genAtomExpr <*>
  genMaybeF
    (genBeforeF
      (genWhitespaceAfter $ pure AST.DoubleAsterisk)
      genFactor) <*>
  pure ()

genFactorOp :: MonadGen m => m AST.FactorOp
genFactorOp =
  Gen.element
    [ AST.FactorNeg
    , AST.FactorPos
    , AST.FactorInv
    ]
    
genFactor :: MonadGen m => m (AST.Factor ())
genFactor =
  Gen.choice
    [ AST.FactorNone <$>
      genPower <*>
      pure ()
    , AST.FactorSome <$>
      genBeforeF (genWhitespaceAfter genFactorOp) genFactor <*>
      pure ()
    ]

genTerm :: MonadGen m => m (AST.Term ())
genTerm =
  AST.Term <$>
  genFactor <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace genTermOp)
      genFactor) <*>
  pure ()

genArithExpr :: MonadGen m => m (AST.ArithExpr ())
genArithExpr =
  AST.ArithExpr <$>
  genTerm <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $
          Gen.element [Left AST.Plus, Right AST.Minus])
      genTerm) <*>
  pure ()

genShiftExpr :: MonadGen m => m (AST.ShiftExpr ())
genShiftExpr =
  AST.ShiftExpr <$>
  genArithExpr <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $
          Gen.element [Left AST.DoubleLT, Right AST.DoubleGT])
      genArithExpr) <*>
  pure ()

genAndExpr :: MonadGen m => m (AST.AndExpr ())
genAndExpr =
  AST.AndExpr <$>
  genShiftExpr <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $ pure AST.Ampersand)
      genShiftExpr) <*>
  pure ()

genXorExpr :: MonadGen m => m (AST.XorExpr ())
genXorExpr =
  AST.XorExpr <$>
  genAndExpr <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $ pure AST.Caret)
      genAndExpr) <*>
  pure ()

genExpr :: MonadGen m => m (AST.Expr ())
genExpr =
  AST.Expr <$>
  genXorExpr <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace $ pure AST.Pipe)
      genXorExpr) <*>
  pure ()

genCompOperator :: MonadGen m => m AST.CompOperator
genCompOperator =
  Gen.choice
    [ pure AST.CompLT
    , pure AST.CompGT
    , pure AST.CompEq
    , pure AST.CompGEq
    , pure AST.CompLEq
    , pure AST.CompNEq
    , AST.CompIs <$> genWhitespaceChar
    , AST.CompIsNot <$> genWhitespace1 <*> genWhitespaceChar
    , AST.CompIn <$> genWhitespaceChar
    , AST.CompNotIn <$> genWhitespace1 <*> genWhitespaceChar
    ]

genComparison :: MonadGen m => m (AST.Comparison ())
genComparison =
  AST.Comparison <$>
  genExpr <*>
  genListF
    (genBeforeF
      (genBetweenWhitespace genCompOperator)
      genExpr) <*>
  pure ()

genNotTest :: MonadGen m => m (AST.NotTest ())
genNotTest =
  Gen.choice
    [ AST.NotTestSome <$>
      genBeforeF
        (genWhitespaceAfter1 $ pure AST.KNot)
        genNotTest <*>
      pure ()
    , AST.NotTestNone <$> genComparison <*> pure ()
    ]
    
genAndTest :: MonadGen m => m (AST.AndTest ())
genAndTest =
  AST.AndTest <$>
  genNotTest <*>
  genListF
    (genBeforeF
      (genBetween' genWhitespace1 $ pure AST.KAnd)
      genAndTest) <*>
  pure ()

genOrTest :: MonadGen m => m (AST.OrTest ())
genOrTest =
  AST.OrTest <$>
  genAndTest <*>
  genListF
    (genBeforeF
      (genBetween' genWhitespace1 $ pure AST.KOr)
      genAndTest) <*>
  pure ()

genTest :: MonadGen m => m (AST.Test ())
genTest =
  Gen.choice
    [ AST.TestCond <$>
      genOrTest <*>
      (Compose <$>
         Gen.maybe
           (Compose <$>
              genBefore
                genWhitespace1 
                genIfThenElse)) <*>
      pure ()
    -- , pure AST.TestLambdef
    ]
