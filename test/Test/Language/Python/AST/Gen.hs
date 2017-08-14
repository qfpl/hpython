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

genBefore :: Monad m => Gen m s -> Gen m a -> Gen m (Before s a)
genBefore = liftA2 Before

genBeforeF
  :: Monad m
  => Gen m s
  -> Gen m (f a)
  -> Gen m (Compose (Before s) f a)
genBeforeF ms = fmap Compose . genBefore ms

genAfter :: Monad m => Gen m s -> Gen m a -> Gen m (After s a)
genAfter = liftA2 After

genBetween :: Monad m => Gen m s -> Gen m t -> Gen m a -> Gen m (Between s t a)
genBetween ms mt ma = Between <$> ms <*> ma <*> mt

genBetweenF
  :: Monad m
  => Gen m s
  -> Gen m t
  -> Gen m (f a)
  -> Gen m (Compose (Between s t) f a)
genBetweenF ms mt = fmap Compose . genBetween ms mt

genBetween' :: Monad m => Gen m s -> Gen m a -> Gen m (Between' s a)
genBetween' ms ma = Between' <$> genBetween ms ms ma

genNewlineChar :: Monad m => Gen m AST.NewlineChar
genNewlineChar = Gen.element [AST.CR, AST.LF, AST.CRLF]

genWhitespaceChar :: Monad m => Gen m AST.WhitespaceChar
genWhitespaceChar =
  Gen.choice
    [ pure AST.Space
    , pure AST.Tab
    , AST.Continued <$> genNewlineChar
    ]

genListF :: Monad m => Gen m (f a) -> Gen m (Compose [] f a)
genListF ma =
  Compose <$>
  Gen.list (Range.linear 0 10) ma
  
genMaybeF :: Monad m => Gen m (f a) -> Gen m (Compose Maybe f a)
genMaybeF ma = Compose <$> Gen.maybe ma
    
genWhitespace1 :: Monad m => Gen m (NonEmpty AST.WhitespaceChar)
genWhitespace1 = Gen.nonEmpty (Range.linear 1 10) genWhitespaceChar

genWhitespaceBefore
  :: Monad m
  => Gen m a
  -> Gen m (Before [AST.WhitespaceChar] a)
genWhitespaceBefore ma = Before <$> genWhitespace <*> ma

genWhitespaceBeforeF
  :: Monad m
  => Gen m (f a)
  -> Gen m (Compose (Before [AST.WhitespaceChar]) f a)
genWhitespaceBeforeF = fmap Compose . genWhitespaceBefore

genWhitespaceBefore1
  :: Monad m
  => Gen m a
  -> Gen m (Before (NonEmpty AST.WhitespaceChar) a)
genWhitespaceBefore1 ma = Before <$> genWhitespace1 <*> ma

genWhitespaceBefore1F
  :: Monad m
  => Gen m (f a)
  -> Gen m (Compose (Before (NonEmpty AST.WhitespaceChar)) f a)
genWhitespaceBefore1F = fmap Compose . genWhitespaceBefore1

genWhitespaceAfter
  :: Monad m
  => Gen m a
  -> Gen m (After [AST.WhitespaceChar] a)
genWhitespaceAfter ma = After <$> genWhitespace <*> ma

genWhitespaceAfterF
  :: Monad m
  => Gen m (f a)
  -> Gen m (Compose (After [AST.WhitespaceChar]) f a)
genWhitespaceAfterF = fmap Compose . genWhitespaceAfter

genWhitespaceAfter1
  :: Monad m
  => Gen m a
  -> Gen m (After (NonEmpty AST.WhitespaceChar) a)
genWhitespaceAfter1 ma = After <$> genWhitespace1 <*> ma

genWhitespaceAfter1F
  :: Monad m
  => Gen m (f a)
  -> Gen m (Compose (After (NonEmpty AST.WhitespaceChar)) f a)
genWhitespaceAfter1F = fmap Compose . genWhitespaceAfter1

genWhitespace :: Monad m => Gen m [AST.WhitespaceChar]
genWhitespace = Gen.list (Range.linear 0 10) genWhitespaceChar

genBetweenWhitespace
  :: Monad m
  => Gen m a
  -> Gen m (Between' [AST.WhitespaceChar] a)
genBetweenWhitespace = genBetween' genWhitespace

genBetweenWhitespaceF
  :: Monad m
  => Gen m (f a)
  -> Gen m (Compose (Between' [AST.WhitespaceChar]) f a)
genBetweenWhitespaceF = fmap Compose . genBetweenWhitespace

genBetweenWhitespace1
  :: Monad m
  => Gen m a
  -> Gen m (Between' (NonEmpty AST.WhitespaceChar) a)
genBetweenWhitespace1 = genBetween' genWhitespace1

genBetweenWhitespace1F
  :: Monad m
  => Gen m (f a)
  -> Gen m (Compose (Between' (NonEmpty AST.WhitespaceChar)) f a)
genBetweenWhitespace1F = fmap Compose . genBetweenWhitespace1
    
genIfThenElse :: Monad m => Gen m (AST.IfThenElse ())
genIfThenElse =
  Gen.recursive Gen.choice
    []
    [ AST.IfThenElse <$>
      genBetweenWhitespace1F genOrTest <*>
      genWhitespaceBefore1F genTest
    ]

genTermOp :: Monad m => Gen m AST.TermOp
genTermOp =
  Gen.element
    [ AST.TermMult
    , AST.TermAt
    , AST.TermFloorDiv
    , AST.TermDiv
    , AST.TermMod
    ]
    
genStarExpr :: Monad m => Gen m (AST.StarExpr ())
genStarExpr =
  Gen.recursive Gen.choice
    []
    [ AST.StarExpr <$> genWhitespaceBeforeF genExpr <*> pure () ]
    
genTestlistComp :: Monad m => Gen m (AST.TestlistComp ())
genTestlistComp =
  Gen.recursive Gen.choice
    []
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

genTestList :: Monad m => Gen m (AST.TestList ())
genTestList =
  Gen.recursive Gen.choice
    []
    [ AST.TestList <$>
      genTest <*>
      genBeforeF (genBetweenWhitespace $ pure AST.Comma) genTest <*>
      Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
      pure ()
    ]

genYieldArg :: Monad m => Gen m (AST.YieldArg ())
genYieldArg =
  Gen.recursive Gen.choice
    []
    [ AST.YieldArgFrom <$> genWhitespaceBefore1F genTest <*> pure ()
    , AST.YieldArgList <$> genTestList <*> pure ()
    ]
    
genYieldExpr :: Monad m => Gen m (AST.YieldExpr ())
genYieldExpr =
  Gen.recursive Gen.choice
    []
    [ AST.YieldExpr <$>
      genMaybeF (genWhitespaceBefore1F genYieldArg) <*>
      pure ()
    ]

genDictOrSetMaker :: Monad m => Gen m (AST.DictOrSetMaker ())
genDictOrSetMaker = pure AST.DictOrSetMaker

genDigit :: Monad m => Gen m AST.Digit
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

genNonZeroDigit :: Monad m => Gen m AST.NonZeroDigit
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
    
genOctDigit :: Monad m => Gen m AST.OctDigit
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
    
genHexDigit :: Monad m => Gen m AST.HexDigit
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
    
genBinDigit :: Monad m => Gen m AST.BinDigit
genBinDigit = Gen.element [AST.BinDigit_0, AST.BinDigit_1]

genInteger :: Monad m => Gen m (AST.Integer' ())
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

genFloat :: Monad m => Gen m (AST.Float' ())
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

genStringPrefix :: Monad m => Gen m AST.StringPrefix
genStringPrefix =
  Gen.element
    [ AST.StringPrefix_r
    , AST.StringPrefix_u
    , AST.StringPrefix_R
    , AST.StringPrefix_U
    ]

genShortStringCharSingle
  :: Monad m
  => Gen m (AST.ShortStringChar AST.SingleQuote)
genShortStringCharSingle =
  Gen.just (fmap (^? AST._ShortStringCharSingle) Gen.ascii)

genShortStringCharDouble
  :: Monad m
  => Gen m (AST.ShortStringChar AST.DoubleQuote)
genShortStringCharDouble =
  Gen.just (fmap (^? AST._ShortStringCharDouble) Gen.ascii)

genStringEscapeSeq
  :: Monad m
  => Gen m AST.StringEscapeSeq
genStringEscapeSeq = AST.StringEscapeSeq <$> Gen.ascii

genShortString :: Monad m => Gen m (AST.ShortString ())
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
  :: Monad m
  => Gen m AST.LongStringChar
genLongStringChar =
  Gen.just (fmap (^? AST._LongStringChar) Gen.ascii)

genLongString :: Monad m => Gen m (AST.LongString ())
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

genStringLiteral :: Monad m => Gen m (AST.StringLiteral ())
genStringLiteral =
  AST.StringLiteral <$>
  genBeforeF
    (Gen.maybe genStringPrefix)
    (Gen.choice [InL <$> genShortString, InR <$> genLongString]) <*>
  pure ()

genLongBytesChar
  :: Monad m
  => Gen m AST.LongBytesChar
genLongBytesChar =
  Gen.just (fmap (^? AST._LongBytesChar) Gen.ascii)

genLongBytes :: Monad m => Gen m (AST.LongBytes ())
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
  :: Monad m
  => Gen m (AST.ShortBytesChar AST.SingleQuote)
genShortBytesCharSingle =
  Gen.just (fmap (^? AST._ShortBytesCharSingle) Gen.ascii)
  
genShortBytesCharDouble
  :: Monad m
  => Gen m (AST.ShortBytesChar AST.DoubleQuote)
genShortBytesCharDouble =
  Gen.just (fmap (^? AST._ShortBytesCharDouble) Gen.ascii)

genBytesEscapeSeq
  :: Monad m
  => Gen m AST.BytesEscapeSeq
genBytesEscapeSeq =
  Gen.just (fmap (^? AST._BytesEscapeSeq) Gen.ascii)

genShortBytes :: Monad m => Gen m (AST.ShortBytes ())
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

genBytesLiteral :: Monad m => Gen m (AST.BytesLiteral ())
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
    
genAtom :: Monad m => Gen m (AST.Atom ())
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
    , AST.AtomCurly <$>
      genBetweenWhitespaceF (genMaybeF genDictOrSetMaker) <*>
      pure ()  
    ]
  where
    genStringOrBytes =
      Gen.choice [ InL <$> genStringLiteral, InR <$> genBytesLiteral ]

genVarargsList :: Monad m => Gen m (AST.VarargsList ())
genVarargsList = pure AST.VarargsList

genLambdefNocond :: Monad m => Gen m (AST.LambdefNocond ())
genLambdefNocond =
  Gen.recursive Gen.choice
    []
    [ AST.LambdefNocond <$>
      genMaybeF (genBetweenF genWhitespace1 genWhitespace genVarargsList) <*>
      genWhitespaceBeforeF genTestNocond <*>
      pure ()
    ]

genTestNocond :: Monad m => Gen m (AST.TestNocond ())
genTestNocond =
  AST.TestNocond <$>
  Gen.recursive Gen.choice
    []
    [ InL <$> genOrTest, InR <$> genLambdefNocond ] <*>
  pure () 

genCompIf :: Monad m => Gen m (AST.CompIf ())
genCompIf =
  Gen.recursive Gen.choice
    []
    [ AST.CompIf <$>
      genWhitespaceBeforeF genTestNocond <*>
      genMaybeF (genWhitespaceBeforeF genCompIter) <*>
      pure ()
    ]

genCompIter :: Monad m => Gen m (AST.CompIter ())
genCompIter =
  AST.CompIter <$>
  Gen.recursive Gen.choice
    []
    [ InL <$> genCompFor, InR <$> genCompIf ] <*>
  pure ()

genExprList :: Monad m => Gen m (AST.ExprList ())
genExprList =
  Gen.recursive Gen.choice
    []
    [ AST.ExprList <$>
      genSumOrStar <*>
      genListF
        (genBeforeF (genBetweenWhitespace $ pure AST.Comma) genSumOrStar) <*>
      pure ()
    ]
  where
    genSumOrStar =
      Gen.recursive Gen.choice [] [InL <$> genExpr, InR <$> genStarExpr]
    
genCompFor :: Monad m => Gen m (AST.CompFor ())
genCompFor =
  Gen.recursive Gen.choice
    []
    [ AST.CompFor <$>
      genBetweenWhitespaceF genExprList <*>
      genWhitespaceBeforeF genOrTest <*>
      genMaybeF (genWhitespaceBeforeF genCompIter) <*>
      pure ()
    ]
  
genArgument :: Monad m => Gen m (AST.Argument ())
genArgument =
  Gen.recursive Gen.choice
    []
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
    
genArgList :: Monad m => Gen m (AST.ArgList ())
genArgList =
  Gen.recursive Gen.choice
    []
    [ AST.ArgList <$>
      genArgument <*>
      genListF
        (genBeforeF (genBetweenWhitespace $ pure AST.Comma) genArgument) <*>
      Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
      pure () 
    ]

genSliceOp :: Monad m => Gen m (AST.SliceOp ())
genSliceOp =
  Gen.recursive Gen.choice
    []
    [ AST.SliceOp <$> genMaybeF (genWhitespaceBeforeF genTest) <*> pure () ]

genSubscript :: Monad m => Gen m (AST.Subscript ())
genSubscript =
  Gen.recursive Gen.choice
    []
    [ AST.SubscriptTest <$> genTest <*> pure ()
    , AST.SubscriptSlice <$>
      genMaybeF (genWhitespaceAfterF genTest) <*>
      genMaybeF (genWhitespaceBeforeF genTest) <*>
      genMaybeF (genWhitespaceBeforeF genSliceOp) <*>
      pure ()
    ]

genSubscriptList :: Monad m => Gen m (AST.SubscriptList ())
genSubscriptList =
  Gen.recursive Gen.choice
    []
    [ AST.SubscriptList <$>
      genSubscript <*>
      genMaybeF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Comma)
          genSubscript) <*>
      Gen.maybe (genWhitespaceBefore $ pure AST.Comma) <*>
      pure ()
    ]

genIdentifier :: Monad m => Gen m (AST.Identifier ())
genIdentifier =
  AST.Identifier <$>
  (T.pack <$> Gen.list
    (Range.linear 1 10)
    (Gen.frequency [(1, Gen.upper), (1, Gen.lower), (26, pure '_')])) <*>
  pure ()

genTrailer :: Monad m => Gen m (AST.Trailer ())
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
    
genAtomExpr :: Monad m => Gen m (AST.AtomExpr ())
genAtomExpr =
  Gen.recursive Gen.choice
    []
    [ AST.AtomExpr <$>
      genMaybeF (genWhitespaceAfter1 $ pure AST.KAwait) <*>
      genAtom <*>
      genListF (genWhitespaceBeforeF genTrailer) <*>
      pure ()
    ]
    
genPower :: Monad m => Gen m (AST.Power ())
genPower =
  Gen.recursive Gen.choice
    []
    [ AST.Power <$>
      genAtomExpr <*>
      genMaybeF
        (genBeforeF
          (genWhitespaceAfter $ pure AST.DoubleAsterisk)
          genFactor) <*>
      pure ()
    ]

genFactorOp :: Monad m => Gen m AST.FactorOp
genFactorOp =
  Gen.element
    [ AST.FactorNeg
    , AST.FactorPos
    , AST.FactorInv
    ]
    
genFactor :: Monad m => Gen m (AST.Factor ())
genFactor =
  Gen.recursive Gen.choice
    []
    [ AST.FactorNone <$>
      genPower <*>
      pure ()
    , AST.FactorSome <$>
      genBeforeF (genWhitespaceAfter genFactorOp) genFactor <*>
      pure ()
    ]

genTerm :: Monad m => Gen m (AST.Term ())
genTerm =
  Gen.recursive Gen.choice
    []
    [ AST.Term <$>
      genFactor <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace genTermOp)
          genFactor) <*>
      pure ()
    ]

genArithExpr :: Monad m => Gen m (AST.ArithExpr ())
genArithExpr =
  Gen.recursive Gen.choice
    []
    [ AST.ArithExpr <$>
      genTerm <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $
             Gen.element [Left AST.Plus, Right AST.Minus])
          genTerm) <*>
      pure ()
    ]

genShiftExpr :: Monad m => Gen m (AST.ShiftExpr ())
genShiftExpr =
  Gen.recursive Gen.choice
    []
    [ AST.ShiftExpr <$>
      genArithExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $
             Gen.element [Left AST.DoubleLT, Right AST.DoubleGT])
          genArithExpr) <*>
      pure ()
    ]

genAndExpr :: Monad m => Gen m (AST.AndExpr ())
genAndExpr =
  Gen.recursive Gen.choice
    []
    [ AST.AndExpr <$>
      genShiftExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Ampersand)
          genShiftExpr) <*>
      pure ()
    ]

genXorExpr :: Monad m => Gen m (AST.XorExpr ())
genXorExpr =
  Gen.recursive Gen.choice
    []
    [ AST.XorExpr <$>
      genAndExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Caret)
          genAndExpr) <*>
      pure ()
    ]

genExpr :: Monad m => Gen m (AST.Expr ())
genExpr =
  Gen.recursive Gen.choice
    []
    [ AST.Expr <$>
      genXorExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure AST.Pipe)
          genXorExpr) <*>
      pure ()
    ]

genCompOperator :: Monad m => Gen m AST.CompOperator
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

genComparison :: Monad m => Gen m (AST.Comparison ())
genComparison =
  Gen.recursive Gen.choice
    []
    [ AST.Comparison <$>
      genExpr <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace genCompOperator)
          genExpr) <*>
      pure ()
    ]

genNotTest :: Monad m => Gen m (AST.NotTest ())
genNotTest =
  Gen.recursive Gen.choice
    []
    [ AST.NotTestSome <$>
      genBeforeF
        (genWhitespaceAfter1 $ pure AST.KNot)
        genNotTest <*>
      pure ()
    , AST.NotTestNone <$> genComparison <*> pure ()
    ]
    
genAndTest :: Monad m => Gen m (AST.AndTest ())
genAndTest =
  Gen.recursive Gen.choice
    []
    [ AST.AndTest <$>
      genNotTest <*>
      genListF
        (genBeforeF
          (genBetween' genWhitespace1 $ pure AST.KAnd)
          genAndTest) <*>
      pure ()
    ]

genOrTest :: Monad m => Gen m (AST.OrTest ())
genOrTest =
  Gen.recursive Gen.choice
    []
    [ AST.OrTest <$>
      genAndTest <*>
      genListF
        (genBeforeF
          (genBetween' genWhitespace1 $ pure AST.KOr)
          genAndTest) <*>
      pure ()
    ]

genTest :: Monad m => Gen m (AST.Test ())
genTest =
  Gen.recursive Gen.choice
    []
    [ AST.TestCond <$>
      genOrTest <*>
      (Compose <$>
         Gen.maybe
           (Compose <$>
              genBefore
                genWhitespace1 
                genIfThenElse)) <*>
      pure ()
    , pure AST.TestLambdef
    ]
