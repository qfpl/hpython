{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
module Language.Python.AST where

import Papa hiding (Plus, Sum, Product)

import Data.Eq.Deriving
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between
import Data.Text (Text)
import Text.Show.Deriving

import Language.Python.AST.BytesEscapeSeq
import Language.Python.AST.LongBytesChar
import Language.Python.AST.ShortBytesChar
import Language.Python.AST.LongStringChar
import Language.Python.AST.ShortStringChar
import Language.Python.AST.Symbols

type Token = After [WhitespaceChar]
type TokenF = Compose (After [WhitespaceChar])

data Identifier a
  = Identifier
  { _identifier_value :: Text
  , _identifier_ann :: a
  } deriving (Functor, Foldable, Traversable)

data StringPrefix
  = StringPrefix_r
  | StringPrefix_u
  | StringPrefix_R
  | StringPrefix_U
  deriving (Eq, Show)

newtype StringEscapeSeq
  = StringEscapeSeq
  { _stringEscapeSeq_value :: Char
  } deriving (Eq, Show)

data ShortString a
  = ShortStringSingle
  { _shortStringSingle_value
    :: Between'
         SingleQuote
         [Either (ShortStringChar SingleQuote) StringEscapeSeq]
  , _shortString_ann :: a
  }
  | ShortStringDouble
  { _shortStringDouble_value
    :: Between'
         DoubleQuote
         [Either (ShortStringChar DoubleQuote) StringEscapeSeq]
  , _shortString_ann :: a
  } deriving (Functor, Foldable, Traversable)


data LongString a
  = LongStringSingle
  { _longStringSingle_value
    :: Between'
         TripleSingleQuote
         [Either LongStringChar StringEscapeSeq]
  , _longStringSingle_ann :: a
  }
  | LongStringDouble
  { _longStringDouble_value
    :: Between'
         TripleDoubleQuote
         [Either LongStringChar StringEscapeSeq]
  , _longStringDouble_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data BytesPrefix
  = BytesPrefix_b
  | BytesPrefix_B
  | BytesPrefix_br
  | BytesPrefix_Br
  | BytesPrefix_bR
  | BytesPrefix_BR
  | BytesPrefix_rb
  | BytesPrefix_rB
  | BytesPrefix_Rb
  | BytesPrefix_RB
  deriving (Eq, Show)

data ShortBytes a
  = ShortBytesSingle
  { _shortBytesSingle_value
    :: Between'
         SingleQuote
         [Either (ShortBytesChar SingleQuote) BytesEscapeSeq]
  , _shortBytes_ann :: a
  }
  | ShortBytesDouble
  { _shortBytesDouble_value
    :: Between'
         DoubleQuote
         [Either (ShortBytesChar DoubleQuote) BytesEscapeSeq]
  , _shortBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data LongBytes a
  = LongBytesSingle
  { _longBytesSingle_value
    :: Between'
         TripleSingleQuote
         [Either LongBytesChar BytesEscapeSeq]
  , _longBytes_ann :: a
  }
  | LongBytesDouble
  { _longBytesDouble_value
    :: Between'
         TripleDoubleQuote
         [Either LongBytesChar BytesEscapeSeq]
  , _longBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Digit
  = Digit_0
  | Digit_1
  | Digit_2
  | Digit_3
  | Digit_4
  | Digit_5
  | Digit_6
  | Digit_7
  | Digit_8
  | Digit_9
  deriving (Eq, Show)
  
data NonZeroDigit
  = NonZeroDigit_1
  | NonZeroDigit_2
  | NonZeroDigit_3
  | NonZeroDigit_4
  | NonZeroDigit_5
  | NonZeroDigit_6
  | NonZeroDigit_7
  | NonZeroDigit_8
  | NonZeroDigit_9
  deriving (Eq, Show)

data OctDigit
  = OctDigit_0
  | OctDigit_1
  | OctDigit_2
  | OctDigit_3
  | OctDigit_4
  | OctDigit_5
  | OctDigit_6
  | OctDigit_7
  deriving (Eq, Show)

data HexDigit
  = HexDigit_0
  | HexDigit_1
  | HexDigit_2
  | HexDigit_3
  | HexDigit_4
  | HexDigit_5
  | HexDigit_6
  | HexDigit_7
  | HexDigit_8
  | HexDigit_9
  | HexDigit_a
  | HexDigit_A
  | HexDigit_b
  | HexDigit_B
  | HexDigit_c
  | HexDigit_C
  | HexDigit_d
  | HexDigit_D
  | HexDigit_e
  | HexDigit_E
  | HexDigit_f
  | HexDigit_F
  deriving (Eq, Show)

data BinDigit = BinDigit_0 | BinDigit_1 deriving (Eq, Show)

data Integer' a
  = IntegerDecimal
  { _integerDecimal_value
    :: Either (NonZeroDigit, [Digit]) (NonEmpty Zero)
  , _integer_ann :: a
  }
  | IntegerOct
  { _integerOct_value
    :: Before
         (Either Char_o Char_O)
         (NonEmpty OctDigit)
  , _integer_ann :: a
  }
  | IntegerHex
  { _integerHex_value
    :: Before
         (Either Char_x Char_X)
         (NonEmpty HexDigit)
  , _integer_ann :: a
  }
  | IntegerBin
  { _integerBin_value
    :: Before
         (Either Char_b Char_B)
         (NonEmpty BinDigit)
  , _integer_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data PointFloat
  = WithDecimalPlaces (Maybe (NonEmpty Digit)) (NonEmpty Digit)
  | NoDecimalPlaces (NonEmpty Digit)
  deriving (Eq, Show)

data Float' a
  = FloatPoint
  { _floatPoint_value
    :: PointFloat
  , _float_ann :: a
  }
  | FloatExponent
  { _floatExponent_base
    :: Either (NonEmpty Digit) PointFloat
  , _floatExponent_exponent
    :: Before (Either Char_e Char_E) (Before (Either Plus Minus) (NonEmpty Digit))
  , _float_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Imag a
  = Imag
  { _imag_value
    :: Compose
         (After (Either Char_j Char_J))
         (Sum Float' (Const (NonEmpty Digit)))
         a
  , _imag_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data StringLiteral a
  = StringLiteral
  { _stringLiteral_value
    :: Compose
         (Before (Maybe StringPrefix))
         (Sum ShortString LongString)
         a
  , _stringLiteral_ann :: a
  } deriving (Functor, Foldable, Traversable)

data BytesLiteral a
  = BytesLiteral
  { _bytesLiteral_value
    :: Compose
         (Before (Maybe BytesPrefix))
         (Sum ShortBytes LongBytes)
         a
  , _bytesLiteral_ann :: a
  } deriving (Functor, Foldable, Traversable)

data Literal a
  = LiteralString
  { _literalString_head :: Sum StringLiteral BytesLiteral a
  , _literalString_tail
    :: Compose
         (Before [WhitespaceChar])
         (Compose [] (Sum StringLiteral BytesLiteral))
         a
  , _literal_ann :: a
  }
  | LiteralInteger
  { _literalInteger_value :: Integer' a
  , _literal_ann :: a
  }
  | LiteralFloat
  { _literalFloat_value :: Float' a
  , _literal_ann :: a
  }
  | LiteralImag
  { _literalImag_value :: Imag a
  , _literal_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data IfThenElse a
  = IfThenElse
  { _ifThenElse_if :: Compose (Between' [WhitespaceChar]) OrTest a
  , _ifThenElse_else :: Compose (Before [WhitespaceChar]) Expression a
  }
  deriving (Functor, Foldable, Traversable)

data CompOperator
  = CompLT
  | CompGT
  | CompEq
  | CompGEq
  | CompLEq
  | CompNEq
  | CompIs
  | CompIsNot (NonEmpty WhitespaceChar)
  | CompIn
  | CompNotIn (NonEmpty WhitespaceChar)
  deriving (Eq, Show)

data ExpressionList a
  = ExpressionList
  { _expressionList_head :: Expression a
  , _expressionList_tail
    :: Compose
         []
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           Expression)
         a
  , _expressionList_trailingComma :: Maybe (Before [WhitespaceChar] Comma)
  }
  deriving (Functor, Foldable, Traversable)

data Subscription a
  = Subscription
  { _subscription_outer :: Compose (After [WhitespaceChar]) Primary a
  , _subscription_inner :: Compose (Between' [WhitespaceChar]) ExpressionList a
  , _subscription_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data AttRef a
  = AttRef
  { _attrRef_left :: Compose (After [WhitespaceChar]) Primary a
  , _attrRef_right :: Compose (Before [WhitespaceChar]) Identifier a
  , _attrRef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data ProperSlice a
  = ProperSlice
  { _properSlice_lower
    :: Compose
         Maybe
         (Compose (After [WhitespaceChar]) Expression)
         a
  , _properSlice_upper
    :: Compose
         Maybe
         (Compose (Between' [WhitespaceChar]) Expression)
         a
  , _properSlice_stride
    :: Compose
         Maybe
         (Compose
           ((,) (Before [WhitespaceChar] Colon))
           (Compose
             Maybe
             (Compose
               (Before [WhitespaceChar])
               Expression)))
         a
  , _properSlice_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data SliceItem a
  = SliceItemExpr
  { _sliceItemExpr_value :: Expression a
  , _sliceItem_ann :: a
  }
  | SliceItemProper
  { _sliceItemProper_value :: ProperSlice a
  , _sliceItem_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data SliceList a
  = SliceList
  { _sliceList_head :: SliceItem a
  , _sliceList_tail
    :: Compose
         []
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           SliceItem)
         a
  , _sliceList_trailingComma :: Maybe (Before [WhitespaceChar] Comma)
  }
  deriving (Functor, Foldable, Traversable)

data Slicing a
  = Slicing
  { _slicing_outer :: Compose (After [WhitespaceChar]) Primary a
  , _slicing_innter :: Compose (Between' [WhitespaceChar]) SliceList a
  , _slicing_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data PositionalArgs a
  = PositionalArgs
  { _positionalArgs_head
    :: Compose
         ((,) (After [WhitespaceChar] Asterisk))
         Expression
         a
  , _positionalArgs_tail
    :: Compose
         [] 
         (Compose
           ((,) (After [WhitespaceChar] Comma))
           (Compose
             ((,) (After [WhitespaceChar] Asterisk))
             Expression))
         a
  , _positionalArgs_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data KeywordItem a
  = KeywordItem
  { _keywordItem_left :: Compose (After [WhitespaceChar]) Identifier a
  , _keywordItem_right :: Compose (Before [WhitespaceChar]) Expression a
  , _keywordItem_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data StarredAndKeywords a
  = StarredAndKeywords
  { _starredAndKeywords_head
    :: Sum
         (Compose
           ((,) (After [WhitespaceChar] Asterisk))
           Expression)
         KeywordItem
         a
  , _starredAndKeywords_tail
    :: Compose
         ((,) (After [WhitespaceChar] Comma))
         (Sum
           (Compose
             ((,) (After [WhitespaceChar] Asterisk))
             Expression)
           KeywordItem)
         a
  , _starredAndKeywords_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data KeywordsArgs a
  = KeywordsArgs
  { _keywordsArgs_head
    :: Sum
         KeywordItem
         (Compose
           ((,) (After [WhitespaceChar] DoubleAsterisk))
           Expression)
         a
  , _keywordsArgs_tail
    :: Compose
       ((,) (After [WhitespaceChar] Comma))
       (Sum
         KeywordItem
         (Compose
           ((,) (After [WhitespaceChar] DoubleAsterisk))
           Expression))
       a
  , _keywordsArgs_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data ArgList a
  = ArgListAll
  { _argListAll_positional :: PositionalArgs a
  , _argListAll_starred
    :: Compose
         Maybe
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           StarredAndKeywords)
         a
  , _argListAll_keyword
    :: Compose
         Maybe
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           KeywordsArgs)
         a
  , _argList_ann :: a
  }
  | ArgListStarred
  { _argListStarred_starred :: StarredAndKeywords a
  , _argListStarred_keyword
    :: Compose
         Maybe
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           KeywordsArgs)
         a
  , _argList_ann :: a
  }
  | ArgListKeywords
  { _argListKeywords_keyword :: KeywordsArgs a
  , _argList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Target a
  = TargetIdentifier
  { _targetIdentifier_value :: Identifier a
  , _target_ann :: a
  }
  | TargetTuple
  { _targetTuple_value :: Compose (Between' [WhitespaceChar]) TargetList a
  , _target_ann :: a
  }
  | TargetList'
  { _targetList_value
    :: Compose
         (Between' [WhitespaceChar])
         (Compose Maybe TargetList)
         a
  , _target_ann :: a
  }
  | TargetAttRef
  { _targetAttRef_value :: AttRef a
  , _target_ann :: a
  }
  | TargetSubscription
  { _targetSubscription_value :: Subscription a
  , _target_ann :: a
  }
  | TargetSlicing
  { _targetSlicing_value :: Slicing a
  , _target_ann :: a
  }
  | TargetUnpacked
  { _targetUnpacked_value :: Compose (Before [WhitespaceChar]) Target a
  , _target_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data TargetList a
  = TargetList
  { _targetList_head :: Target a
  , _targetList_tail
    :: Compose
         []
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           Target)
         a
  , _targetList_trailingComma :: Maybe (Before [WhitespaceChar] Comma)
  }
  deriving (Functor, Foldable, Traversable)

data ParameterList a = ParameterList
  deriving (Functor, Foldable, Traversable)

data LambdaExpressionNocond a
  = LambdaExprNocond
  { _lambdaExprNocond_params
    :: Compose
         (Between' [WhitespaceChar])
         (Compose
           Maybe
           (Compose
             (Before WhitespaceChar)
             ParameterList))
         a
  , _lambdaExprNocond_expr :: Compose (Before [WhitespaceChar]) ExpressionNocond a
  , _lambdaExprNocond_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data ExpressionNocond a
  = ExpressionNocond
  { _expressionNocond_value :: Sum OrTest LambdaExpressionNocond a
  , _expressionNocond_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data CompIter a
  = CompIter
  { _compIter_value :: Sum CompFor CompIf a
  , _compIter_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data CompIf a
  = CompIf
  { _compIf_expr :: Compose (Before [WhitespaceChar]) ExpressionNocond a
  , _compIf_iter
    :: Compose
         (Before [WhitespaceChar])
         CompIter
         a
  , _compIf_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data CompFor a
  = CompFor
  { _compFor_targets :: Compose (Between' [WhitespaceChar]) TargetList a
  , _compFor_expr :: Compose (Before [WhitespaceChar]) OrTest a
  , _compFor_iter
    :: Compose
         (Before [WhitespaceChar])
         CompIter
         a
  , _compFor_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Comprehension a
  = Comprehension
  { _comprehension_expr :: Compose (After [WhitespaceChar]) Expression a
  , _comprehension_for :: CompFor a
  , _comprehension_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Call a
  = Call
  { _call_name :: Compose (After [WhitespaceChar]) Primary a
  , _call_args ::
      Compose
        Maybe
        (Compose
          (Between' [WhitespaceChar])
          (Sum
            (Product
              (Compose (After [WhitespaceChar]) ArgList)
              (Const (Maybe Comma)))
            Comprehension))
        a
  , _call_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Primary a
  = PrimaryAtom
  { _primaryAtom_value :: Atom a
  , _primary_ann :: a
  }
  | PrimaryAttRef
  { _primaryAttRef_value :: AttRef a
  , _primary_ann :: a
  }
  | PrimarySubscription
  { _primarySubscription_value :: Subscription a
  , _primary_ann :: a
  }
  | PrimarySlicing
  { _primarySlicing_value :: Slicing a
  , _primary_ann :: a
  }
  | PrimaryCall
  { _primaryCall_value :: Call a
  , _primary_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data AwaitExpr a
  = Await
  { _await_value :: Compose (Before [WhitespaceChar]) Primary a
  , _await_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Power a
  = Power
  { _power_left :: Sum AwaitExpr Primary a
  , _power_right
    :: Compose
         Maybe
         (Compose (Before [WhitespaceChar]) UExpr)
         a
  , _power_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data UExpr a
  = UExprNone
  { _uExprNone_value :: Power a
  , _uExpr_ann :: a
  }
  | UExprNeg
  { _uExprNeg_value :: Compose (Before [WhitespaceChar]) UExpr a
  , _uExpr_ann :: a
  }
  | UExprPos
  { _uExprPos_value :: Compose (Before [WhitespaceChar]) UExpr a
  , _uExpr_ann :: a
  }
  | UExprInv
  { _uExprInv_value :: Compose (Before [WhitespaceChar]) UExpr a
  , _uExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data MExpr a
  = MExprNone
  { _mExprNone_value :: UExpr a
  , _mExpr_ann :: a
  }
  | MExprMult
  { _mExprMult_left :: Compose (After [WhitespaceChar]) MExpr a
  , _mExprMult_right :: Compose (Before [WhitespaceChar]) UExpr a
  , _mExpr_ann :: a
  }
  | MExprAt
  { _mExprAt_left :: Compose (After [WhitespaceChar]) MExpr a
  , _mExprAt_right :: Compose (Before [WhitespaceChar]) MExpr a
  , _mExpr_ann :: a
  }
  | MExprFloorDiv
  { _mExprFloorDiv_left :: Compose (After [WhitespaceChar]) MExpr a
  , _mExprFloorDiv_right :: Compose (Before [WhitespaceChar]) UExpr a
  , _mExpr_ann :: a
  }
  | MExprDiv
  { _mExprDiv_left :: Compose (After [WhitespaceChar]) MExpr a
  , _mExprDiv_right :: Compose (Before [WhitespaceChar]) UExpr a
  , _mExpr_ann :: a
  }
  | MExprMod
  { _mExprMod_left :: Compose (After [WhitespaceChar]) MExpr a
  , _mExprMod_right :: Compose (Before [WhitespaceChar]) UExpr a
  , _mExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data AExpr a
  = AExprNone
  { _aExprNone_value :: MExpr a
  , _aExpr_ann :: a
  }
  | AExprAdd
  { _aExprAdd_left :: Compose (After [WhitespaceChar]) AExpr a
  , _aExprAdd_right :: Compose (Before [WhitespaceChar]) MExpr a
  , _aExpr_ann :: a
  }
  | AExprSubtract
  { _aExprSubtract_left :: Compose (After [WhitespaceChar]) AExpr a
  , _aExprSubtract_right :: Compose (Before [WhitespaceChar]) MExpr a
  , _aExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data ShiftExpr a
  = ShiftExprNone
  { _shiftExprNone_value :: AExpr a
  , _shiftExpr_ann :: a
  }
  | ShiftExprLeft
  { _shiftExprLeft_left :: Compose (After [WhitespaceChar]) ShiftExpr a
  , _shiftExprLeft_right :: Compose (Before [WhitespaceChar]) AExpr a
  , _shiftExpr_ann :: a
  }
  | ShiftExprRight
  { _shiftExprRight_left :: Compose (After [WhitespaceChar]) ShiftExpr a
  , _shiftExprRight_right :: Compose (Before [WhitespaceChar]) AExpr a
  , _shiftExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data AndExpr a
  = AndExprNone
  { _andExprNone_value :: ShiftExpr a
  , _andExpr_ann :: a
  }
  | AndExprSome
  { _andExprSome_left :: Compose (After [WhitespaceChar]) AndExpr a
  , _andExprSome_right :: Compose (Before [WhitespaceChar]) ShiftExpr a
  , _andExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data XorExpr a
  = XorExprNone
  { _xorExprNone_value :: AndExpr a
  , _xorExpr_ann :: a
  }
  | XorExprSome
  { _xorExprSome_left :: Compose (After [WhitespaceChar]) XorExpr a
  , _xorExprSome_right :: Compose (Before [WhitespaceChar]) AndExpr a
  , _xorExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data OrExpr a
  = OrExprNone
  { _orExprNone_value :: XorExpr a
  , _orExpr_ann :: a
  }
  | OrExprSome
  { _orExprSome_left :: Compose (After [WhitespaceChar]) OrExpr a
  , _orExprSome_Right :: Compose (Before [WhitespaceChar]) XorExpr a
  , _orExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Comparison a
  = Comparison
  { _comparison_left :: OrExpr a
  , _comparison_right
    :: Compose
         []
         (Compose
           ((,) (Between' [WhitespaceChar] CompOperator))
           OrExpr)
         a
  , _comparison_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data NotTest a
  = NotTestNone
  { _notTestNone_value :: Comparison a
  , _notTest_ann :: a
  }
  | NotTestSome
  { _notTestSome_value :: Compose (Before [WhitespaceChar]) NotTest a
  , _notTest_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data AndTest a
  = AndTestNone
  { _andTestNone_value :: NotTest a
  , _andTest_ann :: a
  }
  | AndTestSome
  { _andTestSome_left :: Compose (After [WhitespaceChar]) AndTest a
  , _andTestSome_right :: Compose (Before [WhitespaceChar]) NotTest a
  , _andTest_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data OrTest a
  = OrTestNone
  { _orTestNone_value :: AndTest a
  , _orTest_ann :: a
  }
  | OrTestSome
  { _orTestSome_left :: Compose (After [WhitespaceChar]) OrTest a
  , _orTestSome_Right :: Compose (Before [WhitespaceChar]) AndTest a
  , _orTest_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Expression a
  = ExpressionConditional
  { _expressionConditional_head :: OrTest a
  , _expressionConditional_tail :: Compose Maybe (TokenF IfThenElse) a
  }
  | ExpressionLambda
  deriving (Functor, Foldable, Traversable)

data StarredItem a
  = StarredItemExpr
  { _starredItemExpr_value :: Expression a
  , _starredItem_ann :: a
  }
  | StarredItemUnpack
  { _starredItemUnpack_value :: OrExpr a
  , _starredItem_ann :: a
  } deriving (Functor, Foldable, Traversable)

data StarredExpression a
  = StarredExpressionExpr
  { _starredExpressionExpr_value :: Expression a
  , _starredExpression_ann :: a
  }
  | StarredExpressionUnpack
  { _starredExpressionUnpack_init
    :: Compose []
         (Compose (After (Token Comma)) (TokenF StarredItem))
         a
  , _starredExpressionUnpack_last :: Compose Maybe (TokenF StarredItem) a
  , _starredExpression_ann :: a
  } deriving (Functor, Foldable, Traversable)

data Enclosure a
  = EnclosureParen
  { _enclosureParen_value
    :: Compose
         (Between
           (After [WhitespaceChar] LeftParen)
           (Before [WhitespaceChar] RightParen))
         (Compose Maybe StarredExpression)
         a
  , _enclosure_ann :: a
  }
  | EnclosureList
  | EnclosureDict
  | EnclosureSet
  | EnclosureGenerator
  | EnclosureYield
  deriving (Functor, Foldable, Traversable)

data Atom a
  = AtomIdentifier
  { _atom_identifier_value :: Identifier a
  , _atom_ann :: a
  }
  | AtomLiteral
  { _atom_literal_value :: Literal a
  , _atom_ann :: a
  }
  | AtomEnclosure
  { _atom_enclosure_value :: Enclosure a
  , _atom_ann :: a
  } deriving (Functor, Foldable, Traversable)

data Comment a
  = Comment
  { _comment_text :: Before Hash Text
  , _comment_ann :: a
  } deriving (Functor, Foldable, Traversable)

data Module a
  = Module
  { _module_content :: a
  , _module_ann :: a
  } deriving (Functor, Foldable, Traversable)

deriveEq ''ShortString
deriveShow ''ShortString
deriveEq1 ''ShortString
deriveShow1 ''ShortString
makeLenses ''ShortString

deriveEq ''LongString
deriveShow ''LongString
deriveEq1 ''LongString
deriveShow1 ''LongString
makeLenses ''LongString

deriveEq ''ShortBytes
deriveShow ''ShortBytes
deriveEq1 ''ShortBytes
deriveShow1 ''ShortBytes
makeLenses ''ShortBytes

deriveEq ''LongBytes
deriveShow ''LongBytes
deriveEq1 ''LongBytes
deriveShow1 ''LongBytes
makeLenses ''LongBytes

deriveEq ''Float'
deriveShow ''Float'
deriveEq1 ''Float'
deriveShow1 ''Float'
makeLenses ''Float'

deriveEq ''StringLiteral
deriveShow ''StringLiteral
deriveEq1 ''StringLiteral
deriveShow1 ''StringLiteral
makeLenses ''StringLiteral

deriveEq ''BytesLiteral
deriveShow ''BytesLiteral
deriveEq1 ''BytesLiteral
deriveShow1 ''BytesLiteral
makeLenses ''BytesLiteral

deriveEq ''Comparison
deriveShow ''Comparison
deriveEq1 ''Comparison
deriveShow1 ''Comparison
makeLenses ''Comparison

deriveEq ''NotTest
deriveShow ''NotTest
deriveEq1 ''NotTest
deriveShow1 ''NotTest
makeLenses ''NotTest

deriveEq ''AndTest
deriveShow ''AndTest
deriveEq1 ''AndTest
deriveShow1 ''AndTest
makeLenses ''AndTest

deriveEq ''OrTest
deriveShow ''OrTest
deriveEq1 ''OrTest
deriveShow1 ''OrTest
makeLenses ''OrTest

deriveEq ''IfThenElse
deriveShow ''IfThenElse
deriveEq1 ''IfThenElse
deriveShow1 ''IfThenElse
makeLenses ''IfThenElse

deriveEq ''Expression
deriveShow ''Expression
deriveEq1 ''Expression
deriveShow1 ''Expression
makeLenses ''Expression

deriveEq ''ExpressionList
deriveShow ''ExpressionList
deriveEq1 ''ExpressionList
deriveShow1 ''ExpressionList
makeLenses ''ExpressionList

deriveEq ''Subscription
deriveShow ''Subscription
deriveEq1 ''Subscription
deriveShow1 ''Subscription
makeLenses ''Subscription

deriveEq ''Identifier
deriveShow ''Identifier
deriveEq1 ''Identifier
deriveShow1 ''Identifier
makeLenses ''Identifier

deriveEq ''AttRef
deriveShow ''AttRef
deriveEq1 ''AttRef
deriveShow1 ''AttRef
makeLenses ''AttRef

deriveEq ''ProperSlice
deriveShow ''ProperSlice
deriveEq1 ''ProperSlice
deriveShow1 ''ProperSlice
makeLenses ''ProperSlice

deriveEq ''SliceItem
deriveShow ''SliceItem
deriveEq1 ''SliceItem
deriveShow1 ''SliceItem
makeLenses ''SliceItem

deriveEq ''SliceList
deriveShow ''SliceList
deriveEq1 ''SliceList
deriveShow1 ''SliceList
makeLenses ''SliceList

deriveEq ''Slicing
deriveShow ''Slicing
deriveEq1 ''Slicing
deriveShow1 ''Slicing
makeLenses ''Slicing

deriveEq ''PositionalArgs
deriveShow ''PositionalArgs
deriveEq1 ''PositionalArgs
deriveShow1 ''PositionalArgs
makeLenses ''PositionalArgs

deriveEq ''KeywordItem
deriveShow ''KeywordItem
deriveEq1 ''KeywordItem
deriveShow1 ''KeywordItem
makeLenses ''KeywordItem

deriveEq ''StarredAndKeywords
deriveShow ''StarredAndKeywords
deriveEq1 ''StarredAndKeywords
deriveShow1 ''StarredAndKeywords
makeLenses ''StarredAndKeywords

deriveEq ''KeywordsArgs
deriveShow ''KeywordsArgs
deriveEq1 ''KeywordsArgs
deriveShow1 ''KeywordsArgs
makeLenses ''KeywordsArgs

deriveEq ''ArgList
deriveShow ''ArgList
deriveEq1 ''ArgList
deriveShow1 ''ArgList
makeLenses ''ArgList

deriveEq ''Target
deriveShow ''Target
deriveEq1 ''Target
deriveShow1 ''Target
makeLenses ''Target

deriveEq ''TargetList
deriveShow ''TargetList
deriveEq1 ''TargetList
deriveShow1 ''TargetList
makeLenses ''TargetList

deriveEq ''ParameterList
deriveShow ''ParameterList
deriveEq1 ''ParameterList
deriveShow1 ''ParameterList
makeLenses ''ParameterList

deriveEq ''LambdaExpressionNocond
deriveShow ''LambdaExpressionNocond
deriveEq1 ''LambdaExpressionNocond
deriveShow1 ''LambdaExpressionNocond
makeLenses ''LambdaExpressionNocond

deriveEq ''ExpressionNocond
deriveShow ''ExpressionNocond
deriveEq1 ''ExpressionNocond
deriveShow1 ''ExpressionNocond
makeLenses ''ExpressionNocond

makeLenses ''CompIter
deriveEq ''CompIter
deriveShow ''CompIter
deriveEq1 ''CompIter
deriveShow1 ''CompIter

makeLenses ''CompIf
deriveEq ''CompIf
deriveShow ''CompIf
deriveEq1 ''CompIf
deriveShow1 ''CompIf

makeLenses ''CompFor
deriveEq ''CompFor
deriveShow ''CompFor
deriveEq1 ''CompFor
deriveShow1 ''CompFor

makeLenses ''Comprehension
deriveEq ''Comprehension
deriveShow ''Comprehension
deriveEq1 ''Comprehension
deriveShow1 ''Comprehension

makeLenses ''Call
deriveEq ''Call
deriveShow ''Call
deriveEq1 ''Call
deriveShow1 ''Call

makeLenses ''Primary
deriveEq ''Primary
deriveShow ''Primary
deriveEq1 ''Primary
deriveShow1 ''Primary

makeLenses ''AwaitExpr
deriveEq ''AwaitExpr
deriveShow ''AwaitExpr
deriveEq1 ''AwaitExpr
deriveShow1 ''AwaitExpr

makeLenses ''Power
deriveEq ''Power
deriveShow ''Power
deriveEq1 ''Power
deriveShow1 ''Power

makeLenses ''UExpr
deriveEq ''UExpr
deriveShow ''UExpr
deriveEq1 ''UExpr
deriveShow1 ''UExpr

makeLenses ''MExpr
deriveEq ''MExpr
deriveShow ''MExpr
deriveEq1 ''MExpr
deriveShow1 ''MExpr

makeLenses ''AExpr
deriveEq ''AExpr
deriveShow ''AExpr
deriveEq1 ''AExpr
deriveShow1 ''AExpr

makeLenses ''ShiftExpr
deriveEq ''ShiftExpr
deriveShow ''ShiftExpr
deriveEq1 ''ShiftExpr
deriveShow1 ''ShiftExpr

makeLenses ''AndExpr
deriveEq ''AndExpr
deriveShow ''AndExpr
deriveEq1 ''AndExpr
deriveShow1 ''AndExpr

makeLenses ''XorExpr
deriveEq ''XorExpr
deriveShow ''XorExpr
deriveEq1 ''XorExpr
deriveShow1 ''XorExpr
  
makeLenses ''OrExpr
deriveEq ''OrExpr
deriveShow ''OrExpr
deriveEq1 ''OrExpr
deriveShow1 ''OrExpr

makeLenses ''StarredItem
deriveEq ''StarredItem
deriveShow ''StarredItem
deriveEq1 ''StarredItem
deriveShow1 ''StarredItem

makeLenses ''StarredExpression
deriveEq ''StarredExpression
deriveShow ''StarredExpression
deriveEq1 ''StarredExpression
deriveShow1 ''StarredExpression

makeLenses ''Enclosure
deriveEq ''Enclosure
deriveShow ''Enclosure
deriveEq1 ''Enclosure
deriveShow1 ''Enclosure

makeLenses ''Integer'
deriveEq ''Integer'
deriveShow ''Integer'
deriveEq1 ''Integer'
deriveShow1 ''Integer'

makeLenses ''Imag
deriveEq ''Imag
deriveShow ''Imag
deriveEq1 ''Imag
deriveShow1 ''Imag

makeLenses ''Literal
deriveEq ''Literal
deriveShow ''Literal
deriveEq1 ''Literal
deriveShow1 ''Literal

makeLenses ''Atom
deriveEq ''Atom
deriveShow ''Atom
deriveEq1 ''Atom
deriveShow1 ''Atom

makeLenses ''Comment
deriveEq ''Comment
deriveShow ''Comment
deriveEq1 ''Comment
deriveShow1 ''Comment

makeLenses ''Module
deriveEq ''Module
deriveShow ''Module
deriveEq1 ''Module
deriveShow1 ''Module
