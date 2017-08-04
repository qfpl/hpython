{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
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
import Language.Python.AST.Keywords
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
  } deriving (Eq, Foldable, Functor, Traversable, Show)

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
  } deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''ShortString
deriveShow1 ''ShortString

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
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''LongString
deriveShow1 ''LongString

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
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''ShortBytes
deriveShow1 ''ShortBytes

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
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''LongBytes
deriveShow1 ''LongBytes

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
  deriving (Eq, Show, Functor, Foldable, Traversable)

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
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Float'
deriveShow1 ''Float'

data Imag a
  = Imag
  { _imag_value
    :: Compose
         (After (Either Char_j Char_J))
         (Sum Float' (Const (NonEmpty Digit)))
         a
  , _imag_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data StringLiteral a
  = StringLiteral
  { _stringLiteral_value
    :: Compose
         (Before (Maybe StringPrefix))
         (Sum ShortString LongString)
         a
  , _stringLiteral_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''StringLiteral
deriveShow1 ''StringLiteral

data BytesLiteral a
  = BytesLiteral
  { _bytesLiteral_value
    :: Compose
         (Before (Maybe BytesPrefix))
         (Sum ShortBytes LongBytes)
         a
  , _bytesLiteral_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''BytesLiteral
deriveShow1 ''BytesLiteral

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
  deriving (Eq, Show, Functor)

data IfThenElse a
  = IfThenElse
  { _ifThenElse_if :: Compose (Between' [WhitespaceChar]) OrTest a
  , _ifThenElse_else :: Compose (Before [WhitespaceChar]) Expression a
  }

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

data ExpressionList a
  = ExpressionList
  { _expressionList_head :: Expression a
  , _expressionList_tail
    :: Compose
         []
         (Product (Between' [WhitespaceChar] Comma) Expression)
         a
  , _expressionList_trailingComma :: Maybe (Before [WhitespaceChar] Comma)
  }
  deriving (Eq, Show)

data Subscription a
  = Subscription
  { _subscription_outer :: Compose (After [WhitespaceChar]) Primary a
  , _subscription_inner :: Compose (Between' [WhitespaceChar]) ExpressionList a
  , _subscription_ann :: a
  }
  deriving (Eq, Show)

data AttRef a
  = AttRef
  { _attrRef_left :: Compose (After [WhitespaceChar]) Primary a
  , _attrRef_right :: Compose (Before [WhitespaceChar]) Identifier a
  , _attrRef_ann :: a
  }
  deriving (Eq, Show)

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
           (Compose Maybe (Before [WhitespaceChar] Expression)))
         a
  , _properSlice_ann :: a
  }
  deriving (Eq, Show)

data SliceItem a
  = SliceItemExpr
  { _sliceItemExpr_value :: Expression a
  , _sliceItem_ann :: a
  }
  | SliceItemProper
  { _sliceItemProper_value :: ProperSlice a
  , _sliceItem_ann :: a
  }
  deriving (Eq, Show)

data SliceList a
  = SliceList
  { _sliceList_head :: SliceItem a
  , _sliceList_tail
    :: Compose
         []
         (Product (Between' [WhitespaceChar] Comma) SliceItem)
         a
  , _sliceList_trailingComma :: Maybe (Before [WhitespaceChar] Comma)
  }
  deriving (Eq, Show)

data Slicing a
  = Slicing
  { _slicing_outer :: Compose (After [WhitespaceChar]) Primary a
  , _slicing_innter :: Compose (Between' [WhitespaceChar]) SliceList a
  , _slicing_ann :: a
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data KeywordItem a
  = KeywordItem
  { _keywordItem_left :: After [WhitespaceChar] Identifier
  , _keywordItem_right :: Before [WhitespaceChar] Expression
  , _keywordItem_ann :: a
  }
  deriving (Eq, Show)

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
             ((,) (After [WhitespaceChar]) Asterisk)
             Expression)
           KeywordItem)
         a
  , _starredAndKeywords_ann :: a
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data ArgList a
  = ArgListAll
  { _argList_positional :: PositionalArgs a
  , _argList_starred
    :: Compose
         Maybe
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           StarredAndKeywords)
         a
  , _argList_keyword
    :: Compose
         Maybe
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           KeywordsArgs)
         a
  , _argList_ann :: a
  }
  | ArgListStarred
  { _argList_starred :: StarredAndKeywords a
  , _argList_keyword
    :: Compose
         Maybe
         (Compose
           ((,) (Between' [WhitespaceChar] Comma))
           KeywordsArgs)
         a
  , _argList_ann :: a
  }
  | ArgListKeywords
  { _argList_keyword :: KeywordsArgs a
  , _argList_ann :: a
  }
  deriving (Eq, Show)

data Target a
  = TargetIdentifier
  { _targetIdentifier_value :: Identifier a
  , _target_ann :: a
  }
  | TargetTuple
  { _targetTuple_value :: Compose ((,) (Between' [WhitespaceChar])) TargetList a
  , _target_ann :: a
  }
  | TargetList'
  { _targetTuple_value
    :: Compose
         ((,) (Between' [WhitespaceChar]))
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
  deriving (Eq, Show)

data ParameterList a = ParameterList

data LambdaExpressionNocond a
  = LambdaExprNocond
  { _lambdaExprNocond_params
    :: Compose
         ((,) (Between' [WhitespaceChar]))
         (Compose
           Maybe
           (Compose
             (Before WhitespaceChar)
             ParameterList))
         a
  , _lambdaExprNocond_expr :: Compose (Before [WhitespaceChar]) ExpressionNocond a
  , _lambdaExprNocond_ann :: a
  }
  deriving (Eq, Show)

data ExpressionNocond a
  = ExpressionNocond
  { _expressionNocond_value :: Sum OrTest LambdaExprNocond a
  , _expressionNocond_ann :: a
  }
  deriving (Eq, Show)

data CompIter a
  = CompIter
  { _compIter_value :: Sum CompFor CompIf a
  , _compIter_ann :: a
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Comprehension a
  = Comprehension
  { _comprehension_expr :: Compose (After [WhitespaceChar]) Expression a
  , _comprehension_for :: CompFor a
  , _comprehension_ann :: a
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data AwaitExpr a
  = Await
  { _await_value :: Compose (Before [WhitespaceChar]) Primary a
  , _await_ann :: a
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)
  
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
  deriving (Eq, Show)

data Comparison a
  = Comparison
  { _comparison_left :: OrExpr a
  , _comparison_right
    :: Compose
         []
         (Product
           (Compose (Between' [WhitespaceChar]) CompOperator)
           OrExpr)
         a
  , _comparison_ann :: a
  }

data NotTest a
  = NotTestNone
  { _notTestNone_value :: Comparison a
  , _notTest_ann :: a
  }
  | NotTestSome
  { _notTestSome_value :: Compose (Before [WhitespaceChar]) NotTest a
  , _notTest_ann :: a
  }

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

data Expression a
  = ExpressionConditional
  { _expressionConditional_head :: OrTest a
  , _expressionConditional_tail :: Compose Maybe (TokenF IfThenElse) a
  }
  | ExpressionLambda

data StarredItem a
  = StarredItemExpr
  { _starredItemExpr_value :: Expression a
  , _starredItem_ann :: a
  }
  | StarredItemUnpack
  { _starredItemUnpack_value :: OrExpr a
  , _starredItem_ann :: a
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  } deriving (Eq, Show)

data Comment a
  = Comment
  { _comment_text :: Before Hash Text
  , _comment_ann :: a
  } deriving (Eq, Foldable, Functor, Traversable, Show)

data Module a
  = Module
  { _module_content :: a
  , _module_ann :: a
  } deriving (Eq, Foldable, Functor, Traversable, Show)

makeLenses ''Comment
makeLenses ''Identifier
makeLenses ''Module
