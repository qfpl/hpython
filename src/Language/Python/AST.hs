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
import Language.Python.AST.Keywords
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

newtype StringEscapeSeq = StringEscapeSeq Char
  deriving (Eq, Show)

-- | Strings between one single or double quote
data ShortString a
  = ShortStringSingle
  { _shortStringSingle_value
    :: [Either (ShortStringChar SingleQuote) StringEscapeSeq]
  , _shortString_ann :: a
  }
  | ShortStringDouble
  { _shortStringDouble_value
    :: [Either (ShortStringChar DoubleQuote) StringEscapeSeq]
  , _shortString_ann :: a
  } deriving (Functor, Foldable, Traversable)

-- | Between three quotes
data LongString a
  = LongStringSingle
  { _longStringSingle_value
    :: [Either LongStringChar StringEscapeSeq]
  , _longStringSingle_ann :: a
  }
  | LongStringDouble
  { _longStringDouble_value
    :: [Either LongStringChar StringEscapeSeq]
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
    :: [Either (ShortBytesChar SingleQuote) BytesEscapeSeq]
  , _shortBytes_ann :: a
  }
  | ShortBytesDouble
  { _shortBytesDouble_value
    :: [Either (ShortBytesChar DoubleQuote) BytesEscapeSeq]
  , _shortBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

-- | Between triple quotes
data LongBytes a
  = LongBytesSingle
  { _longBytesSingle_value
    :: [Either LongBytesChar BytesEscapeSeq]
  , _longBytes_ann :: a
  }
  | LongBytesDouble
  { _longBytesDouble_value
    :: [Either LongBytesChar BytesEscapeSeq]
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
  = FloatNoDecimal
  { _floatNoDecimal_base :: Integer' a
  , _float_exponent
    :: Compose
         Maybe
         (Compose
           (Before (Either Char_e Char_E))
           Integer')
         a
  , _float_ann :: a
  }
  | FloatDecimalNoBase
  { _floatDecimalNoBase_fraction :: Integer' a
  , _float_exponent
    :: Compose
         Maybe
         (Compose
           (Before (Either Char_e Char_E))
           Integer')
         a
  , _float_ann :: a
  }
  | FloatDecimalBase
  { _floatDecimalBase_base :: Integer' a
  , _floatDecimalBase_fraction :: Compose Maybe Integer' a
  , _float_exponent
    :: Compose
         Maybe
         (Compose
           (Before (Either Char_e Char_E))
           Integer')
         a
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
  { _bytesLiteral_prefix :: BytesPrefix
  , _bytesLiteral_value :: Sum ShortBytes LongBytes a
  , _bytesLiteral_ann :: a
  } deriving (Functor, Foldable, Traversable)

data Literal a
  = LiteralString
  { _literalString_head :: Sum StringLiteral BytesLiteral a
  , _literalString_tail
    :: Compose
         []
         (Compose (Before [WhitespaceChar]) (Sum StringLiteral BytesLiteral))
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

data Argument a
  = ArgumentFor
  { _argumentFor_expr :: Test a
  , _argumentFor_for
    :: Compose
         Maybe
         (Compose
           (Before [WhitespaceChar])
           CompFor)
         a
  , _argument_ann :: a    
  }
  | ArgumentDefault
  { _argumentDefault_left :: Compose (After [WhitespaceChar]) Test a
  , _argumentDefault_right :: Compose (Before [WhitespaceChar]) Test a
  , _argument_ann :: a    
  }
  | ArgumentUnpack
  { _argumentUnpack_symbol :: Either Asterisk DoubleAsterisk
  , _argumentUnpack_val :: Compose (Before [WhitespaceChar]) Test a
  , _argument_ann :: a    
  }
  deriving (Functor, Foldable, Traversable)

data ArgList a
  = ArgList
  { _argList_head :: Argument a
  , _argList_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           Argument)
         a
  , _argList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _argList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data VarargsList a
  = VarargsList
  deriving (Functor, Foldable, Traversable)

data LambdefNocond a
  = LambdefNocond
  { _lambdefNocond_args
    :: Compose
         Maybe
         (Compose
           (Between (NonEmpty WhitespaceChar) [WhitespaceChar])
           VarargsList)
         a
  , _lambdefNocond_expr
    :: Compose
         (Before [WhitespaceChar])
         TestNocond
         a
  , _lambdefNocond_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data TestNocond a
  = TestNocond
  { _expressionNocond_value :: Sum OrTest LambdefNocond a
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
  { _compIf_expr :: Compose (Before [WhitespaceChar]) TestNocond a
  , _compIf_iter
    :: Compose
         Maybe
         (Compose (Before [WhitespaceChar]) CompIter)
         a
  , _compIf_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data StarExpr a
  = StarExpr
  { _starExpr_value :: Compose (Before [WhitespaceChar]) Expr a
  , _starExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data ExprList a
  = ExprList
  { _exprList_head :: Sum Expr StarExpr a
  , _exprList_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum Expr StarExpr))
         a
  , _exprList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data CompFor a
  = CompFor
  { _compFor_targets :: Compose (Between' [WhitespaceChar]) ExprList a
  , _compFor_expr :: Compose (Before [WhitespaceChar]) OrTest a
  , _compFor_iter
    :: Compose
         Maybe
         (Compose (Before [WhitespaceChar]) CompIter)
         a
  , _compFor_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data SliceOp a
  = SliceOp
  { _sliceOp_val
    :: Compose
         Maybe
         (Compose (Before [WhitespaceChar]) Test)
         a
  , _sliceOp_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Subscript a
  = SubscriptTest
  { _subscriptTest_val :: Test a
  , _subscript_ann :: a
  }
  | SubscriptSlice
  { _subscriptSlice_left
    :: Compose
         Maybe
         (Compose (After [WhitespaceChar]) Test)
         a
  , _subscriptSlice_right
    :: Compose
         Maybe
         (Compose (Before [WhitespaceChar]) Test)
         a
  , _subscriptSlice_sliceOp
    :: Compose
         Maybe
         (Compose (Before [WhitespaceChar]) SliceOp)
         a 
  , _subscript_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data SubscriptList a
  = SubscriptList
  { _subscriptList_head :: Subscript a
  , _subscriptList_tail
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           Subscript)
         a
  , _subscriptList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _subscriptList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Trailer a
  = TrailerCall
  { _trailerCall_value
    :: Compose
         Maybe
         (Compose (Between' [WhitespaceChar]) ArgList)
         a
  , _trailer_ann :: a
  }
  | TrailerSubscript
  { _trailerSubscript_value
    :: Compose
         Maybe
         (Compose (Between' [WhitespaceChar]) SubscriptList)
         a
  , _trailer_ann :: a
  }
  | TrailerAccess
  { _trailerAccess_value :: Compose (Before [WhitespaceChar]) Identifier a
  , _trailer_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data AtomExpr a
  = AtomExpr
  { _atomExpr_await :: Compose Maybe (After (NonEmpty WhitespaceChar)) KAwait
  , _atomExpr_atom :: Compose (After [WhitespaceChar]) Atom a
  , _atomExpr_trailers
    :: Compose [] (Compose (Before [WhitespaceChar]) Trailer) a
  , _atomExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Power a
  = Power
  { _power_left :: AtomExpr a
  , _power_right
    :: Compose
         Maybe
         (Compose
           (Before (After [WhitespaceChar] DoubleAsterisk))
           Factor)
         a
  , _power_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data FactorOp
  = FactorNeg
  | FactorPos
  | FactorInv
  deriving (Eq, Show)

data Factor a
  = FactorNone
  { _factorNone_value :: Power a
  , _factor_ann :: a
  }
  | FactorSome
  { _factorSome_value
    :: Compose
         (Before (After [WhitespaceChar] FactorOp))
         Factor
         a
  , _factor_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data TermOp
  = TermMult
  | TermAt
  | TermFloorDiv
  | TermDiv
  | TermMod
  deriving (Eq, Show)

data Term a
  = Term
  { _term_left :: Factor a
  , _term_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] TermOp))
           Factor)
         a
  , _term_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data ArithExpr a
  = ArithExpr
  { _arithExpr_left :: Term a
  , _arithExpr_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] (Either Plus Minus)))
           Term)
         a
  , _arithExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data ShiftExpr a
  = ShiftExpr
  { _shiftExpr_left :: ArithExpr a
  , _shiftExpr_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] (Either DoubleLT DoubleGT)))
           ArithExpr)
         a
  , _shiftExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data AndExpr a
  = AndExpr
  { _andExpr_left :: ShiftExpr a
  , _andExprSome_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Ampersand))
           ShiftExpr)
         a
  , _andExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data XorExpr a
  = XorExpr
  { _xorExpr_left :: AndExpr a
  , _xorExprSome_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Caret))
           AndExpr)
         a
  , _xorExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Expr a
  = Expr
  { _expr_left :: XorExpr a
  , _expr_right
    :: Compose
        []
        (Compose
          (Before (Between' [WhitespaceChar] Pipe))
          XorExpr)
        a
  , _expr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Comparison a
  = Comparison
  { _comparison_left :: Expr a
  , _comparison_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] CompOperator))
           Expr)
         a
  , _comparison_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data NotTest a
  = NotTestSome
  { _notTestSome_value
    :: Compose
         (Before (After (NonEmpty WhitespaceChar) KNot))
         NotTest
         a
  , _notTest_ann :: a
  }
  | NotTestNone
  { _notTestNone_value :: Comparison a
  , _notTest_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data AndTest a
  = AndTest
  { _andTest_left :: NotTest a
  , _andTest_right
    :: Compose
         []
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAnd))
           AndTest)
         a
  , _andTest_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data OrTest a
  = OrTest
  { _orTest_left :: AndTest a
  , _orTest_right
    :: Compose
         []
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KOr))
           AndTest)
         a
  , _orTest_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data IfThenElse a
  = IfThenElse
  { _ifThenElse_if :: Compose (Between' (NonEmpty WhitespaceChar)) OrTest a
  , _ifThenElse_else :: Compose (Before (NonEmpty WhitespaceChar)) Test a
  }
  deriving (Functor, Foldable, Traversable)

data Test a
  = TestCond
  { _testCond_head :: OrTest a
  , _testCond_tail
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           IfThenElse)
         a
  , _test_ann :: a
  }
  | TestLambdef
  deriving (Functor, Foldable, Traversable)

data TestList a
  = TestList
  { _testList_head :: Test a
  , _testList_tail :: Compose (Before (Between' [WhitespaceChar] Comma)) Test a
  , _testList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _testList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data YieldArg a
  = YieldArgFrom
  { _yieldArgFrom_value :: Compose (Before (NonEmpty WhitespaceChar)) Test a
  , _yieldArg_ann :: a
  }
  | YieldArgList
  { _yieldArgList_value :: TestList a
  , _yieldArg_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data YieldExpr a
  = YieldExpr
  { _yieldExpr_value
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           YieldArg)
         a
  , _yieldExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data TestlistComp a
  = TestlistCompFor
  { _testlistComp_head :: Sum Test StarExpr a
  , _testlistCompFor_tail :: Compose (Before [WhitespaceChar]) CompFor a
  , _testlistComp_ann :: a
  }
  | TestlistCompList
  { _testlistComp_head :: Sum Test StarExpr a
  , _testlistCompList_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum Test StarExpr))
         a
  , _testlistCompList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _testlistComp_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data DictOrSetMaker a
  = DictOrSetMaker
  deriving (Functor, Foldable, Traversable)

data Atom a
  = AtomParen
  { _atomParen_val
    :: Compose
         (Between' [WhitespaceChar])
         (Sum YieldExpr TestlistComp)
         a
  , _atom_ann :: a
  }
  | AtomBracket
  { _atomBracket_val
    :: Compose
         (Between' [WhitespaceChar])
         TestlistComp
         a
  , _atom_ann :: a
  }
  | AtomCurly
  { _atomCurly_val
    :: Compose
         (Between' [WhitespaceChar])
         DictOrSetMaker
         a
  , _atom_ann :: a
  }
  | AtomIdentifier
  { _atomIdentifier_value :: Identifier a
  , _atom_ann :: a
  }
  | AtomInteger
  { _atomInteger :: Integer' a
  , _atom_ann :: a
  }
  | AtomFloat
  { _atomFloat :: Float' a
  , _atom_ann :: a
  }
  | AtomString
  { _atomString_value :: Compose NonEmpty (Sum StringLiteral BytesLiteral) a
  , _atom_ann :: a
  }
  | AtomEllipsis
  { _atom_ann :: a
  }
  | AtomNone
  { _atom_ann :: a
  }
  | AtomTrue
  { _atom_ann :: a
  }
  | AtomFalse
  { _atom_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Comment a
  = Comment
  { _comment_text :: Text
  , _comment_ann :: a
  } deriving (Functor, Foldable, Traversable)

data PythonModule a
  = PythonModule
  { _pythonModule_content :: a
  , _pythonModule_ann :: a
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

deriveEq ''Test
deriveShow ''Test
deriveEq1 ''Test
deriveShow1 ''Test
makeLenses ''Test

deriveEq ''TestList
deriveShow ''TestList
deriveEq1 ''TestList
deriveShow1 ''TestList
makeLenses ''TestList

deriveEq ''Identifier
deriveShow ''Identifier
deriveEq1 ''Identifier
deriveShow1 ''Identifier
makeLenses ''Identifier

deriveEq ''Argument
deriveShow ''Argument
deriveEq1 ''Argument
deriveShow1 ''Argument
makeLenses ''Argument

deriveEq ''ArgList
deriveShow ''ArgList
deriveEq1 ''ArgList
deriveShow1 ''ArgList
makeLenses ''ArgList

deriveEq ''VarargsList
deriveShow ''VarargsList
deriveEq1 ''VarargsList
deriveShow1 ''VarargsList
makeLenses ''VarargsList

deriveEq ''LambdefNocond
deriveShow ''LambdefNocond
deriveEq1 ''LambdefNocond
deriveShow1 ''LambdefNocond
makeLenses ''LambdefNocond

deriveEq ''TestNocond
deriveShow ''TestNocond
deriveEq1 ''TestNocond
deriveShow1 ''TestNocond
makeLenses ''TestNocond

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

makeLenses ''StarExpr
deriveEq ''StarExpr
deriveShow ''StarExpr
deriveEq1 ''StarExpr
deriveShow1 ''StarExpr

makeLenses ''ExprList
deriveEq ''ExprList
deriveShow ''ExprList
deriveEq1 ''ExprList
deriveShow1 ''ExprList

makeLenses ''SliceOp
deriveEq ''SliceOp
deriveShow ''SliceOp
deriveEq1 ''SliceOp
deriveShow1 ''SliceOp

makeLenses ''Subscript
deriveEq ''Subscript
deriveShow ''Subscript
deriveEq1 ''Subscript
deriveShow1 ''Subscript

makeLenses ''SubscriptList
deriveEq ''SubscriptList
deriveShow ''SubscriptList
deriveEq1 ''SubscriptList
deriveShow1 ''SubscriptList

makeLenses ''CompFor
deriveEq ''CompFor
deriveShow ''CompFor
deriveEq1 ''CompFor
deriveShow1 ''CompFor

makeLenses ''Trailer
deriveEq ''Trailer
deriveShow ''Trailer
deriveEq1 ''Trailer
deriveShow1 ''Trailer

makeLenses ''AtomExpr
deriveEq ''AtomExpr
deriveShow ''AtomExpr
deriveEq1 ''AtomExpr
deriveShow1 ''AtomExpr

makeLenses ''Power
deriveEq ''Power
deriveShow ''Power
deriveEq1 ''Power
deriveShow1 ''Power

makeLenses ''Factor
deriveEq ''Factor
deriveShow ''Factor
deriveEq1 ''Factor
deriveShow1 ''Factor

makeLenses ''Term
deriveEq ''Term
deriveShow ''Term
deriveEq1 ''Term
deriveShow1 ''Term

makeLenses ''ArithExpr
deriveEq ''ArithExpr
deriveShow ''ArithExpr
deriveEq1 ''ArithExpr
deriveShow1 ''ArithExpr

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
  
makeLenses ''Expr
deriveEq ''Expr
deriveShow ''Expr
deriveEq1 ''Expr
deriveShow1 ''Expr

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

makeLenses ''YieldArg
deriveEq ''YieldArg
deriveShow ''YieldArg
deriveEq1 ''YieldArg
deriveShow1 ''YieldArg

makeLenses ''YieldExpr
deriveEq ''YieldExpr
deriveShow ''YieldExpr
deriveEq1 ''YieldExpr
deriveShow1 ''YieldExpr

makeLenses ''TestlistComp
deriveEq ''TestlistComp
deriveShow ''TestlistComp
deriveEq1 ''TestlistComp
deriveShow1 ''TestlistComp

makeLenses ''DictOrSetMaker
deriveEq ''DictOrSetMaker
deriveShow ''DictOrSetMaker
deriveEq1 ''DictOrSetMaker
deriveShow1 ''DictOrSetMaker

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

makeLenses ''PythonModule
deriveEq ''PythonModule
deriveShow ''PythonModule
deriveEq1 ''PythonModule
deriveShow1 ''PythonModule

deriveEq1 ''NonEmpty
deriveShow1 ''NonEmpty
