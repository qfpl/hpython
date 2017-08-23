{-# language DataKinds #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
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

import Language.Python.AST.EscapeSeq
import Language.Python.AST.Digits
import Language.Python.AST.Keywords
import Language.Python.AST.LongBytesChar
import Language.Python.AST.LongStringChar
import Language.Python.AST.ShortBytesChar
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

newtype StringEscapeSeq = StringEscapeSeq Char
  deriving (Eq, Show)

-- | Strings between one single or double quote
data ShortString a
  = ShortStringSingle
  { _shortStringSingle_value
    :: [Either (ShortStringChar SingleQuote) EscapeSeq]
  , _shortString_ann :: a
  }
  | ShortStringDouble
  { _shortStringDouble_value
    :: [Either (ShortStringChar DoubleQuote) EscapeSeq]
  , _shortString_ann :: a
  } deriving (Functor, Foldable, Traversable)

-- | Between three quotes
data LongString a
  = LongStringSingle
  { _longStringSingle_value
    :: [Either LongStringChar EscapeSeq]
  , _longStringSingle_ann :: a
  }
  | LongStringDouble
  { _longStringDouble_value
    :: [Either LongStringChar EscapeSeq]
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
    :: [Either (ShortBytesChar SingleQuote) EscapeSeq]
  , _shortBytes_ann :: a
  }
  | ShortBytesDouble
  { _shortBytesDouble_value
    :: [Either (ShortBytesChar DoubleQuote) EscapeSeq]
  , _shortBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

-- | Between triple quotes
data LongBytes a
  = LongBytesSingle
  { _longBytesSingle_value
    :: [Either Char EscapeSeq]
  , _longBytes_ann :: a
  }
  | LongBytesDouble
  { _longBytesDouble_value
    :: [Either Char EscapeSeq]
  , _longBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

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
  { _floatNoDecimal_base :: NonEmpty Digit
  , _float_exponent
    :: Maybe (Before (Either Char_e Char_E) (NonEmpty Digit))
  , _float_ann :: a
  }
  | FloatDecimalNoBase
  { _floatDecimalNoBase_fraction :: NonEmpty Digit
  , _float_exponent
    :: Maybe (Before (Either Char_e Char_E) (NonEmpty Digit))
  , _float_ann :: a
  }
  | FloatDecimalBase
  { _floatDecimalBase_base :: NonEmpty Digit
  , _floatDecimalBase_fraction :: Compose Maybe NonEmpty Digit
  , _float_exponent
    :: Maybe (Before (Either Char_e Char_E) (NonEmpty Digit))
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
  { _compIs_spaceAfter :: WhitespaceChar
  }
  | CompIsNot
  { _compIsNot_spaceBetween :: NonEmpty WhitespaceChar
  , _compIsNot_spaceAfter :: WhitespaceChar
  }
  | CompIn
  { _compIn_spaceAfter :: WhitespaceChar
  }
  | CompNotIn
  { _compNotIn_spaceBetween :: NonEmpty WhitespaceChar
  , _compNotIn_spaceAfter :: WhitespaceChar
  }
  deriving (Eq, Show)

data Argument (atomType :: AtomType) (ctxt :: ExprContext) a
  = ArgumentFor
  { _argumentFor_expr :: Test atomType ctxt a
  , _argumentFor_for
    :: Compose
         Maybe
         (Compose
           (Before [WhitespaceChar])
           (CompFor atomType ctxt))
         a
  , _argument_ann :: a    
  }
  | ArgumentDefault
  { _argumentDefault_left :: Compose (After [WhitespaceChar]) (Test atomType ctxt) a
  , _argumentDefault_right :: Compose (Before [WhitespaceChar]) (Test atomType ctxt) a
  , _argument_ann :: a    
  }
  | ArgumentUnpack
  { _argumentUnpack_symbol :: Either Asterisk DoubleAsterisk
  , _argumentUnpack_val :: Compose (Before [WhitespaceChar]) (Test atomType ctxt) a
  , _argument_ann :: a    
  }
  deriving (Functor, Foldable, Traversable)

data ArgList (atomType :: AtomType) (ctxt :: ExprContext) a
  = ArgList
  { _argList_head :: Argument atomType ctxt a
  , _argList_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Argument atomType ctxt))
         a
  , _argList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _argList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data VarargsList (atomType :: AtomType) (ctxt :: ExprContext) a
  = VarargsList
  deriving (Functor, Foldable, Traversable)

data LambdefNocond (atomType :: AtomType) (ctxt :: ExprContext) a
  = LambdefNocond
  { _lambdefNocond_args
    :: Compose
         Maybe
         (Compose
           (Between (NonEmpty WhitespaceChar) [WhitespaceChar])
           (VarargsList atomType ctxt))
         a
  , _lambdefNocond_expr
    :: Compose
         (Before [WhitespaceChar])
         (TestNocond atomType ctxt)
         a
  , _lambdefNocond_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data TestNocond (atomType :: AtomType) (ctxt :: ExprContext) a
  = TestNocond
  { _expressionNocond_value :: Sum (OrTest atomType ctxt) (LambdefNocond atomType ctxt) a
  , _expressionNocond_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data CompIter :: AtomType -> ExprContext -> * -> * where
  CompIter ::
    { _compIter_value :: Sum (CompFor atomType ctxt) (CompIf atomType ctxt) a
    , _compIter_ann :: a
    } -> CompIter 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data CompIf :: AtomType -> ExprContext -> * -> * where
  CompIf ::
    { _compIf_expr :: Compose (Before [WhitespaceChar]) (TestNocond atomType ctxt) a
    , _compIf_iter
      :: Compose
          Maybe
          (Compose (Before [WhitespaceChar]) (CompIter atomType ctxt))
          a
    , _compIf_ann :: a
    } -> CompIf 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data StarExpr (atomType :: AtomType) (ctxt :: ExprContext) a
  = StarExpr
  { _starExpr_value :: Compose (Before [WhitespaceChar]) (Expr 'Assignable ctxt) a
  , _starExpr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data ExprList :: AtomType -> ExprContext -> * -> * where
  ExprList ::
    { _exprList_head :: Sum (Expr atomType ctxt) (StarExpr atomType ctxt) a
    , _exprList_tail
      :: Compose
          []
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (Sum (Expr atomType ctxt) (StarExpr atomType ctxt)))
          a
    , _exprList_ann :: a
    } -> ExprList atomType ctxt a
  deriving (Functor, Foldable, Traversable)

data CompFor :: AtomType -> ExprContext -> * -> * where
  CompFor ::
    { _compFor_targets
      :: Compose
          (Before (Between' (NonEmpty WhitespaceChar) KFor))
          (Compose
            (After (NonEmpty WhitespaceChar))
            (ExprList 'Assignable ctxt))
          a
    , _compFor_expr :: Compose (Before (NonEmpty WhitespaceChar)) (OrTest atomExpr ctxt) a
    , _compFor_iter
      :: Compose
          Maybe
          (Compose (Before [WhitespaceChar]) (CompIter atomExpr ctxt))
          a
    , _compFor_ann :: a
    } -> CompFor 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data SliceOp :: AtomType -> ExprContext -> * -> * where
  SliceOp ::
    { _sliceOp_val
      :: Compose
          Maybe
          (Compose (Before [WhitespaceChar]) (Test atomType ctxt))
          a
    , _sliceOp_ann :: a
    } -> SliceOp 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data Subscript :: AtomType -> ExprContext -> * -> * where
  SubscriptTest ::
    { _subscriptTest_val :: Test atomType ctxt a
    , _subscript_ann :: a
    } -> Subscript 'NotAssignable ctxt a
  SubscriptSlice ::
    { _subscriptSlice_left
      :: Compose
          Maybe
          (Compose (After [WhitespaceChar]) (Test atomType ctxt))
          a
    , _subscriptSlice_right
      :: Compose
          Maybe
          (Compose (Before [WhitespaceChar]) (Test atomType ctxt))
          a
    , _subscriptSlice_sliceOp
      :: Compose
          Maybe
          (Compose (Before [WhitespaceChar]) (SliceOp atomType ctxt))
          a 
    , _subscript_ann :: a
    } -> Subscript 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data SubscriptList :: AtomType -> ExprContext -> * -> * where
  SubscriptList ::
    { _subscriptList_head :: Subscript atomType ctxt a
    , _subscriptList_tail
      :: Compose
          Maybe
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (Subscript atomType ctxt))
          a
    , _subscriptList_comma :: Maybe (Before [WhitespaceChar] Comma)
    , _subscriptList_ann :: a
    } -> SubscriptList 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data Trailer :: AtomType -> ExprContext -> * -> * where
  TrailerCall ::
    { _trailerCall_value
      :: Compose
          (Between' [WhitespaceChar])
          (Compose
            Maybe
            (ArgList atomType ctxt))
          a
    , _trailer_ann :: a
    } -> Trailer 'NotAssignable ctxt a
  TrailerSubscript ::
    { _trailerSubscript_value
      :: Compose
          (Between' [WhitespaceChar])
          (Compose
            Maybe
            (SubscriptList atomType ctxt))
          a
    , _trailer_ann :: a
    } -> Trailer 'NotAssignable ctxt a
  TrailerAccess ::
    { _trailerAccess_value :: Compose (Before [WhitespaceChar]) Identifier a
    , _trailer_ann :: a
    } -> Trailer 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data ExprContext = TopLevel | FunDef FunType
data FunType = Normal | Async
data AtomType = Assignable | NotAssignable

data AtomExpr :: AtomType -> ExprContext -> * -> * where
  AtomExprNoAwait ::
    { _atomExpr_atom :: Atom atomType ctxt a
    , _atomExpr_trailers
      :: Compose [] (Compose (Before [WhitespaceChar]) (Trailer 'NotAssignable ctxt)) a
    , _atomExpr_ann :: a
    } -> AtomExpr atomType ctxt a
  AtomExprAwait ::
    { _atomExprAwait_await :: Compose Maybe (After (NonEmpty WhitespaceChar)) KAwait
    , _atomExprAwait_atom :: Atom atomType ('FunDef 'Async) a
    , _atomExprAwait_trailers
      :: Compose
           []
           (Compose
             (Before [WhitespaceChar])
             (Trailer 'NotAssignable ('FunDef 'Async)))
           a
    , _atomExprAwait_ann :: a
    } -> AtomExpr atomType ('FunDef 'Async) a
deriving instance Eq a => Eq (AtomExpr b a)
deriving instance Functor (AtomExpr a)
deriving instance Foldable (AtomExpr a)
deriving instance Traversable (AtomExpr a)

data Power :: AtomType -> ExprContext -> * -> * where
  PowerOne ::
    { _powerOne_value :: AtomExpr atomType ctxt a
    , _power_ann :: a
    } -> Power atomType ctxt a

  PowerSome ::
    { _power_left :: AtomExpr atomType ctxt a
    , _power_right
      :: Compose
          Maybe
          (Compose
            (Before (After [WhitespaceChar] DoubleAsterisk))
            (Factor atomExpr ctxt))
          a
    , _power_ann :: a
    } -> Power 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data FactorOp
  = FactorNeg
  | FactorPos
  | FactorInv
  deriving (Eq, Show)

data Factor :: AtomType -> ExprContext -> * -> * where
  FactorNone ::
    { _factorNone_value :: Power atomType ctxt a
    , _factor_ann :: a
    } -> Factor atomType ctxt a

  FactorSome ::
    { _factorSome_value
      :: Compose
          (Before (After [WhitespaceChar] FactorOp))
          (Factor atomType ctxt)
          a
    , _factor_ann :: a
    } -> Factor 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data TermOp
  = TermMult
  | TermAt
  | TermFloorDiv
  | TermDiv
  | TermMod
  deriving (Eq, Show)

data Term :: AtomType -> ExprContext -> * -> * where
  TermOne ::
    { _termOne_value :: Factor atomType ctxt a
    , _term_ann :: a
    } -> Term atomType ctxt a

  TermSome ::
    { _termSome_left :: Factor atomType ctxt a
    , _termSome_right
      :: Compose
          []
          (Compose
            (Before (Between' [WhitespaceChar] TermOp))
            (Factor atomType ctxt))
          a
    , _term_ann :: a
    } -> Term 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data ArithExpr :: AtomType -> ExprContext -> * -> * where
  ArithExprOne ::
    { _arithExpr_left :: Term atomType ctxt a
    , _arithExpr_ann :: a
    } -> ArithExpr atomType ctxt a

  ArithExprSome ::
    { _arithExpr_left :: Term atomType ctxt a
    , _arithExpr_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] (Either Plus Minus)))
            (Term atomType ctxt))
          a
    , _arithExpr_ann :: a
    } -> ArithExpr 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data ShiftExpr :: AtomType -> ExprContext -> * -> * where
  ShiftExprOne ::
    { _shiftExprOne_value :: ArithExpr atomType ctxt a
    , _shiftExpr_ann :: a
    } -> ShiftExpr atomType ctxt a

  ShiftExprMany ::
    { _shiftExprMany_left :: ArithExpr atomType ctxt a
    , _shiftExprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] (Either DoubleLT DoubleGT)))
            (ArithExpr atomType ctxt))
          a
    , _shiftExpr_ann :: a
    } -> ShiftExpr 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data AndExpr :: AtomType -> ExprContext -> * -> * where
  AndExprOne ::
    { _andExprOne_value :: ShiftExpr atomType ctxt a
    , _andExpr_ann :: a
    } -> AndExpr atomType ctxt a

  AndExprMany ::
    { _andExprMany_left :: ShiftExpr atomType ctxt a
    , _andExprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] Ampersand))
            (ShiftExpr atomType ctxt))
          a
    , _andExpr_ann :: a
    } -> AndExpr 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data XorExpr :: AtomType -> ExprContext -> * -> * where
  XorExprOne ::
    { _xorExprOne_value :: AndExpr atomType ctxt a
    , _xorExpr_ann :: a
    } -> XorExpr atomType ctxt a
  XorExprSome ::
    { _xorExprSome_left :: AndExpr atomType ctxt a
    , _xorExprSome_right
      :: Compose
          []
          (Compose
            (Before (Between' [WhitespaceChar] Caret))
            (AndExpr atomType ctxt))
          a
    , _xorExpr_ann :: a
    } -> XorExpr 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data Expr (atomType :: AtomType) (ctxt :: ExprContext) a
  = Expr
  { _expr_left :: XorExpr ctxt a
  , _expr_right
    :: Compose
        []
        (Compose
          (Before (Between' [WhitespaceChar] Pipe))
          (XorExpr ctxt))
        a
  , _expr_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data Comparison :: AtomType -> ExprContext -> * -> * where
  ComparisonOne ::
    { _comparisonOne_value :: Expr atomType ctxt a
    , _comparison_ann :: a
    } -> Comparison atomType ctxt a
  ComparisonMany ::
    { _comparisonMany_left :: Expr atomType ctxt a
    , _comparisonMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before
              (Between' [WhitespaceChar] CompOperator))
            (Expr atomType ctxt))
          a
    , _comparison_ann :: a
    } -> Comparison 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data NotTest :: AtomType -> ExprContext -> * -> * where
  NotTestSome ::
    { _notTestSome_value
      :: Compose
          (Before (After (NonEmpty WhitespaceChar) KNot))
          (NotTest atomType ctxt)
          a
    , _notTest_ann :: a
    } -> NotTest 'NotAssignable ctxt a
  NotTestNone ::
    { _notTestNone_value :: Comparison atomType ctxt a
    , _notTest_ann :: a
    } -> NotTest atomType ctxt a
  deriving (Functor, Foldable, Traversable)

data AndTest :: AtomType -> ExprContext -> * -> * where
  AndTestOne ::
    { _andTestOne_value :: NotTest atomType ctxt a
    , _andTest_ann :: a
    } -> AndTest atomType ctxt a
    
  AndTestMany ::
    { _andTestMany_left :: NotTest atomType ctxt a
    , _andTestMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' (NonEmpty WhitespaceChar) KAnd))
            (AndTest atomType ctxt))
          a
    , _andTest_ann :: a
    } -> AndTest 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data OrTest :: AtomType -> ExprContext -> * -> * where
  OrTestOne ::
    { _orTestOne_value :: AndTest atomType ctxt a
    , _orTest_ann :: a
    } -> OrTest atomType ctxt a

  OrTestMany ::
    { _orTestMany_left :: AndTest atomType ctxt a
    , _orTestMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' (NonEmpty WhitespaceChar) KOr))
            (AndTest atomType ctxt))
          a
    , _orTest_ann :: a
    } -> OrTest 'NotAssignable ctxt a
  deriving (Functor, Foldable, Traversable)

data IfThenElse (ctxt :: ExprContext) a
  = IfThenElse
  { _ifThenElse_if :: Compose (Between' (NonEmpty WhitespaceChar)) (OrTest ctxt) a
  , _ifThenElse_else :: Compose (Before (NonEmpty WhitespaceChar)) (Test ctxt) a
  }
  deriving (Functor, Foldable, Traversable)

data Test (atomType :: AtomType) (ctxt :: ExprContext) a
  = TestCond
  { _testCond_head :: OrTest atomType ctxt a
  , _testCond_tail
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           (IfThenElse atomType ctxt))
         a
  , _test_ann :: a
  }
  | TestLambdef
  deriving (Functor, Foldable, Traversable)

data TestList (ctxt :: ExprContext) a
  = TestList
  { _testList_head :: Test ctxt a
  , _testList_tail :: Compose (Before (Between' [WhitespaceChar] Comma)) (Test ctxt) a
  , _testList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _testList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data YieldArg (ctxt :: ExprContext) a
  = YieldArgFrom
  { _yieldArgFrom_value :: Compose (Before (NonEmpty WhitespaceChar)) (Test ctxt) a
  , _yieldArg_ann :: a
  }
  | YieldArgList
  { _yieldArgList_value :: TestList ctxt a
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
          (YieldArg ('FunDef 'Normal)))
        a
  , _yieldExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)

data TestlistComp :: AtomType -> ExprContext -> * -> * where
  TestlistCompFor ::
    { _testlistComp_head :: Sum (Test atomType ctxt) (StarExpr atomType ctxt) a
    , _testlistCompFor_tail :: Compose (Before [WhitespaceChar]) (CompFor atomType ctxt) a
    , _testlistComp_ann :: a
    } -> TestlistComp 'NotAssignable ctxt a

  TestlistCompList ::
    { _testlistComp_head :: Sum (Test atomType ctxt) (StarExpr atomType ctxt) a
    , _testlistCompList_tail
      :: Compose
          []
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (Sum (Test atomType ctxt) (StarExpr atomType ctxt)))
          a
    , _testlistCompList_comma :: Maybe (Before [WhitespaceChar] Comma)
    , _testlistComp_ann :: a
    } -> TestlistComp atomType ctxt a
  deriving (Functor, Foldable, Traversable)

data DictOrSetMaker (atomType :: AtomType) (ctxt :: ExprContext) a
  = DictOrSetMaker
  deriving (Functor, Foldable, Traversable)

data Atom :: AtomType -> ExprContext -> * -> * where
  AtomParenNoYield ::
    { _atomParenNoYield_val
      :: Compose
          (Between' [WhitespaceChar])
          (Compose
            Maybe
            (TestlistComp atomType ctxt))
          a
    , _atomParenNoYield_ann :: a
    } -> Atom atomType ctxt a

  -- A yield expression can only be used within a normal function definition
  AtomParenYield ::
    { _atomParenYield_val
      :: Compose
          (Between' [WhitespaceChar])
          YieldExpr
          a
    , _atomParenYield_ann :: a
    } -> Atom 'NotAssignable ('FunDef 'Normal) a

  AtomBracket ::
    { _atomBracket_val
      :: Compose
          (Between' [WhitespaceChar])
          (Compose
            Maybe
            (TestlistComp atomType ctxt))
          a
    , _atom_ann :: a
    } -> Atom atomType ctxt a

  AtomCurly ::
    { _atomCurly_val
      :: Compose
          (Between' [WhitespaceChar])
          (Compose
            Maybe
            (DictOrSetMaker atomType ctxt))
          a
    , _atom_ann :: a
    } -> Atom atomType ctxt a

  AtomIdentifier ::
    { _atomIdentifier_value :: Identifier a
    , _atom_ann :: a
    } -> Atom 'Assignable ctxt a

  AtomInteger ::
    { _atomInteger :: Integer' a
    , _atom_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomFloat ::
    { _atomFloat :: Float' a
    , _atom_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomString ::
    { _atomString_head :: Sum StringLiteral BytesLiteral a
    , _atomString_tail
      :: Compose
          []
          (Compose
            (Before [WhitespaceChar])
            (Sum StringLiteral BytesLiteral))
          a
    , _atom_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomEllipsis ::
    { _atom_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomNone ::
    { _atom_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomTrue ::
    { _atom_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomFalse ::
    { _atom_ann :: a
    } -> Atom 'NotAssignable ctxt a
deriving instance Eq a => Eq (Atom ctxt a)
deriving instance Functor (Atom ctxt)
deriving instance Foldable (Atom ctxt)
deriving instance Traversable (Atom ctxt)

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
