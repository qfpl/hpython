{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.IR where

import Papa hiding (Plus, Sum, Product)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Identifier
import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.IR.ArgsList
import Language.Python.IR.ArgumentList
import Language.Python.IR.TestlistStarExpr
import Language.Python.Expr.AST.BytesLiteral
import Language.Python.Expr.AST.CompOperator
import Language.Python.Expr.AST.FactorOperator
import Language.Python.Expr.AST.Float
import Language.Python.Expr.AST.Imag
import Language.Python.Expr.AST.Integer
import Language.Python.Expr.AST.StringLiteral
import Language.Python.Expr.AST.TermOperator

data Argument a
  = ArgumentFor
  { _argumentFor_expr :: Test a
  , _argumentFor_for
    :: Compose
        Maybe
        CompFor
        a
  , _argument_ann :: a
  }
  | ArgumentForParens
  { _argumentForParens_lparen :: After [WhitespaceChar] LeftParen
  , _argumentForParens_expr :: Test a
  , _argumentForParens_for :: CompFor a
  , _argumentForParens_rparen :: Before [WhitespaceChar] RightParen
  , _argument_ann :: a
  }
  | ArgumentDefault
  { _argumentDefault_left
    :: Compose
          (After [WhitespaceChar])
          Test
          a
  , _argumentDefault_right
    :: Compose
          (Before [WhitespaceChar])
          Test
          a
  , _argument_ann :: a
  }
  | ArgumentUnpack
  { _argumentUnpack_symbol :: Either Asterisk DoubleAsterisk
  , _argumentUnpack_val
    :: Compose
          (Before [WhitespaceChar])
          Test
          a
  , _argument_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Argument a)
deriving instance Show a => Show (Argument a)
deriving instance Ord a => Ord (Argument a)

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

data LambdefNocond a
  = LambdefNocond
  { _lambdefNocond_args
    :: Compose
         Maybe
         (Compose
           (Between (NonEmpty WhitespaceChar) [WhitespaceChar])
           (ArgsList Identifier Test))
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
  { _expressionNocond_value
    :: Sum
         OrTest
         LambdefNocond
         a
  , _expressionNocond_ann :: a
  } deriving (Functor, Foldable, Traversable)

data CompIter a
  = CompIter
  { _compIter_value :: Sum CompFor CompIf a
  , _compIter_ann :: a
  } deriving (Functor, Foldable, Traversable)

data CompIf a
  = CompIf
  { _compIf_if :: Between' (NonEmpty WhitespaceChar) KIf
  , _compIf_expr :: TestNocond a
  , _compIf_iter
    :: Compose
        Maybe
        (Compose
          (Before [WhitespaceChar])
          CompIter)
        a
  , _compIf_ann :: a
  } deriving (Functor, Foldable, Traversable)

data StarExpr a
  = StarExpr
  { _starExpr_value
    :: Compose
         (Before [WhitespaceChar])
         Expr
         a
  , _starExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)

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
  , _exprList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _exprList_ann :: a
  } deriving (Functor, Foldable, Traversable)

data CompFor a
  = CompFor
  { _compFor_targets
    :: Compose
        (Before (Between' (NonEmpty WhitespaceChar) KFor))
        (Compose
          (After (NonEmpty WhitespaceChar))
          (TestlistStarExpr Expr StarExpr))
        a
  , _compFor_expr :: Compose (Before (NonEmpty WhitespaceChar)) OrTest a
  , _compFor_iter
    :: Compose
        Maybe
        (Compose (Before [WhitespaceChar]) CompIter)
        a
  , _compFor_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (CompFor a)
deriving instance Show a => Show (CompFor a)
deriving instance Ord a => Ord (CompFor a)

data SliceOp a
  = SliceOp
  { _sliceOp_val
    :: Compose
        Maybe
        (Compose (Before [WhitespaceChar]) Test)
        a
  , _sliceOp_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SliceOp a)
deriving instance Show a => Show (SliceOp a)
deriving instance Ord a => Ord (SliceOp a)

data Subscript a
  = SubscriptTest
  { _subscriptTest_val :: Test a
  , _subscript_ann :: a
  }
  | SubscriptSlice
  { _subscriptSlice_left
    :: Compose
         (After [WhitespaceChar])
         (Compose
           Maybe
           Test)
         a
  , _subscriptSlice_colon :: After [WhitespaceChar] Colon
  , _subscriptSlice_right
    :: Compose
        Maybe
        (Compose (After [WhitespaceChar]) Test)
        a
  , _subscriptSlice_sliceOp
    :: Compose
        Maybe
        (Compose (After [WhitespaceChar]) SliceOp)
        a
  , _subscript_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Subscript a)
deriving instance Show a => Show (Subscript a)
deriving instance Ord a => Ord (Subscript a)

data SubscriptList a
  = SubscriptList
  { _subscriptList_head :: Subscript a
  , _subscriptList_tail
    :: Compose
        []
        (Compose
          (Before (Between' [WhitespaceChar] Comma))
          Subscript)
        a
  , _subscriptList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _subscriptList_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SubscriptList a)
deriving instance Show a => Show (SubscriptList a)
deriving instance Ord a => Ord (SubscriptList a)

data Trailer a
  = TrailerCall
  { _trailerCall_value
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          (ArgumentList Identifier Test))
        a
  , _trailer_ann :: a
  }
  | TrailerSubscript
  { _trailerSubscript_value
    :: Compose
        (Between' [WhitespaceChar])
        SubscriptList
        a
  , _trailer_ann :: a
  }
  | TrailerAccess
  { _trailerAccess_value :: Compose (Before [WhitespaceChar]) Identifier a
  , _trailer_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Trailer a)
deriving instance Show a => Show (Trailer a)
deriving instance Ord a => Ord (Trailer a)

data AtomExpr a
  = AtomExprSingle
  { _atomExpr_await
    :: Maybe (After (NonEmpty WhitespaceChar) KAwait)
  , _atomExprSingle_atom :: Atom a
  , _atomExpr_ann :: a
  }
  | AtomExprTrailers
  { _atomExpr_await
    :: Maybe (After (NonEmpty WhitespaceChar) KAwait)
  , _atomExprTrailers_atom :: AtomNoInt a
  , _atomExprTrailers_trailers
    :: Compose
          NonEmpty
          (Compose
            (Before [WhitespaceChar])
            Trailer)
          a
  , _atomExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AtomExpr a)
deriving instance Show a => Show (AtomExpr a)
deriving instance Ord a => Ord (AtomExpr a)

data Power a
  = Power
  { _power_left :: AtomExpr a
  , _power_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] DoubleAsterisk))
           Factor)
         a
  , _power_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Power a)
deriving instance Show a => Show (Power a)
deriving instance Ord a => Ord (Power a)

data Factor a
  = FactorNone
  { _factorNone_value :: Power a
  , _factor_ann :: a
  }
  | FactorOne
  { _factorOne_op :: After [WhitespaceChar] FactorOperator
  , _factorOne_value :: Factor a
  , _factorSome_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Factor a)
deriving instance Show a => Show (Factor a)
deriving instance Ord a => Ord (Factor a)

data Term a
  = Term
  { _term_left :: Factor a
  , _term_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] TermOperator))
           Factor)
         a
  , _termMany_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Term a)
deriving instance Show a => Show (Term a)
deriving instance Ord a => Ord (Term a)

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
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ArithExpr a)
deriving instance Show a => Show (ArithExpr a)
deriving instance Ord a => Ord (ArithExpr a)

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
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ShiftExpr a)
deriving instance Show a => Show (ShiftExpr a)
deriving instance Ord a => Ord (ShiftExpr a)

data AndExpr a
  = AndExpr
  { _andExpr_left :: ShiftExpr a
  , _andExpr_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Ampersand))
           ShiftExpr)
         a
  , _andExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AndExpr a)
deriving instance Show a => Show (AndExpr a)
deriving instance Ord a => Ord (AndExpr a)

data XorExpr a
  = XorExpr
  { _xorExpr_left :: AndExpr a
  , _xorExpr_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Caret))
           AndExpr)
         a
  , _xorExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (XorExpr a)
deriving instance Show a => Show (XorExpr a)
deriving instance Ord a => Ord (XorExpr a)

data Expr a
  = Expr
  { _expr_value :: XorExpr a
  , _expr_right
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Pipe))
           XorExpr)
        a
  , _expr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Expr a)
deriving instance Show a => Show (Expr a)
deriving instance Ord a => Ord (Expr a)

data Comparison a
  = Comparison
  { _comparison_left :: Expr a
  , _comparison_right
    :: Compose
         []
         (Compose
           (Before CompOperator)
           Expr)
         a
  , _comparison_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Comparison a)
deriving instance Show a => Show (Comparison a)
deriving instance Ord a => Ord (Comparison a)

data NotTest a
  = NotTestMany
  { _notTestMany_value
    :: Compose
        (Before (After (NonEmpty WhitespaceChar) KNot))
        NotTest
        a
  , _notTestMany_ann :: a
  }
  | NotTestOne
  { _notTestNone_value :: Comparison a
  , _notTestNone_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (NotTest a)
deriving instance Show a => Show (NotTest a)
deriving instance Ord a => Ord (NotTest a)

data AndTest a
  = AndTest
  { _andTest_left :: NotTest a
  , _andTest_right
    :: Compose
         []
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAnd))
           NotTest)
         a
  , _andTest_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AndTest a)
deriving instance Show a => Show (AndTest a)
deriving instance Ord a => Ord (AndTest a)

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
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (OrTest a)
deriving instance Show a => Show (OrTest a)
deriving instance Ord a => Ord (OrTest a)

data IfThenElse a
  = IfThenElse
  { _ifThenElse_if :: Between' (NonEmpty WhitespaceChar) KIf
  , _ifThenElse_value1 :: OrTest a
  , _ifThenElse_else :: Between' (NonEmpty WhitespaceChar) KElse
  , _ifThenElse_value2 :: Test a
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
  { _testLambdef_value :: Lambdef a
  , _test_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Test a)
deriving instance Show a => Show (Test a)
deriving instance Ord a => Ord (Test a)

data Lambdef a
  = Lambdef
  { _lambdef_args
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           (ArgsList Identifier Test))
         a
  , _lambdef_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Test
         a
  , _lambdef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Lambdef a)
deriving instance Show a => Show (Lambdef a)
deriving instance Ord a => Ord (Lambdef a)

data TestList a
  = TestList
  { _testList_head :: Test a
  , _testList_tail
    :: Compose
       []
       (Compose
         (Before (Between' [WhitespaceChar] Comma))
         Test)
       a
  , _testList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _testList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (TestList a)
deriving instance Show a => Show (TestList a)
deriving instance Ord a => Ord (TestList a)

data YieldArg a
  = YieldArgFrom
  { _yieldArgFrom_value
    :: Compose (Before (NonEmpty WhitespaceChar)) Test a
  , _yieldArgFrom_ann :: a
  }
  | YieldArgList
  { _yieldArgList_value :: TestList a
  , _yieldArgList_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (YieldArg a)
deriving instance Show a => Show (YieldArg a)
deriving instance Ord a => Ord (YieldArg a)

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
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (YieldExpr a)
deriving instance Show a => Show (YieldExpr a)
deriving instance Ord a => Ord (YieldExpr a)

data TupleTestlistComp a
  = TupleTestlistCompFor
  { _tupleTestlistCompFor_head :: Sum Test StarExpr a
  , _tupleTestlistCompFor_tail :: CompFor a
  , _tupleTestlistCompFor_ann :: a
  }

  | TupleTestlistCompList
  { _tupleTestlistCompList_head :: Sum Test StarExpr a
  , _tupleTestlistCompList_tail
    :: Compose
        []
        (Compose
          (Before (Between' [WhitespaceChar] Comma))
          (Sum Test StarExpr))
        a
  , _tupleTestlistCompList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _tupleTestlistCompList_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (TupleTestlistComp a)
deriving instance Show a => Show (TupleTestlistComp a)
deriving instance Ord a => Ord (TupleTestlistComp a)

data ListTestlistComp a
  = ListTestlistCompFor
  { _listTestlistCompFor_head :: Sum Test StarExpr a
  , _listTestlistCompFor_tail :: CompFor a
  , _listTestlistCompFor_ann :: a
  }

  | ListTestlistCompList
  { _listTestlistCompList_head :: Sum Test StarExpr a
  , _listTestlistCompList_tail
    :: Compose
        []
        (Compose
          (Before (Between' [WhitespaceChar] Comma))
          (Sum Test StarExpr))
        a
  , _listTestlistCompList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _listTestlistCompList_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ListTestlistComp a)
deriving instance Show a => Show (ListTestlistComp a)
deriving instance Ord a => Ord (ListTestlistComp a)

data DictItem a
  = DictItem
  { _dictItem_key :: Test a
  , _dictItem_colon :: Between' [WhitespaceChar] Colon
  , _dictItem_value :: Test a
  , _dictItem_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (DictItem a)
deriving instance Show a => Show (DictItem a)
deriving instance Ord a => Ord (DictItem a)

data DictUnpacking a
  = DictUnpacking
  { _dictUnpacking_value
     :: Compose
          (Before (Between' [WhitespaceChar] DoubleAsterisk))
          Expr
          a
  , _dictUnpacking_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (DictUnpacking a)
deriving instance Show a => Show (DictUnpacking a)
deriving instance Ord a => Ord (DictUnpacking a)

data DictOrSetMaker a
  = DictOrSetMakerDict
  { _dictOrSetMakerDict_head
    :: Sum
         DictItem
         DictUnpacking
         a
  , _dictOrSetMakerDict_tail
    :: Sum
         CompFor
         (Compose
           (After (Maybe (Between' [WhitespaceChar] Comma)))
           (Compose
             []
             (Compose
               (Before (Between' [WhitespaceChar] Comma))
               (Sum DictItem DictUnpacking))))
         a
  , _dictOrSetMaker_ann :: a
  }
  | DictOrSetMakerSet
  { _dictOrSetMakerSet_head :: Sum Test StarExpr a
  , _dictOrSetMakerSet_tail
    :: Sum
         CompFor
         (Compose
           (After (Maybe (Between' [WhitespaceChar] Comma)))
           (Compose
             []
             (Compose
               (Before (Between' [WhitespaceChar] Comma))
               (Sum Test StarExpr))))
         a
  , _dictOrSetMaker_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (DictOrSetMaker a)
deriving instance Show a => Show (DictOrSetMaker a)
deriving instance Ord a => Ord (DictOrSetMaker a)

data AtomNoInt a
  = AtomParen
  { _atomParen_value
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          (Sum YieldExpr TupleTestlistComp))
        a
  , _atomNoInt_ann :: a
  }

  | AtomBracket
  { _atomBracket_value
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          ListTestlistComp)
        a
  , _atomNoInt_ann :: a
  }

  | AtomCurly
  { _atomCurly_value
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          DictOrSetMaker)
        a
  , _atomNoInt_ann :: a
  }

  | AtomIdentifier
  { _atomIdentifier_value :: Identifier a
  , _atomNoInt_ann :: a
  }

  | AtomFloat
  { _atomFloat_value :: Float' a
  , _atomNoInt_ann :: a
  }

  | AtomString
  { _atomString_head :: Sum StringLiteral BytesLiteral a
  , _atomNoInt_tail
    :: Compose
        []
        (Compose
          (Before [WhitespaceChar])
          (Sum StringLiteral BytesLiteral))
        a
  , _atomNoInt_ann :: a
  }

  | AtomImag
  { _atomImag_value
    :: Compose
          (Before [WhitespaceChar])
          Imag
        a
  , _atomNoInt_ann :: a
  }

  | AtomEllipsis
  { _atomNoInt_ann :: a
  }

  | AtomNone
  { _atomNoInt_ann :: a
  }

  | AtomTrue
  { _atomNoInt_ann :: a
  }

  | AtomFalse
  { _atomNoInt_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AtomNoInt a)
deriving instance Show a => Show (AtomNoInt a)
deriving instance Ord a => Ord (AtomNoInt a)

data Atom a
  = AtomNoInt
  { _atomNoInt_value :: AtomNoInt a
  , _atom_ann :: a
  } 
  | AtomInteger
  { _atomInteger_value :: Integer' a
  , _atom_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Atom a)
deriving instance Show a => Show (Atom a)
deriving instance Ord a => Ord (Atom a)

deriveEq1 ''Comparison
deriveOrd1 ''Comparison
deriveShow1 ''Comparison
makeLenses ''Comparison

deriveEq1 ''NotTest
deriveOrd1 ''NotTest
deriveShow1 ''NotTest
makeLenses ''NotTest

deriveEq1 ''AndTest
deriveOrd1 ''AndTest
deriveShow1 ''AndTest
makeLenses ''AndTest

deriveEq1 ''OrTest
deriveOrd1 ''OrTest
deriveShow1 ''OrTest
makeLenses ''OrTest

deriveEq1 ''IfThenElse
deriveOrd1 ''IfThenElse
deriveShow1 ''IfThenElse
makeLenses ''IfThenElse

deriveEq1 ''Test
deriveOrd1 ''Test
deriveShow1 ''Test
makeLenses ''Test

deriveEq1 ''TestList
deriveOrd1 ''TestList
deriveShow1 ''TestList
makeLenses ''TestList

deriveEq1 ''Argument
deriveOrd1 ''Argument
deriveShow1 ''Argument
makeLenses ''Argument

deriveEq1 ''ArgList
deriveOrd1 ''ArgList
deriveShow1 ''ArgList
makeLenses ''ArgList

deriveEq1 ''LambdefNocond
deriveOrd1 ''LambdefNocond
deriveShow1 ''LambdefNocond
makeLenses ''LambdefNocond

deriveEq1 ''TestNocond
deriveOrd1 ''TestNocond
deriveShow1 ''TestNocond
makeLenses ''TestNocond

deriveEq1 ''CompIter
deriveOrd1 ''CompIter
deriveShow1 ''CompIter
makeLenses ''CompIter

deriveEq1 ''CompIf
deriveOrd1 ''CompIf
deriveShow1 ''CompIf
makeLenses ''CompIf

deriveEq1 ''StarExpr
deriveOrd1 ''StarExpr
deriveShow1 ''StarExpr
makeLenses ''StarExpr

deriveEq1 ''ExprList
deriveOrd1 ''ExprList
deriveShow1 ''ExprList
makeLenses ''ExprList

deriveEq1 ''SliceOp
deriveOrd1 ''SliceOp
deriveShow1 ''SliceOp
makeLenses ''SliceOp

deriveEq1 ''Subscript
deriveOrd1 ''Subscript
deriveShow1 ''Subscript
makeLenses ''Subscript

deriveEq1 ''SubscriptList
deriveOrd1 ''SubscriptList
deriveShow1 ''SubscriptList
makeLenses ''SubscriptList

deriveEq1 ''CompFor
deriveOrd1 ''CompFor
deriveShow1 ''CompFor
makeLenses ''CompFor

deriveEq1 ''Trailer
deriveOrd1 ''Trailer
deriveShow1 ''Trailer
makeLenses ''Trailer

deriveEq1 ''AtomNoInt
deriveOrd1 ''AtomNoInt
deriveShow1 ''AtomNoInt
makeLenses ''AtomNoInt

deriveEq1 ''AtomExpr
deriveOrd1 ''AtomExpr
deriveShow1 ''AtomExpr
makeLenses ''AtomExpr

deriveEq1 ''Power
deriveOrd1 ''Power
deriveShow1 ''Power
makeLenses ''Power

deriveEq1 ''Factor
deriveOrd1 ''Factor
deriveShow1 ''Factor
makeLenses ''Factor

deriveEq1 ''Term
deriveOrd1 ''Term
deriveShow1 ''Term
makeLenses ''Term

deriveEq1 ''ArithExpr
deriveOrd1 ''ArithExpr
deriveShow1 ''ArithExpr
makeLenses ''ArithExpr

deriveEq1 ''ShiftExpr
deriveOrd1 ''ShiftExpr
deriveShow1 ''ShiftExpr
makeLenses ''ShiftExpr

deriveEq1 ''AndExpr
deriveOrd1 ''AndExpr
deriveShow1 ''AndExpr
makeLenses ''AndExpr

deriveEq1 ''XorExpr
deriveOrd1 ''XorExpr
deriveShow1 ''XorExpr
makeLenses ''XorExpr
  
deriveEq1 ''Expr
deriveOrd1 ''Expr
deriveShow1 ''Expr
makeLenses ''Expr

deriveEq1 ''YieldArg
deriveOrd1 ''YieldArg
deriveShow1 ''YieldArg
makeLenses ''YieldArg

deriveEq1 ''YieldExpr
deriveOrd1 ''YieldExpr
deriveShow1 ''YieldExpr
makeLenses ''YieldExpr

deriveEq1 ''ListTestlistComp
deriveOrd1 ''ListTestlistComp
deriveShow1 ''ListTestlistComp
makeLenses ''ListTestlistComp

deriveEq1 ''TupleTestlistComp
deriveOrd1 ''TupleTestlistComp
deriveShow1 ''TupleTestlistComp
makeLenses ''TupleTestlistComp

deriveEq1 ''Atom
deriveOrd1 ''Atom
deriveShow1 ''Atom
makeLenses ''Atom

deriveEq1 ''Lambdef
deriveOrd1 ''Lambdef
deriveShow1 ''Lambdef
makeLenses ''Lambdef

deriveEq1 ''DictItem
deriveOrd1 ''DictItem
deriveShow1 ''DictItem
makeLenses ''DictItem

deriveEq1 ''DictUnpacking
deriveOrd1 ''DictUnpacking
deriveShow1 ''DictUnpacking
makeLenses ''DictUnpacking

deriveEq1 ''DictOrSetMaker
deriveOrd1 ''DictOrSetMaker
deriveShow1 ''DictOrSetMaker
makeLenses ''DictOrSetMaker
