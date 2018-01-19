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
import Data.Functor.Product
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

data LambdefNocond ws a
  = LambdefNocond
  { _lambdefNocond_args
    :: Compose
         Maybe
         (Compose
           (Between (NonEmpty ws) [ws])
           (ArgsList ws Identifier (Test ws)))
         a
  , _lambdefNocond_expr
    :: Compose
         (Before [ws])
         (TestNocond ws)
         a
  , _lambdefNocond_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data TestNocond ws a
  = TestNocond
  { _expressionNocond_value
    :: Sum
         (OrTest ws)
         (LambdefNocond ws)
         a
  , _expressionNocond_ann :: a
  } deriving (Functor, Foldable, Traversable)

data CompIter ws a
  = CompIter
  { _compIter_value :: Sum (CompFor ws) (CompIf ws) a
  , _compIter_ann :: a
  } deriving (Functor, Foldable, Traversable)

data CompIf ws a
  = CompIf
  { _compIf_if :: Between' (NonEmpty ws) KIf
  , _compIf_expr :: TestNocond ws a
  , _compIf_iter :: Compose Maybe (CompIter ws) a
  , _compIf_ann :: a
  } deriving (Functor, Foldable, Traversable)

data StarExpr ws a
  = StarExpr
  { _starExpr_value
    :: Compose
         (Before [ws])
         (Expr ws)
         a
  , _starExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)

data ExprList ws a
  = ExprList
  { _exprList_head :: Sum (Expr ws) (StarExpr ws) a
  , _exprList_tail
    :: Compose
        []
        (Compose
          (Before (Between' [ws] Comma))
          (Sum (Expr ws) (StarExpr ws)))
        a
  , _exprList_comma :: Maybe (Before [ws] Comma)
  , _exprList_ann :: a
  } deriving (Functor, Foldable, Traversable)

data CompFor ws a
  = CompFor
  { _compFor_targets
    :: Compose
        (Before (Between' (NonEmpty ws) KFor))
        (Compose
          (After (NonEmpty ws))
          (ExprList ws))
        a
  , _compFor_expr :: Compose (Before (NonEmpty ws)) (OrTest ws) a
  , _compFor_iter :: Compose Maybe (CompIter ws) a
  , _compFor_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (CompFor ws a)
deriving instance (Show a, Show ws) => Show (CompFor ws a)
deriving instance (Ord a, Ord ws) => Ord (CompFor ws a)

data SliceOp ws a
  = SliceOp
  { _sliceOp_val
    :: Compose
        Maybe
        (Compose (Before [ws]) (Test ws))
        a
  , _sliceOp_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (SliceOp ws a)
deriving instance (Show a, Show ws) => Show (SliceOp ws a)
deriving instance (Ord a, Ord ws) => Ord (SliceOp ws a)

data Subscript ws a
  = SubscriptTest
  { _subscriptTest_val :: Test ws a
  , _subscript_ann :: a
  }
  | SubscriptSlice
  { _subscriptSlice_left
    :: Compose
         (After [ws])
         (Compose
           Maybe
           (Test ws))
         a
  , _subscriptSlice_colon :: After [ws] Colon
  , _subscriptSlice_right
    :: Compose
        Maybe
        (Compose (After [ws]) (Test ws))
        a
  , _subscriptSlice_sliceOp
    :: Compose
        Maybe
        (Compose (After [ws]) (SliceOp ws))
        a
  , _subscript_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Subscript ws a)
deriving instance (Show a, Show ws) => Show (Subscript ws a)
deriving instance (Ord a, Ord ws) => Ord (Subscript ws a)

data SubscriptList ws a
  = SubscriptList
  { _subscriptList_head :: Subscript ws a
  , _subscriptList_tail
    :: Compose
        []
        (Compose
          (Before (Between' [ws] Comma))
          (Subscript ws))
        a
  , _subscriptList_comma :: Maybe (Before [ws] Comma)
  , _subscriptList_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (SubscriptList ws a)
deriving instance (Show a, Show ws) => Show (SubscriptList ws a)
deriving instance (Ord a, Ord ws) => Ord (SubscriptList ws a)

data Trailer ws a
  = TrailerCall
  { _trailerCall_value
    :: Compose
        (Before [AnyWhitespaceChar])
        (Compose
          Maybe
          (ArgumentList
            (Product
              (Test AnyWhitespaceChar)
              (Compose
                Maybe
                (Compose
                  (Before (NonEmpty AnyWhitespaceChar))
                  (CompFor AnyWhitespaceChar))))
            (Test AnyWhitespaceChar)
            Identifier Test))
        a
  , _trailer_ann :: a
  }
  | TrailerSubscript
  { _trailerSubscript_value
    :: Compose
        (Between' [AnyWhitespaceChar])
        (SubscriptList AnyWhitespaceChar)
        a
  , _trailer_ann :: a
  }
  | TrailerAccess
  { _trailerAccess_value :: Compose (Before [ws]) Identifier a
  , _trailer_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Trailer ws a)
deriving instance (Show a, Show ws) => Show (Trailer ws a)
deriving instance (Ord a, Ord ws) => Ord (Trailer ws a)

data AtomExpr ws a
  = AtomExprSingle
  { _atomExpr_await
    :: Maybe (After (NonEmpty ws) KAwait)
  , _atomExprSingle_atom :: Atom ws a
  , _atomExpr_ann :: a
  }
  | AtomExprTrailers
  { _atomExpr_await
    :: Maybe (After (NonEmpty ws) KAwait)
  , _atomExprTrailers_atom :: AtomNoInt ws a
  , _atomExprTrailers_trailers
    :: Compose
          NonEmpty
          (Compose
            (Before [ws])
            (Trailer ws))
          a
  , _atomExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (AtomExpr ws a)
deriving instance (Show a, Show ws) => Show (AtomExpr ws a)
deriving instance (Ord a, Ord ws) => Ord (AtomExpr ws a)

data Power ws a
  = Power
  { _power_left :: AtomExpr ws a
  , _power_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' [ws] DoubleAsterisk))
           (Factor ws))
         a
  , _power_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Power ws a)
deriving instance (Show a, Show ws) => Show (Power ws a)
deriving instance (Ord a, Ord ws) => Ord (Power ws a)

data Factor ws a
  = FactorNone
  { _factorNone_value :: Power ws a
  , _factor_ann :: a
  }
  | FactorOne
  { _factorOne_op :: After [ws] FactorOperator
  , _factorOne_value :: Factor ws a
  , _factorSome_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Factor ws a)
deriving instance (Show a, Show ws) => Show (Factor ws a)
deriving instance (Ord a, Ord ws) => Ord (Factor ws a)

data Term ws a
  = Term
  { _term_left :: Factor ws a
  , _term_right
    :: Compose
         []
         (Compose
           (Before (Between' [ws] TermOperator))
           (Factor ws))
         a
  , _termMany_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Term ws a)
deriving instance (Show a, Show ws) => Show (Term ws a)
deriving instance (Ord a, Ord ws) => Ord (Term ws a)

data ArithExpr ws a
  = ArithExpr
  { _arithExpr_left :: Term ws a
  , _arithExpr_right
    :: Compose
        []
        (Compose
          (Before (Between' [ws] (Either Plus Minus)))
          (Term ws))
        a
  , _arithExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (ArithExpr ws a)
deriving instance (Show a, Show ws) => Show (ArithExpr ws a)
deriving instance (Ord a, Ord ws) => Ord (ArithExpr ws a)

data ShiftExpr ws a
  = ShiftExpr
  { _shiftExpr_left :: ArithExpr ws a
  , _shiftExpr_right
    :: Compose
         []
         (Compose
           (Before (Between' [ws] (Either DoubleLT DoubleGT)))
           (ArithExpr ws))
         a
  , _shiftExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (ShiftExpr ws a)
deriving instance (Show a, Show ws) => Show (ShiftExpr ws a)
deriving instance (Ord a, Ord ws) => Ord (ShiftExpr ws a)

data AndExpr ws a
  = AndExpr
  { _andExpr_left :: ShiftExpr ws a
  , _andExpr_right
    :: Compose
         []
         (Compose
           (Before (Between' [ws] Ampersand))
           (ShiftExpr ws))
         a
  , _andExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (AndExpr ws a)
deriving instance (Show a, Show ws) => Show (AndExpr ws a)
deriving instance (Ord a, Ord ws) => Ord (AndExpr ws a)

data XorExpr ws a
  = XorExpr
  { _xorExpr_left :: AndExpr ws a
  , _xorExpr_right
    :: Compose
         []
         (Compose
           (Before (Between' [ws] Caret))
           (AndExpr ws))
         a
  , _xorExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (XorExpr ws a)
deriving instance (Show a, Show ws) => Show (XorExpr ws a)
deriving instance (Ord a, Ord ws) => Ord (XorExpr ws a)

data Expr ws a
  = Expr
  { _expr_value :: XorExpr ws a
  , _expr_right
    :: Compose
         []
         (Compose
           (Before (Between' [ws] Pipe))
           (XorExpr ws))
        a
  , _expr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Expr ws a)
deriving instance (Show a, Show ws) => Show (Expr ws a)
deriving instance (Ord a, Ord ws) => Ord (Expr ws a)

data Comparison ws a
  = Comparison
  { _comparison_left :: Expr ws a
  , _comparison_right
    :: Compose
         []
         (Compose
           (Before (CompOperator ws))
           (Expr ws))
         a
  , _comparison_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Comparison ws a)
deriving instance (Show a, Show ws) => Show (Comparison ws a)
deriving instance (Ord a, Ord ws) => Ord (Comparison ws a)

data NotTest ws a
  = NotTestMany
  { _notTestMany_value
    :: Compose
        (Before (After (NonEmpty ws) KNot))
        (NotTest ws)
        a
  , _notTestMany_ann :: a
  }
  | NotTestOne
  { _notTestNone_value :: Comparison ws a
  , _notTestNone_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (NotTest ws a)
deriving instance (Show a, Show ws) => Show (NotTest ws a)
deriving instance (Ord a, Ord ws) => Ord (NotTest ws a)

data AndTest ws a
  = AndTest
  { _andTest_left :: NotTest ws a
  , _andTest_right
    :: Compose
         []
         (Compose
           (Before (Between' (NonEmpty ws) KAnd))
           (NotTest ws))
         a
  , _andTest_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (AndTest ws a)
deriving instance (Show a, Show ws) => Show (AndTest ws a)
deriving instance (Ord a, Ord ws) => Ord (AndTest ws a)

data OrTest ws a
  = OrTest
  { _orTest_left :: AndTest ws a
  , _orTest_right
    :: Compose
         []
         (Compose
           (Before (Between' (NonEmpty ws) KOr))
           (AndTest ws))
         a
  , _orTest_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (OrTest ws a)
deriving instance (Show a, Show ws) => Show (OrTest ws a)
deriving instance (Ord a, Ord ws) => Ord (OrTest ws a)

data IfThenElse ws a
  = IfThenElse
  { _ifThenElse_if :: After (NonEmpty ws) KIf
  , _ifThenElse_value1 :: OrTest ws a
  , _ifThenElse_else :: Between' (NonEmpty ws) KElse
  , _ifThenElse_value2 :: Test ws a
  }
  deriving (Functor, Foldable, Traversable)

data Test ws a
  = TestCond
  { _testCond_head :: OrTest ws a
  , _testCond_tail
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty ws))
           (IfThenElse ws))
        a
  , _test_ann :: a
  }
  | TestLambdef
  { _testLambdef_value :: Lambdef ws a
  , _test_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Test ws a)
deriving instance (Show a, Show ws) => Show (Test ws a)
deriving instance (Ord a, Ord ws) => Ord (Test ws a)

data Lambdef ws a
  = Lambdef
  { _lambdef_args
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty ws))
           (ArgsList ws Identifier (Test ws)))
         a
  , _lambdef_body
    :: Compose
         (Before (Between' [ws] Colon))
         (Test ws)
         a
  , _lambdef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Lambdef ws a)
deriving instance (Show a, Show ws) => Show (Lambdef ws a)
deriving instance (Ord a, Ord ws) => Ord (Lambdef ws a)

data TestList ws a
  = TestList
  { _testList_head :: Test ws a
  , _testList_tail
    :: Compose
       []
       (Compose
         (Before (Between' [ws] Comma))
         (Test ws))
       a
  , _testList_comma :: Maybe (Before [ws] Comma)
  , _testList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (TestList ws a)
deriving instance (Show a, Show ws) => Show (TestList ws a)
deriving instance (Ord a, Ord ws) => Ord (TestList ws a)

data YieldArg ws a
  = YieldArgFrom
  { _yieldArgFrom_value
    :: Compose (Before (NonEmpty ws)) (Test ws) a
  , _yieldArgFrom_ann :: a
  }
  | YieldArgList
  { _yieldArgList_value :: TestList ws a
  , _yieldArgList_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (YieldArg ws a)
deriving instance (Show a, Show ws) => Show (YieldArg ws a)
deriving instance (Ord a, Ord ws) => Ord (YieldArg ws a)

data YieldExpr ws a
  = YieldExpr
  { _yieldExpr_value
    :: Compose
        Maybe
        (Compose
          (Before (NonEmpty ws))
          (YieldArg ws))
        a
  , _yieldExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (YieldExpr ws a)
deriving instance (Show a, Show ws) => Show (YieldExpr ws a)
deriving instance (Ord a, Ord ws) => Ord (YieldExpr ws a)

data TupleTestlistComp ws a
  = TupleTestlistCompFor
  { _tupleTestlistCompFor_head :: Sum (Test ws) (StarExpr ws) a
  , _tupleTestlistCompFor_tail :: CompFor ws a
  , _tupleTestlistCompFor_ann :: a
  }

  | TupleTestlistCompList
  { _tupleTestlistCompList_head :: Sum (Test ws) (StarExpr ws) a
  , _tupleTestlistCompList_tail
    :: Compose
        []
        (Compose
          (Before (Between' [ws] Comma))
          (Sum (Test ws) (StarExpr ws)))
        a
  , _tupleTestlistCompList_comma :: Maybe (Before [ws] Comma)
  , _tupleTestlistCompList_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (TupleTestlistComp ws a)
deriving instance (Show a, Show ws) => Show (TupleTestlistComp ws a)
deriving instance (Ord a, Ord ws) => Ord (TupleTestlistComp ws a)

data ListTestlistComp ws a
  = ListTestlistCompFor
  { _listTestlistCompFor_head :: Sum (Test ws) (StarExpr ws) a
  , _listTestlistCompFor_tail :: CompFor ws a
  , _listTestlistCompFor_ann :: a
  }

  | ListTestlistCompList
  { _listTestlistCompList_head :: Sum (Test ws) (StarExpr ws) a
  , _listTestlistCompList_tail
    :: Compose
        []
        (Compose
          (Before (Between' [ws] Comma))
          (Sum (Test ws) (StarExpr ws)))
        a
  , _listTestlistCompList_comma :: Maybe (Before [ws] Comma)
  , _listTestlistCompList_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (ListTestlistComp ws a)
deriving instance (Show a, Show ws) => Show (ListTestlistComp ws a)
deriving instance (Ord a, Ord ws) => Ord (ListTestlistComp ws a)

data DictItem ws a
  = DictItem
  { _dictItem_key :: Test ws a
  , _dictItem_colon :: Between' [ws] Colon
  , _dictItem_value :: Test ws a
  , _dictItem_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (DictItem ws a)
deriving instance (Show a, Show ws) => Show (DictItem ws a)
deriving instance (Ord a, Ord ws) => Ord (DictItem ws a)

data DictUnpacking ws a
  = DictUnpacking
  { _dictUnpacking_value
     :: Compose
          (Before (Between' [ws] DoubleAsterisk))
          (Expr ws)
          a
  , _dictUnpacking_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (DictUnpacking ws a)
deriving instance (Show a, Show ws) => Show (DictUnpacking ws a)
deriving instance (Ord a, Ord ws) => Ord (DictUnpacking ws a)

data DictOrSetMaker ws a
  = DictOrSetMakerDict
  { _dictOrSetMakerDict_head
    :: Sum
         (DictItem ws)
         (DictUnpacking ws)
         a
  , _dictOrSetMakerDict_tail
    :: Sum
         (CompFor ws)
         (Compose
           (After (Maybe (Between' [ws] Comma)))
           (Compose
             []
             (Compose
               (Before (Between' [ws] Comma))
               (Sum (DictItem ws) (DictUnpacking ws)))))
         a
  , _dictOrSetMaker_ann :: a
  }
  | DictOrSetMakerSet
  { _dictOrSetMakerSet_head :: Sum (Test ws) (StarExpr ws) a
  , _dictOrSetMakerSet_tail
    :: Sum
         (CompFor ws)
         (Compose
           (After (Maybe (Between' [ws] Comma)))
           (Compose
             []
             (Compose
               (Before (Between' [ws] Comma))
               (Sum (Test ws) (StarExpr ws)))))
         a
  , _dictOrSetMaker_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (DictOrSetMaker ws a)
deriving instance (Show a, Show ws) => Show (DictOrSetMaker ws a)
deriving instance (Ord a, Ord ws) => Ord (DictOrSetMaker ws a)

data AtomNoInt ws a
  = AtomParen
  { _atomParen_value
    :: Compose
        (Between' [AnyWhitespaceChar])
        (Compose
          Maybe
          (Sum (YieldExpr AnyWhitespaceChar) (TupleTestlistComp AnyWhitespaceChar)))
        a
  , _atomNoInt_ann :: a
  }

  | AtomBracket
  { _atomBracket_value
    :: Compose
        (Between' [AnyWhitespaceChar])
        (Compose
          Maybe
          (ListTestlistComp AnyWhitespaceChar))
        a
  , _atomNoInt_ann :: a
  }

  | AtomCurly
  { _atomCurly_value
    :: Compose
        (Between' [AnyWhitespaceChar])
        (Compose
          Maybe
          (DictOrSetMaker AnyWhitespaceChar))
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
          (Before [ws])
          (Sum StringLiteral BytesLiteral))
        a
  , _atomNoInt_ann :: a
  }

  | AtomImag
  { _atomImag_value
    :: Compose
          (Before [ws])
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
deriving instance (Eq a, Eq ws) => Eq (AtomNoInt ws a)
deriving instance (Show a, Show ws) => Show (AtomNoInt ws a)
deriving instance (Ord a, Ord ws) => Ord (AtomNoInt ws a)

data Atom ws a
  = AtomNoInt
  { _atomNoInt_value :: AtomNoInt ws a
  , _atom_ann :: a
  } 
  | AtomInteger
  { _atomInteger_value :: Integer' a
  , _atom_ann :: a
  } deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (Atom ws a)
deriving instance (Show a, Show ws) => Show (Atom ws a)
deriving instance (Ord a, Ord ws) => Ord (Atom ws a)

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
