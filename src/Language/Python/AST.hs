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

import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between
import Data.Text (Text)

import Language.Python.AST.BytesLiteral
import Language.Python.AST.CompOperator
import Language.Python.AST.FactorOperator
import Language.Python.AST.Float
import Language.Python.AST.Identifier
import Language.Python.AST.Imag
import Language.Python.AST.Integer
import Language.Python.AST.Keywords
import Language.Python.AST.TermOperator
import Language.Python.AST.StringLiteral
import Language.Python.AST.Symbols

data Argument :: AtomType -> ExprContext -> * -> * where
  ArgumentFor ::
    { _argumentFor_expr :: Test 'NotAssignable ctxt a
    , _argumentFor_for
      :: Compose
          Maybe
          (Compose
            (Before [WhitespaceChar])
            (CompFor 'NotAssignable ctxt))
          a
    , _argumentFor_ann :: a
    } -> Argument 'NotAssignable ctxt a
  ArgumentDefault ::
    { _argumentDefault_left
      :: Compose
           (After [WhitespaceChar])
           (Test 'Assignable ctxt)
           a
    , _argumentDefault_right
      :: Compose
           (Before [WhitespaceChar])
           (Test 'NotAssignable ctxt)
           a
    , _argumentDefault_ann :: a
    } -> Argument 'NotAssignable ctxt a
  ArgumentUnpack ::
    { _argumentUnpack_symbol :: Either Asterisk DoubleAsterisk
    , _argumentUnpack_val
      :: Compose
           (Before [WhitespaceChar])
           (Test 'NotAssignable ctxt)
           a
    , _argumentUnpack_ann :: a
    } -> Argument 'NotAssignable ctxt a
deriving instance Eq c => Eq (Argument a b c)
deriving instance Functor (Argument a b)
deriving instance Foldable (Argument a b)
deriving instance Traversable (Argument a b)

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
deriving instance Functor (TestNocond a b)
deriving instance Foldable (TestNocond a b)
deriving instance Traversable (TestNocond a b)

data CompIter :: AtomType -> ExprContext -> * -> * where
  CompIter ::
    { _compIter_value :: Sum (CompFor 'NotAssignable ctxt) (CompIf 'NotAssignable ctxt) a
    , _compIter_ann :: a
    } -> CompIter 'NotAssignable ctxt a
deriving instance Eq c => Eq (CompIter a b c)
deriving instance Functor (CompIter a b)
deriving instance Foldable (CompIter a b)
deriving instance Traversable (CompIter a b)

data CompIf :: AtomType -> ExprContext -> * -> * where
  CompIf ::
    { _compIf_if :: Between' (NonEmpty WhitespaceChar) KIf
    , _compIf_expr
      :: TestNocond 'NotAssignable ctxt a
    , _compIf_iter
      :: Compose
          Maybe
          (Compose (Before [WhitespaceChar]) (CompIter 'NotAssignable ctxt))
          a
    , _compIf_ann :: a
    } -> CompIf 'NotAssignable ctxt a
deriving instance Eq c => Eq (CompIf a b c)
deriving instance Functor (CompIf a b)
deriving instance Foldable (CompIf a b)
deriving instance Traversable (CompIf a b)

data StarExpr (atomType :: AtomType) (ctxt :: ExprContext) a
  = StarExpr
  { _starExpr_value
    :: Compose
         (Before [WhitespaceChar])
         (Expr 'Assignable ctxt)
         a
  , _starExpr_ann :: a
  }
deriving instance Functor (StarExpr a b)
deriving instance Foldable (StarExpr a b)
deriving instance Traversable (StarExpr a b)

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
deriving instance Functor (ExprList a b)
deriving instance Foldable (ExprList a b)
deriving instance Traversable (ExprList a b)

data CompFor :: AtomType -> ExprContext -> * -> * where
  CompFor ::
    { _compFor_targets
      :: Compose
          (Before (Between' (NonEmpty WhitespaceChar) KFor))
          (Compose
            (After (NonEmpty WhitespaceChar))
            (ExprList 'Assignable ctxt))
          a
    , _compFor_expr :: Compose (Before (NonEmpty WhitespaceChar)) (OrTest 'NotAssignable ctxt) a
    , _compFor_iter
      :: Compose
          Maybe
          (Compose
            (Before [WhitespaceChar])
            (CompIter 'NotAssignable ctxt))
          a
    , _compFor_ann :: a
    } -> CompFor 'NotAssignable ctxt a
deriving instance Eq c => Eq (CompFor a b c)
deriving instance Functor (CompFor a b)
deriving instance Foldable (CompFor a b)
deriving instance Traversable (CompFor a b)

data SliceOp :: AtomType -> ExprContext -> * -> * where
  SliceOp ::
    { _sliceOp_val
      :: Compose
          Maybe
          (Compose (Before [WhitespaceChar]) (Test 'NotAssignable ctxt))
          a
    , _sliceOp_ann :: a
    } -> SliceOp 'NotAssignable ctxt a
deriving instance Eq c => Eq (SliceOp a b c)
deriving instance Functor (SliceOp a b)
deriving instance Foldable (SliceOp a b)
deriving instance Traversable (SliceOp a b)

data Subscript :: AtomType -> ExprContext -> * -> * where
  SubscriptTest ::
    { _subscriptTest_val :: Test 'NotAssignable ctxt a
    , _subscript_ann :: a
    } -> Subscript 'NotAssignable ctxt a
  SubscriptSlice ::
    { _subscriptSlice_left
      :: Compose
           (After [WhitespaceChar])
           (Compose
             Maybe
             (Test 'NotAssignable ctxt))
           a
    , _subscriptSlice_colon :: After [WhitespaceChar] Colon
    , _subscriptSlice_right
      :: Compose
          Maybe
          (Compose (After [WhitespaceChar]) (Test 'NotAssignable ctxt))
          a
    , _subscriptSlice_sliceOp
      :: Compose
          Maybe
          (Compose (After [WhitespaceChar]) (SliceOp 'NotAssignable ctxt))
          a 
    , _subscript_ann :: a
    } -> Subscript 'NotAssignable ctxt a
deriving instance Eq c => Eq (Subscript a b c)
deriving instance Functor (Subscript a b)
deriving instance Foldable (Subscript a b)
deriving instance Traversable (Subscript a b)

data SubscriptList :: AtomType -> ExprContext -> * -> * where
  SubscriptList ::
    { _subscriptList_head :: Subscript 'NotAssignable ctxt a
    , _subscriptList_tail
      :: Compose
          []
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (Subscript 'NotAssignable ctxt))
          a
    , _subscriptList_comma :: Maybe (Before [WhitespaceChar] Comma)
    , _subscriptList_ann :: a
    } -> SubscriptList 'NotAssignable ctxt a
deriving instance Eq c => Eq (SubscriptList a b c)
deriving instance Functor (SubscriptList a b)
deriving instance Foldable (SubscriptList a b)
deriving instance Traversable (SubscriptList a b)

data Trailer :: AtomType -> ExprContext -> * -> * where
  TrailerCall ::
    { _trailerCall_value
      :: Compose
          (Between' [WhitespaceChar])
          (Compose
            Maybe
            (ArgList 'NotAssignable ctxt))
          a
    , _trailer_ann :: a
    } -> Trailer 'NotAssignable ctxt a
  TrailerSubscript ::
    { _trailerSubscript_value
      :: Compose
          (Between' [WhitespaceChar])
          (SubscriptList 'NotAssignable ctxt)
          a
    , _trailer_ann :: a
    } -> Trailer 'NotAssignable ctxt a
  TrailerAccess ::
    { _trailerAccess_value :: Compose (Before [WhitespaceChar]) Identifier a
    , _trailer_ann :: a
    } -> Trailer 'NotAssignable ctxt a
deriving instance Eq c => Eq (Trailer a b c)
deriving instance Functor (Trailer a b)
deriving instance Foldable (Trailer a b)
deriving instance Traversable (Trailer a b)

data ExprContext = TopLevel | FunDef FunType
data SExprContext :: ExprContext -> * where
  STopLevel :: SExprContext 'TopLevel
  SFunDef :: SFunType f -> SExprContext ('FunDef f)

data FunType = Normal | Async
data SFunType :: FunType -> * where
  SNormal :: SFunType 'Normal
  SAsync :: SFunType 'Async

data AtomType = Assignable | NotAssignable
data SAtomType :: AtomType -> * where
  SAssignable :: SAtomType 'Assignable
  SNotAssignable :: SAtomType 'NotAssignable

data AtomExpr :: AtomType -> ExprContext -> * -> * where
  AtomExprNoAwait ::
    { _atomExpr_atom :: Atom atomType ctxt a
    , _atomExpr_trailers
      :: Compose
           []
           (Compose
             (Before [WhitespaceChar])
             (Trailer 'NotAssignable ctxt))
           a
    , _atomExpr_ann :: a
    } -> AtomExpr atomType ctxt a
  AtomExprAwait ::
    { _atomExprAwait_await :: After (NonEmpty WhitespaceChar) KAwait
    , _atomExprAwait_atom :: Atom 'NotAssignable ('FunDef 'Async) a
    , _atomExprAwait_trailers
      :: Compose
           []
           (Compose
             (Before [WhitespaceChar])
             (Trailer 'NotAssignable ('FunDef 'Async)))
           a
    , _atomExprAwait_ann :: a
    } -> AtomExpr 'NotAssignable ('FunDef 'Async) a
deriving instance Eq a => Eq (AtomExpr c b a)
deriving instance Functor (AtomExpr b a)
deriving instance Foldable (AtomExpr b a)
deriving instance Traversable (AtomExpr b a)

data Power :: AtomType -> ExprContext -> * -> * where
  PowerOne ::
    { _powerOne_value :: AtomExpr atomType ctxt a
    , _powerOne_ann :: a
    } -> Power atomType ctxt a

  PowerMany ::
    { _powerMany_left :: AtomExpr 'NotAssignable ctxt a
    , _powerMany_right
      :: Compose
           (Before (Between' [WhitespaceChar] DoubleAsterisk))
           (Factor 'NotAssignable ctxt)
           a
    , _powerMany_ann :: a
    } -> Power 'NotAssignable ctxt a
deriving instance Eq c => Eq (Power a b c)
deriving instance Functor (Power a b)
deriving instance Foldable (Power a b)
deriving instance Traversable (Power a b)

data Factor :: AtomType -> ExprContext -> * -> * where
  FactorNone ::
    { _factorNone_value :: Power atomType ctxt a
    , _factorNone_ann :: a
    } -> Factor atomType ctxt a

  FactorOne ::
    { _factorOne_op :: After [WhitespaceChar] FactorOperator
    , _factorOne_value :: Factor 'NotAssignable ctxt a
    , _factorOne_ann :: a
    } -> Factor 'NotAssignable ctxt a
deriving instance Eq c => Eq (Factor a b c)
deriving instance Functor (Factor a b)
deriving instance Foldable (Factor a b)
deriving instance Traversable (Factor a b)

data Term :: AtomType -> ExprContext -> * -> * where
  TermOne ::
    { _termOne_value :: Factor atomType ctxt a
    , _termOne_ann :: a
    } -> Term atomType ctxt a

  TermMany ::
    { _termMany_left :: Factor 'NotAssignable ctxt a
    , _termMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] TermOperator))
            (Factor 'NotAssignable ctxt))
          a
    , _termMany_ann :: a
    } -> Term 'NotAssignable ctxt a
deriving instance Eq c => Eq (Term a b c)
deriving instance Functor (Term a b)
deriving instance Foldable (Term a b)
deriving instance Traversable (Term a b)

data ArithExpr :: AtomType -> ExprContext -> * -> * where
  ArithExprOne ::
    { _arithExprOne_value :: Term atomType ctxt a
    , _arithExprOne_ann :: a
    } -> ArithExpr atomType ctxt a

  ArithExprMany ::
    { _arithExprSome_left :: Term 'NotAssignable ctxt a
    , _arithExprSome_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] (Either Plus Minus)))
            (Term 'NotAssignable ctxt))
          a
    , _arithExprSome_ann :: a
    } -> ArithExpr 'NotAssignable ctxt a
deriving instance Eq c => Eq (ArithExpr a b c)
deriving instance Functor (ArithExpr a b)
deriving instance Foldable (ArithExpr a b)
deriving instance Traversable (ArithExpr a b)

data ShiftExpr :: AtomType -> ExprContext -> * -> * where
  ShiftExprOne ::
    { _shiftExprOne_value :: ArithExpr atomType ctxt a
    , _shiftExprOne_ann :: a
    } -> ShiftExpr atomType ctxt a

  ShiftExprMany ::
    { _shiftExprMany_left :: ArithExpr 'NotAssignable ctxt a
    , _shiftExprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] (Either DoubleLT DoubleGT)))
            (ArithExpr 'NotAssignable ctxt))
          a
    , _shiftExprMany_ann :: a
    } -> ShiftExpr 'NotAssignable ctxt a
deriving instance Eq c => Eq (ShiftExpr a b c)
deriving instance Functor (ShiftExpr a b)
deriving instance Foldable (ShiftExpr a b)
deriving instance Traversable (ShiftExpr a b)

data AndExpr :: AtomType -> ExprContext -> * -> * where
  AndExprOne ::
    { _andExprOne_value :: ShiftExpr atomType ctxt a
    , _andExprOne_ann :: a
    } -> AndExpr atomType ctxt a

  AndExprMany ::
    { _andExprMany_left :: ShiftExpr 'NotAssignable ctxt a
    , _andExprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] Ampersand))
            (ShiftExpr 'NotAssignable ctxt))
          a
    , _andExprMany_ann :: a
    } -> AndExpr 'NotAssignable ctxt a
deriving instance Eq c => Eq (AndExpr a b c)
deriving instance Functor (AndExpr a b)
deriving instance Foldable (AndExpr a b)
deriving instance Traversable (AndExpr a b)

data XorExpr :: AtomType -> ExprContext -> * -> * where
  XorExprOne ::
    { _xorExprOne_value :: AndExpr atomType ctxt a
    , _xorExprOne_ann :: a
    } -> XorExpr atomType ctxt a
  XorExprMany ::
    { _xorExprMany_left :: AndExpr 'NotAssignable ctxt a
    , _xorExprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] Caret))
            (AndExpr 'NotAssignable ctxt))
          a
    , _xorExprMany_ann :: a
    } -> XorExpr 'NotAssignable ctxt a
deriving instance Eq c => Eq (XorExpr a b c)
deriving instance Functor (XorExpr a b)
deriving instance Foldable (XorExpr a b)
deriving instance Traversable (XorExpr a b)

data Expr :: AtomType -> ExprContext -> * -> * where
  ExprOne ::
    { _exprOne_value :: XorExpr atomType ctxt a
    , _exprOne_ann :: a
    } -> Expr atomType ctxt a
  ExprMany ::
    { _exprMany_left :: XorExpr 'NotAssignable ctxt a
    , _exprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] Pipe))
            (XorExpr 'NotAssignable ctxt))
          a
    , _exprMany_ann :: a
    } -> Expr 'NotAssignable ctxt a
deriving instance Eq c => Eq (Expr a b c)
deriving instance Functor (Expr a b)
deriving instance Foldable (Expr a b)
deriving instance Traversable (Expr a b)

data Comparison :: AtomType -> ExprContext -> * -> * where
  ComparisonOne ::
    { _comparisonOne_value :: Expr atomType ctxt a
    , _comparisonOne_ann :: a
    } -> Comparison atomType ctxt a
  ComparisonMany ::
    { _comparisonMany_left :: Expr 'NotAssignable ctxt a
    , _comparisonMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before CompOperator)
            (Expr 'NotAssignable ctxt))
          a
    , _comparisonMany_ann :: a
    } -> Comparison 'NotAssignable ctxt a
deriving instance Eq c => Eq (Comparison a b c)
deriving instance Functor (Comparison a b)
deriving instance Foldable (Comparison a b)
deriving instance Traversable (Comparison a b)

data NotTest :: AtomType -> ExprContext -> * -> * where
  NotTestMany ::
    { _notTestMany_value
      :: Compose
          (Before (After (NonEmpty WhitespaceChar) KNot))
          (NotTest 'NotAssignable ctxt)
          a
    , _notTestMany_ann :: a
    } -> NotTest 'NotAssignable ctxt a
  NotTestOne ::
    { _notTestNone_value :: Comparison atomType ctxt a
    , _notTestNone_ann :: a
    } -> NotTest atomType ctxt a
deriving instance Eq c => Eq (NotTest a b c)
deriving instance Functor (NotTest a b)
deriving instance Foldable (NotTest a b)
deriving instance Traversable (NotTest a b)

data AndTest :: AtomType -> ExprContext -> * -> * where
  AndTestOne ::
    { _andTestOne_value :: NotTest atomType ctxt a
    , _andTestOne_ann :: a
    } -> AndTest atomType ctxt a

  AndTestMany ::
    { _andTestMany_left :: NotTest 'NotAssignable ctxt a
    , _andTestMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' (NonEmpty WhitespaceChar) KAnd))
            (AndTest 'NotAssignable ctxt))
          a
    , _andTestMany_ann :: a
    } -> AndTest 'NotAssignable ctxt a
deriving instance Eq c => Eq (AndTest a b c)
deriving instance Functor (AndTest a b)
deriving instance Foldable (AndTest a b)
deriving instance Traversable (AndTest a b)

data OrTest :: AtomType -> ExprContext -> * -> * where
  OrTestOne ::
    { _orTestOne_value :: AndTest atomType ctxt a
    , _orTestOne_ann :: a
    } -> OrTest atomType ctxt a

  OrTestMany ::
    { _orTestMany_left :: AndTest 'NotAssignable ctxt a
    , _orTestMany_right
      :: Compose
           NonEmpty
           (Compose
             (Before (Between' (NonEmpty WhitespaceChar) KOr))
             (AndTest 'NotAssignable ctxt))
           a
    , _orTestMany_ann :: a
    } -> OrTest 'NotAssignable ctxt a
deriving instance Eq c => Eq (OrTest a b c)
deriving instance Functor (OrTest a b)
deriving instance Foldable (OrTest a b)
deriving instance Traversable (OrTest a b)

data IfThenElse :: AtomType -> ExprContext -> * -> * where
  IfThenElse ::
    { _ifThenElse_if :: Between' (NonEmpty WhitespaceChar) KIf
    , _ifThenElse_value1 :: OrTest 'NotAssignable ctxt a
    , _ifThenElse_else :: Between' (NonEmpty WhitespaceChar) KElse
    , _ifThenElse_value2 :: Test 'NotAssignable ctxt a
    } -> IfThenElse 'NotAssignable ctxt a
deriving instance Eq c => Eq (IfThenElse a b c)
deriving instance Functor (IfThenElse a b)
deriving instance Foldable (IfThenElse a b)
deriving instance Traversable (IfThenElse a b)

data Test :: AtomType -> ExprContext -> * -> * where
  TestCondNoIf ::
    { _testCondNoIf_value :: OrTest atomType ctxt a
    , _testCondNoIf_ann :: a
    } -> Test atomType ctxt a
  TestCondIf ::
    { _testCondIf_head :: OrTest 'NotAssignable ctxt a
    , _testCondIf_tail
      :: Compose
          (Before (NonEmpty WhitespaceChar))
          (IfThenElse 'NotAssignable ctxt)
          a
    , _testCondIf_ann :: a
    } -> Test 'NotAssignable ctxt a

  TestLambdef :: Test atomType ctxt a
deriving instance Eq c => Eq (Test a b c)
deriving instance Functor (Test a b)
deriving instance Foldable (Test a b)
deriving instance Traversable (Test a b)

data TestList (atomType :: AtomType) (ctxt :: ExprContext) a
  = TestList
  { _testList_head :: Test atomType ctxt a
  , _testList_tail
    :: Compose
         (Before (Between' [WhitespaceChar] Comma))
         (Test atomType ctxt)
         a
  , _testList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _testList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data YieldArg :: AtomType -> ExprContext -> * -> * where
  YieldArgFrom ::
    { _yieldArgFrom_value
      :: Compose
           (Before (NonEmpty WhitespaceChar))
           (Test 'NotAssignable ctxt)
           a
    , _yieldArgFrom_ann :: a
    } -> YieldArg 'NotAssignable ctxt a
  YieldArgList ::
    { _yieldArgList_value :: TestList atomType ctxt a
    , _yieldArgList_ann :: a
    } -> YieldArg atomType ctxt a
deriving instance Eq c => Eq (YieldArg a b c)
deriving instance Functor (YieldArg a b)
deriving instance Foldable (YieldArg a b)
deriving instance Traversable (YieldArg a b)

data YieldExpr a
  = YieldExpr
  { _yieldExpr_value
    :: Compose
        Maybe
        (Compose
          (Before (NonEmpty WhitespaceChar))
          (YieldArg 'NotAssignable ('FunDef 'Normal)))
        a
  , _yieldExpr_ann :: a
  } deriving (Functor, Foldable, Traversable)

data TestlistComp :: AtomType -> ExprContext -> * -> * where
  TestlistCompFor ::
    { _testlistCompFor_head
      :: Sum
           (Test 'NotAssignable ctxt)
           (StarExpr 'NotAssignable ctxt)
           a
    , _testlistCompFor_tail
      :: Compose
           (Before [WhitespaceChar])
           (CompFor 'NotAssignable ctxt)
           a
    , _testlistCompFor_ann :: a
    } -> TestlistComp 'NotAssignable ctxt a

  TestlistCompList ::
    { _testlistCompList_head :: Sum (Test atomType ctxt) (StarExpr atomType ctxt) a
    , _testlistCompList_tail
      :: Compose
          []
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (Sum (Test atomType ctxt) (StarExpr atomType ctxt)))
          a
    , _testlistCompList_comma :: Maybe (Before [WhitespaceChar] Comma)
    , _testlistCompList_ann :: a
    } -> TestlistComp atomType ctxt a
deriving instance Eq c => Eq (TestlistComp a b c)
deriving instance Functor (TestlistComp a b)
deriving instance Foldable (TestlistComp a b)
deriving instance Traversable (TestlistComp a b)

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
    , _atomBracket_ann :: a
    } -> Atom atomType ctxt a

  AtomCurly ::
    { _atomCurly_val
      :: Compose
          (Between' [WhitespaceChar])
          (Compose
            Maybe
            (DictOrSetMaker atomType ctxt))
          a
    , _atomCurly_ann :: a
    } -> Atom atomType ctxt a

  AtomIdentifier ::
    { _atomIdentifier_value :: Identifier a
    , _atomIdentifier_ann :: a
    } -> Atom atomType ctxt a

  AtomInteger ::
    { _atomInteger :: Integer' a
    , _atomInteger_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomFloat ::
    { _atomFloat :: Float' a
    , _atomFloat_ann :: a
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
    , _atomString_ann :: a
    } -> Atom 'NotAssignable ctxt a
    
  AtomImag ::
    { _atomImag_value
      :: Compose
           (Before [WhitespaceChar])
           Imag
          a
    , _atomString_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomEllipsis ::
    { _atomEllipsis_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomNone ::
    { _atomNone_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomTrue ::
    { _atomTrue_ann :: a
    } -> Atom 'NotAssignable ctxt a

  AtomFalse ::
    { _atomFalse_ann :: a
    } -> Atom 'NotAssignable ctxt a
deriving instance Eq a => Eq (Atom atomType ctxt a)
deriving instance Functor (Atom atomType ctxt)
deriving instance Foldable (Atom atomType ctxt)
deriving instance Traversable (Atom atomType ctxt)

data PythonModule a
  = PythonModule
  { _pythonModule_content :: a
  , _pythonModule_ann :: a
  } deriving (Functor, Foldable, Traversable)

deriveShow ''Comparison
deriveEq1 ''Comparison
deriveShow1 ''Comparison
makeLenses ''Comparison

deriveShow ''NotTest
deriveEq1 ''NotTest
deriveShow1 ''NotTest
makeLenses ''NotTest

deriveShow ''AndTest
deriveEq1 ''AndTest
deriveShow1 ''AndTest
makeLenses ''AndTest

deriveShow ''OrTest
deriveEq1 ''OrTest
deriveShow1 ''OrTest
makeLenses ''OrTest

deriveShow ''IfThenElse
deriveEq1 ''IfThenElse
deriveShow1 ''IfThenElse
makeLenses ''IfThenElse

deriveShow ''Test
deriveEq1 ''Test
deriveShow1 ''Test
makeLenses ''Test

deriveEq ''TestList
deriveShow ''TestList
deriveEq1 ''TestList
deriveShow1 ''TestList
makeLenses ''TestList

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
deriveShow ''CompIter
deriveEq1 ''CompIter
deriveShow1 ''CompIter

makeLenses ''CompIf
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
deriveShow ''SliceOp
deriveEq1 ''SliceOp
deriveShow1 ''SliceOp

makeLenses ''Subscript
deriveShow ''Subscript
deriveEq1 ''Subscript
deriveShow1 ''Subscript

makeLenses ''SubscriptList
deriveShow ''SubscriptList
deriveEq1 ''SubscriptList
deriveShow1 ''SubscriptList

makeLenses ''CompFor
deriveShow ''CompFor
deriveEq1 ''CompFor
deriveShow1 ''CompFor

makeLenses ''Trailer
deriveShow ''Trailer
deriveEq1 ''Trailer
deriveShow1 ''Trailer

makeLenses ''AtomExpr
deriveShow ''AtomExpr
deriveEq1 ''AtomExpr
deriveShow1 ''AtomExpr

makeLenses ''Power
deriveShow ''Power
deriveEq1 ''Power
deriveShow1 ''Power

makeLenses ''Factor
deriveShow ''Factor
deriveEq1 ''Factor
deriveShow1 ''Factor

makeLenses ''Term
deriveShow ''Term
deriveEq1 ''Term
deriveShow1 ''Term

makeLenses ''ArithExpr
deriveShow ''ArithExpr
deriveEq1 ''ArithExpr
deriveShow1 ''ArithExpr

makeLenses ''ShiftExpr
deriveShow ''ShiftExpr
deriveEq1 ''ShiftExpr
deriveShow1 ''ShiftExpr

makeLenses ''AndExpr
deriveShow ''AndExpr
deriveEq1 ''AndExpr
deriveShow1 ''AndExpr

makeLenses ''XorExpr
deriveShow ''XorExpr
deriveEq1 ''XorExpr
deriveShow1 ''XorExpr
  
makeLenses ''Expr
deriveShow ''Expr
deriveEq1 ''Expr
deriveShow1 ''Expr

makeLenses ''YieldArg
deriveShow ''YieldArg
deriveEq1 ''YieldArg
deriveShow1 ''YieldArg

makeLenses ''YieldExpr
deriveEq ''YieldExpr
deriveShow ''YieldExpr
deriveEq1 ''YieldExpr
deriveShow1 ''YieldExpr

makeLenses ''TestlistComp
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

makeLenses ''PythonModule
deriveEq ''PythonModule
deriveShow ''PythonModule
deriveEq1 ''PythonModule
deriveShow1 ''PythonModule
