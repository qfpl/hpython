{-# language DataKinds #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RecordWildCards #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
module Language.Python.Expr.AST where

import Papa hiding (Plus, Sum, Product)

import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.ArgsList
import Language.Python.AST.ArgumentList
import Language.Python.AST.Identifier
import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.AST.TestlistStarExpr
import Language.Python.Expr.AST.BytesLiteral
import Language.Python.Expr.AST.CompOperator
import Language.Python.Expr.AST.FactorOperator
import Language.Python.Expr.AST.Float
import Language.Python.Expr.AST.Imag
import Language.Python.Expr.AST.Integer
import Language.Python.Expr.AST.StringLiteral
import Language.Python.Expr.AST.TermOperator
import Language.Python.IR.ExprConfig

data LambdefNocond (ws :: *) (atomType :: AtomType) (ctxt :: DefinitionContext) (a :: *)
  = LambdefNocond
  { _lambdefNocond_args
    :: Compose
         Maybe
         (Compose
           (Between (NonEmpty ws) [ws])
           (ArgsList ws Identifier (Test ws atomType ctxt)))
         a
  , _lambdefNocond_expr
    :: Compose
         (Before [ws])
         (TestNocond ws atomType ('FunDef 'Normal))
         a
  , _lambdefNocond_ann :: a
  }
deriving instance Functor (LambdefNocond ws as ctxt)
deriving instance Foldable (LambdefNocond ws as ctxt)
deriving instance Traversable (LambdefNocond ws as ctxt)

data TestNocond (ws :: *) (atomType :: AtomType) (ctxt :: DefinitionContext) a
  = TestNocond
  { _expressionNocond_value :: Sum (OrTest ws atomType ctxt) (LambdefNocond ws atomType ctxt) a
  , _expressionNocond_ann :: a
  }
deriving instance Functor (TestNocond ws a b)
deriving instance Foldable (TestNocond ws a b)
deriving instance Traversable (TestNocond ws a b)

data CompIter :: * -> AtomType -> DefinitionContext -> * -> * where
  CompIter ::
    { _compIter_value :: Sum (CompFor ws 'NotAssignable ctxt) (CompIf ws 'NotAssignable ctxt) a
    , _compIter_ann :: a
    } -> CompIter ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (CompIter ws a b c)
deriving instance Functor (CompIter ws a b)
deriving instance Foldable (CompIter ws a b)
deriving instance Traversable (CompIter ws a b)

data CompIf :: * -> AtomType -> DefinitionContext -> * -> * where
  CompIf ::
    { _compIf_if :: Between' (NonEmpty ws) KIf
    , _compIf_expr
      :: TestNocond ws 'NotAssignable ctxt a
    , _compIf_iter
      :: Compose
          Maybe
          (Compose (Before [ws]) (CompIter ws 'NotAssignable ctxt))
          a
    , _compIf_ann :: a
    } -> CompIf ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (CompIf ws a b c)
deriving instance Functor (CompIf ws a b)
deriving instance Foldable (CompIf ws a b)
deriving instance Traversable (CompIf ws a b)

data StarExpr ws (atomType :: AtomType) (ctxt :: DefinitionContext) a
  = StarExpr
  { _starExpr_value
    :: Compose
         (Before [ws])
         (Expr ws 'Assignable ctxt)
         a
  , _starExpr_ann :: a
  }
deriving instance Functor (StarExpr ws a b)
deriving instance Foldable (StarExpr ws a b)
deriving instance Traversable (StarExpr ws a b)

data ExprList :: * -> AtomType -> DefinitionContext -> * -> * where
  ExprListSingleStarredComma ::
    { _exprListSingleStarredComma_value :: StarExpr ws atomType ctxt a
    , _exprListSingleStarredComma_comma :: Before [ws] Comma
    , _exprListSingleStarredComma_ann :: a
    } -> ExprList ws atomType ctxt a

  ExprListSingleStarredNoComma ::
    { _exprListSingleStarredNoComma_value :: StarExpr ws 'NotAssignable ctxt a
    , _exprListSingleStarredNoComma_ann :: a
    } -> ExprList ws 'NotAssignable ctxt a

  ExprListSingle ::
    { _exprListSingle_value :: Expr ws atomType ctxt a
    , _exprListSingle_comma :: Maybe (Before [ws] Comma)
    , _exprListSingle_ann :: a
    } -> ExprList ws atomType ctxt a

  ExprListMany ::
    { _exprListMany_head :: Sum (Expr ws atomType ctxt) (StarExpr ws atomType ctxt) a
    , _exprListMany_tail
      :: Compose
           NonEmpty
           (Compose
             (Before (Between' [ws] Comma))
             (Sum (Expr ws atomType ctxt) (StarExpr ws atomType ctxt)))
           a
    , _exprListMany_comma :: Maybe (Before [ws] Comma)
    , _exprListMany_ann :: a
    } -> ExprList ws atomType ctxt a
deriving instance Functor (ExprList ws a b)
deriving instance Foldable (ExprList ws a b)
deriving instance Traversable (ExprList ws a b)

data CompFor :: * -> AtomType -> DefinitionContext -> * -> * where
  CompFor ::
    { _compFor_targets
      :: Compose
          (Before (Between' (NonEmpty ws) KFor))
          (Compose
            (After (NonEmpty ws))
            (TestlistStarExpr ws Expr StarExpr 'Assignable ctxt))
          a
    , _compFor_expr :: Compose (Before (NonEmpty ws)) (OrTest ws 'NotAssignable ctxt) a
    , _compFor_iter
      :: Compose
          Maybe
          (Compose
            (Before [ws])
            (CompIter ws 'NotAssignable ctxt))
          a
    , _compFor_ann :: a
    } -> CompFor ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (CompFor ws a b c)
deriving instance Functor (CompFor ws a b)
deriving instance Foldable (CompFor ws a b)
deriving instance Traversable (CompFor ws a b)

data SliceOp :: * -> AtomType -> DefinitionContext -> * -> * where
  SliceOp ::
    { _sliceOp_val
      :: Compose
          Maybe
          (Compose (Before [ws]) (Test ws 'NotAssignable ctxt))
          a
    , _sliceOp_ann :: a
    } -> SliceOp ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (SliceOp ws a b c)
deriving instance Functor (SliceOp ws a b)
deriving instance Foldable (SliceOp ws a b)
deriving instance Traversable (SliceOp ws a b)

data Subscript :: * -> AtomType -> DefinitionContext -> * -> * where
  SubscriptTest ::
    { _subscriptTest_val :: Test ws 'NotAssignable ctxt a
    , _subscript_ann :: a
    } -> Subscript ws 'NotAssignable ctxt a
  SubscriptSlice ::
    { _subscriptSlice_left
      :: Compose
           (After [ws])
           (Compose
             Maybe
             (Test ws 'NotAssignable ctxt))
           a
    , _subscriptSlice_colon :: After [ws] Colon
    , _subscriptSlice_right
      :: Compose
          Maybe
          (Compose (After [ws]) (Test ws 'NotAssignable ctxt))
          a
    , _subscriptSlice_sliceOp
      :: Compose
          Maybe
          (Compose (After [ws]) (SliceOp ws 'NotAssignable ctxt))
          a 
    , _subscript_ann :: a
    } -> Subscript ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (Subscript ws a b c)
deriving instance Functor (Subscript ws a b)
deriving instance Foldable (Subscript ws a b)
deriving instance Traversable (Subscript ws a b)

data SubscriptList :: * -> AtomType -> DefinitionContext -> * -> * where
  SubscriptList ::
    { _subscriptList_head :: Subscript ws 'NotAssignable ctxt a
    , _subscriptList_tail
      :: Compose
          []
          (Compose
            (Before (Between' [ws] Comma))
            (Subscript ws 'NotAssignable ctxt))
          a
    , _subscriptList_comma :: Maybe (Before [ws] Comma)
    , _subscriptList_ann :: a
    } -> SubscriptList ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (SubscriptList ws a b c)
deriving instance Functor (SubscriptList ws a b)
deriving instance Foldable (SubscriptList ws a b)
deriving instance Traversable (SubscriptList ws a b)

data Trailer :: * -> AtomType -> DefinitionContext -> * -> * where
  TrailerCall ::
    { _trailerCall_value
      :: Compose
          (Between' [AnyWhitespaceChar])
          (Compose
            Maybe
            (ArgumentList Identifier Test 'NotAssignable ctxt))
          a
    , _trailerCall_ann :: a
    } -> Trailer ws 'NotAssignable ctxt a
  TrailerSubscript ::
    { _trailerSubscript_value
      :: Compose
          (Between' [AnyWhitespaceChar])
          (SubscriptList AnyWhitespaceChar 'NotAssignable ctxt)
          a
    , _trailerSubscript_ann :: a
    } -> Trailer ws atomType ctxt a
  TrailerAccess ::
    { _trailerAccess_value :: Compose (Before [ws]) Identifier a
    , _trailerAccess_ann :: a
    } -> Trailer ws atomType ctxt a
deriving instance (Eq c, Eq ws) => Eq (Trailer ws a b c)
deriving instance Functor (Trailer ws a b)
deriving instance Foldable (Trailer ws a b)
deriving instance Traversable (Trailer ws a b)

data AtomExprTrailers :: * -> AtomType -> DefinitionContext -> * -> * where
  AtomExprTrailersBase ::
    { _atomExprTrailersBase_value :: AtomNoInt ws 'NotAssignable ctxt a
    , _atomExprTrailersBase_trailers
      :: Compose
           (Before [ws])
           (Trailer ws atomType ctxt)
           a
    , _atomExprTrailersBase_ann :: a
    } -> AtomExprTrailers ws atomType ctxt a
  AtomExprTrailersMany ::
    { _atomExprTrailersMany_value :: AtomExprTrailers ws 'NotAssignable ctxt a
    , _atomExprTrailersMany_trailers
      :: Compose
           (Before [ws])
           (Trailer ws atomType ctxt)
           a
    , _atomExprTrailersMany_ann :: a
    } -> AtomExprTrailers ws atomType ctxt a
deriving instance (Eq c, Eq ws) => Eq (AtomExprTrailers ws a b c)
deriving instance Functor (AtomExprTrailers ws a b)
deriving instance Foldable (AtomExprTrailers ws a b)
deriving instance Traversable (AtomExprTrailers ws a b)

data AtomExpr :: * -> AtomType -> DefinitionContext -> * -> * where
  AtomExprSingle ::
    { _atomExprSingle_value :: Atom ws atomType ctxt a
    , _atomExprSingle_ann :: a
    } -> AtomExpr ws atomType ctxt a
  AtomExprTrailers ::
    { _atomExprTrailers_value :: AtomExprTrailers ws atomType ctxt a
    , _atomExprTrailers_ann :: a
    } -> AtomExpr ws atomType ctxt a
  AtomExprAwaitSingle ::
    { _atomExprAwaitSingle_await :: After (NonEmpty ws) KAwait
    , _atomExprAwaitSingle_atom :: Atom ws 'NotAssignable ('FunDef 'Async) a
    , _atomExprAwaitSingle_ann :: a
    } -> AtomExpr ws 'NotAssignable ('FunDef 'Async) a
  AtomExprAwaitTrailers ::
    { _atomExprAwaitTrailers_await :: After (NonEmpty ws) KAwait
    , _atomExprAwaitTrailers_trailers
      :: AtomExprTrailers ws 'NotAssignable ('FunDef 'Async) a
    , _atomExprAwaitTrailers_ann :: a
    } -> AtomExpr ws 'NotAssignable ('FunDef 'Async) a
deriving instance (Eq a, Eq ws) => Eq (AtomExpr ws c b a)
deriving instance Functor (AtomExpr ws b a)
deriving instance Foldable (AtomExpr ws b a)
deriving instance Traversable (AtomExpr ws b a)

data Power :: * -> AtomType -> DefinitionContext -> * -> * where
  PowerOne ::
    { _powerOne_value :: AtomExpr ws atomType ctxt a
    , _powerOne_ann :: a
    } -> Power ws atomType ctxt a

  PowerMany ::
    { _powerMany_left :: AtomExpr ws 'NotAssignable ctxt a
    , _powerMany_right
      :: Compose
           (Before (Between' [ws] DoubleAsterisk))
           (Factor ws 'NotAssignable ctxt)
           a
    , _powerMany_ann :: a
    } -> Power ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (Power ws a b c)
deriving instance Functor (Power ws a b)
deriving instance Foldable (Power ws a b)
deriving instance Traversable (Power ws a b)

data Factor :: * -> AtomType -> DefinitionContext -> * -> * where
  FactorNone ::
    { _factorNone_value :: Power ws atomType ctxt a
    , _factorNone_ann :: a
    } -> Factor ws atomType ctxt a

  FactorOne ::
    { _factorOne_op :: After [ws] FactorOperator
    , _factorOne_value :: Factor ws 'NotAssignable ctxt a
    , _factorOne_ann :: a
    } -> Factor ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (Factor ws a b c)
deriving instance Functor (Factor ws a b)
deriving instance Foldable (Factor ws a b)
deriving instance Traversable (Factor ws a b)

data Term :: * -> AtomType -> DefinitionContext -> * -> * where
  TermOne ::
    { _termOne_value :: Factor ws atomType ctxt a
    , _termOne_ann :: a
    } -> Term ws atomType ctxt a

  TermMany ::
    { _termMany_left :: Factor ws 'NotAssignable ctxt a
    , _termMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [ws] TermOperator))
            (Factor ws 'NotAssignable ctxt))
          a
    , _termMany_ann :: a
    } -> Term ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (Term ws a b c)
deriving instance Functor (Term ws a b)
deriving instance Foldable (Term ws a b)
deriving instance Traversable (Term ws a b)

data ArithExpr :: * -> AtomType -> DefinitionContext -> * -> * where
  ArithExprOne ::
    { _arithExprOne_value :: Term ws atomType ctxt a
    , _arithExprOne_ann :: a
    } -> ArithExpr ws atomType ctxt a

  ArithExprMany ::
    { _arithExprSome_left :: Term ws 'NotAssignable ctxt a
    , _arithExprSome_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [ws] (Either Plus Minus)))
            (Term ws 'NotAssignable ctxt))
          a
    , _arithExprSome_ann :: a
    } -> ArithExpr ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (ArithExpr ws a b c)
deriving instance Functor (ArithExpr ws a b)
deriving instance Foldable (ArithExpr ws a b)
deriving instance Traversable (ArithExpr ws a b)

data ShiftExpr :: * -> AtomType -> DefinitionContext -> * -> * where
  ShiftExprOne ::
    { _shiftExprOne_value :: ArithExpr ws atomType ctxt a
    , _shiftExprOne_ann :: a
    } -> ShiftExpr ws atomType ctxt a

  ShiftExprMany ::
    { _shiftExprMany_left :: ArithExpr ws 'NotAssignable ctxt a
    , _shiftExprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [ws] (Either DoubleLT DoubleGT)))
            (ArithExpr ws 'NotAssignable ctxt))
          a
    , _shiftExprMany_ann :: a
    } -> ShiftExpr ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (ShiftExpr ws a b c)
deriving instance Functor (ShiftExpr ws a b)
deriving instance Foldable (ShiftExpr ws a b)
deriving instance Traversable (ShiftExpr ws a b)

data AndExpr :: * -> AtomType -> DefinitionContext -> * -> * where
  AndExprOne ::
    { _andExprOne_value :: ShiftExpr ws atomType ctxt a
    , _andExprOne_ann :: a
    } -> AndExpr ws atomType ctxt a

  AndExprMany ::
    { _andExprMany_left :: ShiftExpr ws 'NotAssignable ctxt a
    , _andExprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [ws] Ampersand))
            (ShiftExpr ws 'NotAssignable ctxt))
          a
    , _andExprMany_ann :: a
    } -> AndExpr ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (AndExpr ws a b c)
deriving instance Functor (AndExpr ws a b)
deriving instance Foldable (AndExpr ws a b)
deriving instance Traversable (AndExpr ws a b)

data XorExpr :: * -> AtomType -> DefinitionContext -> * -> * where
  XorExprOne ::
    { _xorExprOne_value :: AndExpr ws atomType ctxt a
    , _xorExprOne_ann :: a
    } -> XorExpr ws atomType ctxt a
  XorExprMany ::
    { _xorExprMany_left :: AndExpr ws 'NotAssignable ctxt a
    , _xorExprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [ws] Caret))
            (AndExpr ws 'NotAssignable ctxt))
          a
    , _xorExprMany_ann :: a
    } -> XorExpr ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (XorExpr ws a b c)
deriving instance Functor (XorExpr ws a b)
deriving instance Foldable (XorExpr ws a b)
deriving instance Traversable (XorExpr ws a b)

data Expr :: * -> AtomType -> DefinitionContext -> * -> * where
  ExprOne ::
    { _exprOne_value :: XorExpr ws atomType ctxt a
    , _exprOne_ann :: a
    } -> Expr ws atomType ctxt a
  ExprMany ::
    { _exprMany_left :: XorExpr ws 'NotAssignable ctxt a
    , _exprMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [ws] Pipe))
            (XorExpr ws 'NotAssignable ctxt))
          a
    , _exprMany_ann :: a
    } -> Expr ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (Expr ws a b c)
deriving instance Functor (Expr ws a b)
deriving instance Foldable (Expr ws a b)
deriving instance Traversable (Expr ws a b)

data Comparison :: * -> AtomType -> DefinitionContext -> * -> * where
  ComparisonOne ::
    { _comparisonOne_value :: Expr ws atomType ctxt a
    , _comparisonOne_ann :: a
    } -> Comparison ws atomType ctxt a
  ComparisonMany ::
    { _comparisonMany_left :: Expr ws 'NotAssignable ctxt a
    , _comparisonMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (CompOperator ws))
            (Expr ws 'NotAssignable ctxt))
          a
    , _comparisonMany_ann :: a
    } -> Comparison ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (Comparison ws a b c)
deriving instance Functor (Comparison ws a b)
deriving instance Foldable (Comparison ws a b)
deriving instance Traversable (Comparison ws a b)

data NotTest :: * -> AtomType -> DefinitionContext -> * -> * where
  NotTestMany ::
    { _notTestMany_value
      :: Compose
          (Before (After (NonEmpty ws) KNot))
          (NotTest ws 'NotAssignable ctxt)
          a
    , _notTestMany_ann :: a
    } -> NotTest ws 'NotAssignable ctxt a
  NotTestOne ::
    { _notTestNone_value :: Comparison ws atomType ctxt a
    , _notTestNone_ann :: a
    } -> NotTest ws atomType ctxt a
deriving instance (Eq c, Eq ws) => Eq (NotTest ws a b c)
deriving instance Functor (NotTest ws a b)
deriving instance Foldable (NotTest ws a b)
deriving instance Traversable (NotTest ws a b)

data AndTest :: * -> AtomType -> DefinitionContext -> * -> * where
  AndTestOne ::
    { _andTestOne_value :: NotTest ws atomType ctxt a
    , _andTestOne_ann :: a
    } -> AndTest ws atomType ctxt a

  AndTestMany ::
    { _andTestMany_left :: NotTest ws 'NotAssignable ctxt a
    , _andTestMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' (NonEmpty ws) KAnd))
            (NotTest ws 'NotAssignable ctxt))
          a
    , _andTestMany_ann :: a
    } -> AndTest ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (AndTest ws a b c)
deriving instance Functor (AndTest ws a b)
deriving instance Foldable (AndTest ws a b)
deriving instance Traversable (AndTest ws a b)

data OrTest :: * -> AtomType -> DefinitionContext -> * -> * where
  OrTestOne ::
    { _orTestOne_value :: AndTest ws atomType ctxt a
    , _orTestOne_ann :: a
    } -> OrTest ws atomType ctxt a

  OrTestMany ::
    { _orTestMany_left :: AndTest ws 'NotAssignable ctxt a
    , _orTestMany_right
      :: Compose
           NonEmpty
           (Compose
             (Before (Between' (NonEmpty ws) KOr))
             (AndTest ws 'NotAssignable ctxt))
           a
    , _orTestMany_ann :: a
    } -> OrTest ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (OrTest ws a b c)
deriving instance Functor (OrTest ws a b)
deriving instance Foldable (OrTest ws a b)
deriving instance Traversable (OrTest ws a b)

data IfThenElse :: * -> AtomType -> DefinitionContext -> * -> * where
  IfThenElse ::
    { _ifThenElse_if :: After (NonEmpty ws) KIf
    , _ifThenElse_value1 :: OrTest ws 'NotAssignable ctxt a
    , _ifThenElse_else :: Between' (NonEmpty ws) KElse
    , _ifThenElse_value2 :: Test ws 'NotAssignable ctxt a
    } -> IfThenElse ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (IfThenElse ws a b c)
deriving instance Functor (IfThenElse ws a b)
deriving instance Foldable (IfThenElse ws a b)
deriving instance Traversable (IfThenElse ws a b)

data Test :: * -> AtomType -> DefinitionContext -> * -> * where
  TestCondNoIf ::
    { _testCondNoIf_value :: OrTest ws atomType ctxt a
    , _testCondNoIf_ann :: a
    } -> Test ws atomType ctxt a
  TestCondIf ::
    { _testCondIf_head :: OrTest ws 'NotAssignable ctxt a
    , _testCondIf_tail
      :: Compose
          (Before (NonEmpty ws))
          (IfThenElse ws 'NotAssignable ctxt)
          a
    , _testCondIf_ann :: a
    } -> Test ws 'NotAssignable ctxt a

  TestLambdef ::
    { _testLambdef_value :: Lambdef ws 'NotAssignable ctxt a
    , _testLambdef_ann :: a
    } -> Test ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (Test ws a b c)
deriving instance Functor (Test ws a b)
deriving instance Foldable (Test ws a b)
deriving instance Traversable (Test ws a b)

data Lambdef :: * -> AtomType -> DefinitionContext -> * -> * where
  Lambdef ::
    { _lambdef_Args
      :: Compose
          Maybe
          (Compose
            (Before (NonEmpty ws))
            (ArgsList ws Identifier (Test ws 'NotAssignable ctxt)))
          a
    , _lambdef_body
      :: Compose
           (Before (Between' [ws] Colon))
           (Test ws 'NotAssignable ('FunDef 'Normal))
           a
    , _lambdef_ann :: a
    } -> Lambdef ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (Lambdef ws a b c)
deriving instance Functor (Lambdef ws a b)
deriving instance Foldable (Lambdef ws a b)
deriving instance Traversable (Lambdef ws a b)

data TestList ws (atomType :: AtomType) (ctxt :: DefinitionContext) a
  = TestList
  { _testList_head :: Test ws atomType ctxt a
  , _testList_tail
    :: Compose
         []
         (Compose
           (Before (Between' [ws] Comma))
           (Test ws atomType ctxt))
         a
  , _testList_comma :: Maybe (Before [ws] Comma)
  , _testList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

data YieldArg :: * -> AtomType -> DefinitionContext -> * -> * where
  YieldArgFrom ::
    { _yieldArgFrom_value
      :: Compose
           (Before (NonEmpty ws))
           (Test ws 'NotAssignable ctxt)
           a
    , _yieldArgFrom_ann :: a
    } -> YieldArg ws 'NotAssignable ctxt a
  YieldArgList ::
    { _yieldArgList_value :: TestList ws atomType ctxt a
    , _yieldArgList_ann :: a
    } -> YieldArg ws atomType ctxt a
deriving instance (Eq c, Eq ws) => Eq (YieldArg ws a b c)
deriving instance Functor (YieldArg ws a b)
deriving instance Foldable (YieldArg ws a b)
deriving instance Traversable (YieldArg ws a b)

data YieldExpr (ws :: *) (ctxt :: DefinitionContext) a where
  YieldExpr ::
    { _yieldExpr_value
      :: Compose
          Maybe
          (Compose
            (Before (NonEmpty ws))
            (YieldArg ws 'NotAssignable ('FunDef 'Normal)))
          a
    , _yieldExpr_ann :: a
    } -> YieldExpr ws ('FunDef 'Normal) a
deriving instance (Eq a, Eq ws) => Eq (YieldExpr ws ctxt a)
deriving instance (Show a, Show ws) => Show (YieldExpr ws ctxt a)
deriving instance Functor (YieldExpr ws ctxt)
deriving instance Foldable (YieldExpr ws ctxt)
deriving instance Traversable (YieldExpr ws ctxt)

data ListTestlistComp :: * -> AtomType -> DefinitionContext -> * -> * where
  ListTestlistCompStarred ::
    { _ListTestlistCompStarred_head :: StarExpr ws atomType ctxt a
    , _ListTestlistCompStarred_tail
      :: Compose
          []
          (Compose
            (Before (Between' [ws] Comma))
            (Sum (Test ws atomType ctxt) (StarExpr ws atomType ctxt)))
          a
    , _ListTestlistCompStarred_comma :: Maybe (Before [ws] Comma)
    , _ListTestlistCompStarred_ann :: a
    } -> ListTestlistComp ws atomType ctxt a

  ListTestlistCompList ::
    { _ListTestlistCompList_head :: Test ws atomType ctxt a
    , _ListTestlistCompList_tail
      :: Compose
          []
          (Compose
            (Before (Between' [ws] Comma))
            (Sum (Test ws atomType ctxt) (StarExpr ws atomType ctxt)))
          a
    , _ListTestlistCompList_comma :: Maybe (Before [ws] Comma)
    , _ListTestlistCompList_ann :: a
    } -> ListTestlistComp ws atomType ctxt a

  ListTestlistCompFor ::
    { _ListTestlistCompFor_head
      :: Test ws 'NotAssignable ctxt a
    , _ListTestlistCompFor_tail
      :: CompFor ws 'NotAssignable ctxt a
    , _ListTestlistCompFor_ann :: a
    } -> ListTestlistComp ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (ListTestlistComp ws a b c)
deriving instance Functor (ListTestlistComp ws a b)
deriving instance Foldable (ListTestlistComp ws a b)
deriving instance Traversable (ListTestlistComp ws a b)

data TupleTestlistComp :: * -> AtomType -> DefinitionContext -> * -> * where
  TupleTestlistCompStarredOne ::
    { _tupleTestlistCompStarredOne_head :: StarExpr ws atomType ctxt a
    , _tupleTestlistCompStarredOne_comma :: Before [ws] Comma
    , _tupleTestlistCompStarredOne_ann :: a
    } -> TupleTestlistComp ws atomType ctxt a

  TupleTestlistCompStarredMany ::
    { _tupleTestlistCompStarredMany_head :: StarExpr ws atomType ctxt a
    , _tupleTestlistCompStarredMany_tail
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [ws] Comma))
            (Sum (Test ws atomType ctxt) (StarExpr ws atomType ctxt)))
          a
    , _tupleTestlistCompStarredMany_comma :: Maybe (Before [ws] Comma)
    , _tupleTestlistCompStarredMany_ann :: a
    } -> TupleTestlistComp ws atomType ctxt a

  TupleTestlistCompList ::
    { _tupleTestlistCompList_head :: Test ws atomType ctxt a
    , _tupleTestlistCompList_tail
      :: Compose
          []
          (Compose
            (Before (Between' [ws] Comma))
            (Sum (Test ws atomType ctxt) (StarExpr ws atomType ctxt)))
          a
    , _tupleTestlistCompList_comma :: Maybe (Before [ws] Comma)
    , _tupleTestlistCompList_ann :: a
    } -> TupleTestlistComp ws atomType ctxt a

  TupleTestlistCompFor ::
    { _tupleTestlistCompFor_head
      :: Test ws 'NotAssignable ctxt a
    , _tupleTestlistCompFor_tail
      :: CompFor ws 'NotAssignable ctxt a
    , _tupleTestlistCompFor_ann :: a
    } -> TupleTestlistComp ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (TupleTestlistComp ws a b c)
deriving instance Functor (TupleTestlistComp ws a b)
deriving instance Foldable (TupleTestlistComp ws a b)
deriving instance Traversable (TupleTestlistComp ws a b)

data DictItem (ws :: *) (atomType :: AtomType) (ctxt :: DefinitionContext) a where
  DictItem ::
    { _dictItem_key :: Test ws 'NotAssignable ctxt a
    , _dictItem_colon :: Between' [ws] Colon
    , _dictItem_value :: Test ws 'NotAssignable ctxt a
    , _dictItem_ann :: a
    } -> DictItem ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (DictItem ws a b c)
deriving instance (Show c, Show ws) => Show (DictItem ws a b c)
deriving instance Functor (DictItem ws a b)
deriving instance Foldable (DictItem ws a b)
deriving instance Traversable (DictItem ws a b)

data DictUnpacking (ws :: *) (atomType :: AtomType) (ctxt :: DefinitionContext) a where
  DictUnpacking ::
    { _dictUnpacking_value
      :: Compose
            (Before (Between' [ws] DoubleAsterisk))
            (Expr ws 'NotAssignable ctxt)
            a
    , _dictUnpacking_ann :: a
    } -> DictUnpacking ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (DictUnpacking ws a b c)
deriving instance (Show c, Show ws) => Show (DictUnpacking ws a b c)
deriving instance Functor (DictUnpacking ws a b)
deriving instance Foldable (DictUnpacking ws a b)
deriving instance Traversable (DictUnpacking ws a b)

data DictOrSetMaker (ws :: *) (atomType :: AtomType) (ctxt :: DefinitionContext) a where
  DictOrSetMakerDictComp ::
    { _dictOrSetMakerDictComp_head :: DictItem ws 'NotAssignable ctxt a
    , _dictOrSetMakerDictComp_tail :: CompFor ws 'NotAssignable ctxt a
    , _dictOrSetMakerDictComp_ann :: a
    } -> DictOrSetMaker ws 'NotAssignable ctxt a
  DictOrSetMakerDictUnpack ::
    { _dictOrSetMakerDictUnpack_head
      :: Sum
           (DictItem ws 'NotAssignable ctxt)
           (DictUnpacking ws 'NotAssignable ctxt)
           a
    , _dictOrSetMakerDictUnpack_tail
      :: Compose
           []
           (Compose
             (Before (Between' [ws] Comma))
             (Sum
               (DictItem ws 'NotAssignable ctxt)
               (DictUnpacking ws 'NotAssignable ctxt)))
           a
    , _dictOrSetMakerDictUnpack_comma
      :: Maybe (Between' [ws] Comma)
    , _dictOrSetMakerDictUnpack_ann :: a
    } -> DictOrSetMaker ws 'NotAssignable ctxt a
  DictOrSetMakerSetComp ::
    { _dictOrSetMakerSetComp_head :: Test ws 'NotAssignable ctxt a
    , _dictOrSetMakerSetComp_tail :: CompFor ws 'NotAssignable ctxt a
    , _dictOrSetMakerSetComp_ann :: a
    } -> DictOrSetMaker ws 'NotAssignable ctxt a
  DictOrSetMakerSetUnpack ::
    { _dictOrSetMakerSetUnpack_head
      :: Sum
           (Test ws 'NotAssignable ctxt)
           (StarExpr ws 'NotAssignable ctxt)
           a
    , _dictOrSetMakerSetUnpack_tail
      :: Compose
           []
           (Compose
             (Before (Between' [ws] Comma))
             (Sum
               (Test ws 'NotAssignable ctxt)
               (StarExpr ws 'NotAssignable ctxt)))
           a
    , _dictOrSetMakerSetUnpack_comma
      :: Maybe (Between' [ws] Comma)
    , _dictOrSetMakerSetUnpack_ann :: a
    } -> DictOrSetMaker ws 'NotAssignable ctxt a
deriving instance (Eq c, Eq ws) => Eq (DictOrSetMaker ws a b c)
deriving instance (Show c, Show ws) => Show (DictOrSetMaker ws a b c)
deriving instance Functor (DictOrSetMaker ws a b)
deriving instance Foldable (DictOrSetMaker ws a b)
deriving instance Traversable (DictOrSetMaker ws a b)

data AtomNoInt :: * -> AtomType -> DefinitionContext -> * -> * where
  AtomParenNoYield ::
    { _atomParenNoYield_val
      :: Compose
          (Between' [AnyWhitespaceChar])
          (Compose
            Maybe
            (TupleTestlistComp AnyWhitespaceChar atomType ctxt))
          a
    , _atomParenNoYield_ann :: a
    } -> AtomNoInt ws atomType ctxt a

  -- A yield expression can only be used within a normal function definition
  AtomParenYield ::
    { _atomParenYield_val
      :: Compose
          (Between' [AnyWhitespaceChar])
          (YieldExpr AnyWhitespaceChar ctxt)
          a
    , _atomParenYield_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a

  AtomBracket ::
    { _atomBracket_val
      :: Compose
          (Between' [AnyWhitespaceChar])
          (Compose
            Maybe
            (ListTestlistComp AnyWhitespaceChar atomType ctxt))
          a
    , _atomBracket_ann :: a
    } -> AtomNoInt ws atomType ctxt a

  AtomCurly ::
    { _atomCurly_val
      :: Compose
          (Between' [AnyWhitespaceChar])
          (Compose
            Maybe
            (DictOrSetMaker AnyWhitespaceChar 'NotAssignable ctxt))
          a
    , _atomCurly_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a

  AtomIdentifier ::
    { _atomIdentifier_value :: Identifier a
    , _atomIdentifier_ann :: a
    } -> AtomNoInt ws atomType ctxt a

  AtomFloat ::
    { _atomFloat :: Float' a
    , _atomFloat_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a

  AtomString ::
    { _atomString_head :: Sum StringLiteral BytesLiteral a
    , _atomString_tail
      :: Compose
          []
          (Compose
            (Before [ws])
            (Sum StringLiteral BytesLiteral))
          a
    , _atomString_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a

  AtomImag ::
    { _atomImag_value
      :: Compose
           (Before [ws])
           Imag
          a
    , _atomString_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a

  AtomEllipsis ::
    { _atomEllipsis_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a

  AtomNone ::
    { _atomNone_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a

  AtomTrue ::
    { _atomTrue_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a

  AtomFalse ::
    { _atomFalse_ann :: a
    } -> AtomNoInt ws 'NotAssignable ctxt a
deriving instance (Eq a, Eq ws) => Eq (AtomNoInt ws atomType ctxt a)
deriving instance Functor (AtomNoInt ws atomType ctxt)
deriving instance Foldable (AtomNoInt ws atomType ctxt)
deriving instance Traversable (AtomNoInt ws atomType ctxt)

data Atom :: * -> AtomType -> DefinitionContext -> * -> * where
  AtomNoInt ::
    { _atomNoInt_value :: AtomNoInt ws atomType ctxt a
    , _atomNoInt_ann :: a
    } -> Atom ws atomType ctxt a

  AtomInteger ::
    { _atomInteger :: Integer' a
    , _atomInteger_ann :: a
    } -> Atom ws 'NotAssignable ctxt a
deriving instance (Eq a, Eq ws) => Eq (Atom ws atomType ctxt a)
deriving instance Functor (Atom ws atomType ctxt)
deriving instance Foldable (Atom ws atomType ctxt)
deriving instance Traversable (Atom ws atomType ctxt)

data PythonModule a
  = PythonModule
  { _pythonModule_content :: a
  , _pythonModule_ann :: a
  } deriving (Functor, Foldable, Traversable)

deriveShow ''Comparison
deriveEq1 ''Comparison
deriveOrd1 ''Comparison
deriveShow1 ''Comparison

deriveShow ''NotTest
deriveEq1 ''NotTest
deriveOrd1 ''NotTest
deriveShow1 ''NotTest

deriveShow ''AndTest
deriveEq1 ''AndTest
deriveOrd1 ''AndTest
deriveShow1 ''AndTest

deriveShow ''OrTest
deriveEq1 ''OrTest
deriveOrd1 ''OrTest
deriveShow1 ''OrTest

deriveShow ''IfThenElse
deriveEq1 ''IfThenElse
deriveOrd1 ''IfThenElse
deriveShow1 ''IfThenElse

deriveShow ''Test
deriveEq1 ''Test
deriveOrd1 ''Test
deriveShow1 ''Test

deriveEq ''TestList
deriveShow ''TestList
deriveEq1 ''TestList
deriveOrd1 ''TestList
deriveShow1 ''TestList

deriveEq ''LambdefNocond
deriveShow ''LambdefNocond
deriveEq1 ''LambdefNocond
deriveOrd1 ''LambdefNocond
deriveShow1 ''LambdefNocond

deriveEq ''TestNocond
deriveShow ''TestNocond
deriveEq1 ''TestNocond
deriveOrd1 ''TestNocond
deriveShow1 ''TestNocond

deriveShow ''CompIter
deriveEq1 ''CompIter
deriveOrd1 ''CompIter
deriveShow1 ''CompIter

deriveShow ''CompIf
deriveEq1 ''CompIf
deriveOrd1 ''CompIf
deriveShow1 ''CompIf

deriveEq ''StarExpr
deriveShow ''StarExpr
deriveEq1 ''StarExpr
deriveOrd1 ''StarExpr
deriveShow1 ''StarExpr

deriveEq ''ExprList
deriveShow ''ExprList
deriveEq1 ''ExprList
deriveOrd1 ''ExprList
deriveShow1 ''ExprList

deriveShow ''SliceOp
deriveEq1 ''SliceOp
deriveOrd1 ''SliceOp
deriveShow1 ''SliceOp

deriveShow ''Subscript
deriveEq1 ''Subscript
deriveOrd1 ''Subscript
deriveShow1 ''Subscript

deriveShow ''SubscriptList
deriveEq1 ''SubscriptList
deriveOrd1 ''SubscriptList
deriveShow1 ''SubscriptList

deriveShow ''CompFor
deriveEq1 ''CompFor
deriveOrd1 ''CompFor
deriveShow1 ''CompFor

deriveShow ''Trailer
deriveEq1 ''Trailer
deriveOrd1 ''Trailer
deriveShow1 ''Trailer

deriveShow ''AtomExprTrailers
deriveEq1 ''AtomExprTrailers
deriveOrd1 ''AtomExprTrailers
deriveShow1 ''AtomExprTrailers

deriveShow ''AtomExpr
deriveEq1 ''AtomExpr
deriveOrd1 ''AtomExpr
deriveShow1 ''AtomExpr

deriveShow ''Power
deriveEq1 ''Power
deriveOrd1 ''Power
deriveShow1 ''Power

deriveShow ''Factor
deriveEq1 ''Factor
deriveOrd1 ''Factor
deriveShow1 ''Factor

deriveShow ''Term
deriveEq1 ''Term
deriveOrd1 ''Term
deriveShow1 ''Term

deriveShow ''ArithExpr
deriveEq1 ''ArithExpr
deriveOrd1 ''ArithExpr
deriveShow1 ''ArithExpr

deriveShow ''ShiftExpr
deriveEq1 ''ShiftExpr
deriveOrd1 ''ShiftExpr
deriveShow1 ''ShiftExpr

deriveShow ''AndExpr
deriveEq1 ''AndExpr
deriveOrd1 ''AndExpr
deriveShow1 ''AndExpr

deriveShow ''XorExpr
deriveEq1 ''XorExpr
deriveOrd1 ''XorExpr
deriveShow1 ''XorExpr
  
deriveShow ''Expr
deriveEq1 ''Expr
deriveOrd1 ''Expr
deriveShow1 ''Expr

deriveShow ''YieldArg
deriveEq1 ''YieldArg
deriveOrd1 ''YieldArg
deriveShow1 ''YieldArg

deriveEq1 ''YieldExpr
deriveOrd1 ''YieldExpr
deriveShow1 ''YieldExpr

deriveShow ''TupleTestlistComp
deriveEq1 ''TupleTestlistComp
deriveOrd1 ''TupleTestlistComp
deriveShow1 ''TupleTestlistComp

deriveShow ''ListTestlistComp
deriveEq1 ''ListTestlistComp
deriveOrd1 ''ListTestlistComp
deriveShow1 ''ListTestlistComp

deriveEq1 ''DictOrSetMaker
deriveOrd1 ''DictOrSetMaker
deriveShow1 ''DictOrSetMaker

deriveShow ''AtomNoInt
deriveEq1 ''AtomNoInt
deriveOrd1 ''AtomNoInt
deriveShow1 ''AtomNoInt

deriveShow ''Atom
deriveEq1 ''Atom
deriveOrd1 ''Atom
deriveShow1 ''Atom

deriveEq ''PythonModule
deriveShow ''PythonModule
deriveEq1 ''PythonModule
deriveOrd1 ''PythonModule
deriveShow1 ''PythonModule

deriveShow ''Lambdef
deriveEq1 ''Lambdef
deriveOrd1 ''Lambdef
deriveShow1 ''Lambdef

deriveEq1 ''DictUnpacking
deriveOrd1 ''DictUnpacking
deriveShow1 ''DictUnpacking

deriveEq1 ''DictItem
deriveOrd1 ''DictItem
deriveShow1 ''DictItem
