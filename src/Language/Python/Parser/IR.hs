module Language.Python.AST.IR where

import Language.Python.AST.BytesLiteral
import Language.Python.AST.CompOperator
import Language.Python.AST.FactorOperator
import Language.Python.AST.Float
import Language.Python.AST.Identifier
import Language.Python.AST.Imag
import Language.Python.AST.Integer
import Language.Python.AST.Keywords
import Language.Python.AST.StringLiteral
import Language.Python.AST.Symbols

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
  , _argumentFor_ann :: a
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
  , _argumentDefault_ann :: a
  }
  | ArgumentUnpack
  { _argumentUnpack_symbol :: Either Asterisk DoubleAsterisk
  , _argumentUnpack_val
    :: Compose
          (Before [WhitespaceChar])
          Test
          a
  , _argumentUnpack_ann :: a
  }
  deriving (Eq, Functor, Foldable, Traversable)

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
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TestNocond a
  = TestNocond
  { _expressionNocond_value
    :: Sum
         OrTest
         LambdefNocond
         a
  , _expressionNocond_ann :: a
  } deriving (Eq, Show, Foldable, Traversable)

data CompIter a
  = CompIter
  { _compIter_value :: Sum CompFor CompIf a
  , _compIter_ann :: a
  } deriving (Eq, Functor, Foldable, Traversable)

data CompIf a
  = CompIf
  { _compIf_expr
    :: Compose
         (Before [WhitespaceChar])
         TestNocond
         a
  , _compIf_iter
    :: Compose
        Maybe
        (Compose
          (Before [WhitespaceChar])
          CompIter)
        a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data StarExpr a
  = StarExpr
  { _starExpr_value :: Compose (Before [WhitespaceChar]) Expr a
  , _starExpr_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data CompFor a
  = CompFor
  { _compFor_targets
    :: Compose
        (Before (Between' (NonEmpty WhitespaceChar) KFor))
        (Compose
          (After (NonEmpty WhitespaceChar))
          ExprList)
        a
  , _compFor_expr :: Compose (Before (NonEmpty WhitespaceChar)) OrTest a
  , _compFor_iter
    :: Compose
        Maybe
        (Compose (Before [WhitespaceChar]) CompIter)
        a
  , _compFor_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data SliceOp a
  = SliceOp
  { _sliceOp_val
    :: Compose
        Maybe
        (Compose (Before [WhitespaceChar]) Test)
        a
  , _sliceOp_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Trailer a
  = TrailerCall
  { _trailerCall_value
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          ArgList)
        a
  , _trailer_ann :: a
  }
  | TrailerSubscript
  { _trailerSubscript_value
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          SubscriptList)
        a
  , _trailer_ann :: a
  }
  | TrailerAccess
  { _trailerAccess_value :: Compose (Before [WhitespaceChar]) Identifier a
  , _trailer_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data AtomExpr a
  = AtomExprNoAwait
  { _atomExpr_atom :: Atom a
  , _atomExpr_trailers
    :: Compose
          []
          (Compose
            (Before [WhitespaceChar])
            Trailer)
          a
  , _atomExpr_ann :: a
  }
  | AtomExprAwait
  { _atomExprAwait_await
    :: Compose Maybe (After (NonEmpty WhitespaceChar)) KAwait
  , _atomExprAwait_atom :: Atom a
  , _atomExprAwait_trailers
    :: Compose
          []
          (Compose
            (Before [WhitespaceChar])
            Trailer)
          a
  , _atomExprAwait_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Power a
  = PowerOne
  { _powerOne_value :: AtomExpr a
  , _powerOne_ann :: a
  }
  | PowerMany
  { _powerMany_left :: AtomExpr a
  , _powerMany_right
    :: Compose
          (Before (After [WhitespaceChar] DoubleAsterisk))
          Factor
          a
  , _powerMany_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Factor :: AtomType -> ExprContext -> * -> * where
  FactorNone ::
    { _factorNone_value :: Power atomType ctxt a
    , _factorNone_ann :: a
    } -> Factor atomType ctxt a

  FactorMany ::
    { _factorSome_value
      :: Compose
          (Before (After [WhitespaceChar] FactorOp))
          (Factor 'NotAssignable ctxt)
          a
    , _factorSome_ann :: a
    } -> Factor 'NotAssignable ctxt a
deriving instance Eq c => Eq (Factor a b c)
deriving instance Functor (Factor a b)
deriving instance Foldable (Factor a b)
deriving instance Traversable (Factor a b)

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
    , _termOne_ann :: a
    } -> Term atomType ctxt a

  TermMany ::
    { _termMany_left :: Factor 'NotAssignable ctxt a
    , _termMany_right
      :: Compose
          NonEmpty
          (Compose
            (Before (Between' [WhitespaceChar] TermOp))
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

data ShiftExpr a
  = ShiftExprOne
  { _shiftExprOne_value :: ArithExpr a
  , _shiftExprOne_ann :: a
  }
  | ShiftExprMany
  { _shiftExprMany_left :: ArithExpr a
  , _shiftExprMany_right
    :: Compose
        NonEmpty
        (Compose
          (Before (Between' [WhitespaceChar] (Either DoubleLT DoubleGT)))
          ArithExpr)
        a
  , _shiftExprMany_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data AndExpr a
  = AndExprOne
  { _andExprOne_value :: ShiftExpr a
  , _andExprOne_ann :: a
  }
  | AndExprMany
  { _andExprMany_left :: ShiftExpr a
  , _andExprMany_right
    :: Compose
        NonEmpty
        (Compose
          (Before (Between' [WhitespaceChar] Ampersand))
          ShiftExpr)
        a
  , _andExprMany_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data XorExpr a
  = XorExprOne
  { _xorExprOne_value :: AndExpr a
  , _xorExprOne_ann :: a
  }
  | XorExprMany
  { _xorExprMany_left :: AndExpr a
  , _xorExprMany_right
    :: Compose
        NonEmpty
        (Compose
          (Before (Between' [WhitespaceChar] Caret))
          AndExpr)
        a
  , _xorExprMany_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Expr a
  = ExprOne
  { _exprOne_value :: XorExpr a
  , _exprOne_ann :: a
  }
  | ExprMany
  { _exprMany_left :: XorExpr a
  , _exprMany_right
    :: Compose
        NonEmpty
        (Compose
          (Before (Between' [WhitespaceChar] Pipe))
          XorExpr)
        a
  , _exprMany_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Comparison a
  = ComparisonOne
  { _comparisonOne_value :: Expr a
  , _comparisonOne_ann :: a
  }
  | ComparisonMany
  { _comparisonMany_left :: Expr a
  , _comparisonMany_right
    :: Compose
        NonEmpty
        (Compose
          (Before
            (Between' [WhitespaceChar] CompOperator))
          Expr)
        a
  , _comparisonMany_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data NotTest a
  = NotTestMany
  { _notTestMany_value
    :: Compose
        (Before (After (NonEmpty WhitespaceChar) KNot))
        NotTest
        a
  , _notTestMany_ann :: a
  }
  | NotTestNone
  { _notTestNone_value :: Comparison a
  , _notTestNone_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data AndTest a
  = AndTestOne
  { _andTestOne_value :: NotTest a
  , _andTestOne_ann :: a
  }
  | AndTestMany
  { _andTestMany_left :: NotTest a
  , _andTestMany_right
    :: Compose
        NonEmpty
        (Compose
          (Before (Between' (NonEmpty WhitespaceChar) KAnd))
          AndTest)
        a
  , _andTestMany_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data OrTest a
  = OrTestOne
  { _orTestOne_value :: AndTest a
  , _orTestOne_ann :: a
  }
  | OrTestMany
  { _orTestMany_left :: AndTest a
  , _orTestMany_right
    :: Compose
        NonEmpty
        (Compose
          (Before (Between' (NonEmpty WhitespaceChar) KOr))
          AndTest)
        a
  , _orTestMany_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data IfThenElse a
  = IfThenElse
  { _ifThenElse_if :: Compose (Between' (NonEmpty WhitespaceChar)) OrTest a
  , _ifThenElse_else :: Compose (Before (NonEmpty WhitespaceChar)) Test a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Test a
  = TestCondNoIf
  { _testCondNoIf_value :: OrTest a
  , _testCondNoIf_ann :: a
  }
  | TestCondIf
  { _testCondIf_head :: OrTest a
  , _testCondIf_tail
    :: Compose
        (Before (NonEmpty WhitespaceChar))
        IfThenElse
        a
  , _testCondIf_ann :: a
  }
  | TestLambdef
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TestList a
  = TestList
  { _testList_head :: Test a
  , _testList_tail
    :: Compose (Before (Between' [WhitespaceChar] Comma)) Test a
  , _testList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _testList_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data YieldArg a
  = YieldArgFrom
  { _yieldArgFrom_value
    :: Compose (Before (NonEmpty WhitespaceChar)) Test a
  , _yieldArgFrom_ann :: a
  }
  | YieldArgList
  { _yieldArgList_value :: TestList a
  , _yieldArgList_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data TestlistComp a
  = TestlistCompFor
  { _testlistCompFor_head :: Sum Test StarExpr a
  , _testlistCompFor_tail :: Compose (Before [WhitespaceChar]) CompFor a
  , _testlistCompFor_ann :: a
  }

  | TestlistCompList
  { _testlistCompList_head :: Sum Test StarExpr a
  , _testlistCompList_tail
    :: Compose
        []
        (Compose
          (Before (Between' [WhitespaceChar] Comma))
          (Sum Test StarExpr))
        a
  , _testlistCompList_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _testlistCompList_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data DictOrSetMaker a
  = DictOrSetMaker
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Atom a
  = AtomParen
  { _atomParen_val
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          (Sum YieldExpr TestlistComp))
        a
  , _atomParen_ann :: a
  }

  | AtomBracket
  { _atomBracket_val
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          TestlistComp)
        a
  , _atomBracket_ann :: a
  }

  | AtomCurly
  { _atomCurly_val
    :: Compose
        (Between' [WhitespaceChar])
        (Compose
          Maybe
          DictOrSetMaker)
        a
  , _atomCurly_ann :: a
  }

  | AtomIdentifier
  { _atomIdentifier_value :: Identifier a
  , _atomIdentifier_ann :: a
  }

  | AtomInteger
  { _atomInteger :: Integer' a
  , _atomInteger_ann :: a
  }

  | AtomFloat
  { _atomFloat :: Float' a
  , _atomFloat_ann :: a
  }

  | AtomString
  { _atomString_head :: Sum StringLiteral BytesLiteral a
  , _atomString_tail
    :: Compose
        []
        (Compose
          (Before [WhitespaceChar])
          (Sum StringLiteral BytesLiteral))
        a
  , _atomString_ann :: a
  }

  | AtomImag
  { _atomImag_value
    :: Compose
          (Before [WhitespaceChar])
          Imag
        a
  , _atomString_ann :: a
  }

  | AtomEllipsis
  { _atomEllipsis_ann :: a
  }

  | AtomNone
  { _atomNone_ann :: a
  }

  | AtomTrue
  { _atomTrue_ann :: a
  }

  | AtomFalse
  { _atomFalse_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)
