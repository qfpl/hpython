{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module Language.Python.Statement.AST where

import Papa hiding (Sum, Product, Space)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between
import Data.Singletons.Prelude ((:==))

import Language.Python.AST.ArgsList
import Language.Python.AST.ArgumentList
import Language.Python.AST.Comment
import Language.Python.AST.DottedName
import Language.Python.AST.Identifier
import Language.Python.AST.IndentedLines
import Language.Python.AST.IsArgList
import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.AST.TestlistStarExpr
import Language.Python.Expr.AST
import Language.Python.Statement.AST.AugAssign
import Language.Python.Statement.AST.Imports
import Language.Python.IR.ExprConfig
import Language.Python.IR.StatementConfig

data Statement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = StatementSimple
  { _statementSimple_value :: SimpleStatement lctxt ctxt a
  , _statementSimple_ann :: a
  }
  | StatementCompound
  { _statementCompound_value :: CompoundStatement lctxt ctxt a
  , _statementCompound_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Statement lctxt ctxt a)
deriving instance Show a => Show (Statement lctxt ctxt a)

data CompoundStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = CompoundStatementIf
  { _compoundStatementIf_value :: IfStatement lctxt ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementWhile
  { _compoundStatementWhile_value :: WhileStatement lctxt ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementFor
  { _compoundStatementFor_value :: ForStatement lctxt ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementTry
  { _compoundStatementTry_value :: TryStatement lctxt ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementWith
  { _compoundStatementWith_value :: WithStatement lctxt ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementFuncDef
  { _compoundStatementFuncDef_value :: FuncDef ctxt ('FunDef 'Normal) a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementClassDef
  { _compoundStatementClassDef_value :: ClassDef ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementDecorated
  { _compoundStatementDecorated_value :: Decorated ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementAsync
  { _compoundStatementAsyncIf_value :: AsyncStatement lctxt ctxt a
  , _compoundStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (CompoundStatement lctxt ctxt a)
deriving instance Show a => Show (CompoundStatement lctxt ctxt a)

data Decorated (ctxt :: DefinitionContext) a
  = Decorated
  { _decorated_decorators :: Compose NonEmpty (Decorator ctxt) a
  , _decorated_body
    :: Sum
         (Sum
           (ClassDef ctxt)
           (FuncDef ctxt ('FunDef 'Normal)))
         (AsyncFuncDef ctxt) a
  , _decorated_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Decorated ctxt a)
deriving instance Show a => Show (Decorated ctxt a)

data AsyncFuncDef (ctxt :: DefinitionContext) a
  = AsyncFuncDef
  { _asyncFuncDef_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (FuncDef ctxt ('FunDef 'Async))
         a
  , _asyncFuncDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AsyncFuncDef ctxt a)
deriving instance Show a => Show (AsyncFuncDef ctxt a)

data Decorator (ctxt :: DefinitionContext) a
  = Decorator
  { _decorator_name
    :: Compose
         (Before [WhitespaceChar])
         DottedName
         a
  , _decorator_args
    :: Compose
         Maybe
         (Compose
           (Between' [AnyWhitespaceChar])
           (Compose
             Maybe
             (ArgumentList Identifier Test 'NotAssignable ctxt)))
         a
  , _decorator_newline :: NewlineChar
  , _decorator_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Decorator ctxt a)
deriving instance Show a => Show (Decorator ctxt a)

data ClassDef (ctxt :: DefinitionContext) a
  = ClassDef
  { _classDef_name
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         Identifier
         a
  , _classDef_args
    :: Compose
         Maybe
         (Compose
           (Before [WhitespaceChar])
           (Compose
             (Between' [AnyWhitespaceChar])
             (Compose
               Maybe
               (ArgumentList Identifier Test 'NotAssignable ctxt))))
         a
  , _classDef_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite 'NotInLoop ctxt)
         a
  , _classDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ClassDef ctxt a)
deriving instance Show a => Show (ClassDef ctxt a)

data Suite (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = SuiteSingle
  { _suiteSingle_value :: SimpleStatement lctxt ctxt a
  , _suite_ann :: a
  }
  | SuiteMulti
  { _suiteMulti_comment
    :: Compose
         (Before [WhitespaceChar])
         (Compose Maybe Comment)
         a
  , _suiteMulti_newline :: NewlineChar
  , _suiteMulti_statements
    :: IndentedLines
         (Compose
           (Before [WhitespaceChar])
           (Compose
             (After NewlineChar)
             (Compose
               Maybe
               Comment)))
         (Statement lctxt ctxt)
         a
  , _suiteMulti_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Suite lctxt ctxt a)
deriving instance Show a => Show (Suite lctxt ctxt a)

data FuncDef (outer :: DefinitionContext) (inner :: DefinitionContext) (a :: *)
  = FuncDef
  { _funcDef_name
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         Identifier
         a
  , _funcDef_parameters
    :: Compose
         (Before [WhitespaceChar])
         (Parameters outer)
         a
  , _funcDef_type
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] RightArrow))
           (Test WhitespaceChar 'NotAssignable outer))
         a
  , _funcDef_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite 'NotInLoop inner)
         a
  , _funcDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (FuncDef outer inner a)
deriving instance Show a => Show (FuncDef outer inner a)

data Parameters (ctxt :: DefinitionContext) a
  = Parameters
  { _parameters_value
    :: Compose
         (Between' [AnyWhitespaceChar])
         (Compose
           Maybe
           (ArgumentList
             (TypedArg AnyWhitespaceChar)
             Test
             'NotAssignable
             ctxt))
         a
  , _parameters_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Parameters ctxt a)
deriving instance Show a => Show (Parameters ctxt a)

data TypedArg ws a
  = TypedArg
  { _typedArg_value :: Identifier a
  , _typedArg_type
    :: Compose
         Maybe
         (Compose
           (Before (Between' [ws] Colon))
           (Test ws 'NotAssignable ('FunDef 'Normal)))
         a
  , _typedArg_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (TypedArg ws a)
deriving instance (Show a, Show ws) => Show (TypedArg ws a)
deriving instance (Ord a, Ord ws) => Ord (TypedArg ws a)

data WithStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = WithStatement
  { _withStatement_itemHead
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (WithItem ctxt)
         a
  , _withStatement_itemTail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (WithItem ctxt))
         a
  , _withStatement_suite
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite lctxt ctxt)
         a
  , _withStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WithStatement lctxt ctxt a)
deriving instance Show a => Show (WithStatement lctxt ctxt a)

data WithItem (ctxt :: DefinitionContext) a
  = WithItem
  { _withItem_left :: Test WhitespaceChar 'NotAssignable ctxt a
  , _withItem_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAs))
           (Expr WhitespaceChar 'Assignable ctxt))
         a
  , _withItem_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WithItem ctxt a)
deriving instance Show a => Show (WithItem ctxt a)

data AsyncStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a where
  AsyncStatement ::
    { _asyncStatement_value
      :: Compose
          (Before (NonEmpty WhitespaceChar))
          (Sum
            (Sum
              (FuncDef ('FunDef 'Async) ('FunDef 'Async))
              (WithStatement lctxt ('FunDef 'Async)))
            (ForStatement lctxt ('FunDef 'Async)))
          a
    , _asyncStatement_ann :: a
    } -> AsyncStatement lctxt ('FunDef 'Async) a
deriving instance Functor (AsyncStatement lctxt ctxt)
deriving instance Foldable (AsyncStatement lctxt ctxt)
deriving instance Traversable (AsyncStatement lctxt ctxt)
deriving instance Eq a => Eq (AsyncStatement lctxt ctxt a)
deriving instance Show a => Show (AsyncStatement lctxt ctxt a)

data IfStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = IfStatement
  { _ifStatement_cond
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Test WhitespaceChar 'NotAssignable ctxt)
         a
  , _ifStatement_then
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite lctxt ctxt)
         a
  , _ifStatement_elifs
    :: Compose
         []
         (Product
           (Compose
             (Before (NonEmpty WhitespaceChar))
             (Test WhitespaceChar 'NotAssignable ctxt))
           (Compose
             (Before (Between' [WhitespaceChar] Colon))
             (Suite lctxt ctxt)))
         a
  , _ifStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite lctxt ctxt))
         a
  , _ifStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (IfStatement lctxt ctxt a)
deriving instance Show a => Show (IfStatement lctxt ctxt a)

data WhileStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = WhileStatement
  { _whileStatement_cond
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Test WhitespaceChar 'NotAssignable ctxt)
         a
  , _whileStatement_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite 'InLoop ctxt)
         a
  , _whileStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite lctxt ctxt))
         a
  , _whileStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WhileStatement lctxt ctxt a)
deriving instance Show a => Show (WhileStatement lctxt ctxt a)

data ForStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = ForStatement
  { _forStatement_for
    :: Compose
         (Between' (NonEmpty WhitespaceChar))
         (TestlistStarExpr WhitespaceChar Expr StarExpr 'Assignable ctxt)
         a
  , _forStatement_in
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (TestList WhitespaceChar 'NotAssignable ctxt)
         a
  , _forStatement_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite 'InLoop ctxt)
         a
  , _forStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite lctxt ctxt))
         a
  , _forStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ForStatement lctxt ctxt a)
deriving instance Show a => Show (ForStatement lctxt ctxt a)

data TryStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = TryStatementExcepts
  { _tryStatement_try
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite lctxt ctxt)
         a
  , _tryStatementExcepts_excepts
    :: Compose
         NonEmpty
         (Product
           (ExceptClause ctxt)
           (Compose
             (Before (Between' [WhitespaceChar] Colon))
             (Suite lctxt ctxt)))
         a
  , _tryStatementExcepts_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite lctxt ctxt))
         a
  , _tryStatementExcepts_finally
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite lctxt ctxt))
         a
  , _tryStatement_ann :: a
  }
  | TryStatementFinally
  { _tryStatement_try
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite lctxt ctxt)
         a
  , _tryStatementFinally_finally
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite lctxt ctxt)
         a
  , _tryStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (TryStatement lctxt ctxt a)
deriving instance Show a => Show (TryStatement lctxt ctxt a)

data ExceptClause (ctxt :: DefinitionContext) a
  = ExceptClause
  { _exceptClause_value
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           (Product
             (Test WhitespaceChar 'NotAssignable ctxt)
             (Compose
               Maybe
               (Compose
                 (Before (Between' (NonEmpty WhitespaceChar) KAs))
                 Identifier))))
         a
  , _exceptClause_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ExceptClause ctxt a)
deriving instance Show a => Show (ExceptClause ctxt a)

data SimpleStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a
  = SimpleStatement
  { _simpleStatement_head :: SmallStatement lctxt ctxt a
  , _simpleStatement_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Semicolon))
           (SmallStatement lctxt ctxt))
         a
  , _simpleStatement_semicolon :: Maybe (Before [WhitespaceChar] Semicolon)
  , _simpleStatement_newline
    :: Compose
         (Between [WhitespaceChar] NewlineChar)
         (Compose Maybe Comment)
         a
  , _simpleStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SimpleStatement lctxt ctxt a)
deriving instance Show a => Show (SimpleStatement lctxt ctxt a)

data SmallStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a where
  SmallStatementExpr ::
    { _smallStatementExpr_value :: Test WhitespaceChar 'NotAssignable ctxt a
    , _smallStatementExpr_ann :: a
    } -> SmallStatement lctxt ctxt a

  SmallStatementAssign ::
    { _smallStatementAssign_left
      :: TestlistStarExpr WhitespaceChar Test StarExpr 'Assignable ctxt a
    , _smallStatementAssign_middle
      :: Compose
           []
           (Compose
             (Before (Between' [WhitespaceChar] Equals))
             (TestlistStarExpr WhitespaceChar Test StarExpr 'Assignable ctxt))
         a
    , _smallStatementAssign_right
        :: Compose
             (Before (Between' [WhitespaceChar] Equals))
             (Sum
               (YieldExpr WhitespaceChar ctxt)
               (TestlistStarExpr WhitespaceChar Test StarExpr 'NotAssignable ctxt))
           a
    , _smallStatementAssign_ann :: a
    } -> SmallStatement lctxt ctxt a

  SmallStatementAugAssign ::
    { _smallStatementAugAssign_left :: Test WhitespaceChar 'Assignable ctxt a
    , _smallStatementAugAssign_right
      :: Compose
            (Before (Between' [WhitespaceChar] AugAssign))
            (Sum
              (YieldExpr WhitespaceChar ctxt)
              (TestList WhitespaceChar 'NotAssignable ctxt))
          a
    , _smallStatementAugAssign_ann :: a
    } -> SmallStatement lctxt ctxt a

  SmallStatementDel ::
    { _smallStatementDel_value
      :: Compose
          (Before (NonEmpty WhitespaceChar))
          (ExprList WhitespaceChar 'Assignable ctxt)
          a
    , _smallStatement_ann :: a
    } -> SmallStatement lctxt ctxt a

  SmallStatementPass ::
    { _smallStatement_ann :: a
    } -> SmallStatement lctxt ctxt a

  SmallStatementFlow ::
    { _smallStatementFlow_value :: FlowStatement lctxt ctxt a
    , _smallStatement_ann :: a
    } -> SmallStatement lctxt ctxt a

  SmallStatementImport ::
    { _smallStatementImport_value :: ImportStatement a
    , _smallStatement_ann :: a
    } -> SmallStatement lctxt ctxt a

  SmallStatementGlobal ::
    { _smallStatementGlobal_head
      :: Compose
           (Before (NonEmpty WhitespaceChar))
           Identifier
           a
    , _smallStatementGlobal_tail
      :: Compose
          []
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            Identifier)
          a
    , _smallStatement_ann :: a
    } -> SmallStatement lctxt ctxt a

  SmallStatementNonlocal
    :: ((ctxt :== 'TopLevel) ~ 'False)
    => Compose
         (Before (NonEmpty WhitespaceChar))
         Identifier
         a
    -> Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           Identifier)
         a
    -> a
    -> SmallStatement lctxt ctxt a

  SmallStatementAssert ::
    { _smallStatementAssert_head
      :: Compose
           (Before (NonEmpty WhitespaceChar))
           (Test WhitespaceChar 'NotAssignable ctxt)
           a
    , _smallStatementAssert_tail
      :: Compose
          Maybe
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (Test WhitespaceChar 'NotAssignable ctxt))
          a
    , _smallStatement_ann :: a
    } -> SmallStatement lctxt ctxt a

deriving instance Functor (SmallStatement lctxt ctxt)
deriving instance Foldable (SmallStatement lctxt ctxt)
deriving instance Traversable (SmallStatement lctxt ctxt)
deriving instance Eq a => Eq (SmallStatement lctxt ctxt a)
deriving instance Show a => Show (SmallStatement lctxt ctxt a)

_smallStatementNonlocal_head
  :: SmallStatement lctxt ctxt a
  -> Compose (Before (NonEmpty WhitespaceChar)) Identifier a
_smallStatementNonlocal_head (SmallStatementNonlocal a _ _) = a

_smallStatementNonlocal_tail
  :: SmallStatement lctxt ctxt a
  -> Compose
      []
      (Compose
        (Before (Between' [WhitespaceChar] Comma))
        Identifier)
      a
_smallStatementNonlocal_tail (SmallStatementNonlocal _ a _) = a

_smallStatementNonlocal_ann :: SmallStatement lctxt ctxt a -> a
_smallStatementNonlocal_ann (SmallStatementNonlocal _ _ a) = a

data FlowStatement (lctxt :: LoopContext) (ctxt :: DefinitionContext) a where
  FlowStatementBreak ::
    { _flowStatementBreak_ann :: a
    } -> FlowStatement 'InLoop ctxt a
  FlowStatementContinue ::
    { _flowStatementContinue_ann :: a
    } -> FlowStatement 'InLoop ctxt a
  FlowStatementReturn ::
    { _flowStatementReturn_value
      :: Compose
          Maybe
          (Compose
            (Before (NonEmpty WhitespaceChar))
            (TestList WhitespaceChar 'NotAssignable ('FunDef b)))
          a
    , _flowStatementReturn_ann :: a
    } -> FlowStatement lctxt ('FunDef b) a
  FlowStatementRaise ::
    { _flowStatementRaise_value
      :: Compose
          Maybe
          (Compose
            (Before (NonEmpty WhitespaceChar))
            (RaiseStatement ctxt))
          a
    , _flowStatementRaise_ann :: a
    } -> FlowStatement lctxt ctxt a
  FlowStatementYield ::
    { _flowStatementYield_value :: YieldExpr WhitespaceChar ctxt a
    , _flowStatementYield_ann :: a
    } -> FlowStatement lctxt ctxt a

deriving instance Functor (FlowStatement lctxt ctxt)
deriving instance Foldable (FlowStatement lctxt ctxt)
deriving instance Traversable (FlowStatement lctxt ctxt)
deriving instance Eq a => Eq (FlowStatement lctxt ctxt a)
deriving instance Show a => Show (FlowStatement lctxt ctxt a)

data RaiseStatement (ctxt :: DefinitionContext) a
  = RaiseStatement
  { _raiseStatement_left :: Test WhitespaceChar 'NotAssignable ctxt a
  , _raiseStatement_right
    :: Compose
         Maybe
         (Compose
           (Before KFrom)
           (Compose
             (Before (NonEmpty WhitespaceChar))
             (Test WhitespaceChar 'NotAssignable ctxt)))
         a
  , _raiseStatement_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''SmallStatement
deriveOrd1 ''SmallStatement
deriveShow1 ''SmallStatement

deriveEq1 ''RaiseStatement
deriveOrd1 ''RaiseStatement
deriveShow1 ''RaiseStatement

deriveEq1 ''FlowStatement
deriveOrd1 ''FlowStatement
deriveShow1 ''FlowStatement

deriveEq1 ''Decorator
deriveOrd1 ''Decorator
deriveShow1 ''Decorator

deriveEq1 ''ClassDef
deriveOrd1 ''ClassDef
deriveShow1 ''ClassDef

deriveEq1 ''FuncDef
deriveOrd1 ''FuncDef
deriveShow1 ''FuncDef

deriveEq1 ''AsyncFuncDef
deriveOrd1 ''AsyncFuncDef
deriveShow1 ''AsyncFuncDef

deriveEq1 ''Suite
deriveOrd1 ''Suite
deriveShow1 ''Suite

deriveEq1 ''Statement
deriveOrd1 ''Statement
deriveShow1 ''Statement

deriveEq1 ''Parameters
deriveOrd1 ''Parameters
deriveShow1 ''Parameters

deriveEq1 ''TypedArg
deriveOrd1 ''TypedArg
deriveShow1 ''TypedArg
makeLenses ''TypedArg

deriveEq1 ''WithItem
deriveOrd1 ''WithItem
deriveShow1 ''WithItem

deriveEq1 ''WithStatement
deriveOrd1 ''WithStatement
deriveShow1 ''WithStatement

deriveEq1 ''WhileStatement
deriveOrd1 ''WhileStatement
deriveShow1 ''WhileStatement

deriveEq1 ''ForStatement
deriveOrd1 ''ForStatement
deriveShow1 ''ForStatement

deriveEq1 ''ExceptClause
deriveOrd1 ''ExceptClause
deriveShow1 ''ExceptClause

deriveEq1 ''SimpleStatement
deriveOrd1 ''SimpleStatement
deriveShow1 ''SimpleStatement

deriveEq1 ''CompoundStatement
deriveOrd1 ''CompoundStatement
deriveShow1 ''CompoundStatement

deriveEq1 ''IfStatement
deriveOrd1 ''IfStatement
deriveShow1 ''IfStatement

deriveEq1 ''TryStatement
deriveOrd1 ''TryStatement
deriveShow1 ''TryStatement

deriveEq1 ''Decorated
deriveOrd1 ''Decorated
deriveShow1 ''Decorated

deriveEq1 ''AsyncStatement
deriveOrd1 ''AsyncStatement
deriveShow1 ''AsyncStatement

instance HasName (TypedArg ws) where
  name = typedArg_value.identifier_value
