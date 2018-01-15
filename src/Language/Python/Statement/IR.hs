{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.Statement.IR where

import Papa hiding (Sum, Product, Space)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Comment
import Language.Python.AST.DottedName
import Language.Python.AST.Identifier
import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.Expr.IR
import Language.Python.IR.ArgsList
import Language.Python.IR.ArgumentList
import Language.Python.IR.TestlistStarExpr
import Language.Python.Statement.AST.AugAssign
import Language.Python.Statement.AST.Imports

data Statement a
  = StatementSimple
  { _statementSimple_value :: SimpleStatement a
  , _statementSimple_ann :: a
  }
  | StatementCompound
  { _statementCompound_value :: CompoundStatement a
  , _statementCompound_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Statement a)
deriving instance Ord a => Ord (Statement a)
deriving instance Show a => Show (Statement a)

data CompoundStatement a
  = CompoundStatementIf
  { _compoundStatementIf_value :: IfStatement a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementWhile
  { _compoundStatementWhile_value :: WhileStatement a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementFor
  { _compoundStatementFor_value :: ForStatement a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementTry
  { _compoundStatementTry_value :: TryStatement a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementWith
  { _compoundStatementWith_value :: WithStatement a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementFuncDef
  { _compoundStatementFuncDef_value :: FuncDef a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementClassDef
  { _compoundStatementClassDef_value :: ClassDef a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementDecorated
  { _compoundStatementDecorated_value :: Decorated a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementAsync
  { _compoundStatementAsyncIf_value :: AsyncStatement a
  , _compoundStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (CompoundStatement a)
deriving instance Ord a => Ord (CompoundStatement a)
deriving instance Show a => Show (CompoundStatement a)

data Decorated a
  = Decorated
  { _decorated_decorators :: Compose NonEmpty Decorator a
  , _decorated_body :: Sum (Sum ClassDef FuncDef) AsyncFuncDef a
  , _decorated_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Decorated a)
deriving instance Ord a => Ord (Decorated a)
deriving instance Show a => Show (Decorated a)

data AsyncFuncDef a
  = AsyncFuncDef
  { _asyncFuncDef_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         FuncDef
         a
  , _asyncFuncDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AsyncFuncDef a)
deriving instance Ord a => Ord (AsyncFuncDef a)
deriving instance Show a => Show (AsyncFuncDef a)

data Decorator a
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
             (ArgumentList Identifier Test)))
         a
  , _decorator_newline :: NewlineChar
  , _decorator_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Decorator a)
deriving instance Ord a => Ord (Decorator a)
deriving instance Show a => Show (Decorator a)

data ClassDef a
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
               (ArgumentList Identifier Test))))
         a
  , _classDef_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _classDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ClassDef a)
deriving instance Ord a => Ord (ClassDef a)
deriving instance Show a => Show (ClassDef a)

data Suite a
  = SuiteSingle
  { _suiteSingle_value :: SimpleStatement a
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
    :: Compose
         NonEmpty
         (Sum
           (Compose
             (Between [WhitespaceChar] NewlineChar)
             (Compose
               Maybe
               Comment))
           (Compose
             (Before (NonEmpty IndentationChar))
             Statement))
         a
  , _suiteMulti_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Suite a)
deriving instance Ord a => Ord (Suite a)
deriving instance Show a => Show (Suite a)

data FuncDef a
  = FuncDef
  { _funcDef_name
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         Identifier
         a
  , _funcDef_parameters
    :: Compose
         (Before [WhitespaceChar])
         Parameters
         a
  , _funcDef_type
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] RightArrow))
           (Test WhitespaceChar))
         a
  , _funcDef_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _funcDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (FuncDef a)
deriving instance Ord a => Ord (FuncDef a)
deriving instance Show a => Show (FuncDef a)

data Parameters a
  = Parameters
  { _parameters_value
    :: Compose
         (Between' [AnyWhitespaceChar])
         (Compose
           Maybe
           (ArgumentList (TypedArg AnyWhitespaceChar) Test))
         a
  , _parameters_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Parameters a)
deriving instance Ord a => Ord (Parameters a)
deriving instance Show a => Show (Parameters a)

data TypedArg ws a
  = TypedArg
  { _typedArg_value :: Identifier a
  , _typedArg_type
    :: Compose
         Maybe
         (Compose
           (Before (Between' [ws] Colon))
           (Test ws))
         a
  , _typedArg_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq a, Eq ws) => Eq (TypedArg ws a)
deriving instance (Ord a, Ord ws) => Ord (TypedArg ws a)
deriving instance (Show a, Show ws) => Show (TypedArg ws a)

data WithStatement a
  = WithStatement
  { _withStatement_itemHead
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         WithItem
         a
  , _withStatement_itemTail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           WithItem)
         a
  , _withStatement_suite
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _withStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WithStatement a)
deriving instance Ord a => Ord (WithStatement a)
deriving instance Show a => Show (WithStatement a)

data WithItem a
  = WithItem
  { _withItem_left :: Test WhitespaceChar a
  , _withItem_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAs))
           (Expr WhitespaceChar))
         a
  , _withItem_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WithItem a)
deriving instance Ord a => Ord (WithItem a)
deriving instance Show a => Show (WithItem a)

data AsyncStatement a
  = AsyncStatement
  { _asyncStatement_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Sum
           (Sum FuncDef WithStatement)
           ForStatement)
         a
  , _asyncStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AsyncStatement a)
deriving instance Ord a => Ord (AsyncStatement a)
deriving instance Show a => Show (AsyncStatement a)

data IfStatement a
  = IfStatement
  { _ifStatement_cond
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Test WhitespaceChar)
         a
  , _ifStatement_then
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _ifStatement_elifs
    :: Compose
         []
         (Product
           (Compose
             (Before (NonEmpty WhitespaceChar))
             (Test WhitespaceChar))
           (Compose
             (Before (Between' [WhitespaceChar] Colon))
             Suite))
         a
  , _ifStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           Suite)
         a
  , _ifStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (IfStatement a)
deriving instance Ord a => Ord (IfStatement a)
deriving instance Show a => Show (IfStatement a)

data WhileStatement a
  = WhileStatement
  { _whileStatement_cond
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Test WhitespaceChar)
         a
  , _whileStatement_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _whileStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           Suite)
         a
  , _whileStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WhileStatement a)
deriving instance Ord a => Ord (WhileStatement a)
deriving instance Show a => Show (WhileStatement a)

data ForStatement a
  = ForStatement
  { _forStatement_for
    :: Compose
         (Between' (NonEmpty WhitespaceChar))
         (TestlistStarExpr WhitespaceChar Expr StarExpr)
         a
  , _forStatement_in
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (TestList WhitespaceChar)
         a
  , _forStatement_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _forStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           Suite)
         a
  , _forStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ForStatement a)
deriving instance Ord a => Ord (ForStatement a)
deriving instance Show a => Show (ForStatement a)

data TryStatement a
  = TryStatementExcepts
  { _tryStatement_try
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _tryStatementExcepts_excepts
    :: Compose
         NonEmpty
         (Product
           ExceptClause
           (Compose
             (Before (Between' [WhitespaceChar] Colon))
             Suite))
         a
  , _tryStatementExcepts_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           Suite)
         a
  , _tryStatementExcepts_finally
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           Suite)
         a
  , _tryStatement_ann :: a
  }
  | TryStatementFinally
  { _tryStatement_try
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _tryStatementFinally_finally
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _tryStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (TryStatement a)
deriving instance Ord a => Ord (TryStatement a)
deriving instance Show a => Show (TryStatement a)

data ExceptClause a
  = ExceptClause
  { _exceptClause_value
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           (Product
             (Test WhitespaceChar)
             (Compose
               Maybe
               (Compose
                 (Before (Between' (NonEmpty WhitespaceChar) KAs))
                 Identifier))))
         a
  , _exceptClause_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ExceptClause a)
deriving instance Ord a => Ord (ExceptClause a)
deriving instance Show a => Show (ExceptClause a)

data SimpleStatement a
  = SimpleStatement
  { _simpleStatement_head :: SmallStatement a
  , _simpleStatement_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Semicolon))
           SmallStatement)
         a
  , _simpleStatement_semicolon :: Maybe (Before [WhitespaceChar] Semicolon)
  , _simpleStatement_newline
    :: Compose
         (Between [WhitespaceChar] NewlineChar)
         (Compose Maybe Comment) a
  , _simpleStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SimpleStatement a)
deriving instance Ord a => Ord (SimpleStatement a)
deriving instance Show a => Show (SimpleStatement a)

data SmallStatement a
  = SmallStatementExpr
  { _smallStatementExpr_left
    :: TestlistStarExpr WhitespaceChar Test StarExpr a
  , _smallStatementExpr_right
    :: Sum
         (Compose
           (Before (Between' [WhitespaceChar] AugAssign))
           (Sum (YieldExpr WhitespaceChar) (TestList WhitespaceChar)))
         (Compose
           []
           (Compose
             (Before (Between' [WhitespaceChar] Equals))
             (Sum
               (YieldExpr WhitespaceChar)
               (TestlistStarExpr WhitespaceChar Test StarExpr))))
         a
  , _smallStatement_ann :: a
  }
  | SmallStatementDel
  { _smallStatementDel_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (ExprList WhitespaceChar)
         a
  , _smallStatement_ann :: a
  }
  | SmallStatementPass
  { _smallStatement_ann :: a
  }
  | SmallStatementFlow
  { _smallStatementFlow_value :: FlowStatement a
  , _smallStatement_ann :: a
  }
  | SmallStatementImport
  { _smallStatementImport_value :: ImportStatement a
  , _smallStatement_ann :: a
  }
  | SmallStatementGlobal
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
  }
  | SmallStatementNonlocal
  { _smallStatementNonLocal_head
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         Identifier
         a
  , _smallStatementNonLocal_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           Identifier)
         a
  , _smallStatement_ann :: a
  }
  | SmallStatementAssert
  { _smallStatementAssert_head
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Test WhitespaceChar)
         a
  , _smallStatementAssert_tail
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Test WhitespaceChar))
         a
  , _smallStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SmallStatement a)
deriving instance Ord a => Ord (SmallStatement a)
deriving instance Show a => Show (SmallStatement a)

data FlowStatement a
  = FlowStatementBreak
  { _flowStatement_ann :: a
  }
  | FlowStatementContinue
  { _flowStatement_ann :: a
  }
  | FlowStatementReturn
  { _flowStatementReturn_value
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           (TestList WhitespaceChar))
         a
  , _flowStatement_ann :: a
  }
  | FlowStatementRaise
  { _flowStatementRaise_value
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           RaiseStatement)
         a
  , _flowStatement_ann :: a
  }
  | FlowStatementYield
  { _flowStatementYield_value :: YieldExpr WhitespaceChar a
  , _flowStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (FlowStatement a)
deriving instance Ord a => Ord (FlowStatement a)
deriving instance Show a => Show (FlowStatement a)

data RaiseStatement a
  = RaiseStatement
  { _raiseStatement_left :: Test WhitespaceChar a
  , _raiseStatement_right
    :: Compose
         Maybe
         (Compose
           (Before KFrom)
           (Compose
             (Before (NonEmpty WhitespaceChar))
             (Test WhitespaceChar)))
         a
  , _raiseStatement_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''SmallStatement
deriveEq1 ''SmallStatement
deriveOrd1 ''SmallStatement
deriveShow1 ''SmallStatement

makeLenses ''RaiseStatement
deriveEq1 ''RaiseStatement
deriveOrd1 ''RaiseStatement
deriveShow1 ''RaiseStatement

makeLenses ''FlowStatement
deriveEq1 ''FlowStatement
deriveOrd1 ''FlowStatement
deriveShow1 ''FlowStatement

makeLenses ''Decorator
deriveEq1 ''Decorator
deriveOrd1 ''Decorator
deriveShow1 ''Decorator

makeLenses ''ClassDef
deriveEq1 ''ClassDef
deriveOrd1 ''ClassDef
deriveShow1 ''ClassDef

makeLenses ''FuncDef
deriveEq1 ''FuncDef
deriveOrd1 ''FuncDef
deriveShow1 ''FuncDef

makeLenses ''AsyncFuncDef
deriveEq1 ''AsyncFuncDef
deriveOrd1 ''AsyncFuncDef
deriveShow1 ''AsyncFuncDef

makeLenses ''Suite
deriveEq1 ''Suite
deriveOrd1 ''Suite
deriveShow1 ''Suite

makeLenses ''Statement
deriveEq1 ''Statement
deriveOrd1 ''Statement
deriveShow1 ''Statement

makeLenses ''Parameters
deriveEq1 ''Parameters
deriveOrd1 ''Parameters
deriveShow1 ''Parameters

makeLenses ''TypedArg
deriveEq1 ''TypedArg
deriveOrd1 ''TypedArg
deriveShow1 ''TypedArg

makeLenses ''WithItem
deriveEq1 ''WithItem
deriveOrd1 ''WithItem
deriveShow1 ''WithItem

makeLenses ''WithStatement
deriveEq1 ''WithStatement
deriveOrd1 ''WithStatement
deriveShow1 ''WithStatement

makeLenses ''WhileStatement
deriveEq1 ''WhileStatement
deriveOrd1 ''WhileStatement
deriveShow1 ''WhileStatement

makeLenses ''ForStatement
deriveEq1 ''ForStatement
deriveOrd1 ''ForStatement
deriveShow1 ''ForStatement

makeLenses ''ExceptClause
deriveEq1 ''ExceptClause
deriveOrd1 ''ExceptClause
deriveShow1 ''ExceptClause

makeLenses ''SimpleStatement
deriveEq1 ''SimpleStatement
deriveOrd1 ''SimpleStatement
deriveShow1 ''SimpleStatement

makeLenses ''CompoundStatement
deriveEq1 ''CompoundStatement
deriveOrd1 ''CompoundStatement
deriveShow1 ''CompoundStatement

makeLenses ''IfStatement
deriveEq1 ''IfStatement
deriveOrd1 ''IfStatement
deriveShow1 ''IfStatement

makeLenses ''TryStatement
deriveEq1 ''TryStatement
deriveOrd1 ''TryStatement
deriveShow1 ''TryStatement

makeLenses ''Decorated
deriveEq1 ''Decorated
deriveOrd1 ''Decorated
deriveShow1 ''Decorated

makeLenses ''AsyncStatement
deriveEq1 ''AsyncStatement
deriveOrd1 ''AsyncStatement
deriveShow1 ''AsyncStatement
