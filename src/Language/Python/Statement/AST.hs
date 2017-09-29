{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.Statement.AST where

import Papa hiding (Sum, Product, Space)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Identifier
import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.Expr.AST
import Language.Python.AST.ArgsList
import Language.Python.IR.SyntaxConfig

data Statement (ctxt :: ExprContext) a
  = StatementSimple
  { _statementSimple_value :: SimpleStatement ctxt a
  , _statementSimple_ann :: a
  }
  | StatementCompound
  { _statementCompound_value :: CompoundStatement ctxt a
  , _statementCompound_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Statement ctxt a)
deriving instance Show a => Show (Statement ctxt a)

data CompoundStatement (ctxt :: ExprContext) a
  = CompoundStatementIf
  { _compoundStatementIf_value :: IfStatement ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementWhile
  { _compoundStatementWhile_value :: WhileStatement ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementFor
  { _compoundStatementFor_value :: ForStatement ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementTry
  { _compoundStatementTry_value :: TryStatement ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementWith
  { _compoundStatementWith_value :: WithStatement ctxt a
  , _compoundStatement_ann :: a
  }
  | CompoundStatementFuncDef
  { _compoundStatementFuncDef_value :: FuncDef ctxt a
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
  { _compoundStatementAsyncIf_value :: AsyncStatement ctxt a
  , _compoundStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (CompoundStatement ctxt a)
deriving instance Show a => Show (CompoundStatement ctxt a)

data Decorated (ctxt :: ExprContext) a
  = Decorated
  { _decorated_decorators :: Compose NonEmpty (Decorator ctxt) a
  , _decorated_body :: Sum (Sum (ClassDef ctxt) (FuncDef ctxt)) (AsyncFuncDef ctxt) a
  , _decorated_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Decorated ctxt a)
deriving instance Show a => Show (Decorated ctxt a)

data AsyncFuncDef (ctxt :: ExprContext) a
  = AsyncFuncDef
  { _asyncFuncDef_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (FuncDef ('FunDef 'Async))
         a
  , _asyncFuncDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AsyncFuncDef ctxt a)
deriving instance Show a => Show (AsyncFuncDef ctxt a)

data Decorator (ctxt :: ExprContext) a
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
           (Between' [WhitespaceChar])
           (Compose
             Maybe
             (ArgList 'NotAssignable ctxt)))
         a
  , _decorator_newline :: NewlineChar
  , _decorator_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Decorator ctxt a)
deriving instance Show a => Show (Decorator ctxt a)

data ClassDef (ctxt :: ExprContext) a
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
             (Between' [WhitespaceChar])
             (Compose
               Maybe
               (ArgList 'NotAssignable ctxt))))
         a
  , _classDef_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite ctxt)
         a
  , _classDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ClassDef ctxt a)
deriving instance Show a => Show (ClassDef ctxt a)

data Suite (ctxt :: ExprContext) a
  = SuiteSingle
  { _suiteSingle_value :: SimpleStatement ctxt a
  , _suite_ann :: a
  }
  | SuiteMulti
  { _suiteMulti_newline :: NewlineChar
  , _suiteMulti_statements
    :: Compose
         NonEmpty
         (Compose
           (Before (NonEmpty IndentationChar))
           (Statement ctxt))
         a
  , _suiteMulti_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Suite ctxt a)
deriving instance Show a => Show (Suite ctxt a)

data FuncDef (ctxt :: ExprContext) a
  = FuncDef
  { _funcDef_name
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         Identifier
         a
  , _funcDef_parameters
    :: Compose
         (Before [WhitespaceChar])
         (Parameters ctxt)
         a
  , _funcDef_type
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] RightArrow))
           (Test 'NotAssignable ctxt))
         a
  , _funcDef_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite ctxt)
         a
  , _funcDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (FuncDef ctxt a)
deriving instance Show a => Show (FuncDef ctxt a)

data Parameters (ctxt :: ExprContext) a
  = Parameters
  { _parameters_value
    :: Compose
         (Between' [WhitespaceChar])
         (Compose
           Maybe
           (ArgsList TypedArg (Test 'NotAssignable ctxt)))
         a
  , _parameters_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (Parameters ctxt a)
deriving instance Show a => Show (Parameters ctxt a)

data TypedArg a
  = TypedArg
  { _typedArg_value :: Identifier a
  , _typedArg_type
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Test 'NotAssignable ('FunDef 'Normal)))
         a
  , _typedArg_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (TypedArg a)
deriving instance Show a => Show (TypedArg a)

data WithStatement (ctxt :: ExprContext) a
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
         (Suite ctxt)
         a
  , _withStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WithStatement ctxt a)
deriving instance Show a => Show (WithStatement ctxt a)

data WithItem (ctxt :: ExprContext) a
  = WithItem
  { _withItem_left :: Test 'NotAssignable ctxt a
  , _withItem_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAs))
           (Expr 'Assignable ctxt))
         a
  , _withItem_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WithItem ctxt a)
deriving instance Show a => Show (WithItem ctxt a)

data AsyncStatement (ctxt :: ExprContext) a where
  AsyncStatement ::
    { _asyncStatement_value
      :: Compose
          (Before (NonEmpty WhitespaceChar))
          (Sum
            (Sum (FuncDef ('FunDef 'Async)) (WithStatement ('FunDef 'Async)))
            (ForStatement ('FunDef 'Async)))
          a
    , _asyncStatement_ann :: a
    } -> AsyncStatement ('FunDef 'Async) a
deriving instance Functor (AsyncStatement ctxt)
deriving instance Foldable (AsyncStatement ctxt)
deriving instance Traversable (AsyncStatement ctxt)
deriving instance Eq a => Eq (AsyncStatement ctxt a)
deriving instance Show a => Show (AsyncStatement ctxt a)

data IfStatement (ctxt :: ExprContext) a
  = IfStatement
  { _ifStatement_cond
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Test 'NotAssignable ctxt)
         a
  , _ifStatement_then
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite ctxt)
         a
  , _ifStatement_elifs
    :: Compose
         []
         (Product
           (Compose
             (Before (NonEmpty WhitespaceChar))
             (Test 'NotAssignable ctxt))
           (Compose
             (Before (Between' [WhitespaceChar] Colon))
             (Suite ctxt)))
         a
  , _ifStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite ctxt))
         a
  , _ifStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (IfStatement ctxt a)
deriving instance Show a => Show (IfStatement ctxt a)

data WhileStatement (ctxt :: ExprContext) a
  = WhileStatement
  { _whileStatement_cond
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Test 'NotAssignable ctxt)
         a
  , _whileStatement_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite ctxt)
         a
  , _whileStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite ctxt))
         a
  , _whileStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WhileStatement ctxt a)
deriving instance Show a => Show (WhileStatement ctxt a)

data ForStatement (ctxt :: ExprContext) a
  = ForStatement
  { _forStatement_for
    :: Compose
         (Between' (NonEmpty WhitespaceChar))
         (ExprList 'Assignable ctxt)
         a
  , _forStatement_in
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (TestList 'NotAssignable ctxt)
         a
  , _forStatement_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite ctxt)
         a
  , _forStatement_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite ctxt))
         a
  , _forStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ForStatement ctxt a)
deriving instance Show a => Show (ForStatement ctxt a)

data TryStatement (ctxt :: ExprContext) a
  = TryStatementExcepts
  { _tryStatement_try
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite ctxt)
         a
  , _tryStatementExcepts_excepts
    :: Compose
         NonEmpty
         (Product
           (ExceptClause ctxt)
           (Compose
             (Before (Between' [WhitespaceChar] Colon))
             (Suite ctxt)))
         a
  , _tryStatementExcepts_else
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite ctxt))
         a
  , _tryStatementExcepts_finally
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Colon))
           (Suite ctxt))
         a
  , _tryStatement_ann :: a
  }
  | TryStatementFinally
  { _tryStatement_try
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite ctxt)
         a
  , _tryStatementFinally_finally
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         (Suite ctxt)
         a
  , _tryStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (TryStatement ctxt a)
deriving instance Show a => Show (TryStatement ctxt a)

data ExceptClause (ctxt :: ExprContext) a
  = ExceptClause
  { _exceptClause_value
    :: Compose
         Maybe
         (Product
           (Test 'NotAssignable ctxt)
           (Compose
             Maybe
             (Compose
               (Before (Between' (NonEmpty WhitespaceChar) KAs))
               Identifier)))
         a
  , _exceptClause_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ExceptClause ctxt a)
deriving instance Show a => Show (ExceptClause ctxt a)

data SimpleStatement (ctxt :: ExprContext) a
  = SimpleStatement
  { _simpleStatement_head :: SmallStatement ctxt a
  , _simpleStatement_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Semicolon))
           (SmallStatement ctxt))
         a
  , _simpleStatement_semicolon :: Maybe (Before [WhitespaceChar] Semicolon)
  , _simpleStatement_newline :: Before [WhitespaceChar] NewlineChar
  , _simpleStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SimpleStatement ctxt a)
deriving instance Show a => Show (SimpleStatement ctxt a)

data AugAssign
  = PlusEquals
  | MinusEquals
  | StarEquals
  | AtEquals
  | SlashEquals
  | PercentEquals
  | AmphersandEquals
  | PipeEquals
  | CaretEquals
  | ShiftLeftEquals
  | ShiftRightEquals
  | DoubleStarEquals
  | DoubleSlashEquals
  deriving (Eq, Show)

data SmallStatement (ctxt :: ExprContext) a
  = SmallStatementExpr
  { _smallStatementExpr_left :: TestlistStarExpr 'Assignable ctxt a
  , _smallStatementExpr_right
    :: Sum
         (Compose
           (Before (Between' [WhitespaceChar] AugAssign))
           (Sum YieldExpr (TestList 'NotAssignable ctxt)))
         (Compose
           []
           (Compose
             (Before (Between' [WhitespaceChar] Equals))
             (Sum YieldExpr (TestlistStarExpr 'NotAssignable ctxt))))
         a
  , _smallStatement_ann :: a
  }
  | SmallStatementDel
  { _smallStatementDel_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (ExprList 'Assignable ctxt)
         a
  , _smallStatement_ann :: a
  }
  | SmallStatementPass
  { _smallStatement_ann :: a
  }
  | SmallStatementFlow
  { _smallStatementFlow_value :: FlowStatement ctxt a
  , _smallStatement_ann :: a
  }
  | SmallStatementImport
  { _smallStatementImport_value :: ImportStatement a
  , _smallStatement_ann :: a
  }
  | SmallStatementGlobal
  { _smallStatementGlobal_head :: Identifier a
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
  { _smallStatementNonLocal_head :: Identifier a
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
  { _smallStatementAssert_head :: Test 'NotAssignable ctxt a
  , _smallStatementAssert_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Test 'NotAssignable ctxt))
         a
  , _smallStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SmallStatement ctxt a)
deriving instance Show a => Show (SmallStatement ctxt a)

data FlowStatement (ctxt :: ExprContext) a
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
           (TestList 'NotAssignable ctxt))
         a
  , _flowStatement_ann :: a
  }
  | FlowStatementRaise
  { _flowStatementRaise_value
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           (RaiseStatement ctxt))
         a
  , _flowStatement_ann :: a
  }
  | FlowStatementYield
  { _flowStatementYield_value :: YieldExpr a
  , _flowStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (FlowStatement ctxt a)
deriving instance Show a => Show (FlowStatement ctxt a)

data ImportStatement a
  = ImportStatementName
  { _importStatementName_value :: ImportName a
  , _importStatement_ann :: a
  }
  | ImportStatementFrom
  { _importStatementFrom_value :: ImportFrom a
  , _importStatement_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data RaiseStatement (ctxt :: ExprContext) a
  = RaiseStatement
  { _raiseStatement_left :: Test 'NotAssignable ctxt a
  , _raiseStatement_right
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           (Test 'NotAssignable ctxt))
         a
  , _raiseStatement_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ImportFrom a
  = ImportFrom
  { _importFrom_from
    :: Sum
         (Compose
           (Before [Between' [WhitespaceChar] (Either Dot Ellipsis)])
           DottedName)
         (Const (NonEmpty (Either Dot Ellipsis)))
         a
  , _importFrom_import
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Sum
           (Sum
             (Const Asterisk)
             (Compose
               (Between LeftParen RightParen)
               (Compose
                 (Between' [WhitespaceChar])
                 ImportAsNames)))
           ImportAsNames)
         a
  , _importFrom_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ImportFrom a)
deriving instance Show a => Show (ImportFrom a)

data ImportAsNames a
  = ImportAsNames
  { _importAsNames_head :: ImportAsName a
  , _importAsNames_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           ImportAsName)
         a
  , _importAsNames_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _importAsNames_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ImportAsNames a)
deriving instance Show a => Show (ImportAsNames a)

data ImportAsName a
  = ImportAsName
  { _importAsName_left :: Identifier a
  , _importAsName_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAs))
           Identifier)
         a
  , _importAsName_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ImportName a
  = ImportName
  { _importName_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         DottedAsNames
         a
  , _importName_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ImportName a)
deriving instance Show a => Show (ImportName a)

data DottedAsNames a
  = DottedAsNames
  { _dottedAsNames_head :: DottedAsName a
  , _dottedAsNames_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           DottedAsName)
         a
  , _dottedAsNames_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (DottedAsNames a)
deriving instance Show a => Show (DottedAsNames a)

data DottedAsName a
  = DottedAsName
  { _dottedAsName_left :: DottedName a
  , _dottedAsName_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAs))
           Identifier)
         a
  , _dottedAsName_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data DottedName a
  = DottedName
  { _dottedName_head :: Identifier a
  , _dottedName_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Dot))
           Identifier)
         a
  , _dottedName_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TestlistStarExpr (assignable :: AtomType) (ctxt :: ExprContext) a
  = TestlistStarExpr
  { _testlistStarExpr_head
    :: Sum (Test assignable ctxt) (StarExpr assignable ctxt) a
  , _testlistStarExpr_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum (Test assignable ctxt) (StarExpr assignable ctxt)))
         a
  , _testlistStarExpr_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _testlistStarExpr_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''SmallStatement
deriveEq1 ''SmallStatement
deriveShow1 ''SmallStatement

makeLenses ''TestlistStarExpr
deriveEq1 ''TestlistStarExpr
deriveShow1 ''TestlistStarExpr

makeLenses ''RaiseStatement
deriveEq1 ''RaiseStatement
deriveShow1 ''RaiseStatement

makeLenses ''DottedAsName
deriveEq1 ''DottedAsName
deriveShow1 ''DottedAsName

makeLenses ''ImportAsName
deriveEq1 ''ImportAsName
deriveShow1 ''ImportAsName

makeLenses ''DottedName
deriveEq1 ''DottedName
deriveShow1 ''DottedName

makeLenses ''DottedAsNames
deriveEq1 ''DottedAsNames
deriveShow1 ''DottedAsNames

makeLenses ''ImportAsNames
deriveEq1 ''ImportAsNames
deriveShow1 ''ImportAsNames

makeLenses ''FlowStatement
deriveEq1 ''FlowStatement
deriveShow1 ''FlowStatement

makeLenses ''ImportStatement
deriveEq1 ''ImportStatement
deriveShow1 ''ImportStatement

makeLenses ''ImportName
deriveEq1 ''ImportName
deriveShow1 ''ImportName

makeLenses ''ImportFrom
deriveEq1 ''ImportFrom
deriveShow1 ''ImportFrom

makeLenses ''Decorator
deriveEq1 ''Decorator
deriveShow1 ''Decorator

makeLenses ''ClassDef
deriveEq1 ''ClassDef
deriveShow1 ''ClassDef

makeLenses ''FuncDef
deriveEq1 ''FuncDef
deriveShow1 ''FuncDef

makeLenses ''AsyncFuncDef
deriveEq1 ''AsyncFuncDef
deriveShow1 ''AsyncFuncDef

makeLenses ''Suite
deriveEq1 ''Suite
deriveShow1 ''Suite

makeLenses ''Statement
deriveEq1 ''Statement
deriveShow1 ''Statement

makeLenses ''Parameters
deriveEq1 ''Parameters
deriveShow1 ''Parameters

makeLenses ''TypedArg
deriveEq1 ''TypedArg
deriveShow1 ''TypedArg

makeLenses ''WithItem
deriveEq1 ''WithItem
deriveShow1 ''WithItem

makeLenses ''WithStatement
deriveEq1 ''WithStatement
deriveShow1 ''WithStatement

makeLenses ''WhileStatement
deriveEq1 ''WhileStatement
deriveShow1 ''WhileStatement

makeLenses ''ForStatement
deriveEq1 ''ForStatement
deriveShow1 ''ForStatement

makeLenses ''ExceptClause
deriveEq1 ''ExceptClause
deriveShow1 ''ExceptClause

makeLenses ''SimpleStatement
deriveEq1 ''SimpleStatement
deriveShow1 ''SimpleStatement

makeLenses ''CompoundStatement
deriveEq1 ''CompoundStatement
deriveShow1 ''CompoundStatement

makeLenses ''IfStatement
deriveEq1 ''IfStatement
deriveShow1 ''IfStatement

makeLenses ''TryStatement
deriveEq1 ''TryStatement
deriveShow1 ''TryStatement

makeLenses ''Decorated
deriveEq1 ''Decorated
deriveShow1 ''Decorated

makeLenses ''AsyncStatement
deriveEq1 ''AsyncStatement
deriveShow1 ''AsyncStatement
