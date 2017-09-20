{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.Statement.IR where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Identifier
import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.Expr.IR

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
  | CompoundStatementFuncdef
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
deriving instance Show a => Show (CompoundStatement a)

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
           Test)
         a
  , _funcDef_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _funcDef_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (CompoundStatement a)
deriving instance Show a => Show (CompoundStatement a)

data Parameters a
  = Parameters
  { _parameters_value
    :: Compose
         (Between' [WhitespaceChar])
         (Compose
           Maybe
           TypedArgsList)
         a
  }

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
deriving instance Show a => Show (WithStatement a)

data WithItem a
  = WithItem
  { _withItem_left :: Test a
  , _withItem_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' (NonEmpty WhitespaceChar) KAs))
           Identifier)
         a
  , _withItem_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (WithItem a)
deriving instance Show a => Show (WithItem a)

data AsyncStatement a
  = AsyncStatement
  { _asyncStatement_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         (Sum
           (Sum FuncDef WhileStatement)
           ForStatement)
  , _asyncStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (AsyncStatement a)
deriving instance Show a => Show (AsyncStatement a)

data IfStatement a
  = IfStatement
  { _ifStatement_cond
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         Test
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
             Test)
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
deriving instance Show a => Show (IfStatement a)

data WhileStatement a
  = WhileStatement
  { _whileStatement_cond
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         Test
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
deriving instance Show a => Show (WhileStatement a)

data ForStatement a
  = ForStatement
  { _forStatement_for
    :: Compose
         (Between' (NonEmpty WhitespaceChar))
         ExprList
         a
  , _forStatement_in
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         TestList
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
deriving instance Show a => Show (ForStatement a)

data TryStatement a
  = TryStatementExcepts
  { _tryStatement_body
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
           (Before (Between [WhitespaceChar] Colon))
           Suite)
  , _tryStatement_ann :: a
  }
  | TryStatementFinally
  { _tryStatement_body
    :: Compose
         (Before (Between' [WhitespaceChar] Colon))
         Suite
         a
  , _tryStatementFinally_finally
    :: Compose
         (Before (Between [WhitespaceChar] Colon))
         Suite
  , _tryStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (TryStatement a)
deriving instance Show a => Show (TryStatement a)

data ExceptClause a
  = ExceptClause
  { _exceptClause_value
    :: Compose
         Maybe
         (Product
           Test
           (Compose
             Maybe
             (Compose
               (Between' (NonEmpty WhitespaceChar) KAs
               Identifier))))
         a
  , _exceptClause_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (ExceptClause a)
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
  , _simpleStatement_newline :: Before [WhitespaceChar] NewlineChar
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SimpleStatement a)
deriving instance Show a => Show (SimpleStatement a)

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

data SmallStatement a
  = SmallStatementExpr
  { _smallStatementExpr_left :: TestlistStarExpr a
  , _smallStatementExpr_right
    :: Sum
         (Compose
           (Before (Between' [WhitespaceChar] AugAssign))
           (Sum YieldExpr TestList))
         (Compose
           []
           (Compose
             (Before (Between' [WhitespaceChar] Equals))
             (Sum YieldExpr TestlistStarExpr)))
         a
  , _smallStatement_ann :: a
  }
  | SmallStatementDel
  { _smallStatementDel_value
    :: Compose
         (Before (NonEmpty WhitespaceChar))
         ExprList
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
  { _smallStatementAssert_head :: Test a
  , _smallStatementAssert_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           Test)
         a
  , _smallStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (SmallStatement a)
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
           TestList)
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
  { _flowStatementYield_value :: YieldExpr a
  , _flowStatement_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance Eq a => Eq (FlowStatement a)
deriving instance Show a => Show (FlowStatement a)

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

data RaiseStatement a
  = RaiseStatement
  { _raiseStatement_left :: Test a
  , _raiseStatement_right
    :: Compose
         Maybe
         (Compose
           (Before (NonEmpty WhitespaceChar))
           Test)
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
    :: Sum
         (Before (NonEmpty WhitespaceChar))
         (Compose
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
           (Before (Between' [WhitespaceChar] KAs))
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

data TestlistStarExpr a
  = TestlistStarExpr
  { _testlistStarExpr_head
    :: Sum Test StarExpr a
  , _testlistStarExpr_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum Test StarExpr))
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
