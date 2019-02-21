{-# language DataKinds #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Types
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Datatypes for different parts of Python syntax
-}

module Language.Python.Syntax.Types
  ( -- * Parameters
    -- ** Positional parameters
    PositionalParam(..)
    -- *** Lenses
  , ppAnn
  , ppName
  , ppType
    -- ** Starred Parameters
  , StarParam(..)
    -- *** Lenses
  , spAnn
  , spWhitespace
  , spName
  , spType
    -- ** Unnamed Starred Parameters
  , UnnamedStarParam(..)
    -- *** Lenses
  , uspAnn
  , uspWhitespace
    -- ** Keyword parameters
  , KeywordParam(..)
    -- *** Lenses
  , kpAnn
  , kpName
  , kpType
  , kpEquals
  , kpExpr
    -- * Compound statements
    -- ** Function definitions
  , Fundef(..)
    -- *** Lenses
  , fdAnn
  , fdDecorators
  , fdIndents
  , fdAsync
  , fdDefSpaces
  , fdName
  , fdLeftParenSpaces
  , fdParameters
  , fdRightParenSpaces
  , fdReturnType
  , fdBody
    -- ** Class definitions
  , ClassDef(..)
    -- *** Lenses
  , cdAnn
  , cdDecorators
  , cdIndents
  , cdClass
  , cdName
  , cdArguments
  , cdBody
    -- ** @if@ statements
  , If(..)
    -- *** Lenses
  , ifAnn
  , ifIndents
  , ifIf
  , ifCond
  , ifBody
  , ifElifs
  , ifElse
    -- ** @elif@
  , Elif(..)
    -- *** Lenses
  , elifIndents
  , elifElif
  , elifCond
  , elifBody
    -- ** @for@ statements
  , For(..)
    -- *** Lenses
  , forAnn
  , forIndents
  , forAsync
  , forFor
  , forBinder
  , forIn
  , forCollection
  , forBody
  , forElse
    -- ** @while@ statements
  , While(..)
    -- *** Lenses
  , whileAnn
  , whileIndents
  , whileWhile
  , whileCond
  , whileBody
  , whileElse
    -- ** @try ... except ... else ... finally@
  , TryExcept(..)
    -- *** Lenses
  , teAnn
  , teIndents
  , teTry
  , teBody
  , teExcepts
  , teElse
  , teFinally
    -- *** @except@
  , Except(..)
    -- **** Lenses
  , exceptIndents
  , exceptExcept
  , exceptExceptAs
  , exceptBody
    -- ** @try ... finally@
  , TryFinally(..)
    -- *** Lenses
  , tfAnn
  , tfIndents
  , tfTry
  , tfBody
  , tfFinally
    -- ** @finally@
  , Finally(..)
    -- *** Lenses
  , finallyIndents
  , finallyFinally
  , finallyBody
    -- ** @with@ statements
  , With(..)
    -- *** Lenses
  , withAnn
  , withIndents
  , withAsync
  , withWith
  , withItems
  , withBody
    -- ** @else@
  , Else(..)
    -- *** Lenses
  , elseIndents
  , elseElse
  , elseBody
    -- * Simple statements
    -- ** @import@
  , Import(..)
    -- *** Lenses
  , importAnn
  , importWhitespace
  , importNames
    -- ** @from ... import@
  , FromImport(..)
    -- *** Lenses
  , fiAnn
  , fiFrom
  , fiFromName
  , fiImport
  , fiTargets
    -- ** Assignment
  , Assign(..)
  , unfoldAssign
    -- *** Lenses
  , assignAnn
  , assignTarget
  , assignRest
    -- * Expressions
    -- ** @None@
  , None(..)
    -- *** Lenses
  , noneAnn
  , noneWhitespace
    -- ** Function calls
  , Call(..)
    -- *** Lenses
  , callAnn
  , callFunction
  , callLeftParen
  , callArguments
  , callRightParen
    -- ** Tuples
  , Tuple(..)
    -- *** Lenses
  , tupleAnn
  , tupleHead
  , tupleComma
  , tupleTail
    -- *** Tuple items
    -- **** Unpacking
  , TupleUnpack(..)
    -- ***** Lenses
  , tupleUnpackAnn
  , tupleUnpackParens
  , tupleUnpackWhitespace
  , tupleUnpackValue
    -- ** Lists
  , List(..)
    -- *** Lenses
  , listAnn
  , listWhitespaceLeft
  , listBody
  , listWhitespaceRight
    -- *** List items
    -- **** Unpacking
  , ListUnpack(..)
    -- ***** Lenses
  , listUnpackAnn
  , listUnpackParens
  , listUnpackWhitespace
  , listUnpackValue
  )
where

import Control.Lens.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Data.Bifunctor (first)
import Data.Generics.Product.Typed (typed)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic)

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep (Comma, CommaSep, CommaSep1, CommaSep1')
import Language.Python.Syntax.Expr (Arg, Expr, ListItem, Param, TupleItem)
import Language.Python.Syntax.Ident (Ident)
import Language.Python.Syntax.Import (ImportAs, ImportTargets)
import Language.Python.Syntax.ModuleNames (ModuleName, RelativeModuleName)
import Language.Python.Syntax.Punctuation (Colon, Equals)
import Language.Python.Syntax.Statement (Decorator, ExceptAs, Suite, WithItem)
import Language.Python.Syntax.Whitespace

data Fundef v a
  = MkFundef
  { _fdAnn :: Ann a
  , _fdDecorators :: [Decorator v a]
  , _fdIndents :: Indents a
  , _fdAsync :: Maybe (NonEmpty Whitespace)
  , _fdDefSpaces :: NonEmpty Whitespace
  , _fdName :: Ident v a
  , _fdLeftParenSpaces :: [Whitespace]
  , _fdParameters :: CommaSep (Param v a)
  , _fdRightParenSpaces :: [Whitespace]
  , _fdReturnType :: Maybe ([Whitespace], Expr v a)
  , _fdBody :: Suite v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Fundef

instance HasAnn (Fundef v) where
  annot :: forall a. Lens' (Fundef v a) (Ann a)
  annot = typed @(Ann a)

data Else v a
  = MkElse
  { _elseIndents :: Indents a
  , _elseElse :: [Whitespace]
  , _elseBody :: Suite v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Else

data While v a
  = MkWhile
  { _whileAnn :: Ann a
  , _whileIndents :: Indents a
  , _whileWhile :: [Whitespace]
  , _whileCond :: Expr v a
  , _whileBody :: Suite v a
  , _whileElse :: Maybe (Else v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''While

instance HasAnn (While v) where
  annot :: forall a. Lens' (While v a) (Ann a)
  annot = typed @(Ann a)

data KeywordParam v a
  = MkKeywordParam
  { _kpAnn :: Ann a
  , _kpName :: Ident v a
  , _kpType :: Maybe (Colon, Expr v a)
  , _kpEquals :: [Whitespace]
  , _kpExpr :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''KeywordParam

instance HasAnn (KeywordParam v) where
  annot :: forall a. Lens' (KeywordParam v a) (Ann a)
  annot = typed @(Ann a)

data PositionalParam v a
  = MkPositionalParam
  { _ppAnn :: Ann a
  , _ppName :: Ident v a
  , _ppType :: Maybe (Colon, Expr v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PositionalParam

instance HasAnn (PositionalParam v) where
  annot :: forall a. Lens' (PositionalParam v a) (Ann a)
  annot = typed @(Ann a)

data StarParam v a
  = MkStarParam
  { _spAnn :: Ann a
  , _spWhitespace :: [Whitespace]
  , _spName :: Ident v a
  , _spType :: Maybe (Colon, Expr v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''StarParam

instance HasAnn (StarParam v) where
  annot :: forall a. Lens' (StarParam v a) (Ann a)
  annot = typed @(Ann a)

data UnnamedStarParam (v :: [*]) a
  = MkUnnamedStarParam
  { _uspAnn :: Ann a
  , _uspWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''UnnamedStarParam

instance HasAnn (UnnamedStarParam v) where
  annot :: forall a. Lens' (UnnamedStarParam v a) (Ann a)
  annot = typed @(Ann a)

data Call v a
  = MkCall
  { _callAnn :: Ann a
  , _callFunction :: Expr v a
  , _callLeftParen :: [Whitespace]
  , _callArguments :: Maybe (CommaSep1' (Arg v a))
  , _callRightParen :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Call

instance HasAnn (Call v) where
  annot :: forall a. Lens' (Call v a) (Ann a)
  annot = typed @(Ann a)

data Elif v a
  = MkElif
  { _elifIndents :: Indents a
  , _elifElif :: [Whitespace]
  , _elifCond :: Expr v a
  , _elifBody :: Suite v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Elif

data If v a
  = MkIf
  { _ifAnn :: Ann a
  , _ifIndents :: Indents a
  , _ifIf :: [Whitespace]
  , _ifCond :: Expr v a
  , _ifBody :: Suite v a
  , _ifElifs :: [Elif v a]
  , _ifElse :: Maybe (Else v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''If

instance HasAnn (If v) where
  annot :: forall a. Lens' (If v a) (Ann a)
  annot = typed @(Ann a)

data For v a
  = MkFor
  { _forAnn :: Ann a
  , _forIndents :: Indents a
  , _forAsync :: Maybe (NonEmpty Whitespace)
  , _forFor :: [Whitespace]
  , _forBinder :: Expr v a
  , _forIn :: [Whitespace]
  , _forCollection :: CommaSep1' (Expr v a)
  , _forBody :: Suite v a
  , _forElse :: Maybe (Else v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''For

instance HasAnn (For v) where
  annot :: forall a. Lens' (For v a) (Ann a)
  annot = typed @(Ann a)

data Finally v a
  = MkFinally
  { _finallyIndents :: Indents a
  , _finallyFinally :: [Whitespace]
  , _finallyBody :: Suite v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Finally

data Except v a
  = MkExcept
  { _exceptIndents :: Indents a
  , _exceptExcept :: [Whitespace]
  , _exceptExceptAs :: Maybe (ExceptAs v a)
  , _exceptBody :: Suite v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Except

data TryExcept v a
  = MkTryExcept
  { _teAnn :: Ann a
  , _teIndents :: Indents a
  , _teTry :: [Whitespace]
  , _teBody :: Suite v a
  , _teExcepts :: NonEmpty (Except v a)
  , _teElse :: Maybe (Else v a)
  , _teFinally :: Maybe (Finally v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''TryExcept

instance HasAnn (TryExcept v) where
  annot :: forall a. Lens' (TryExcept v a) (Ann a)
  annot = typed @(Ann a)

data TryFinally v a
  = MkTryFinally
  { _tfAnn :: Ann a
  , _tfIndents :: Indents a
  , _tfTry :: [Whitespace]
  , _tfBody :: Suite v a
  , _tfFinally :: Finally v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''TryFinally

instance HasAnn (TryFinally v) where
  annot :: forall a. Lens' (TryFinally v a) (Ann a)
  annot = typed @(Ann a)

data ClassDef v a
  = MkClassDef
  { _cdAnn :: Ann a
  , _cdDecorators :: [Decorator v a]
  , _cdIndents :: Indents a
  , _cdClass :: NonEmpty Whitespace
  , _cdName :: Ident v a
  , _cdArguments :: Maybe ([Whitespace], Maybe (CommaSep1' (Arg v a)), [Whitespace])
  , _cdBody :: Suite v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''ClassDef

instance HasAnn (ClassDef v) where
  annot :: forall a. Lens' (ClassDef v a) (Ann a)
  annot = typed @(Ann a)

data With v a
  = MkWith
  { _withAnn :: Ann a
  , _withIndents :: Indents a
  , _withAsync :: Maybe (NonEmpty Whitespace)
  , _withWith :: [Whitespace]
  , _withItems :: CommaSep1 (WithItem v a)
  , _withBody :: Suite v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''With

instance HasAnn (With v) where
  annot :: forall a. Lens' (With v a) (Ann a)
  annot = typed @(Ann a)

data Tuple v a
  = MkTuple
  { _tupleAnn :: Ann a
  , _tupleHead :: TupleItem v a
  , _tupleComma :: Comma
  , _tupleTail :: Maybe (CommaSep1' (TupleItem v a))
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Tuple

instance HasAnn (Tuple v) where
  annot :: forall a. Lens' (Tuple v a) (Ann a)
  annot = typed @(Ann a)

data List v a
  = MkList
  { _listAnn :: Ann a
  , _listWhitespaceLeft :: [Whitespace]
  , _listBody :: Maybe (CommaSep1' (ListItem v a))
  , _listWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''List

instance HasAnn (List v) where
  annot :: forall a. Lens' (List v a) (Ann a)
  annot = typed @(Ann a)

data ListUnpack v a
  = MkListUnpack
  { _listUnpackAnn :: Ann a
  , _listUnpackParens :: [([Whitespace], [Whitespace])]
  , _listUnpackWhitespace :: [Whitespace]
  , _listUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''ListUnpack

instance HasAnn (ListUnpack v) where
  annot :: forall a. Lens' (ListUnpack v a) (Ann a)
  annot = typed @(Ann a)

data None (v :: [*]) a
  = MkNone
  { _noneAnn :: Ann a
  , _noneWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''None

instance HasAnn (None v) where
  annot :: forall a. Lens' (None v a) (Ann a)
  annot = typed @(Ann a)

data TupleUnpack v a
  = MkTupleUnpack
  { _tupleUnpackAnn :: Ann a
  , _tupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _tupleUnpackWhitespace :: [Whitespace]
  , _tupleUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''TupleUnpack

instance HasAnn (TupleUnpack v) where
  annot :: forall a. Lens' (TupleUnpack v a) (Ann a)
  annot = typed @(Ann a)

data Import v a
  = MkImport
  { _importAnn :: Ann a
  , _importWhitespace :: NonEmpty Whitespace
  , _importNames :: CommaSep1 (ImportAs ModuleName v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Import

instance HasAnn (Import v) where
  annot :: forall a. Lens' (Import v a) (Ann a)
  annot = typed @(Ann a)

data FromImport v a
  = MkFromImport
  { _fiAnn :: Ann a
  , _fiFrom :: [Whitespace]
  , _fiFromName :: RelativeModuleName v a
  , _fiImport :: [Whitespace]
  , _fiTargets :: ImportTargets v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''FromImport

instance HasAnn (FromImport v) where
  annot :: forall a. Lens' (FromImport v a) (Ann a)
  annot = typed @(Ann a)

data Assign v a
  = MkAssign
  { _assignAnn :: Ann a
  , _assignTarget :: Expr v a
  , _assignRest :: NonEmpty (Equals, Expr v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Assign

instance HasAnn (Assign v) where
  annot :: forall a. Lens' (Assign v a) (Ann a)
  annot = typed @(Ann a)

-- | Unfold an assignment into a 'NonEmpty' sequence of l-values and an r-value
unfoldAssign :: Assign v a -> (NonEmpty (Expr v a), Expr v a)
unfoldAssign (MkAssign _ a bs) = go a bs
  where
    go a ((_, x) :| []) = (pure a, x)
    go a ((_, x) :| y:ys) = first (a `NonEmpty.cons`) $ go x (y :| ys)
