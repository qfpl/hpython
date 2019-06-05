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
import Data.Generics.Product.Typed (typed)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep (Comma, CommaSep, CommaSep1, CommaSep1')
import Language.Python.Syntax.Expr (Arg, Expr, ListItem, Param, TupleItem)
import Language.Python.Syntax.Ident (Ident)
import Language.Python.Syntax.Punctuation (Colon)
import Language.Python.Syntax.Statement (Decorator, ExceptAs, Suite, WithItem)
import Language.Python.Syntax.Whitespace

data Fundef a
  = MkFundef
  { _fdAnn :: Ann a
  , _fdDecorators :: [Decorator a]
  , _fdIndents :: Indents a
  , _fdAsync :: Maybe (NonEmpty Whitespace)
  , _fdDefSpaces :: NonEmpty Whitespace
  , _fdName :: Ident a
  , _fdLeftParenSpaces :: [Whitespace]
  , _fdParameters :: CommaSep (Param a)
  , _fdRightParenSpaces :: [Whitespace]
  , _fdReturnType :: Maybe ([Whitespace], Expr a)
  , _fdBody :: Suite a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Fundef

instance HasAnn Fundef where
  annot :: forall a. Lens' (Fundef a) (Ann a)
  annot = typed @(Ann a)

data Else a
  = MkElse
  { _elseIndents :: Indents a
  , _elseElse :: [Whitespace]
  , _elseBody :: Suite a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Else

data While a
  = MkWhile
  { _whileAnn :: Ann a
  , _whileIndents :: Indents a
  , _whileWhile :: [Whitespace]
  , _whileCond :: Expr a
  , _whileBody :: Suite a
  , _whileElse :: Maybe (Else a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''While

instance HasAnn While where
  annot :: forall a. Lens' (While a) (Ann a)
  annot = typed @(Ann a)

data KeywordParam a
  = MkKeywordParam
  { _kpAnn :: Ann a
  , _kpName :: Ident a
  , _kpType :: Maybe (Colon, Expr a)
  , _kpEquals :: [Whitespace]
  , _kpExpr :: Expr a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''KeywordParam

instance HasAnn KeywordParam where
  annot :: forall a. Lens' (KeywordParam a) (Ann a)
  annot = typed @(Ann a)

data PositionalParam a
  = MkPositionalParam
  { _ppAnn :: Ann a
  , _ppName :: Ident a
  , _ppType :: Maybe (Colon, Expr a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PositionalParam

instance HasAnn PositionalParam where
  annot :: forall a. Lens' (PositionalParam a) (Ann a)
  annot = typed @(Ann a)

data StarParam a
  = MkStarParam
  { _spAnn :: Ann a
  , _spWhitespace :: [Whitespace]
  , _spName :: Ident a
  , _spType :: Maybe (Colon, Expr a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''StarParam

instance HasAnn StarParam where
  annot :: forall a. Lens' (StarParam a) (Ann a)
  annot = typed @(Ann a)

data UnnamedStarParam a
  = MkUnnamedStarParam
  { _uspAnn :: Ann a
  , _uspWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''UnnamedStarParam

instance HasAnn UnnamedStarParam where
  annot :: forall a. Lens' (UnnamedStarParam a) (Ann a)
  annot = typed @(Ann a)

data Call a
  = MkCall
  { _callAnn :: Ann a
  , _callFunction :: Expr a
  , _callLeftParen :: [Whitespace]
  , _callArguments :: Maybe (CommaSep1' (Arg a))
  , _callRightParen :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Call

instance HasAnn Call where
  annot :: forall a. Lens' (Call a) (Ann a)
  annot = typed @(Ann a)

data Elif a
  = MkElif
  { _elifIndents :: Indents a
  , _elifElif :: [Whitespace]
  , _elifCond :: Expr a
  , _elifBody :: Suite a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Elif

data If a
  = MkIf
  { _ifAnn :: Ann a
  , _ifIndents :: Indents a
  , _ifIf :: [Whitespace]
  , _ifCond :: Expr a
  , _ifBody :: Suite a
  , _ifElifs :: [Elif a]
  , _ifElse :: Maybe (Else a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''If

instance HasAnn If where
  annot :: forall a. Lens' (If a) (Ann a)
  annot = typed @(Ann a)

data For a
  = MkFor
  { _forAnn :: Ann a
  , _forIndents :: Indents a
  , _forAsync :: Maybe (NonEmpty Whitespace)
  , _forFor :: [Whitespace]
  , _forBinder :: Expr a
  , _forIn :: [Whitespace]
  , _forCollection :: CommaSep1' (Expr a)
  , _forBody :: Suite a
  , _forElse :: Maybe (Else a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''For

instance HasAnn For where
  annot :: forall a. Lens' (For a) (Ann a)
  annot = typed @(Ann a)

data Finally a
  = MkFinally
  { _finallyIndents :: Indents a
  , _finallyFinally :: [Whitespace]
  , _finallyBody :: Suite a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Finally

data Except a
  = MkExcept
  { _exceptIndents :: Indents a
  , _exceptExcept :: [Whitespace]
  , _exceptExceptAs :: Maybe (ExceptAs a)
  , _exceptBody :: Suite a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Except

data TryExcept a
  = MkTryExcept
  { _teAnn :: Ann a
  , _teIndents :: Indents a
  , _teTry :: [Whitespace]
  , _teBody :: Suite a
  , _teExcepts :: NonEmpty (Except a)
  , _teElse :: Maybe (Else a)
  , _teFinally :: Maybe (Finally a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''TryExcept

instance HasAnn TryExcept where
  annot :: forall a. Lens' (TryExcept a) (Ann a)
  annot = typed @(Ann a)

data TryFinally a
  = MkTryFinally
  { _tfAnn :: Ann a
  , _tfIndents :: Indents a
  , _tfTry :: [Whitespace]
  , _tfBody :: Suite a
  , _tfFinally :: Finally a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''TryFinally

instance HasAnn TryFinally where
  annot :: forall a. Lens' (TryFinally a) (Ann a)
  annot = typed @(Ann a)

data ClassDef a
  = MkClassDef
  { _cdAnn :: Ann a
  , _cdDecorators :: [Decorator a]
  , _cdIndents :: Indents a
  , _cdClass :: NonEmpty Whitespace
  , _cdName :: Ident a
  , _cdArguments :: Maybe ([Whitespace], Maybe (CommaSep1' (Arg a)), [Whitespace])
  , _cdBody :: Suite a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''ClassDef

instance HasAnn ClassDef where
  annot :: forall a. Lens' (ClassDef a) (Ann a)
  annot = typed @(Ann a)

data With a
  = MkWith
  { _withAnn :: Ann a
  , _withIndents :: Indents a
  , _withAsync :: Maybe (NonEmpty Whitespace)
  , _withWith :: [Whitespace]
  , _withItems :: CommaSep1 (WithItem a)
  , _withBody :: Suite a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''With

instance HasAnn With where
  annot :: forall a. Lens' (With a) (Ann a)
  annot = typed @(Ann a)

data Tuple a
  = MkTuple
  { _tupleAnn :: Ann a
  , _tupleHead :: TupleItem a
  , _tupleComma :: Comma
  , _tupleTail :: Maybe (CommaSep1' (TupleItem a))
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Tuple

instance HasAnn Tuple where
  annot :: forall a. Lens' (Tuple a) (Ann a)
  annot = typed @(Ann a)

data List a
  = MkList
  { _listAnn :: Ann a
  , _listWhitespaceLeft :: [Whitespace]
  , _listBody :: Maybe (CommaSep1' (ListItem a))
  , _listWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''List

instance HasAnn List where
  annot :: forall a. Lens' (List a) (Ann a)
  annot = typed @(Ann a)

data ListUnpack a
  = MkListUnpack
  { _listUnpackAnn :: Ann a
  , _listUnpackParens :: [([Whitespace], [Whitespace])]
  , _listUnpackWhitespace :: [Whitespace]
  , _listUnpackValue :: Expr a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''ListUnpack

instance HasAnn ListUnpack where
  annot :: forall a. Lens' (ListUnpack a) (Ann a)
  annot = typed @(Ann a)

data None a
  = MkNone
  { _noneAnn :: Ann a
  , _noneWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''None

instance HasAnn None where
  annot :: forall a. Lens' (None a) (Ann a)
  annot = typed @(Ann a)

data TupleUnpack a
  = MkTupleUnpack
  { _tupleUnpackAnn :: Ann a
  , _tupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _tupleUnpackWhitespace :: [Whitespace]
  , _tupleUnpackValue :: Expr a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''TupleUnpack

instance HasAnn TupleUnpack where
  annot :: forall a. Lens' (TupleUnpack a) (Ann a)
  annot = typed @(Ann a)
