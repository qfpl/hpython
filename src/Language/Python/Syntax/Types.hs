{-# language DataKinds #-}
{-# language TemplateHaskell #-}
module Language.Python.Syntax.Types where

import Control.Lens.TH (makeLenses)
import Data.List.NonEmpty (NonEmpty)

import Language.Python.Internal.Syntax hiding (Fundef, While)

data Fundef v a
  = MkFundef
  { _fdAnn :: a
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
  } deriving (Eq, Show)
makeLenses ''Fundef

data While v a
  = MkWhile
  { _whileAnn :: a
  , _whileIndents :: Indents a
  , _whileWhile :: [Whitespace]
  , _whileCond :: Expr v a
  , _whileBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''While

data KeywordParam v a
  = MkKeywordParam
  { _kpAnn :: a
  , _kpName :: Ident v a
  , _kpType :: Maybe ([Whitespace], Expr v a)
  , _kpEquals :: [Whitespace]
  , _kpExpr :: Expr v a
  } deriving (Eq, Show)
makeLenses ''KeywordParam

data Call v a
  = MkCall
  { _callAnn :: a
  , _callFunction :: Expr v a
  , _callLeftParen :: [Whitespace]
  , _callArguments :: Maybe (CommaSep1' (Arg v a))
  , _callRightParen :: [Whitespace]
  } deriving (Eq, Show)
makeLenses ''Call

data Elif v a
  = MkElif
  { _elifIndents :: Indents a
  , _elifElif :: [Whitespace]
  , _elifCond :: Expr v a
  , _elifBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''Elif

data Else v a
  = MkElse
  { _elseIndents :: Indents a
  , _elseElse :: [Whitespace]
  , _elseBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''Else

data If v a
  = MkIf
  { _ifAnn :: a
  , _ifIndents :: Indents a
  , _ifIf :: [Whitespace]
  , _ifCond :: Expr v a
  , _ifBody :: Suite v a
  , _ifElifs :: [Elif v a]
  , _ifElse :: Maybe (Else v a)
  } deriving (Eq, Show)
makeLenses ''If

data For v a
  = MkFor
  { _forAnn :: a
  , _forIndents :: Indents a
  , _forAsync :: Maybe (NonEmpty Whitespace)
  , _forFor :: [Whitespace]
  , _forBinder :: Expr v a
  , _forIn :: [Whitespace]
  , _forCollection :: Expr v a
  , _forBody :: Suite v a
  , _forElse :: Maybe (Else v a)
  } deriving (Eq, Show)
makeLenses ''For

data Finally v a
  = MkFinally
  { _finallyIndents :: Indents a
  , _finallyFinally :: [Whitespace]
  , _finallyBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''Finally

data Except v a
  = MkExcept
  { _exceptIndents :: Indents a
  , _exceptExcept :: [Whitespace]
  , _exceptExceptAs :: Maybe (ExceptAs v a)
  , _exceptBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''Except

data TryExcept v a
  = MkTryExcept
  { _teAnn :: a
  , _teIndents :: Indents a
  , _teTry :: [Whitespace]
  , _teBody :: Suite v a
  , _teExcepts :: NonEmpty (Except v a)
  , _teElse :: Maybe (Else v a)
  , _teFinally :: Maybe (Finally v a)
  } deriving (Eq, Show)
makeLenses ''TryExcept

data TryFinally v a
  = MkTryFinally
  { _tfAnn :: a
  , _tfIndents :: Indents a
  , _tfTry :: [Whitespace]
  , _tfBody :: Suite v a
  , _tfFinally :: Finally v a
  } deriving (Eq, Show)
makeLenses ''TryFinally
