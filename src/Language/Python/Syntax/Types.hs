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
  }
  deriving (Eq, Show)
makeLenses ''Fundef

data While v a
  = MkWhile
  { _whileAnn :: a
  , _whileIndents :: Indents a
  , _whileWhile :: [Whitespace]
  , _whileCond :: Expr v a
  , _whileBody :: Suite v a
  }
  deriving (Eq, Show)
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
