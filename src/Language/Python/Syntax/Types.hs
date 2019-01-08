{-# language DataKinds #-}
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
    -- ** Parenthesised expressions
  , Parens(..)
    -- *** Lenses
  , parensAnn
  , parensWhitespaceLeft
  , parensValue
  , parensWhitespaceAfter
    -- ** @()@
  , Unit(..)
    -- *** Lenses
  , unitAnn
  , unitWhitespaceInner
  , unitWhitespaceRight
    -- ** @None@
  , None(..)
    -- *** Lenses
  , noneAnn
  , noneWhitespace
    -- ** @Ellipsis@
  , Ellipsis(..)
    -- *** Lenses
  , ellipsisAnn
  , ellipsisWhitespace
    -- ** Booleans
  , PyBool(..)
    -- *** Lenses
  , boolAnn
  , boolValue
  , boolWhitespace
    -- ** Integers
  , PyInt(..)
    -- *** Lenses
  , intAnn
  , intValue
  , intWhitespace
    -- ** Floats
  , PyFloat(..)
    -- *** Lenses
  , floatAnn
  , floatValue
  , floatWhitespace
    -- ** Imaginary literals
  , Imag(..)
    -- *** Lenses
  , imagAnn
  , imagValue
  , imagWhitespace
    -- ** Strings
  , PyString(..)
    -- *** Lenses
  , stringAnn
  , stringValue
    -- ** @lambda@
  , Lambda(..)
    -- *** Lenses
  , lambdaAnn
  , lambdaWhitespace
  , lambdaArgs
  , lambdaColon
  , lambdaBody
    -- ** @yield@
  , Yield(..)
    -- *** Lenses
  , yieldAnn
  , yieldWhitespace
  , yieldValue
    -- ** @yield ... from ... @
  , YieldFrom(..)
  , yfAnn
  , yfYieldWhitespace
  , yfFromWhitespace
  , yfValue
    -- ** @not@
  , Not(..)
    -- *** Lenses
  , notAnn
  , notWhitespace
  , notValue
    -- ** Unary operators
  , Unary(..)
    -- *** Lenses
  , unaryAnn
  , unaryOp
  , unaryValue
    -- ** Binary operators
  , Binary(..)
    -- *** Lenses
  , binaryAnn
  , binaryExprLeft
  , binaryOp
  , binaryRight
    -- ** Ternary expression
  , Ternary(..)
    -- *** Lenses
  , ternaryAnn
  , ternaryValue
  , ternaryIfWhitespace
  , ternaryCond
  , ternaryElseWhitespace
  , ternaryElse
    -- ** Function calls
  , Call(..)
    -- *** Lenses
  , callAnn
  , callFunction
  , callLeftParen
  , callArguments
  , callRightParen
    -- ** Dereferencing
  , Deref(..)
    -- *** Lenses
  , derefAnn
  , derefValueLeft
  , derefWhitespaceLeft
  , derefValueRight
    -- ** Subscripting
  , Subscript(..)
    -- *** Lenses
  , subscriptAnn
  , subscriptValueLeft
  , subscriptWhitespaceLeft
  , subscriptValueRight
  , subscriptWhitespaceRight
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
    -- ** List Comprehensions
  , ListComp(..)
  , lcAnn
  , lcWhitespaceLeft
  , lcValue
  , lcWhitespaceRight
    -- *** List items
    -- **** Unpacking
  , ListUnpack(..)
    -- ***** Lenses
  , listUnpackAnn
  , listUnpackParens
  , listUnpackWhitespace
  , listUnpackValue
    -- ** Dictionaries
  , Dict(..)
  , dictAnn
  , dictWhitespaceLeft
  , dictValues
  , dictWhitespaceRight
    -- ** Dictionary Comprehensions
  , DictComp(..)
    -- *** Lenses
  , dcAnn
  , dcWhitespaceLeft
  , dcValue
  , dcWhitespaceRight
    -- ** Sets
  , Set(..)
    -- *** Lenses
  , setAnn
  , setWhitespaceLeft
  , setValues
  , setWhitespaceRight
    -- ** Set Comprehensions
  , SetComp(..)
    -- *** Lenses
  , scAnn
  , scWhitespaceLeft
  , scValue
  , scWhitespaceRight
    -- ** Generators
  , Generator(..)
    -- *** Lenses
  , generatorAnn
  , generatorValue
    -- ** @await@
  , Await(..)
    -- *** Lenses
  , awaitAnn
  , awaitWhitespace
  , awaitValue
  )
where

import Control.Lens.TH (makeLenses)
import Data.List.NonEmpty (NonEmpty)

import Data.VIdentity
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace

data Fundef v a
  = MkFundef
  { _fdAnn :: a
  , _fdDecorators :: [Decorator v a]
  , _fdIndents :: Indents a
  , _fdAsync :: Maybe (NonEmpty Whitespace)
  , _fdDefSpaces :: NonEmpty Whitespace
  , _fdName :: Ident v a
  , _fdLeftParenSpaces :: [Whitespace]
  , _fdParameters :: CommaSep (Param Expr v a)
  , _fdRightParenSpaces :: [Whitespace]
  , _fdReturnType :: Maybe ([Whitespace], Expr v a)
  , _fdBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''Fundef

data Else v a
  = MkElse
  { _elseIndents :: Indents a
  , _elseElse :: [Whitespace]
  , _elseBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''Else

data While v a
  = MkWhile
  { _whileAnn :: a
  , _whileIndents :: Indents a
  , _whileWhile :: [Whitespace]
  , _whileCond :: Expr v a
  , _whileBody :: Suite v a
  , _whileElse :: Maybe (Else v a)
  } deriving (Eq, Show)
makeLenses ''While

data KeywordParam expr v a
  = MkKeywordParam
  { _kpAnn :: a
  , _kpName :: Ident v a
  , _kpType :: Maybe (Colon, expr v a)
  , _kpEquals :: [Whitespace]
  , _kpExpr :: expr v a
  } deriving (Eq, Show)
makeLenses ''KeywordParam

data PositionalParam expr v a
  = MkPositionalParam
  { _ppAnn :: a
  , _ppName :: Ident v a
  , _ppType :: Maybe (Colon, expr v a)
  } deriving (Eq, Show)
makeLenses ''PositionalParam

data StarParam expr v a
  = MkStarParam
  { _spAnn :: a
  , _spWhitespace :: [Whitespace]
  , _spName :: Ident v a
  , _spType :: Maybe (Colon, expr v a)
  } deriving (Eq, Show)
makeLenses ''StarParam

data UnnamedStarParam (v :: [*]) a
  = MkUnnamedStarParam
  { _uspAnn :: a
  , _uspWhitespace :: [Whitespace]
  } deriving (Eq, Show)
makeLenses ''UnnamedStarParam

data Call expr v a
  = MkCall
  { _callAnn :: a
  , _callFunction :: expr v a
  , _callLeftParen :: [Whitespace]
  , _callArguments :: Maybe (CommaSep1' (Arg expr v a))
  , _callRightParen :: [Whitespace]
  } deriving (Eq, Show)
makeLenses ''Call

data Lambda expr v a
  = MkLambda
  { _lambdaAnn :: a
  , _lambdaWhitespace :: [Whitespace]
  , _lambdaArgs :: CommaSep (Param expr v a)
  , _lambdaColon :: Colon
  , _lambdaBody :: expr v a
  }
makeLenses ''Lambda

data Unit (v :: [*]) a
  = MkUnit
  { _unitAnn :: a
  , _unitWhitespaceInner :: [Whitespace]
  , _unitWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show)
makeLenses ''Unit

data Yield expr (v :: [*]) (a :: *)
  = MkYield
  { _yieldAnn :: a
  , _yieldWhitespace :: [Whitespace]
  , _yieldValue :: CommaSep (expr v a)
  }
makeLenses ''Yield

data Ternary expr (v :: [*]) (a :: *)
  = MkTernary
  { _ternaryAnn :: a
  , _ternaryValue :: expr v a
  , _ternaryIfWhitespace :: [Whitespace]
  , _ternaryCond :: expr v a
  , _ternaryElseWhitespace :: [Whitespace]
  , _ternaryElse :: expr v a
  }
makeLenses ''Ternary

data YieldFrom expr (v :: [*]) (a :: *)
  = MkYieldFrom
  { _yfAnn :: a
  , _yfYieldWhitespace :: [Whitespace]
  , _yfFromWhitespace :: [Whitespace]
  , _yfValue :: expr v a
  }
makeLenses ''YieldFrom

data Elif v a
  = MkElif
  { _elifIndents :: Indents a
  , _elifElif :: [Whitespace]
  , _elifCond :: Expr v a
  , _elifBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''Elif

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
  , _forCollection :: CommaSep1' (Expr v a)
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

data ClassDef v a
  = MkClassDef
  { _cdAnn :: a
  , _cdDecorators :: [Decorator v a]
  , _cdIndents :: Indents a
  , _cdClass :: NonEmpty Whitespace
  , _cdName :: Ident v a
  , _cdArguments :: Maybe ([Whitespace], Maybe (CommaSep1' (Arg Expr v a)), [Whitespace])
  , _cdBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''ClassDef

data With v a
  = MkWith
  { _withAnn :: a
  , _withIndents :: Indents a
  , _withAsync :: Maybe (NonEmpty Whitespace)
  , _withWith :: [Whitespace]
  , _withItems :: CommaSep1 (WithItem v a)
  , _withBody :: Suite v a
  } deriving (Eq, Show)
makeLenses ''With

data Tuple expr v a
  = MkTuple
  { _tupleAnn :: a
  , _tupleHead :: TupleItem expr v a
  , _tupleComma :: Comma
  , _tupleTail :: Maybe (CommaSep1' (TupleItem expr v a))
  } deriving (Eq, Show)
makeLenses ''Tuple

data List expr v a
  = MkList
  { _listAnn :: a
  , _listWhitespaceLeft :: [Whitespace]
  , _listBody :: Maybe (CommaSep1' (ListItem expr v a))
  , _listWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show)
makeLenses ''List

data ListComp expr v a
  = MkListComp
  { _lcAnn :: a
  , _lcWhitespaceLeft :: [Whitespace]
  , _lcValue :: Comprehension VIdentity expr v a
  , _lcWhitespaceRight :: [Whitespace]
  }
makeLenses ''ListComp

data ListUnpack expr (v :: [*]) (a :: *)
  = MkListUnpack
  { _listUnpackAnn :: a
  , _listUnpackParens :: [([Whitespace], [Whitespace])]
  , _listUnpackWhitespace :: [Whitespace]
  , _listUnpackValue :: expr v a
  } deriving (Eq, Show)
makeLenses ''ListUnpack

data None (v :: [*]) a
  = MkNone
  { _noneAnn :: a
  , _noneWhitespace :: [Whitespace]
  } deriving (Eq, Show)
makeLenses ''None

data TupleUnpack expr (v :: [*]) (a :: *)
  = MkTupleUnpack
  { _tupleUnpackAnn :: a
  , _tupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _tupleUnpackWhitespace :: [Whitespace]
  , _tupleUnpackValue :: expr v a
  } deriving (Eq, Show)
makeLenses ''TupleUnpack

data DictComp expr v a
  = MkDictComp
  { _dcAnn :: a
  , _dcWhitespaceLeft :: [Whitespace]
  , _dcValue :: Comprehension DictItem expr v a
  , _dcWhitespaceRight :: [Whitespace]
  }
makeLenses ''DictComp

data Dict expr v a
  = MkDict
  { _dictAnn :: a
  , _dictWhitespaceLeft :: [Whitespace]
  , _dictValues :: Maybe (CommaSep1' (DictItem expr v a))
  , _dictWhitespaceRight :: [Whitespace]
  }
makeLenses ''Dict

data SetComp expr v a
  = MkSetComp
  { _scAnn :: a
  , _scWhitespaceLeft :: [Whitespace]
  , _scValue :: Comprehension SetItem expr v a
  , _scWhitespaceRight :: [Whitespace]
  }
makeLenses ''SetComp

data Set expr v a
  = MkSet
  { _setAnn :: a
  , _setWhitespaceLeft :: [Whitespace]
  , _setValues :: CommaSep1' (SetItem expr v a)
  , _setWhitespaceRight :: [Whitespace]
  }
makeLenses ''Set

data Deref expr v a
  = MkDeref
  { _derefAnn :: a
  , _derefValueLeft :: expr v a
  , _derefWhitespaceLeft :: [Whitespace]
  , _derefValueRight :: Ident v a
  }
makeLenses ''Deref

data Subscript expr v a
  = MkSubscript
  { _subscriptAnn :: a
  , _subscriptValueLeft :: expr v a
  , _subscriptWhitespaceLeft :: [Whitespace]
  , _subscriptValueRight :: CommaSep1' (SubscriptItem expr v a)
  , _subscriptWhitespaceRight :: [Whitespace]
  }
makeLenses ''Subscript

data Ellipsis (v :: [*]) (a :: *)
  = MkEllipsis
  { _ellipsisAnn :: a
  , _ellipsisWhitespace :: [Whitespace]
  }
makeLenses ''Ellipsis

data Binary expr (v :: [*]) (a :: *)
  = MkBinary
  { _binaryAnn :: a
  , _binaryExprLeft :: expr v a
  , _binaryOp :: BinOp a
  , _binaryRight :: expr v a
  }
makeLenses ''Binary

data Unary expr (v :: [*]) (a :: *)
  = MkUnary
  { _unaryAnn :: a
  , _unaryOp :: UnOp a
  , _unaryValue :: expr v a
  }
makeLenses ''Unary

data Parens expr (v :: [*]) (a :: *)
  = MkParens
  { _parensAnn :: a
  , _parensWhitespaceLeft :: [Whitespace]
  , _parensValue :: expr v a
  , _parensWhitespaceAfter :: [Whitespace]
  }
makeLenses ''Parens

data PyInt (v :: [*]) (a :: *)
  = MkInt
  { _intAnn :: a
  , _intValue :: IntLiteral a
  , _intWhitespace :: [Whitespace]
  }
makeLenses ''PyInt

data PyFloat (v :: [*]) (a :: *)
  = MkFloat
  { _floatAnn :: a
  , _floatValue :: FloatLiteral a
  , _floatWhitespace :: [Whitespace]
  }
makeLenses ''PyFloat

data Imag (v :: [*]) (a :: *)
  = MkImag
  { _imagAnn :: a
  , _imagValue :: ImagLiteral a
  , _imagWhitespace :: [Whitespace]
  }
makeLenses ''Imag

data PyBool (v :: [*]) (a :: *)
  = MkBool
  { _boolAnn :: a
  , _boolValue :: Bool
  , _boolWhitespace :: [Whitespace]
  }
makeLenses ''PyBool

data PyString (v :: [*]) (a :: *)
  = MkString
  { _stringAnn :: a
  , _stringValue :: NonEmpty (StringLiteral a)
  }
makeLenses ''PyString

data Not expr (v :: [*]) (a :: *)
  = MkNot
  { _notAnn :: a
  , _notWhitespace :: [Whitespace]
  , _notValue :: expr v a
  }
makeLenses ''Not

data Generator expr v a
  = MkGenerator
  { _generatorAnn :: a
  , _generatorValue :: Comprehension VIdentity expr v a
  }
makeLenses ''Generator

data Await expr (v :: [*]) (a :: *)
  = MkAwait
  { _awaitAnn :: a
  , _awaitWhitespace :: [Whitespace]
  , _awaitValue :: expr v a
  }
makeLenses ''Await