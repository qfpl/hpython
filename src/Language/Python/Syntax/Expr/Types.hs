{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language TemplateHaskell #-}
module Language.Python.Syntax.Expr.Types
  ( -- ** Parenthesised expressions
    Parens(..)
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

import Control.Lens.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Whitespace

data Parens expr (v :: [*]) (a :: *)
  = MkParens
  { _parensAnn :: Ann a
  , _parensWhitespaceLeft :: [Whitespace]
  , _parensValue :: expr v a
  , _parensWhitespaceAfter :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Parens

instance HasAnn (Parens expr v) where
  annot :: forall a. Lens' (Parens expr v a) (Ann a)
  annot = typed @(Ann a)

data Unit (v :: [*]) a
  = MkUnit
  { _unitAnn :: Ann a
  , _unitWhitespaceInner :: [Whitespace]
  , _unitWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Unit

instance HasAnn (Unit v) where
  annot :: forall a. Lens' (Unit v a) (Ann a)
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

data Ellipsis (v :: [*]) (a :: *)
  = MkEllipsis
  { _ellipsisAnn :: Ann a
  , _ellipsisWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Ellipsis

instance HasAnn (Ellipsis v) where
  annot :: forall a. Lens' (Ellipsis v a) (Ann a)
  annot = typed @(Ann a)

data PyBool (v :: [*]) (a :: *)
  = MkBool
  { _boolAnn :: Ann a
  , _boolValue :: Bool
  , _boolWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PyBool

instance HasAnn (PyBool v) where
  annot :: forall a. Lens' (PyBool v a) (Ann a)
  annot = typed @(Ann a)
