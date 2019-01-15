{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language TemplateHaskell #-}
module Language.Python.Syntax.Expr.Types
  ( module Language.Python.Syntax.Numbers
  , module Language.Python.Syntax.Strings
  , -- ** Parenthesised expressions
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
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Data.VFoldable
import Data.VFunctor
import Data.VIdentity
import Data.VTraversable
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Arg
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comprehension
import Language.Python.Syntax.Dicts
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Lists
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Param
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Sets
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Subscript
import Language.Python.Syntax.Tuples
import Language.Python.Syntax.Whitespace

data Parens expr (v :: [*]) (a :: *)
  = MkParens
  { _parensAnn :: Ann a
  , _parensWhitespaceLeft :: [Whitespace]
  , _parensValue :: expr v a
  , _parensWhitespaceAfter :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Parens

instance VTraversable Parens where
  vtraverse f (MkParens a b c d) = (\c' -> MkParens a b c' d) <$> f c
instance VFoldable Parens where; vfoldMap = vfoldMapDefault
instance VFunctor Parens where; vfmap = vfmapDefault

instance HasAnn (Parens expr v) where
  annot :: forall a. Lens' (Parens expr v a) (Ann a)
  annot = typed @(Ann a)

data Unit (expr :: [*] -> * -> *) (v :: [*]) a
  = MkUnit
  { _unitAnn :: Ann a
  , _unitWhitespaceInner :: [Whitespace]
  , _unitWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Unit

instance VTraversable Unit where
  vtraverse _ (MkUnit a b c) = pure $ MkUnit a b c
instance VFoldable Unit where; vfoldMap = vfoldMapDefault
instance VFunctor Unit where; vfmap = vfmapDefault

instance HasAnn (Unit expr v) where
  annot :: forall a. Lens' (Unit expr v a) (Ann a)
  annot = typed @(Ann a)

data None (expr :: [*] -> * -> *) (v :: [*]) a
  = MkNone
  { _noneAnn :: Ann a
  , _noneWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''None

instance VTraversable None where
  vtraverse _ (MkNone a b) = pure $ MkNone a b
instance VFoldable None where; vfoldMap = vfoldMapDefault
instance VFunctor None where; vfmap = vfmapDefault

instance HasAnn (None expr v) where
  annot :: forall a. Lens' (None expr v a) (Ann a)
  annot = typed @(Ann a)

data Ellipsis (expr :: [*] -> * -> *) (v :: [*]) (a :: *)
  = MkEllipsis
  { _ellipsisAnn :: Ann a
  , _ellipsisWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Ellipsis

instance VTraversable Ellipsis where
  vtraverse _ (MkEllipsis a b) = pure $ MkEllipsis a b
instance VFoldable Ellipsis where; vfoldMap = vfoldMapDefault
instance VFunctor Ellipsis where; vfmap = vfmapDefault

instance HasAnn (Ellipsis expr v) where
  annot :: forall a. Lens' (Ellipsis expr v a) (Ann a)
  annot = typed @(Ann a)

data PyBool (expr :: [*] -> * -> *) (v :: [*]) (a :: *)
  = MkBool
  { _boolAnn :: Ann a
  , _boolValue :: Bool
  , _boolWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PyBool

instance VTraversable PyBool where
  vtraverse _ (MkBool a b c) = pure $ MkBool a b c
instance VFoldable PyBool where; vfoldMap = vfoldMapDefault
instance VFunctor PyBool where; vfmap = vfmapDefault

instance HasAnn (PyBool expr v) where
  annot :: forall a. Lens' (PyBool expr v a) (Ann a)
  annot = typed @(Ann a)

data PyString (expr :: [*] -> * -> *) (v :: [*]) (a :: *)
  = MkString
  { _stringAnn :: Ann a
  , _stringValue :: NonEmpty (StringLiteral a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PyString

instance VTraversable PyString where
  vtraverse _ (MkString a b) = pure $ MkString a b
instance VFoldable PyString where; vfoldMap = vfoldMapDefault
instance VFunctor PyString where; vfmap = vfmapDefault

instance HasAnn (PyString expr v) where
  annot :: forall a. Lens' (PyString expr v a) (Ann a)
  annot = typed @(Ann a)

data Lambda expr v a
  = MkLambda
  { _lambdaAnn :: Ann a
  , _lambdaWhitespace :: [Whitespace]
  , _lambdaArgs :: CommaSep (Param expr v a)
  , _lambdaColon :: Colon
  , _lambdaBody :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Lambda

instance VTraversable Lambda where
  vtraverse f (MkLambda a b c d e) =
    (\c' -> MkLambda a b c' d) <$>
    traverse (vtraverse f) c <*>
    f e
instance VFoldable Lambda where; vfoldMap = vfoldMapDefault
instance VFunctor Lambda where; vfmap = vfmapDefault

instance HasAnn (Lambda expr v) where
  annot :: forall a. Lens' (Lambda expr v a) (Ann a)
  annot = typed @(Ann a)

data Yield expr (v :: [*]) (a :: *)
  = MkYield
  { _yieldAnn :: Ann a
  , _yieldWhitespace :: [Whitespace]
  , _yieldValue :: CommaSep (expr v a)
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Yield

instance VTraversable Yield where
  vtraverse f (MkYield a b c) = MkYield a b <$> traverse f c
instance VFoldable Yield where; vfoldMap = vfoldMapDefault
instance VFunctor Yield where; vfmap = vfmapDefault

instance HasAnn (Yield expr v) where
  annot :: forall a. Lens' (Yield expr v a) (Ann a)
  annot = typed @(Ann a)

data YieldFrom expr (v :: [*]) (a :: *)
  = MkYieldFrom
  { _yfAnn :: Ann a
  , _yfYieldWhitespace :: [Whitespace]
  , _yfFromWhitespace :: [Whitespace]
  , _yfValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''YieldFrom

instance VTraversable YieldFrom where
  vtraverse f (MkYieldFrom a b c d) = MkYieldFrom a b c <$> f d
instance VFoldable YieldFrom where; vfoldMap = vfoldMapDefault
instance VFunctor YieldFrom where; vfmap = vfmapDefault

instance HasAnn (YieldFrom expr v) where
  annot :: forall a. Lens' (YieldFrom expr v a) (Ann a)
  annot = typed @(Ann a)

data Not expr (v :: [*]) (a :: *)
  = MkNot
  { _notAnn :: Ann a
  , _notWhitespace :: [Whitespace]
  , _notValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Not

instance VTraversable Not where
  vtraverse f (MkNot a b c) = MkNot a b <$> f c
instance VFoldable Not where; vfoldMap = vfoldMapDefault
instance VFunctor Not where; vfmap = vfmapDefault

instance HasAnn (Not expr v) where
  annot :: forall a. Lens' (Not expr v a) (Ann a)
  annot = typed @(Ann a)

data Unary expr (v :: [*]) (a :: *)
  = MkUnary
  { _unaryAnn :: Ann a
  , _unaryOp :: UnOp a
  , _unaryValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Unary

instance VTraversable Unary where
  vtraverse f (MkUnary a b c) = MkUnary a b <$> f c
instance VFoldable Unary where; vfoldMap = vfoldMapDefault
instance VFunctor Unary where; vfmap = vfmapDefault

instance HasAnn (Unary expr v) where
  annot :: forall a. Lens' (Unary expr v a) (Ann a)
  annot = typed @(Ann a)

data Binary expr (v :: [*]) (a :: *)
  = MkBinary
  { _binaryAnn :: Ann a
  , _binaryExprLeft :: expr v a
  , _binaryOp :: BinOp a
  , _binaryRight :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Binary

instance VTraversable Binary where
  vtraverse f (MkBinary a b c d) = (\b' -> MkBinary a b' c) <$> f b <*> f d
instance VFoldable Binary where; vfoldMap = vfoldMapDefault
instance VFunctor Binary where; vfmap = vfmapDefault

instance HasAnn (Binary expr v) where
  annot :: forall a. Lens' (Binary expr v a) (Ann a)
  annot = typed @(Ann a)

data Ternary expr (v :: [*]) (a :: *)
  = MkTernary
  { _ternaryAnn :: Ann a
  , _ternaryValue :: expr v a
  , _ternaryIfWhitespace :: [Whitespace]
  , _ternaryCond :: expr v a
  , _ternaryElseWhitespace :: [Whitespace]
  , _ternaryElse :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Ternary

instance VTraversable Ternary where
  vtraverse fun (MkTernary a b c d e f) =
    (\b' d' -> MkTernary a b' c d' e) <$>
    fun b <*>
    fun d <*>
    fun f
instance VFoldable Ternary where; vfoldMap = vfoldMapDefault
instance VFunctor Ternary where; vfmap = vfmapDefault

instance HasAnn (Ternary expr v) where
  annot :: forall a. Lens' (Ternary expr v a) (Ann a)
  annot = typed @(Ann a)

data Call expr v a
  = MkCall
  { _callAnn :: Ann a
  , _callFunction :: expr v a
  , _callLeftParen :: [Whitespace]
  , _callArguments :: Maybe (CommaSep1' (Arg expr v a))
  , _callRightParen :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Call

instance VTraversable Call where
  vtraverse f (MkCall a b c d e) =
    (\b' d' -> MkCall a b' c d' e) <$>
    f b <*>
    traverse (traverse (vtraverse f)) d
instance VFoldable Call where; vfoldMap = vfoldMapDefault
instance VFunctor Call where; vfmap = vfmapDefault

instance HasAnn (Call expr v) where
  annot :: forall a. Lens' (Call expr v a) (Ann a)
  annot = typed @(Ann a)

data Deref expr v a
  = MkDeref
  { _derefAnn :: Ann a
  , _derefValueLeft :: expr v a
  , _derefWhitespaceLeft :: [Whitespace]
  , _derefValueRight :: Ident v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Deref

instance VTraversable Deref where
  vtraverse f (MkDeref a b c d) = (\b' -> MkDeref a b' c d) <$> f b
instance VFoldable Deref where; vfoldMap = vfoldMapDefault
instance VFunctor Deref where; vfmap = vfmapDefault

instance HasAnn (Deref expr v) where
  annot :: forall a. Lens' (Deref expr v a) (Ann a)
  annot = typed @(Ann a)

data Subscript expr v a
  = MkSubscript
  { _subscriptAnn :: Ann a
  , _subscriptValueLeft :: expr v a
  , _subscriptWhitespaceLeft :: [Whitespace]
  , _subscriptValueRight :: CommaSep1' (SubscriptItem expr v a)
  , _subscriptWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Subscript

instance VTraversable Subscript where
  vtraverse f (MkSubscript a b c d e) =
    (\b' d' -> MkSubscript a b' c d' e) <$> f b <*> traverse (vtraverse f) d
instance VFoldable Subscript where; vfoldMap = vfoldMapDefault
instance VFunctor Subscript where; vfmap = vfmapDefault

instance HasAnn (Subscript expr v) where
  annot :: forall a. Lens' (Subscript expr v a) (Ann a)
  annot = typed @(Ann a)

data Tuple expr v a
  = MkTuple
  { _tupleAnn :: Ann a
  , _tupleHead :: TupleItem expr v a
  , _tupleComma :: Comma
  , _tupleTail :: Maybe (CommaSep1' (TupleItem expr v a))
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Tuple

instance VTraversable Tuple where
  vtraverse f (MkTuple a b c d) =
    (\b' -> MkTuple a b' c) <$>
    vtraverse f b <*>
    traverse (traverse (vtraverse f)) d
instance VFoldable Tuple where; vfoldMap = vfoldMapDefault
instance VFunctor Tuple where; vfmap = vfmapDefault

instance HasAnn (Tuple expr v) where
  annot :: forall a. Lens' (Tuple expr v a) (Ann a)
  annot = typed @(Ann a)

data TupleUnpack expr (v :: [*]) (a :: *)
  = MkTupleUnpack
  { _tupleUnpackAnn :: Ann a
  , _tupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _tupleUnpackWhitespace :: [Whitespace]
  , _tupleUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''TupleUnpack

instance HasAnn (TupleUnpack expr v) where
  annot :: forall a. Lens' (TupleUnpack expr v a) (Ann a)
  annot = typed @(Ann a)

data List expr v a
  = MkList
  { _listAnn :: Ann a
  , _listWhitespaceLeft :: [Whitespace]
  , _listBody :: Maybe (CommaSep1' (ListItem expr v a))
  , _listWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''List

instance VTraversable List where
  vtraverse f (MkList a b c d) =
    (\c' -> MkList a b c' d) <$> traverse (traverse (vtraverse f)) c
instance VFoldable List where; vfoldMap = vfoldMapDefault
instance VFunctor List where; vfmap = vfmapDefault

instance HasAnn (List expr v) where
  annot :: forall a. Lens' (List expr v a) (Ann a)
  annot = typed @(Ann a)

data ListComp expr v a
  = MkListComp
  { _lcAnn :: Ann a
  , _lcWhitespaceLeft :: [Whitespace]
  , _lcValue :: Comprehension VIdentity expr v a
  , _lcWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''ListComp

instance VTraversable ListComp where
  vtraverse f (MkListComp a b c d) = (\c' -> MkListComp a b c' d) <$> vtraverse f c
instance VFoldable ListComp where; vfoldMap = vfoldMapDefault
instance VFunctor ListComp where; vfmap = vfmapDefault

instance HasAnn (ListComp expr v) where
  annot :: forall a. Lens' (ListComp expr v a) (Ann a)
  annot = typed @(Ann a)

data ListUnpack expr (v :: [*]) (a :: *)
  = MkListUnpack
  { _listUnpackAnn :: Ann a
  , _listUnpackParens :: [([Whitespace], [Whitespace])]
  , _listUnpackWhitespace :: [Whitespace]
  , _listUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''ListUnpack

instance HasAnn (ListUnpack expr v) where
  annot :: forall a. Lens' (ListUnpack expr v a) (Ann a)
  annot = typed @(Ann a)

data Dict expr v a
  = MkDict
  { _dictAnn :: Ann a
  , _dictWhitespaceLeft :: [Whitespace]
  , _dictValues :: Maybe (CommaSep1' (DictItem expr v a))
  , _dictWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Dict

instance VTraversable Dict where
  vtraverse f (MkDict a b c d) =
    (\c' -> MkDict a b c' d) <$> traverse (traverse (vtraverse f)) c
instance VFoldable Dict where; vfoldMap = vfoldMapDefault
instance VFunctor Dict where; vfmap = vfmapDefault

instance HasAnn (Dict expr v) where
  annot :: forall a. Lens' (Dict expr v a) (Ann a)
  annot = typed @(Ann a)

data DictComp expr v a
  = MkDictComp
  { _dcAnn :: Ann a
  , _dcWhitespaceLeft :: [Whitespace]
  , _dcValue :: Comprehension DictItem expr v a
  , _dcWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''DictComp

instance VTraversable DictComp where
  vtraverse f (MkDictComp a b c d) = (\c' -> MkDictComp a b c' d) <$> vtraverse f c
instance VFoldable DictComp where; vfoldMap = vfoldMapDefault
instance VFunctor DictComp where; vfmap = vfmapDefault

instance HasAnn (DictComp expr v) where
  annot :: forall a. Lens' (DictComp expr v a) (Ann a)
  annot = typed @(Ann a)

data Set expr v a
  = MkSet
  { _setAnn :: Ann a
  , _setWhitespaceLeft :: [Whitespace]
  , _setValues :: CommaSep1' (SetItem expr v a)
  , _setWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Set

instance VTraversable Set where
  vtraverse f (MkSet a b c d) = (\c' -> MkSet a b c' d) <$> traverse (vtraverse f) c
instance VFoldable Set where; vfoldMap = vfoldMapDefault
instance VFunctor Set where; vfmap = vfmapDefault

instance HasAnn (Set expr v) where
  annot :: forall a. Lens' (Set expr v a) (Ann a)
  annot = typed @(Ann a)

data SetComp expr v a
  = MkSetComp
  { _scAnn :: Ann a
  , _scWhitespaceLeft :: [Whitespace]
  , _scValue :: Comprehension SetItem expr v a
  , _scWhitespaceRight :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''SetComp

instance VTraversable SetComp where
  vtraverse f (MkSetComp a b c d) = (\c' -> MkSetComp a b c' d) <$> vtraverse f c
instance VFoldable SetComp where; vfoldMap = vfoldMapDefault
instance VFunctor SetComp where; vfmap = vfmapDefault

instance HasAnn (SetComp expr v) where
  annot :: forall a. Lens' (SetComp expr v a) (Ann a)
  annot = typed @(Ann a)

data Generator expr v a
  = MkGenerator
  { _generatorAnn :: Ann a
  , _generatorValue :: Comprehension VIdentity expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Generator

instance VTraversable Generator where
  vtraverse f (MkGenerator a b) = MkGenerator a <$> vtraverse f b
instance VFoldable Generator where; vfoldMap = vfoldMapDefault
instance VFunctor Generator where; vfmap = vfmapDefault

instance HasAnn (Generator expr v) where
  annot :: forall a. Lens' (Generator expr v a) (Ann a)
  annot = typed @(Ann a)

data Await expr (v :: [*]) (a :: *)
  = MkAwait
  { _awaitAnn :: Ann a
  , _awaitWhitespace :: [Whitespace]
  , _awaitValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Await

instance VTraversable Await where
  vtraverse f (MkAwait a b c) = MkAwait a b <$> f c
instance VFoldable Await where; vfoldMap = vfoldMapDefault
instance VFunctor Await where; vfmap = vfmapDefault

instance HasAnn (Await expr v) where
  annot :: forall a. Lens' (Await expr v a) (Ann a)
  annot = typed @(Ann a)
