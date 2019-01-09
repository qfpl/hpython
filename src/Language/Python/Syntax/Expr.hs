{-# language LambdaCase #-}
{-# language DataKinds, KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language ExistentialQuantification #-}

{-|
Module      : Language.Python.Syntax.Expr
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.Expr
  ( -- * Expressions
    Expr(..), HasExprs(..), exprAnn, shouldGroupLeft, shouldGroupRight
    -- * Parameters and arguments
  , Param(..), paramAnn, paramType_, paramType, paramName
  , Arg(..), argExpr
    -- * Comprehension expressions
    -- | https://docs.python.org/3/reference/expressions.html#grammar-token-comprehension
  , Comprehension(..), CompIf(..), CompFor(..)
    -- * Collection items
  , DictItem(..), ListItem(..), SetItem(..), TupleItem(..)
    -- * Subscripts
  , Subscript(..)
  )
where

import Control.Lens.Cons (_last)
import Control.Lens.Fold ((^?), (^?!))
import Control.Lens.Getter ((^.), getting, to, view)
import Control.Lens.Lens (Lens, Lens', lens)
import Control.Lens.Plated (Plated(..))
import Control.Lens.Prism (_Just, _Left, _Right)
import Control.Lens.Setter ((.~), mapped, over)
import Control.Lens.Traversal (Traversal, failing, traverseOf)
import Control.Lens.Tuple (_2)
import Data.Bifunctor (bimap)
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Digit.Integral (integralDecDigits)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Language.Python.Optics.Validated (Validated(..))
import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace

{-

[unsafeCoerce Validation]

We can't 'coerce' 'Expr's because the @v@ parameter is considered to have a
nominal role, due to datatypes like 'Comprehension'. We only ever use @v@ in
as a phantom in 'Expr', so 'unsafeCoerce :: Expr v a -> Expr '[] a' is safe.

-}
instance Validated Expr where; unvalidated = to unsafeCoerce
instance Validated Param where; unvalidated = to unsafeCoerce
instance Validated Arg where; unvalidated = to unsafeCoerce
instance Validated DictItem where; unvalidated = to unsafeCoerce
instance Validated SetItem where; unvalidated = to unsafeCoerce
instance Validated TupleItem where; unvalidated = to unsafeCoerce
instance Validated ListItem where; unvalidated = to unsafeCoerce

-- | 'Control.Lens.Traversal.Traversal' over all the expressions in a term
class HasExprs s where
  _Exprs :: Traversal (s v a) (s '[] a) (Expr v a) (Expr '[] a)

-- | Formal parameters for functions
--
-- See <https://docs.python.org/3.5/reference/compound_stmts.html#function-definitions>
data Param (v :: [*]) a
  -- | @def foo(a):@
  = PositionalParam
  { _paramAnn :: Ann a
  , _paramName :: Ident v a
  , _paramType :: Maybe (Colon, Expr v a)
  }
  -- | @def foo(bar=None):@
  | KeywordParam
  { _paramAnn :: Ann a
  , _paramName :: Ident v a
  -- ':' spaces <expr>
  , _paramType :: Maybe (Colon, Expr v a)
  -- = spaces
  , _unsafeKeywordParamWhitespaceRight :: [Whitespace]
  , _unsafeKeywordParamExpr :: Expr v a
  }
  -- | @def foo(*xs):@
  | StarParam
  { _paramAnn :: Ann a
  -- '*' spaces
  , _unsafeStarParamWhitespace :: [Whitespace]
  , _unsafeStarParamName :: Ident v a
  -- ':' spaces <expr>
  , _paramType :: Maybe (Colon, Expr v a)
  }
  -- | @def foo(*):@
  | UnnamedStarParam
  { _paramAnn :: Ann a
  -- '*' spaces
  , _unsafeUnnamedStarParamWhitespace :: [Whitespace]
  }
  -- | @def foo(**dict):@
  | DoubleStarParam
  { _paramAnn :: Ann a
  -- '**' spaces
  , _unsafeDoubleStarParamWhitespace :: [Whitespace]
  , _paramName :: Ident v a
  -- ':' spaces <expr>
  , _paramType :: Maybe (Colon, Expr v a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance IsString (Param '[] ()) where
  fromString a = PositionalParam (Ann ()) (fromString a) Nothing

instance HasTrailingWhitespace (Param v a) where
  trailingWhitespace =
    lens
      (\case
          PositionalParam _ a b ->
            maybe (a ^. trailingWhitespace) (^. _2.trailingWhitespace) b
          KeywordParam _ _ _ _ a -> a ^. trailingWhitespace
          UnnamedStarParam _ a -> a
          StarParam _ _ b c ->
            maybe
              (b ^. trailingWhitespace)
              (^. _2.trailingWhitespace)
              c
          DoubleStarParam _ _ a b ->
            maybe
              (a ^. trailingWhitespace)
              (^. _2.trailingWhitespace)
              b)
      (\p ws -> case p of
          PositionalParam a b c ->
            PositionalParam a
              (if isNothing c then b & trailingWhitespace .~ ws else b)
              (c & _Just._2.trailingWhitespace .~ ws)
          KeywordParam a b c d e ->
            KeywordParam a b c d $ e & trailingWhitespace .~ ws
          UnnamedStarParam a _ -> UnnamedStarParam a ws
          StarParam a b c d ->
            StarParam a
              b
              (if isNothing d then c & trailingWhitespace .~ ws else c)
              (d & _Just._2.trailingWhitespace .~ ws)
          DoubleStarParam a b c d ->
            DoubleStarParam a b
              (if isNothing d then c & trailingWhitespace .~ ws else c)
              (d & _Just._2.trailingWhitespace .~ ws))

-- | Lens on the syntrax tree annotation on a parameter
paramAnn :: Lens' (Param v a) a
paramAnn = lens (getAnn . _paramAnn) (\s a -> s { _paramAnn = Ann a})

-- | A faux-lens on the optional Python type annotation which may follow a parameter
--
-- This is not a lawful 'Lens' because setting an 'UnnamedStarParam''s type won't
-- have any effect.
--
-- This optic, like many others in hpython, loses validation information
-- (the @v@ type parameter)
--
-- The following is an example, where @int@ is the paramtype:
--
-- @
-- def foo(x: int):
-- @
paramType_
  :: Functor f
  => (Maybe (Colon, Expr v a) -> f (Maybe (Colon, Expr '[] a)))
  -> Param v a -> f (Param '[] a)
paramType_ =
  lens
    (\case
        UnnamedStarParam{} -> Nothing
        a -> _paramType a)
    (\s ty -> case s ^. unvalidated of
       PositionalParam a b _ -> PositionalParam a b ty
       KeywordParam a b _ c d -> KeywordParam a b ty c d
       StarParam a b c _ -> StarParam a b c ty
       UnnamedStarParam a b -> UnnamedStarParam a b
       DoubleStarParam a b c _ -> DoubleStarParam a b c ty)

-- | 'Traversal' targeting the Python type annotations which may follow a parameter
paramType :: Traversal (Param v a) (Param '[] a) (Colon, Expr v a) (Colon, Expr '[] a)
paramType = paramType_._Just

-- | (affine) 'Control.Lens.Traversal.Traversal' on the name of a parameter
--
-- The name is @x@ in the following examples:
--
-- @
-- def foo(x):
-- def foo(x=None):
-- def foo(*x):
-- def foo(**x):
-- @
--
-- But the following example does not have a 'paramName':
--
-- @
-- def foo(*):
-- @
paramName :: Traversal (Param v a) (Param '[] a) (Ident v a) (Ident '[] a)
paramName f (PositionalParam a b c) =
  PositionalParam a <$> f b <*> pure (over (mapped._2) (view unvalidated) c)
paramName f (KeywordParam a b c d e) =
  (\b' -> KeywordParam a b' (over (mapped._2) (view unvalidated) c) d (e ^. unvalidated)) <$>
  f b
paramName f (StarParam a b c d) =
  (\c' -> StarParam a b c' (over (mapped._2) (view unvalidated) d)) <$>
  f c
paramName _ (UnnamedStarParam a b) = pure $ UnnamedStarParam a b
paramName f (DoubleStarParam a b c d) =
  (\c' -> DoubleStarParam a b c' (over (mapped._2) (view unvalidated) d)) <$>
  f c

instance HasExprs Param where
  _Exprs f (KeywordParam a name ty ws2 expr) =
    KeywordParam a (coerce name) <$>
    traverseOf (traverse._2) f ty <*>
    pure ws2 <*>
    f expr
  _Exprs f (PositionalParam a b c) =
    PositionalParam a (coerce b) <$> traverseOf (traverse._2) f c
  _Exprs f (StarParam a b c d) =
    StarParam a b (coerce c) <$> traverseOf (traverse._2) f d
  _Exprs _ (UnnamedStarParam a b) = pure $ UnnamedStarParam a b
  _Exprs f (DoubleStarParam a b c d) =
    DoubleStarParam a b (coerce c) <$> traverseOf (traverse._2) f d

-- | Actual parameters for functions
--
-- In the following examples, @x@ is an actual parameter.
--
-- @
-- y = foo(x)
-- y = bar(quux=x)
-- y = baz(*x)
-- y = flux(**x)
-- @
data Arg (v :: [*]) a
  = PositionalArg
  { _argAnn :: Ann a
  , _argExpr :: Expr v a
  }
  | KeywordArg
  { _argAnn :: Ann a
  , _unsafeKeywordArgName :: Ident v a
  , _unsafeKeywordArgWhitespaceRight :: [Whitespace]
  , _argExpr :: Expr v a
  }
  | StarArg
  { _argAnn :: Ann a
  , _unsafeStarArgWhitespace :: [Whitespace]
  , _argExpr :: Expr v a
  }
  | DoubleStarArg
  { _argAnn :: Ann a
  , _unsafeDoubleStarArgWhitespace :: [Whitespace]
  , _argExpr :: Expr v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance IsString (Arg '[] ()) where
  fromString = PositionalArg (Ann ()) . fromString

-- | Lens on the Python expression which is passed as the argument
argExpr :: Lens (Arg v a) (Arg '[] a) (Expr v a) (Expr '[] a)
argExpr = lens _argExpr (\s a -> (s ^. unvalidated) { _argExpr = a })

instance HasExprs Arg where
  _Exprs f (KeywordArg a name ws2 expr) = KeywordArg a (coerce name) ws2 <$> f expr
  _Exprs f (PositionalArg a expr) = PositionalArg a <$> f expr
  _Exprs f (StarArg a ws expr) = StarArg a ws <$> f expr
  _Exprs f (DoubleStarArg a ws expr) = StarArg a ws <$> f expr

-- | A Python for comprehension, such as
--
-- @
-- x for y in z
-- @
data Comprehension e (v :: [*]) a
  = Comprehension
      (Ann a)
      (e v a)
      (CompFor v a)
      [Either (CompFor v a) (CompIf v a)] -- ^ <expr> <comp_for> (comp_for | comp_if)*
  deriving (Eq, Show, Generic)

instance HasTrailingWhitespace (Comprehension e v a) where
  trailingWhitespace =
    lens
      (\(Comprehension _ _ a b) ->
         case b of
           [] -> a ^. trailingWhitespace
           _ -> b ^?! _last.failing (_Left.trailingWhitespace) (_Right.trailingWhitespace))
      (\(Comprehension a b c d) ws ->
         case d of
           [] -> Comprehension a b (c & trailingWhitespace .~ ws) d
           _ ->
             Comprehension a b c
               (d &
                _last.failing (_Left.trailingWhitespace) (_Right.trailingWhitespace) .~ ws))

instance Functor (e v) => Functor (Comprehension e v) where
  fmap f (Comprehension a b c d) =
    Comprehension (f <$> a) (fmap f b) (fmap f c) (fmap (bimap (fmap f) (fmap f)) d)

instance Foldable (e v) => Foldable (Comprehension e v) where
  foldMap f (Comprehension a b c d) =
    foldMap f a <> foldMap f b <> foldMap f c <> foldMap (bifoldMap (foldMap f) (foldMap f)) d

instance Traversable (e v) => Traversable (Comprehension e v) where
  traverse f (Comprehension a b c d) =
    Comprehension <$>
    traverse f a <*>
    traverse f b <*>
    traverse f c <*>
    traverse (bitraverse (traverse f) (traverse f)) d

-- | A condition inside a comprehension, e.g. @[x for x in xs if even(x)]@
data CompIf (v :: [*]) a
  = CompIf (Ann a) [Whitespace] (Expr v a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasTrailingWhitespace (CompIf v a) where
  trailingWhitespace =
    lens
      (\(CompIf _ _ a) -> a ^. trailingWhitespace)
      (\(CompIf a b c) ws -> CompIf a b $ c & trailingWhitespace .~ ws)

-- | A nested comprehesion, e.g. @[(x, y) for x in xs for y in ys]@
data CompFor (v :: [*]) a
  = CompFor (Ann a) [Whitespace] (Expr v a) [Whitespace] (Expr v a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasTrailingWhitespace (CompFor v a) where
  trailingWhitespace =
    lens
      (\(CompFor _ _ _ _ a) -> a ^. trailingWhitespace)
      (\(CompFor a b c d e) ws -> CompFor a b c d $ e & trailingWhitespace .~ ws)

-- | @a : b@ or @**a@
--
-- Used to construct dictionaries, e.g. @{ 1: a, 2: b, **c }@
--
-- https://docs.python.org/3/reference/expressions.html#dictionary-displays
data DictItem (v :: [*]) a
  = DictItem
  { _dictItemAnn :: Ann a
  , _unsafeDictItemKey :: Expr v a
  , _unsafeDictItemColon :: Colon
  , _unsafeDictItemValue :: Expr v a
  }
  | DictUnpack
  { _dictItemAnn :: Ann a
  , _unsafeDictItemUnpackWhitespace :: [Whitespace]
  , _unsafeDictItemUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasTrailingWhitespace (DictItem v a) where
  trailingWhitespace =
    lens
      (\(DictItem _ _ _ a) -> a ^. trailingWhitespace)
      (\(DictItem a b c d) ws -> DictItem a b c (d & trailingWhitespace .~ ws))

-- | Syntax for things that can be used as subscripts (inside the square brackets)
--
-- e.g.
--
-- @a[b]@
--
-- @a[:]@
--
-- @a[b:]@
--
-- @a[:b]@
--
-- @a[b:c]@
--
-- @a[b:c:d]@
--
-- https://docs.python.org/3/reference/expressions.html#subscriptions
data Subscript (v :: [*]) a
  = SubscriptExpr (Expr v a)
  | SubscriptSlice
      -- [expr]
      (Maybe (Expr v a))
      -- ':' <spaces>
      Colon
      -- [expr]
      (Maybe (Expr v a))
      -- [':' [expr]]
      (Maybe (Colon, Maybe (Expr v a)))
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasTrailingWhitespace (Subscript v a) where
  trailingWhitespace =
    lens
      (\case
          SubscriptExpr e -> e ^. trailingWhitespace
          SubscriptSlice _ b c d ->
            case d of
              Nothing ->
                case c of
                  Nothing -> b ^. trailingWhitespace
                  Just e -> e ^. trailingWhitespace
              Just (e, f) ->
                case f of
                  Nothing -> e ^. trailingWhitespace
                  Just g -> g ^. trailingWhitespace)
      (\x ws ->
         case x of
          SubscriptExpr e -> SubscriptExpr $ e & trailingWhitespace .~ ws
          SubscriptSlice a b c d ->
            (\(b', c', d') -> SubscriptSlice a b' c' d') $
            case d of
              Nothing ->
                case c of
                  Nothing -> (MkColon ws, c, d)
                  Just e -> (b, Just $ e & trailingWhitespace .~ ws, d)
              Just (e, f) ->
                case f of
                  Nothing -> (b, c, Just (MkColon ws, f))
                  Just g -> (b, c, Just (e, Just $ g & trailingWhitespace .~ ws)))

-- | @a@ or @*a@
--
-- Used to construct lists, e.g. @[ 1, 'x', **c ]@
--
-- https://docs.python.org/3/reference/expressions.html#list-displays
data ListItem (v :: [*]) a
  = ListItem
  { _listItemAnn :: Ann a
  , _unsafeListItemValue :: Expr v a
  }
  | ListUnpack
  { _listItemAnn :: Ann a
  , _unsafeListUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeListUnpackWhitespace :: [Whitespace]
  , _unsafeListUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasExprs ListItem where
  _Exprs f (ListItem a b) = ListItem a <$> f b
  _Exprs f (ListUnpack a b c d) = ListUnpack a b c <$> f d

instance HasTrailingWhitespace (ListItem v a) where
  trailingWhitespace =
    lens
      (\case
          ListItem _ a -> a ^. trailingWhitespace
          ListUnpack _ [] _ a -> a ^. trailingWhitespace
          ListUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           ListItem b c -> ListItem b $ c & trailingWhitespace .~ ws
           ListUnpack b [] d e -> ListUnpack b [] d $ e & trailingWhitespace .~ ws
           ListUnpack b ((c, _) : rest) e f -> ListUnpack b ((c, ws) : rest) e f)

-- | @a@ or @*a@
--
-- Used to construct sets, e.g. @{ 1, 'x', **c }@
--
-- https://docs.python.org/3/reference/expressions.html#set-displays
data SetItem (v :: [*]) a
  = SetItem
  { _setItemAnn :: Ann a
  , _unsafeSetItemValue :: Expr v a
  }
  | SetUnpack
  { _setItemAnn :: Ann a
  , _unsafeSetUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeSetUnpackWhitespace :: [Whitespace]
  , _unsafeSetUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasExprs SetItem where
  _Exprs f (SetItem a b) = SetItem a <$> f b
  _Exprs f (SetUnpack a b c d) = SetUnpack a b c <$> f d

instance HasTrailingWhitespace (SetItem v a) where
  trailingWhitespace =
    lens
      (\case
          SetItem _ a -> a ^. trailingWhitespace
          SetUnpack _ [] _ a -> a ^. trailingWhitespace
          SetUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           SetItem b c -> SetItem b $ c & trailingWhitespace .~ ws
           SetUnpack b [] d e -> SetUnpack b [] d $ e & trailingWhitespace .~ ws
           SetUnpack b ((c, _) : rest) e f -> SetUnpack b ((c, ws) : rest) e f)

-- | @a@ or @*a@
--
-- Used to construct tuples, e.g. @(1, 'x', **c)@
data TupleItem (v :: [*]) a
  = TupleItem
  { _tupleItemAnn :: Ann a
  , _unsafeTupleItemValue :: Expr v a
  }
  | TupleUnpack
  { _tupleItemAnn :: Ann a
  , _unsafeTupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeTupleUnpackWhitespace :: [Whitespace]
  , _unsafeTupleUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasExprs TupleItem where
  _Exprs f (TupleItem a b) = TupleItem a <$> f b
  _Exprs f (TupleUnpack a b c d) = TupleUnpack a b c <$> f d

instance HasTrailingWhitespace (TupleItem v a) where
  trailingWhitespace =
    lens
      (\case
          TupleItem _ a -> a ^. trailingWhitespace
          TupleUnpack _ [] _ a -> a ^. trailingWhitespace
          TupleUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           TupleItem b c -> TupleItem b $ c & trailingWhitespace .~ ws
           TupleUnpack b [] d e -> TupleUnpack b [] d $ e & trailingWhitespace .~ ws
           TupleUnpack b ((c, _) : rest) e f -> TupleUnpack b ((c, ws) : rest) e f)

-- | This large sum type covers all valid Python /expressions/
data Expr (v :: [*]) a
  -- | @()@
  --
  -- https://docs.python.org/3/reference/expressions.html#parenthesized-forms
  = Unit
  { _unsafeExprAnn :: Ann a
  , _unsafeUnitWhitespaceInner :: [Whitespace]
  , _unsafeUnitWhitespaceRight :: [Whitespace]
  }
  -- | @lambda x, y: x@
  --
  -- https://docs.python.org/3/reference/expressions.html#lambda
  | Lambda
  { _unsafeExprAnn :: Ann a
  , _unsafeLambdaWhitespace :: [Whitespace]
  , _unsafeLambdaArgs :: CommaSep (Param v a)
  , _unsafeLambdaColon :: Colon
  , _unsafeLambdaBody :: Expr v a
  }
  -- | @yield@
  --
  -- @yield a@
  --
  -- @yield a, b@
  --
  -- https://docs.python.org/3/reference/expressions.html#yield-expressions
  | Yield
  { _unsafeExprAnn :: Ann a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeYieldValue :: CommaSep (Expr v a)
  }
  -- | @yield from a@
  --
  -- https://docs.python.org/3/reference/expressions.html#yield-expressions
  | YieldFrom
  { _unsafeExprAnn :: Ann a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeFromWhitespace :: [Whitespace]
  , _unsafeYieldFromValue :: Expr v a
  }
  -- | @a if b else c@
  --
  -- https://docs.python.org/3/reference/expressions.html#conditional-expressions
  | Ternary
  { _unsafeExprAnn :: Ann a
  -- expr
  , _unsafeTernaryValue :: Expr v a
  -- 'if' spaces
  , _unsafeTernaryWhitespaceIf :: [Whitespace]
  -- expr
  , _unsafeTernaryCond :: Expr v a
  -- 'else' spaces
  , _unsafeTernaryWhitespaceElse :: [Whitespace]
  -- expr
  , _unsafeTernaryElse :: Expr v a
  }
  -- | @[a for b in c if d]@
  --
  -- https://docs.python.org/3/reference/expressions.html#list-displays
  | ListComp
  { _unsafeExprAnn :: Ann a
  -- [ spaces
  , _unsafeListCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeListCompValue :: Comprehension Expr v a
  -- ] spaces
  , _unsafeListCompWhitespaceRight :: [Whitespace]
  }
  -- | @[a, b, c]@
  --
  -- https://docs.python.org/3/reference/expressions.html#list-displays
  | List
  { _unsafeExprAnn :: Ann a
  -- [ spaces
  , _unsafeListWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeListValues :: Maybe (CommaSep1' (ListItem v a))
  -- ] spaces
  , _unsafeListWhitespaceRight :: [Whitespace]
  }
  -- | @{a: b for c in d if e}@
  --
  -- https://docs.python.org/3/reference/expressions.html#dictionary-displays
  | DictComp
  { _unsafeExprAnn :: Ann a
  -- { spaces
  , _unsafeDictCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeDictCompValue :: Comprehension DictItem v a
  -- } spaces
  , _unsafeDictCompWhitespaceRight :: [Whitespace]
  }
  -- | @{}@
  --
  -- @{a: 1, b: 2, c: 3}@
  --
  -- https://docs.python.org/3/reference/expressions.html#dictionary-displays
  | Dict
  { _unsafeExprAnn :: Ann a
  , _unsafeDictWhitespaceLeft :: [Whitespace]
  , _unsafeDictValues :: Maybe (CommaSep1' (DictItem v a))
  , _unsafeDictWhitespaceRight :: [Whitespace]
  }
  -- | @{a for b in c if d}@
  --
  -- https://docs.python.org/3/reference/expressions.html#set-displays
  | SetComp
  { _unsafeExprAnn :: Ann a
  -- { spaces
  , _unsafeSetCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeSetCompValue :: Comprehension SetItem v a
  -- } spaces
  , _unsafeSetCompWhitespaceRight :: [Whitespace]
  }
  -- | @{a, b, c}@
  --
  -- https://docs.python.org/3/reference/expressions.html#set-displays
  | Set
  { _unsafeExprAnn :: Ann a
  , _unsafeSetWhitespaceLeft :: [Whitespace]
  , _unsafeSetValues :: CommaSep1' (SetItem v a)
  , _unsafeSetWhitespaceRight :: [Whitespace]
  }
  -- | @a.b@
  --
  -- https://docs.python.org/3/reference/expressions.html#attribute-references
  | Deref
  { _unsafeExprAnn :: Ann a
  -- expr
  , _unsafeDerefValueLeft :: Expr v a
  -- . spaces
  , _unsafeDerefWhitespaceLeft :: [Whitespace]
  -- ident
  , _unsafeDerefValueRight :: Ident v a
  }
  -- | @a[b]@
  --
  -- @a[:]@
  --
  -- @a[:, b:]@
  --
  -- etc.
  --
  -- https://docs.python.org/3/reference/expressions.html#subscriptions
  | Subscript
  { _unsafeExprAnn :: Ann a
  -- expr
  , _unsafeSubscriptValueLeft :: Expr v a
  -- [ spaces
  , _unsafeSubscriptWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeSubscriptValueRight :: CommaSep1' (Subscript v a)
  -- ] spaces
  , _unsafeSubscriptWhitespaceRight :: [Whitespace]
  }
  -- | @f(x)@
  --
  -- https://docs.python.org/3/reference/expressions.html#calls
  | Call
  { _unsafeExprAnn :: Ann a
  -- expr
  , _unsafeCallFunction :: Expr v a
  -- ( spaces
  , _unsafeCallWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeCallArguments :: Maybe (CommaSep1' (Arg v a))
  -- ) spaces
  , _unsafeCallWhitespaceRight :: [Whitespace]
  }
  -- | @None@
  --
  -- https://docs.python.org/3/library/constants.html#None
  | None
  { _unsafeExprAnn :: Ann a
  , _unsafeNoneWhitespace :: [Whitespace]
  }
  -- | @...@
  --
  -- https://docs.python.org/3/library/constants.html#Ellipsis
  | Ellipsis
  { _unsafeExprAnn :: Ann a
  , _unsafeEllipsisWhitespace :: [Whitespace]
  }
  -- | @a + b@
  --
  -- https://docs.python.org/3/reference/expressions.html#the-power-operator
  --
  -- https://docs.python.org/3/reference/expressions.html#binary-arithmetic-operations
  --
  -- https://docs.python.org/3/reference/expressions.html#shifting-operations
  --
  -- https://docs.python.org/3/reference/expressions.html#binary-bitwise-operations
  --
  -- https://docs.python.org/3/reference/expressions.html#comparisons
  --
  -- https://docs.python.org/3/reference/expressions.html#membership-test-operations
  --
  -- https://docs.python.org/3/reference/expressions.html#is-not
  --
  -- https://docs.python.org/3/reference/expressions.html#boolean-operations
  | BinOp
  { _unsafeExprAnn :: Ann a
  , _unsafeBinOpExprLeft :: Expr v a
  , _unsafeBinOpOp :: BinOp a
  , _unsafeBinOpExprRight :: Expr v a
  }
  -- | @-a@
  --
  -- @~a@
  --
  -- @+a@
  --
  -- https://docs.python.org/3/reference/expressions.html#unary-arithmetic-and-bitwise-operations
  | UnOp
  { _unsafeExprAnn :: Ann a
  , _unsafeUnOpOp :: UnOp a
  , _unsafeUnOpValue :: Expr v a
  }
  | Parens
  { _unsafeExprAnn :: Ann a
  -- ( spaces
  , _unsafeParensWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeParensValue :: Expr v a
  -- ) spaces
  , _unsafeParensWhitespaceAfter :: [Whitespace]
  }
  -- | @a@
  --
  -- https://docs.python.org/3/reference/expressions.html#atom-identifiers
  | Ident
  { _unsafeIdentValue :: Ident v a
  }
  -- | @1@
  --
  -- @0xF3A
  --
  -- @0o177
  --
  -- @0b1011@
  --
  -- https://docs.python.org/3/reference/lexical_analysis.html#grammar-token-integer
  | Int
  { _unsafeExprAnn :: Ann a
  , _unsafeIntValue :: IntLiteral a
  , _unsafeIntWhitespace :: [Whitespace]
  }
  -- | @1.@
  --
  -- @3.14@
  --
  -- @10e100@
  --
  -- https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals
  | Float
  { _unsafeExprAnn :: Ann a
  , _unsafeFloatValue :: FloatLiteral a
  , _unsafeFloatWhitespace :: [Whitespace]
  }
  -- | @10j@
  --
  -- @5.j@
  --
  -- https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals
  | Imag
  { _unsafeExprAnn :: Ann a
  , _unsafeImagValue :: ImagLiteral a
  , _unsafeImagWhitespace :: [Whitespace]
  }
  -- | @True@
  --
  -- @False@
  --
  -- https://docs.python.org/3/library/constants.html#True
  --
  -- https://docs.python.org/3/library/constants.html#False
  | Bool
  { _unsafeExprAnn :: Ann a
  , _unsafeBoolValue :: Bool
  , _unsafeBoolWhitespace :: [Whitespace]
  }
  -- | @\"asdf\"@
  --
  -- @b\"asdf\"@
  --
  -- @\"asdf\" \'asdf\'@
  --
  -- @\'\'\'asdf\'\'\'@
  --
  -- https://docs.python.org/3/reference/lexical_analysis.html#grammar-token-stringliteral
  | String
  { _unsafeExprAnn :: Ann a
  , _unsafeStringValue :: NonEmpty (StringLiteral a)
  }
  -- | @a, b, c@
  --
  -- @(a, b)@
  --
  -- @(a,)@
  --
  -- https://docs.python.org/3/reference/expressions.html#expression-lists
  | Tuple
  { _unsafeExprAnn :: Ann a
  -- expr
  , _unsafeTupleHead :: TupleItem v a
  -- , spaces
  , _unsafeTupleWhitespace :: Comma
  -- [exprs]
  , _unsafeTupleTail :: Maybe (CommaSep1' (TupleItem v a))
  }
  -- | @not a@
  --
  -- https://docs.python.org/3/reference/expressions.html#boolean-operations
  | Not
  { _unsafeExprAnn :: Ann a
  , _unsafeNotWhitespace :: [Whitespace]
  , _unsafeNotValue :: Expr v a
  }
  -- | @(a for b in c)@
  --
  -- https://docs.python.org/3/reference/expressions.html#generator-expressions
  | Generator
  { _unsafeExprAnn :: Ann a
  , _generatorValue :: Comprehension Expr v a
  }
  -- | @await a@
  --
  -- https://docs.python.org/3/reference/expressions.html#await
  | Await
  { _unsafeExprAnn :: Ann a
  , _unsafeAwaitWhitespace :: [Whitespace]
  , _unsafeAwaitValue :: Expr v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | Lens on the top-level annotation in an expression
exprAnn :: Lens' (Expr v a) a
exprAnn =
  lens
    (\case
        Unit a _ _ -> getAnn a
        Lambda a _ _ _ _ -> getAnn a
        Yield a _ _ -> getAnn a
        YieldFrom a _ _ _ -> getAnn a
        Ternary a _ _ _ _ _ -> getAnn a
        None a _ -> getAnn a
        Ellipsis a _ -> getAnn a
        List a _ _ _ -> getAnn a
        ListComp a _ _ _ -> getAnn a
        Deref a _ _ _ -> getAnn a
        Subscript a _ _ _ _ -> getAnn a
        Call a _ _ _ _ -> getAnn a
        BinOp a _ _ _ -> getAnn a
        UnOp a _ _ -> getAnn a
        Parens a _ _ _ -> getAnn a
        Ident a -> a ^. identAnn
        Int a _ _ -> getAnn a
        Float a _ _ -> getAnn a
        Imag a _ _ -> getAnn a
        Bool a _ _ -> getAnn a
        String a _ -> getAnn a
        Not a _ _ -> getAnn a
        Tuple a _ _ _ -> getAnn a
        DictComp a _ _ _ -> getAnn a
        Dict a _ _ _ -> getAnn a
        SetComp a _ _ _ -> getAnn a
        Set a _ _ _ -> getAnn a
        Generator a _ -> getAnn a
        Await a _ _ -> getAnn a)
    (\e ann ->
      case e of
        Unit _ a b -> Unit (Ann ann) a b
        Lambda _ a b c d -> Lambda (Ann ann) a b c d
        Yield _ a b -> Yield (Ann ann) a b
        YieldFrom _ a b c -> YieldFrom (Ann ann) a b c
        Ternary _ a b c d e -> Ternary (Ann ann) a b c d e
        None _ a -> None (Ann ann) a
        Ellipsis _ a -> Ellipsis (Ann ann) a
        List _ a b c -> List (Ann ann) a b c
        ListComp _ a b c -> ListComp (Ann ann) a b c
        Deref _ a b c -> Deref (Ann ann) a b c
        Subscript _ a b c d -> Subscript (Ann ann) a b c d
        Call _ a b c d -> Call (Ann ann) a b c d
        BinOp _ a b c -> BinOp (Ann ann) a b c
        UnOp _ a b -> UnOp (Ann ann) a b
        Parens _ a b c -> Parens (Ann ann) a b c
        Ident a -> Ident $ a & identAnn .~ ann
        Int _ a b -> Int (Ann ann) a b
        Float _ a b -> Float (Ann ann) a b
        Imag _ a b -> Imag (Ann ann) a b
        Bool _ a b -> Bool (Ann ann) a b
        String _ a -> String (Ann ann) a
        Not _ a b -> Not (Ann ann) a b
        Tuple _ a b c -> Tuple (Ann ann) a b c
        DictComp _ a b c -> DictComp (Ann ann) a b c
        Dict _ a b c -> Dict (Ann ann) a b c
        SetComp _ a b c -> SetComp (Ann ann) a b c
        Set _ a b c -> Set (Ann ann) a b c
        Generator _ a -> Generator (Ann ann) a
        Await _ a b -> Not (Ann ann) a b)

instance HasTrailingWhitespace (Expr v a) where
  trailingWhitespace =
    lens
      (\case
          Unit _ _ a -> a
          Lambda _ _ _ _ a -> a ^. trailingWhitespace
          Yield _ ws CommaSepNone -> ws
          Yield _ _ e -> e ^?! csTrailingWhitespace
          YieldFrom _ _ _ e -> e ^. trailingWhitespace
          Ternary _ _ _ _ _ e -> e ^. trailingWhitespace
          None _ ws -> ws
          Ellipsis _ ws -> ws
          List _ _ _ ws -> ws
          ListComp _ _ _ ws -> ws
          Deref _ _ _ a -> a ^. trailingWhitespace
          Subscript _ _ _ _ ws -> ws
          Call _ _ _ _ ws -> ws
          BinOp _ _ _ e -> e ^. trailingWhitespace
          UnOp _ _ e -> e ^. trailingWhitespace
          Parens _ _ _ ws -> ws
          Ident a -> a ^. getting trailingWhitespace
          Int _ _ ws -> ws
          Float _ _ ws -> ws
          Imag _ _ ws -> ws
          Bool _ _ ws -> ws
          String _ v -> v ^. trailingWhitespace
          Not _ _ e -> e ^. trailingWhitespace
          Tuple _ _ (MkComma ws) Nothing -> ws
          Tuple _ _ _ (Just cs) -> cs ^. trailingWhitespace
          DictComp _ _ _ ws -> ws
          Dict _ _ _ ws -> ws
          SetComp _ _ _ ws -> ws
          Set _ _ _ ws -> ws
          Generator  _ a -> a ^. trailingWhitespace
          Await _ _ e -> e ^. trailingWhitespace)
      (\e ws ->
        case e of
          Unit a b _ -> Unit a b ws
          Lambda a b c d f -> Lambda a b c d (f & trailingWhitespace .~ ws)
          Yield a _ CommaSepNone -> Yield a ws CommaSepNone
          Yield a b c -> Yield a b (c & csTrailingWhitespace .~ ws)
          YieldFrom a b c d -> YieldFrom a b c (d & trailingWhitespace .~ ws)
          Ternary a b c d e f -> Ternary a b c d e (f & trailingWhitespace .~ ws)
          None a _ -> None a ws
          Ellipsis a _ -> Ellipsis a ws
          List a b c _ -> List a b (coerce c) ws
          ListComp a b c _ -> ListComp a b (coerce c) ws
          Deref a b c d -> Deref a (coerce b) c (d & trailingWhitespace .~ ws)
          Subscript a b c d _ -> Subscript a (coerce b) c d ws
          Call a b c d _ -> Call a (coerce b) c (coerce d) ws
          BinOp a b c e -> BinOp a (coerce b) c (e & trailingWhitespace .~ ws)
          UnOp a b c -> UnOp a b (c & trailingWhitespace .~ ws)
          Parens a b c _ -> Parens a b (coerce c) ws
          Ident a -> Ident $ a & trailingWhitespace .~ ws
          Int a b _ -> Int a b ws
          Float a b _ -> Float a b ws
          Imag a b _ -> Imag a b ws
          Bool a b _ -> Bool a b ws
          String a v -> String a (v & trailingWhitespace .~ ws)
          Not a b c -> Not a b (c & trailingWhitespace .~ ws)
          Tuple a b _ Nothing -> Tuple a (coerce b) (MkComma ws) Nothing
          Tuple a b c (Just cs) ->
            Tuple a (coerce b) c (Just $ cs & trailingWhitespace .~ ws)
          DictComp a b c _ -> DictComp a b c ws
          Dict a b c _ -> Dict a b c ws
          SetComp a b c _ -> SetComp a b c ws
          Set a b c _ -> Set a b c ws
          Generator a b -> Generator a $ b & trailingWhitespace .~ ws
          Await a b c -> Await a b (c & trailingWhitespace .~ ws))

instance IsString (Expr '[] ()) where
  fromString s = Ident $ MkIdent () s []

instance Num (Expr '[] ()) where
  fromInteger n
    | n >= 0 = Int (Ann ()) (IntLiteralDec () $ integralDecDigits n ^?! _Right) []
    | otherwise =
        UnOp
          (Ann ())
          (Negate () [])
          (Int (Ann ()) (IntLiteralDec () $ integralDecDigits (-n) ^?! _Right) [])

  negate = UnOp (Ann ()) (Negate () [])

  (+) a = BinOp (Ann ()) (a & trailingWhitespace .~ [Space]) (Plus () [Space])
  (*) a = BinOp (Ann ()) (a & trailingWhitespace .~ [Space]) (Multiply () [Space])
  (-) a = BinOp (Ann ()) (a & trailingWhitespace .~ [Space]) (Minus () [Space])
  signum = undefined
  abs = undefined

instance Plated (Expr '[] a) where
  plate fun e =
    case e of
      Unit{} -> pure e
      Lambda a b c d e ->
        (\c' -> Lambda a b c' d) <$>
        (traverse.paramExpr) fun c <*>
        fun e
      Yield a b c ->
        Yield a b <$> traverse fun c
      YieldFrom a b c d ->
        YieldFrom a b c <$> fun d
      Ternary a b c d e f ->
        (\b' d' -> Ternary a b' c d' e) <$>
        fun b <*>
        fun d <*>
        fun f
      None{} -> pure e
      Ellipsis{} -> pure e
      List a b c d ->
        (\c' -> List a b c' d) <$>
        (traverse.traverse.listItemExpr) fun c
      ListComp a b c d ->
        (\c' -> ListComp a b c' d) <$>
        compExpr fun c
      Deref a b c d ->
        (\b' -> Deref a b' c d) <$>
        fun b
      Subscript a b c d e ->
        (\b' d' -> Subscript a b' c d' e) <$>
        fun b <*>
        (traverse.subscriptExpr) fun d
      Call a b c d e ->
        (\b' d' -> Call a b' c d' e) <$>
        fun b <*>
        (traverse.traverse.argExpr) fun d
      BinOp a b c d ->
        (\b' -> BinOp a b' c) <$>
        fun b <*>
        fun d
      UnOp a b c ->
        UnOp a b <$> fun c
      Parens a b c d ->
        (\c' -> Parens a b c' d) <$>
        fun c
      Ident{} -> pure e
      Int{} -> pure e
      Float{} -> pure e
      Imag{} -> pure e
      Bool{} -> pure e
      String{} -> pure e
      Not a b c -> Not a b <$> fun c
      Tuple a b c d ->
        (\b' -> Tuple a b' c) <$>
        tupleItemExpr fun b <*>
        (traverse.traverse.tupleItemExpr) fun d
      DictComp a b c d ->
        (\c' -> DictComp a b c' d) <$>
        dictCompExpr fun c
      Dict a b c d ->
        (\c' -> Dict a b c' d) <$>
        (traverse.traverse.dictItemExpr) fun c
      SetComp a b c d ->
        (\c' -> SetComp a b c' d) <$>
        setCompExpr fun c
      Set a b c d ->
        (\c' -> Set a b c' d) <$>
        (traverse.setItemExpr) fun c
      Generator a b -> Generator a <$> compExpr fun b
      Await a b c -> Await a b <$> fun c
    where
      paramExpr fun' p =
        case p of
          PositionalParam a b c ->
            PositionalParam a b <$>
            (traverse._2) fun' c
          KeywordParam a b c d e ->
            (\c' -> KeywordParam a b c' d) <$>
            (traverse._2) fun' c <*>
            fun' e
          UnnamedStarParam{} -> pure p
          StarParam a b c d ->
            StarParam a b c <$> (traverse._2) fun' d
          DoubleStarParam a b c d ->
            DoubleStarParam a b c <$> (traverse._2) fun' d

      listItemExpr fun' li =
        case li of
          ListItem a b -> ListItem a <$> fun' b
          ListUnpack a b c d -> ListUnpack a b c <$> fun' d

      tupleItemExpr fun' ti =
        case ti of
          TupleItem a b -> TupleItem a <$> fun' b
          TupleUnpack a b c d -> TupleUnpack a b c <$> fun' d

      setItemExpr fun' si =
        case si of
          SetItem a b -> SetItem a <$> fun' b
          SetUnpack a b c d -> SetUnpack a b c <$> fun' d

      dictItemExpr fun' di =
        case di of
          DictItem a b c d ->
            (\b' -> DictItem a b' c) <$>
            fun' b <*>
            fun' d
          DictUnpack a b c -> DictUnpack a b <$> fun' c

      compIfExpr fun' (CompIf a b c) = CompIf a b <$> fun' c

      compForExpr fun' (CompFor a b c d e) =
        (\c' -> CompFor a b c' d) <$>
        fun' c <*>
        fun' e

      compExpr fun' (Comprehension a b c d) =
        Comprehension a <$>
        fun' b <*>
        compForExpr fun' c <*>
        traverse (bitraverse (compForExpr fun') (compIfExpr fun')) d

      dictCompExpr fun' (Comprehension a b c d) =
        Comprehension a <$>
        dictItemExpr fun' b <*>
        compForExpr fun' c <*>
        traverse (bitraverse (compForExpr fun') (compIfExpr fun')) d

      setCompExpr fun' (Comprehension a b c d) =
        Comprehension a <$>
        setItemExpr fun' b <*>
        compForExpr fun' c <*>
        traverse (bitraverse (compForExpr fun') (compIfExpr fun')) d

      subscriptExpr fun' ss =
        case ss of
          SubscriptExpr a -> SubscriptExpr <$> fun' a
          SubscriptSlice a b c d ->
            (\a' -> SubscriptSlice a' b) <$>
            traverse fun' a <*>
            traverse fun' c <*>
            (traverse._2.traverse) fun' d

      argExpr fun' arg =
        case arg of
          PositionalArg a b -> PositionalArg a <$> fun' b
          KeywordArg a b c d -> KeywordArg a b c <$> fun' d
          StarArg a b c -> StarArg a b <$> fun' c
          DoubleStarArg a b c -> DoubleStarArg a b <$> fun' c

instance HasExprs Expr where
  _Exprs = id

-- |
-- @shouldGroupLeft op left@ returns true if @left@ needs to be parenthesised
-- when it is the left argument of @op@
shouldGroupLeft :: BinOp a -> Expr v a -> Bool
shouldGroupLeft op left =
  let
    entry = lookupOpEntry op operatorTable

    lEntry =
      case left of
        BinOp _ _ lOp _ -> Just $ lookupOpEntry lOp operatorTable
        _ -> Nothing

    leftf =
      case entry ^. opAssoc of
        R | Just (OpEntry _ prec R) <- lEntry -> prec <= entry ^. opPrec
        _ -> False

    leftf' =
      case (left, op) of
        (UnOp{}, Exp{}) -> True
        (Tuple{}, _) -> True
        (Not{}, BoolAnd{}) -> False
        (Not{}, BoolOr{}) -> False
        (Not{}, _) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (lEntry ^? _Just.opPrec)
  in
    leftf || leftf'

-- |
-- @shouldGroupRight op right@ returns true if @right@ needs to be parenthesised
-- when it is the right argument of @op@
shouldGroupRight :: BinOp a -> Expr v a -> Bool
shouldGroupRight op right =
  let
    entry = lookupOpEntry op operatorTable

    rEntry =
      case right of
        BinOp _ _ rOp _ -> Just $ lookupOpEntry rOp operatorTable
        _ -> Nothing

    rightf =
      case entry ^. opAssoc of
        L | Just (OpEntry _ prec L) <- rEntry -> prec <= entry ^. opPrec
        _ -> False

    rightf' =
      case (op, right) of
        (_, Tuple{}) -> True
        (BoolAnd{}, Not{}) -> False
        (BoolOr{}, Not{}) -> False
        (_, Not{}) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (rEntry ^? _Just.opPrec)
  in
    rightf || rightf'
