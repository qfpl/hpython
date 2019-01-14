{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language ExistentialQuantification #-}
{-# language InstanceSigs, TypeApplications #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

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
    Expr, ExprF(..), shouldGroupLeft, shouldGroupRight
    -- * Parameters and arguments
  , module Language.Python.Syntax.Param
  , module Language.Python.Syntax.Arg
    -- * Comprehension expressions
  , module Language.Python.Syntax.Comprehension
    -- * Collection items
  , DictItem(..), ListItem(..), SetItem(..), TupleItem(..)
    -- * Subscripts
  , SubscriptItem(..)
  )
where

import Control.Lens.Fold ((^?), (^?!))
import Control.Lens.Getter ((^.), getting, to)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Prism (_Just, _Right)
import Control.Lens.Setter ((.~))
import Control.Lens.Tuple (_2)
import Data.Coerce (coerce)
import Data.Digit.Integral (integralDecDigits)
import Data.Function ((&))
import Data.Generics.Product.Typed (typed)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Data.VFix
import Data.VFoldable
import Data.VFunctor
import Data.VIdentity
import Data.VTraversable
import Data.VariantV
import Language.Python.Optics.Exprs (HasExprs(..))
import Language.Python.Optics.Validated (Validated(..))
import Language.Python.Syntax.Arg
import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comprehension
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Param
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace

{-

[unsafeCoerce Validation]

We can't 'coerce' 'Expr's because the @v@ parameter is considered to have a
nominal role, due to datatypes like 'Comprehension'. We only ever use @v@ in
as a phantom in 'Expr', so 'unsafeCoerce :: Expr v a -> Expr '[] a' is safe.

-}
instance Validated e => Validated (ExprF e) where; unvalidated = to unsafeCoerce
instance Validated e => Validated (DictItem e) where; unvalidated = to unsafeCoerce
instance Validated e => Validated (SetItem e) where; unvalidated = to unsafeCoerce
instance Validated e => Validated (TupleItem e) where; unvalidated = to unsafeCoerce
instance Validated e => Validated (ListItem e) where; unvalidated = to unsafeCoerce

type Expr35 = VFix (VariantV '[ExprF, IntLiteral35])
type Expr36 = VFix (VariantV '[ExprF, IntLiteral36])

-- | @a : b@ or @**a@
--
-- Used to construct dictionaries, e.g. @{ 1: a, 2: b, **c }@
--
-- https://docs.python.org/3/reference/expressions.html#dictionary-displays
data DictItem expr (v :: [*]) a
  = DictItem
  { _dictItemAnn :: Ann a
  , _unsafeDictItemKey :: expr v a
  , _unsafeDictItemColon :: Colon
  , _unsafeDictItemValue :: expr v a
  }
  | DictUnpack
  { _dictItemAnn :: Ann a
  , _unsafeDictItemUnpackWhitespace :: [Whitespace]
  , _unsafeDictItemUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (DictItem expr v) where
  annot :: forall a. Lens' (DictItem expr v a) (Ann a)
  annot = typed @(Ann a)

instance VFunctor DictItem where; vfmap = vfmapDefault
instance VFoldable DictItem where; vfoldMap = vfoldMapDefault
instance VTraversable DictItem where
  vtraverse f (DictItem a b c d) = (\b' -> DictItem a b' c) <$> f b <*> f d
  vtraverse f (DictUnpack a b c) = DictUnpack a b <$> f c

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (DictItem expr v a) where
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
data SubscriptItem expr (v :: [*]) a
  = SubscriptExpr (expr v a)
  | SubscriptSlice
      -- [expr]
      (Maybe (expr v a))
      -- ':' <spaces>
      Colon
      -- [expr]
      (Maybe (expr v a))
      -- [':' [expr]]
      (Maybe (Colon, Maybe (expr v a)))
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance VFunctor SubscriptItem where; vfmap = vfmapDefault
instance VFoldable SubscriptItem where; vfoldMap = vfoldMapDefault
instance VTraversable SubscriptItem where
  vtraverse f (SubscriptExpr a) = SubscriptExpr <$> f a
  vtraverse f (SubscriptSlice a b c d) =
    (\a' c' -> SubscriptSlice a' b c') <$>
    traverse f a <*>
    traverse f c <*>
    (traverse._2.traverse) f d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (SubscriptItem expr v a) where
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
data ListItem expr (v :: [*]) a
  = ListItem
  { _listItemAnn :: Ann a
  , _unsafeListItemValue :: expr v a
  }
  | ListUnpack
  { _listItemAnn :: Ann a
  , _unsafeListUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeListUnpackWhitespace :: [Whitespace]
  , _unsafeListUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (ListItem expr v) where
  annot :: forall a. Lens' (ListItem expr v a) (Ann a)
  annot = typed @(Ann a)

instance VFunctor ListItem where; vfmap = vfmapDefault
instance VFoldable ListItem where; vfoldMap = vfoldMapDefault
instance VTraversable ListItem where
  vtraverse f (ListItem a b) = ListItem a <$> f b
  vtraverse f (ListUnpack a b c d) = ListUnpack a b c <$> f d

instance HasExprs expr expr => HasExprs (ListItem expr) expr where
  _Exprs f (ListItem a b) = ListItem a <$> f b
  _Exprs f (ListUnpack a b c d) = ListUnpack a b c <$> f d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (ListItem expr v a) where
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
data SetItem expr (v :: [*]) a
  = SetItem
  { _setItemAnn :: Ann a
  , _unsafeSetItemValue :: expr v a
  }
  | SetUnpack
  { _setItemAnn :: Ann a
  , _unsafeSetUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeSetUnpackWhitespace :: [Whitespace]
  , _unsafeSetUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (SetItem expr v) where
  annot :: forall a. Lens' (SetItem expr v a) (Ann a)
  annot = typed @(Ann a)

instance VFunctor SetItem where; vfmap = vfmapDefault
instance VFoldable SetItem where; vfoldMap = vfoldMapDefault
instance VTraversable SetItem where
  vtraverse f (SetItem a b) = SetItem a <$> f b
  vtraverse f (SetUnpack a b c d) = SetUnpack a b c <$> f d

instance HasExprs expr expr => HasExprs (SetItem expr) expr where
  _Exprs f (SetItem a b) = SetItem a <$> f b
  _Exprs f (SetUnpack a b c d) = SetUnpack a b c <$> f d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (SetItem expr v a) where
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
data TupleItem expr (v :: [*]) a
  = TupleItem
  { _tupleItemAnn :: Ann a
  , _unsafeTupleItemValue :: expr v a
  }
  | TupleUnpack
  { _tupleItemAnn :: Ann a
  , _unsafeTupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeTupleUnpackWhitespace :: [Whitespace]
  , _unsafeTupleUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (TupleItem expr v) where
  annot :: forall a. Lens' (TupleItem expr v a) (Ann a)
  annot = typed @(Ann a)

instance VFunctor TupleItem where; vfmap = vfmapDefault
instance VFoldable TupleItem where; vfoldMap = vfoldMapDefault
instance VTraversable TupleItem where
  vtraverse f (TupleItem a b) = TupleItem a <$> f b
  vtraverse f (TupleUnpack a b c d) = TupleUnpack a b c <$> f d

instance HasExprs expr expr => HasExprs (TupleItem expr) expr where
  _Exprs f (TupleItem a b) = TupleItem a <$> f b
  _Exprs f (TupleUnpack a b c d) = TupleUnpack a b c <$> f d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (TupleItem expr v a) where
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
data ExprF expr (v :: [*]) a
  -- | @()@
  --
  -- https://docs.python.org/3/reference/expressions.html#parenthesized-forms
  = Unit
  { _exprAnn :: Ann a
  , _unsafeUnitWhitespaceInner :: [Whitespace]
  , _unsafeUnitWhitespaceRight :: [Whitespace]
  }
  -- | @lambda x, y: x@
  --
  -- https://docs.python.org/3/reference/expressions.html#lambda
  | Lambda
  { _exprAnn :: Ann a
  , _unsafeLambdaWhitespace :: [Whitespace]
  , _unsafeLambdaArgs :: CommaSep (Param expr v a)
  , _unsafeLambdaColon :: Colon
  , _unsafeLambdaBody :: expr v a
  }
  -- | @yield@
  --
  -- @yield a@
  --
  -- @yield a, b@
  --
  -- https://docs.python.org/3/reference/expressions.html#yield-expressions
  | Yield
  { _exprAnn :: Ann a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeYieldValue :: CommaSep (expr v a)
  }
  -- | @yield from a@
  --
  -- https://docs.python.org/3/reference/expressions.html#yield-expressions
  | YieldFrom
  { _exprAnn :: Ann a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeFromWhitespace :: [Whitespace]
  , _unsafeYieldFromValue :: expr v a
  }
  -- | @a if b else c@
  --
  -- https://docs.python.org/3/reference/expressions.html#conditional-expressions
  | Ternary
  { _exprAnn :: Ann a
  -- expr
  , _unsafeTernaryValue :: expr v a
  -- 'if' spaces
  , _unsafeTernaryWhitespaceIf :: [Whitespace]
  -- expr
  , _unsafeTernaryCond :: expr v a
  -- 'else' spaces
  , _unsafeTernaryWhitespaceElse :: [Whitespace]
  -- expr
  , _unsafeTernaryElse :: expr v a
  }
  -- | @[a for b in c if d]@
  --
  -- https://docs.python.org/3/reference/expressions.html#list-displays
  | ListComp
  { _exprAnn :: Ann a
  -- [ spaces
  , _unsafeListCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeListCompValue :: Comprehension VIdentity expr v a
  -- ] spaces
  , _unsafeListCompWhitespaceRight :: [Whitespace]
  }
  -- | @[a, b, c]@
  --
  -- https://docs.python.org/3/reference/expressions.html#list-displays
  | List
  { _exprAnn :: Ann a
  -- [ spaces
  , _unsafeListWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeListValues :: Maybe (CommaSep1' (ListItem expr v a))
  -- ] spaces
  , _unsafeListWhitespaceRight :: [Whitespace]
  }
  -- | @{a: b for c in d if e}@
  --
  -- https://docs.python.org/3/reference/expressions.html#dictionary-displays
  | DictComp
  { _exprAnn :: Ann a
  -- { spaces
  , _unsafeDictCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeDictCompValue :: Comprehension DictItem expr v a
  -- } spaces
  , _unsafeDictCompWhitespaceRight :: [Whitespace]
  }
  -- | @{}@
  --
  -- @{a: 1, b: 2, c: 3}@
  --
  -- https://docs.python.org/3/reference/expressions.html#dictionary-displays
  | Dict
  { _exprAnn :: Ann a
  , _unsafeDictWhitespaceLeft :: [Whitespace]
  , _unsafeDictValues :: Maybe (CommaSep1' (DictItem expr v a))
  , _unsafeDictWhitespaceRight :: [Whitespace]
  }
  -- | @{a for b in c if d}@
  --
  -- https://docs.python.org/3/reference/expressions.html#set-displays
  | SetComp
  { _exprAnn :: Ann a
  -- { spaces
  , _unsafeSetCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeSetCompValue :: Comprehension SetItem expr v a
  -- } spaces
  , _unsafeSetCompWhitespaceRight :: [Whitespace]
  }
  -- | @{a, b, c}@
  --
  -- https://docs.python.org/3/reference/expressions.html#set-displays
  | Set
  { _exprAnn :: Ann a
  , _unsafeSetWhitespaceLeft :: [Whitespace]
  , _unsafeSetValues :: CommaSep1' (SetItem expr v a)
  , _unsafeSetWhitespaceRight :: [Whitespace]
  }
  -- | @a.b@
  --
  -- https://docs.python.org/3/reference/expressions.html#attribute-references
  | Deref
  { _exprAnn :: Ann a
  -- expr
  , _unsafeDerefValueLeft :: expr v a
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
  { _exprAnn :: Ann a
  -- expr
  , _unsafeSubscriptValueLeft :: expr v a
  -- [ spaces
  , _unsafeSubscriptWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeSubscriptValueRight :: CommaSep1' (SubscriptItem expr v a)
  -- ] spaces
  , _unsafeSubscriptWhitespaceRight :: [Whitespace]
  }
  -- | @f(x)@
  --
  -- https://docs.python.org/3/reference/expressions.html#calls
  | Call
  { _exprAnn :: Ann a
  -- expr
  , _unsafeCallFunction :: expr v a
  -- ( spaces
  , _unsafeCallWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeCallArguments :: Maybe (CommaSep1' (Arg expr v a))
  -- ) spaces
  , _unsafeCallWhitespaceRight :: [Whitespace]
  }
  -- | @None@
  --
  -- https://docs.python.org/3/library/constants.html#None
  | None
  { _exprAnn :: Ann a
  , _unsafeNoneWhitespace :: [Whitespace]
  }
  -- | @...@
  --
  -- https://docs.python.org/3/library/constants.html#Ellipsis
  | Ellipsis
  { _exprAnn :: Ann a
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
  | Binary
  { _exprAnn :: Ann a
  , _unsafeBinaryExprLeft :: expr v a
  , _unsafeBinaryOp :: BinOp a
  , _unsafeBinaryExprRight :: expr v a
  }
  -- | @-a@
  --
  -- @~a@
  --
  -- @+a@
  --
  -- https://docs.python.org/3/reference/expressions.html#unary-arithmetic-and-bitwise-operations
  | Unary
  { _exprAnn :: Ann a
  , _unsafeUnaryOp :: UnOp a
  , _unsafeUnaryValue :: expr v a
  }
  | Parens
  { _exprAnn :: Ann a
  -- ( spaces
  , _unsafeParensWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeParensValue :: expr v a
  -- ) spaces
  , _unsafeParensWhitespaceAfter :: [Whitespace]
  }
  -- | @a@
  --
  -- https://docs.python.org/3/reference/expressions.html#atom-identifiers
  | Ident
  { _exprAnn :: Ann a
  , _unsafeIdentValue :: Ident v a
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
  | Bool
  { _exprAnn :: Ann a
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
  { _exprAnn :: Ann a
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
  { _exprAnn :: Ann a
  -- expr
  , _unsafeTupleHead :: TupleItem expr v a
  -- , spaces
  , _unsafeTupleWhitespace :: Comma
  -- [exprs]
  , _unsafeTupleTail :: Maybe (CommaSep1' (TupleItem expr v a))
  }
  -- | @not a@
  --
  -- https://docs.python.org/3/reference/expressions.html#boolean-operations
  | Not
  { _exprAnn :: Ann a
  , _unsafeNotWhitespace :: [Whitespace]
  , _unsafeNotValue :: expr v a
  }
  -- | @(a for b in c)@
  --
  -- https://docs.python.org/3/reference/expressions.html#generator-expressions
  | Generator
  { _exprAnn :: Ann a
  , _unsafeGeneratorValue :: Comprehension VIdentity expr v a
  }
  -- | @await a@
  --
  -- https://docs.python.org/3/reference/expressions.html#await
  | Await
  { _exprAnn :: Ann a
  , _unsafeAwaitWhitespace :: [Whitespace]
  , _unsafeAwaitValue :: expr v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance VFunctor ExprF where; vfmap = vfmapDefault
instance VFoldable ExprF where; vfoldMap = vfoldMapDefault
instance VTraversable ExprF where
  vtraverse fun e =
    case e of
      Unit a b c -> pure $ Unit a b c
      Lambda a b c d e ->
        (\c' -> Lambda a b c' d) <$>
        traverse (vtraverse fun) c <*>
        fun e
      Yield a b c -> Yield a b <$> traverse fun c
      YieldFrom a b c d -> YieldFrom a b c <$> fun d
      Ternary a b c d e f ->
        (\b' d' -> Ternary a b' c d' e) <$> fun b <*> fun d <*> fun f
      None a b -> pure $ None a b
      Ellipsis a b -> pure $ Ellipsis a b
      List a b c d ->
        (\c' -> List a b c' d) <$>
        (traverse.traverse) (vtraverse fun) c
      ListComp a b c d ->
        (\c' -> ListComp a b c' d) <$>
        vtraverse fun c
      Deref a b c d ->
        (\b' -> Deref a b' c d) <$>
        fun b
      Subscript a b c d e ->
        (\b' d' -> Subscript a b' c d' e) <$>
        fun b <*>
        traverse (vtraverse fun) d
      Call a b c d e ->
        (\b' d' -> Call a b' c d' e) <$>
        fun b <*>
        (traverse.traverse) (vtraverse fun) d
      Binary a b c d ->
        (\b' -> Binary a b' c) <$>
        fun b <*>
        fun d
      Unary a b c -> Unary a b <$> fun c
      Parens a b c d -> (\c' -> Parens a b c' d) <$> fun c
      Ident a b -> pure $ Ident a b
      Int a b c -> pure $ Int a b c
      Float a b c -> pure $ Float a b c
      Imag a b c -> pure $ Imag a b c
      Bool a b c -> pure $ Bool a b c
      String a b -> pure $ String a b
      Not a b c -> Not a b <$> fun c
      Tuple a b c d ->
        (\b' -> Tuple a b' c) <$>
        vtraverse fun b <*>
        (traverse.traverse) (vtraverse fun) d
      DictComp a b c d ->
        (\c' -> DictComp a b c' d) <$>
        vtraverse fun c
      Dict a b c d ->
        (\c' -> Dict a b c' d) <$> (traverse.traverse) (vtraverse fun) c
      SetComp a b c d ->
        (\c' -> SetComp a b c' d) <$>
        vtraverse fun c
      Set a b c d ->
        (\c' -> Set a b c' d) <$> traverse (vtraverse fun) c
      Generator a b -> Generator a <$> vtraverse fun b
      Await a b c -> Await a b <$> fun c

instance HasAnn (ExprF expr v) where
  annot :: forall a. Lens' (ExprF expr v a) (Ann a)
  annot = typed @(Ann a)

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (ExprF expr v a) where
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
          Binary _ _ _ e -> e ^. trailingWhitespace
          Unary _ _ e -> e ^. trailingWhitespace
          Parens _ _ _ ws -> ws
          Ident _ a -> a ^. getting trailingWhitespace
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
          Binary a b c e -> Binary a (coerce b) c (e & trailingWhitespace .~ ws)
          Unary a b c -> Unary a b (c & trailingWhitespace .~ ws)
          Parens a b c _ -> Parens a b (coerce c) ws
          Ident a b -> Ident a $ b & trailingWhitespace .~ ws
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

instance IsString (ExprF expr '[] ()) where
  fromString s = Ident (Ann ()) $ MkIdent (Ann ()) s []

instance Num (Expr '[] ()) where
  fromInteger n
    | n >= 0 =
      VIn $
      Int (Ann ()) (IntLiteralDec (Ann ()) $ integralDecDigits n ^?! _Right) []
    | otherwise =
        VIn $
        Unary
          (Ann ())
          (Negate (Ann ()) [])
          (VIn $
           Int (Ann ()) (IntLiteralDec (Ann ()) $ integralDecDigits (-n) ^?! _Right) [])

  negate = VIn . Unary (Ann ()) (Negate (Ann ()) [])

  (+) a =
    VIn .
    Binary (Ann ())
      (a & trailingWhitespace .~ [Space])
      (Plus (Ann ()) [Space])
  (*) a =
    VIn .
    Binary (Ann ())
      (a & trailingWhitespace .~ [Space])
      (Multiply (Ann ()) [Space])
  (-) a =
    VIn .
    Binary (Ann ())
      (a & trailingWhitespace .~ [Space])
    (Minus (Ann ()) [Space])
  signum = undefined
  abs = undefined

instance HasExprs Expr Expr where
  _Exprs = id

class AsExpr outer inner | outer -> inner where
  -- |
  -- A faux-Prism for matching on the @Ident@ constructor of an 'Expr'.
  --
  -- It's not a Prism because:
  --
  -- When 'Control.Lens.Fold.preview'ing, it discards the 'Expr'\'s annotation, and when
  -- 'Control.Lens.Review.review'ing, it re-constructs an annotation from the supplied 'Language.Python.Syntax.Ident.Ident'
  --
  -- @'_Ident' :: 'Prism' (outer v a) (outer '[] a) ('Ident' v a) ('Ident' '[] a)@
  _Ident
    :: (Choice p, Applicative f)
    => p (Ident v a) (f (Ident '[] a))
    -> p (outer v a) (f (outer '[] a))
  _Int :: Prism (outer v a) (outer '[] a) (PyInt v a) (PyInt '[] a)
  _Float :: Prism (outer v a) (outer '[] a) (PyFloat v a) (PyFloat '[] a)
  _Imag :: Prism (outer v a) (outer '[] a) (Imag v a) (Imag '[] a)
  _Bool :: Prism (outer v a) (outer '[] a) (PyBool v a) (PyBool '[] a)
  _String :: Prism (outer v a) (outer '[] a) (PyString v a) (PyString '[] a)
  _Unit :: Prism (outer v a) (outer '[] a) (Unit v a) (Unit '[] a)
  _Lambda :: Prism (outer v a) (outer '[] a) (Lambda inner v a) (Lambda inner '[] a)
  _Yield :: Prism (outer v a) (outer '[] a) (Yield inner v a) (Yield inner '[] a)
  _YieldFrom
    :: Prism
         (outer v a)
         (outer '[] a)
         (YieldFrom inner v a)
         (YieldFrom inner '[] a)
  _Ternary
    :: Prism
         (outer v a)
         (outer '[] a)
         (Ternary inner v a)
         (Ternary inner '[] a)
  _ListComp
    :: Prism
         (outer v a)
         (outer '[] a)
         (ListComp inner v a)
         (ListComp inner '[] a)
  _Dict
    :: Prism
         (outer v a)
         (outer '[] a)
         (Dict inner v a)
         (Dict inner '[] a)
  _DictComp
    :: Prism
         (outer v a)
         (outer '[] a)
         (DictComp inner v a)
         (DictComp inner '[] a)
  _Set
    :: Prism
         (outer v a)
         (outer '[] a)
         (Set inner v a)
         (Set inner '[] a)
  _SetComp
    :: Prism
         (outer v a)
         (outer '[] a)
         (SetComp inner v a)
         (SetComp inner '[] a)
  _Deref
    :: Prism
         (outer v a)
         (outer '[] a)
         (Deref inner v a)
         (Deref inner '[] a)
  _Subscript
    :: Prism
         (outer v a)
         (outer '[] a)
         (Subscript inner v a)
         (Subscript inner '[] a)
  _Tuple
    :: Prism
        (outer v a)
        (outer '[] a)
        (Tuple inner v a)
        (Tuple inner '[] a)
  _Binary
    :: Prism
        (outer v a)
        (outer '[] a)
        (Binary inner v a)
        (Binary inner '[] a)
  _Unary
    :: Prism
        (outer v a)
        (outer '[] a)
        (Unary inner v a)
        (Unary inner '[] a)
  _Not
    :: Prism
        (outer v a)
        (outer '[] a)
        (Not inner v a)
        (Not inner '[] a)
  _Generator
    :: Prism
        (outer v a)
        (outer '[] a)
        (Generator inner v a)
        (Generator inner '[] a)
  _Await
    :: Prism
        (outer v a)
        (outer '[] a)
        (Await inner v a)
        (Await inner '[] a)
  _Parens
    :: Prism
        (outer v a)
        (outer '[] a)
        (Parens inner v a)
        (Parens inner '[] a)
  _None :: Prism (outer v a) (outer '[] a) (None v a) (None '[] a)
  _Ellipsis :: Prism (outer v a) (outer '[] a) (Ellipsis v a) (Ellipsis '[] a)
  _Call :: Prism (outer v a) (outer '[] a) (Call inner v a) (Call inner '[] a)
  _List
    :: Prism
        (outer v a)
        (outer '[] a)
        (List inner v a)
        (List inner '[] a)

instance Validated expr => AsExpr (ExprF expr) expr where
  _Ident =
    prism
      (\i -> Ident (i ^. annot) i)
      (\case
          Ident _ a -> Right a
          a -> Left $ a ^. unvalidated)
  _Tuple =
    prism
      (\(MkTuple a b c d) -> Tuple a b c d)
      (\case
          Tuple a b c d -> Right (MkTuple a b c d)
          a -> Left $ a ^. unvalidated)

  _None =
    prism
      (\(MkNone a b) -> None a b)
      (\case
          None a b -> Right (MkNone a b)
          a -> Left $ a ^. unvalidated)

  _Call =
    prism
      (\(MkCall a b c d e) -> Call a b c d e)
      (\case
          Call a b c d e -> Right $ MkCall a b c d e
          a -> Left $ a ^. unvalidated)

  _List =
    prism
      (\(MkList a b c d) -> List a b c d)
      (\case
          List a b c d -> Right (MkList a b c d)
          a -> Left $ a ^. unvalidated)

  _Int =
    prism
      (\(MkInt a b c) -> Int a b c)
      (\case
          Int a b c -> Right (MkInt a b c)
          a -> Left $ a ^. unvalidated)

  _Float =
    prism
      (\(MkFloat a b c) -> Float a b c)
      (\case
          Float a b c -> Right (MkFloat a b c)
          a -> Left $ a ^. unvalidated)

  _Imag =
    prism
      (\(MkImag a b c) -> Imag a b c)
      (\case
          Imag a b c -> Right (MkImag a b c)
          a -> Left $ a ^. unvalidated)

  _Bool =
    prism
      (\(MkBool a b c) -> Bool a b c)
      (\case
          Bool a b c -> Right (MkBool a b c)
          a -> Left $ a ^. unvalidated)

  _String =
    prism
      (\(MkString a b) -> String a b)
      (\case
          String a b -> Right (MkString a b)
          a -> Left $ a ^. unvalidated)

  _Unit =
    prism
      (\(MkUnit a b c) -> Unit a b c)
      (\case
          Unit a b c -> Right (MkUnit a b c)
          a -> Left $ a ^. unvalidated)

  _Lambda =
    prism
      (\(MkLambda a b c d e) -> Lambda a b c d e)
      (\case
          Lambda a b c d e -> Right $ MkLambda a b c d e
          a -> Left $ a ^. unvalidated)

  _Yield =
    prism
      (\(MkYield a b c) -> Yield a b c)
      (\case
          Yield a b c -> Right (MkYield a b c)
          a -> Left $ a ^. unvalidated)

  _YieldFrom =
    prism
      (\(MkYieldFrom a b c d) -> YieldFrom a b c d)
      (\case
          YieldFrom a b c d -> Right (MkYieldFrom a b c d)
          a -> Left $ a ^. unvalidated)

  _Ternary =
    prism
      (\(MkTernary a b c d e f) -> Ternary a b c d e f)
      (\case
          Ternary a b c d e f -> Right $ MkTernary a b c d e f
          a -> Left $ a ^. unvalidated)

  _ListComp =
    prism
      (\(MkListComp a b c d) -> ListComp a b c d)
      (\case
          ListComp a b c d -> Right (MkListComp a b c d)
          a -> Left $ a ^. unvalidated)

  _Dict =
    prism
      (\(MkDict a b c d) -> Dict a b c d)
      (\case
          Dict a b c d -> Right (MkDict a b c d)
          a -> Left $ a ^. unvalidated)

  _DictComp =
    prism
      (\(MkDictComp a b c d) -> DictComp a b c d)
      (\case
          DictComp a b c d -> Right (MkDictComp a b c d)
          a -> Left $ a ^. unvalidated)

  _Set =
    prism
      (\(MkSet a b c d) -> Set a b c d)
      (\case
          Set a b c d -> Right (MkSet a b c d)
          a -> Left $ a ^. unvalidated)

  _SetComp =
    prism
      (\(MkSetComp a b c d) -> SetComp a b c d)
      (\case
          SetComp a b c d -> Right (MkSetComp a b c d)
          a -> Left $ a ^. unvalidated)

  _Deref =
    prism
      (\(MkDeref a b c d) -> Deref a b c d)
      (\case
          Deref a b c d -> Right (MkDeref a b c d)
          a -> Left $ a ^. unvalidated)

  _Subscript =
    prism
      (\(MkSubscript a b c d e) -> Subscript a b c d e)
      (\case
          Subscript a b c d e -> Right (MkSubscript a b c d e)
          a -> Left $ a ^. unvalidated)

  _Binary =
    prism
      (\(MkBinary a b c d) -> Binary a b c d)
      (\case
          Binary a b c d -> Right (MkBinary a b c d)
          a -> Left $ a ^. unvalidated)

  _Unary =
    prism
      (\(MkUnary a b c) -> Unary a b c)
      (\case
          Unary a b c -> Right (MkUnary a b c)
          a -> Left $ a ^. unvalidated)

  _Not =
    prism
      (\(MkNot a b c) -> Not a b c)
      (\case
          Not a b c -> Right (MkNot a b c)
          a -> Left $ a ^. unvalidated)

  _Generator =
    prism
      (\(MkGenerator a b) -> Generator a b)
      (\case
          Generator a b -> Right (MkGenerator a b)
          a -> Left $ a ^. unvalidated)

  _Await =
    prism
      (\(MkAwait a b c) -> Await a b c)
      (\case
          Await a b c -> Right (MkAwait a b c)
          a -> Left $ a ^. unvalidated)

  _Parens =
    prism
      (\(MkParens a b c d) -> Parens a b c d)
      (\case
          Parens a b c d -> Right (MkParens a b c d)
          a -> Left $ a ^. unvalidated)

  _Ellipsis =
    prism
      (\(MkEllipsis a b) -> Ellipsis a b)
      (\case
          Ellipsis a b -> Right (MkEllipsis a b)
          a -> Left $ a ^. unvalidated)

instance AsExpr (expr (VFix expr)) (VFix expr') => AsExpr (VFix expr) (VFix expr') where
  _Ident = _Wrapped._Ident
  _Tuple = _Wrapped._Tuple
  _None = _Wrapped._None
  _Call = _Wrapped._Call
  _List = _Wrapped._List
  _Int = _Wrapped._Int
  _Float = _Wrapped._Float
  _Imag = _Wrapped._Imag
  _Bool = _Wrapped._Bool
  _String = _Wrapped._String
  _Unit = _Wrapped._Unit
  _Lambda = _Wrapped._Lambda
  _Yield = _Wrapped._Yield
  _YieldFrom = _Wrapped._YieldFrom
  _Ternary = _Wrapped._Ternary
  _ListComp = _Wrapped._ListComp
  _Dict = _Wrapped._Dict
  _DictComp = _Wrapped._DictComp
  _Set = _Wrapped._Set
  _SetComp = _Wrapped._SetComp
  _Deref = _Wrapped._Deref
  _Subscript = _Wrapped._Subscript
  _Binary = _Wrapped._Binary
  _Unary = _Wrapped._Unary
  _Not = _Wrapped._Not
  _Generator = _Wrapped._Generator
  _Await = _Wrapped._Await
  _Parens = _Wrapped._Parens
  _Ellipsis = _Wrapped._Ellipsis

-- |
-- @shouldGroupLeft op left@ returns true if @left@ needs to be parenthesised
-- when it is the left argument of @op@
shouldGroupLeft :: AsExpr expr expr => BinOp a -> expr v a -> Bool
shouldGroupLeft op left =
  let
    entry = lookupOpEntry op operatorTable

    lEntry =
      case vout left of
        Binary _ _ lOp _ -> Just $ lookupOpEntry lOp operatorTable
        _ -> Nothing

    leftf =
      case entry ^. opAssoc of
        R | Just (OpEntry _ prec R) <- lEntry -> prec <= entry ^. opPrec
        _ -> False

    leftf' =
      case (vout left, op) of
        (Unary{}, Exp{}) -> True
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
shouldGroupRight :: AsExpr expr expr => BinOp a -> expr v a -> Bool
shouldGroupRight op right =
  let
    entry = lookupOpEntry op operatorTable

    rEntry =
      case vout right of
        Binary _ _ rOp _ -> Just $ lookupOpEntry rOp operatorTable
        _ -> Nothing

    rightf =
      case entry ^. opAssoc of
        L | Just (OpEntry _ prec L) <- rEntry -> prec <= entry ^. opPrec
        _ -> False

    rightf' =
      case (op, vout right) of
        (_, Tuple{}) -> True
        (BoolAnd{}, Not{}) -> False
        (BoolOr{}, Not{}) -> False
        (_, Not{}) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (rEntry ^? _Just.opPrec)
  in
    rightf || rightf'

