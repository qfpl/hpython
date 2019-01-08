{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language ExistentialQuantification #-}
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
    Expr, ExprF(..), exprAnn, shouldGroupLeft, shouldGroupRight
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
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Data.VFix
import Data.VIdentity
import Data.VTraversable
import Language.Python.Optics.Exprs (HasExprs(..))
import Language.Python.Optics.Validated (Validated(..))
import Language.Python.Syntax.Arg
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

type Expr = VFix ExprF

-- | @a : b@ or @**a@
--
-- Used to construct dictionaries, e.g. @{ 1: a, 2: b, **c }@
--
-- https://docs.python.org/3/reference/expressions.html#dictionary-displays
data DictItem expr (v :: [*]) a
  = DictItem
  { _dictItemAnn :: a
  , _unsafeDictItemKey :: expr v a
  , _unsafeDictItemColon :: Colon
  , _unsafeDictItemValue :: expr v a
  }
  | DictUnpack
  { _dictItemAnn :: a
  , _unsafeDictItemUnpackWhitespace :: [Whitespace]
  , _unsafeDictItemUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
  deriving (Eq, Show, Functor, Foldable, Traversable)

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
  { _listItemAnn :: a
  , _unsafeListItemValue :: expr v a
  }
  | ListUnpack
  { _listItemAnn :: a
  , _unsafeListUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeListUnpackWhitespace :: [Whitespace]
  , _unsafeListUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
  { _setItemAnn :: a
  , _unsafeSetItemValue :: expr v a
  }
  | SetUnpack
  { _setItemAnn :: a
  , _unsafeSetUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeSetUnpackWhitespace :: [Whitespace]
  , _unsafeSetUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
  { _tupleItemAnn :: a
  , _unsafeTupleItemValue :: expr v a
  }
  | TupleUnpack
  { _tupleItemAnn :: a
  , _unsafeTupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeTupleUnpackWhitespace :: [Whitespace]
  , _unsafeTupleUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
  { _unsafeExprAnn :: a
  , _unsafeUnitWhitespaceInner :: [Whitespace]
  , _unsafeUnitWhitespaceRight :: [Whitespace]
  }
  -- | @lambda x, y: x@
  --
  -- https://docs.python.org/3/reference/expressions.html#lambda
  | Lambda
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeYieldValue :: CommaSep (expr v a)
  }
  -- | @yield from a@
  --
  -- https://docs.python.org/3/reference/expressions.html#yield-expressions
  | YieldFrom
  { _unsafeExprAnn :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeFromWhitespace :: [Whitespace]
  , _unsafeYieldFromValue :: expr v a
  }
  -- | @a if b else c@
  --
  -- https://docs.python.org/3/reference/expressions.html#conditional-expressions
  | Ternary
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  , _unsafeDictWhitespaceLeft :: [Whitespace]
  , _unsafeDictValues :: Maybe (CommaSep1' (DictItem expr v a))
  , _unsafeDictWhitespaceRight :: [Whitespace]
  }
  -- | @{a for b in c if d}@
  --
  -- https://docs.python.org/3/reference/expressions.html#set-displays
  | SetComp
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  , _unsafeSetWhitespaceLeft :: [Whitespace]
  , _unsafeSetValues :: CommaSep1' (SetItem expr v a)
  , _unsafeSetWhitespaceRight :: [Whitespace]
  }
  -- | @a.b@
  --
  -- https://docs.python.org/3/reference/expressions.html#attribute-references
  | Deref
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  , _unsafeNoneWhitespace :: [Whitespace]
  }
  -- | @...@
  --
  -- https://docs.python.org/3/library/constants.html#Ellipsis
  | Ellipsis
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  , _unsafeUnaryOp :: UnOp a
  , _unsafeUnaryValue :: expr v a
  }
  | Parens
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  , _unsafeFloatValue :: FloatLiteral a
  , _unsafeFloatWhitespace :: [Whitespace]
  }
  -- | @10j@
  --
  -- @5.j@
  --
  -- https://docs.python.org/3/reference/lexical_analysis.html#floating-point-literals
  | Imag
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  , _unsafeNotWhitespace :: [Whitespace]
  , _unsafeNotValue :: expr v a
  }
  -- | @(a for b in c)@
  --
  -- https://docs.python.org/3/reference/expressions.html#generator-expressions
  | Generator
  { _unsafeExprAnn :: a
  , _unsafeGeneratorValue :: Comprehension VIdentity expr v a
  }
  -- | @await a@
  --
  -- https://docs.python.org/3/reference/expressions.html#await
  | Await
  { _unsafeExprAnn :: a
  , _unsafeAwaitWhitespace :: [Whitespace]
  , _unsafeAwaitValue :: expr v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

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
      Ident a -> pure $ Ident a
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

-- | Lens on the top-level annotation in an expression
exprAnn :: Lens' (ExprF expr v a) a
exprAnn =
  lens
    (\case
        Unit a _ _ -> a
        Lambda a _ _ _ _ -> a
        Yield a _ _ -> a
        YieldFrom a _ _ _ -> a
        Ternary a _ _ _ _ _ -> a
        None a _ -> a
        Ellipsis a _ -> a
        List a _ _ _ -> a
        ListComp a _ _ _ -> a
        Deref a _ _ _ -> a
        Subscript a _ _ _ _ -> a
        Call a _ _ _ _ -> a
        Binary a _ _ _ -> a
        Unary a _ _ -> a
        Parens a _ _ _ -> a
        Ident a -> a ^. identAnn
        Int a _ _ -> a
        Float a _ _ -> a
        Imag a _ _ -> a
        Bool a _ _ -> a
        String a _ -> a
        Not a _ _ -> a
        Tuple a _ _ _ -> a
        DictComp a _ _ _ -> a
        Dict a _ _ _ -> a
        SetComp a _ _ _ -> a
        Set a _ _ _ -> a
        Generator a _ -> a
        Await a _ _ -> a)
    (\e ann ->
      case e of
        Unit _ a b -> Unit ann a b
        Lambda _ a b c d -> Lambda ann a b c d
        Yield _ a b -> Yield ann a b
        YieldFrom ann a b c -> YieldFrom ann a b c
        Ternary ann a b c d e -> Ternary ann a b c d e
        None _ a -> None ann a
        Ellipsis _ a -> Ellipsis ann a
        List _ a b c -> List ann a b c
        ListComp _ a b c -> ListComp ann a b c
        Deref _ a b c -> Deref ann a b c
        Subscript _ a b c d -> Subscript ann a b c d
        Call _ a b c d -> Call ann a b c d
        Binary _ a b c -> Binary ann a b c
        Unary _ a b -> Unary ann a b
        Parens _ a b c -> Parens ann a b c
        Ident a -> Ident $ a & identAnn .~ ann
        Int _ a b -> Int ann a b
        Float _ a b -> Float ann a b
        Imag _ a b -> Imag ann a b
        Bool _ a b -> Bool ann a b
        String _ a -> String ann a
        Not _ a b -> Not ann a b
        Tuple _ a b c -> Tuple ann a b c
        DictComp _ a b c -> DictComp ann a b c
        Dict _ a b c -> Dict ann a b c
        SetComp _ a b c -> SetComp ann a b c
        Set _ a b c -> Set ann a b c
        Generator _ a -> Generator ann a
        Await _ a b -> Not ann a b)

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
          Binary a b c e -> Binary a (coerce b) c (e & trailingWhitespace .~ ws)
          Unary a b c -> Unary a b (c & trailingWhitespace .~ ws)
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

instance IsString (ExprF expr '[] ()) where
  fromString s = Ident $ MkIdent () s []

instance Num (Expr '[] ()) where
  fromInteger n
    | n >= 0 = VIn $ Int () (IntLiteralDec () $ integralDecDigits n ^?! _Right) []
    | otherwise =
        VIn $
        Unary
          ()
          (Negate () [])
          (VIn $ Int () (IntLiteralDec () $ integralDecDigits (-n) ^?! _Right) [])

  negate = VIn . Unary () (Negate () [])

  (+) a = VIn . Binary () (a & trailingWhitespace .~ [Space]) (Plus () [Space])
  (*) a = VIn . Binary () (a & trailingWhitespace .~ [Space]) (Multiply () [Space])
  (-) a = VIn . Binary () (a & trailingWhitespace .~ [Space]) (Minus () [Space])
  signum = undefined
  abs = undefined

instance HasExprs Expr Expr where
  _Exprs = id

-- |
-- @shouldGroupLeft op left@ returns true if @left@ needs to be parenthesised
-- when it is the left argument of @op@
shouldGroupLeft :: BinOp a -> Expr v a -> Bool
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
shouldGroupRight :: BinOp a -> Expr v a -> Bool
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

