{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language InstanceSigs, TypeApplications #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
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
    -- ** Base
    ExprF(..)
    -- ** 3.5
  , Expr35(..)
    -- ** 3.6
  , Expr36(..)
    -- * Parameters and arguments
  , module Language.Python.Syntax.Param
  , module Language.Python.Syntax.Arg
    -- * Comprehension expressions
  , module Language.Python.Syntax.Comprehension
    -- * Collection items
  , DictItem(..), ListItem(..), SetItem(..), TupleItem(..)
  )
where

import Control.Lens.Fold ((^?), (^?!))
import Control.Lens.Getter ((^.), getting, to)
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Prism (Prism, prism, _Just, _Right)
import Control.Lens.Review ((#), review)
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeWrapped)
import Control.Lens.Tuple (_2)
import Control.Lens.Wrapped (Unwrapped, _Wrapped, _Unwrapped)
import Data.Coerce (Coercible, coerce)
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
import Language.Python.Optics.Exprs
import Language.Python.Optics.Validated
import Language.Python.Syntax.Arg
import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comprehension
import Language.Python.Syntax.Dicts
import Language.Python.Syntax.Expr.Optics
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Lists
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Param
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Raw
import Language.Python.Syntax.Sets
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Subscript
import Language.Python.Syntax.Tuples
import Language.Python.Syntax.Whitespace

{-

[unsafeCoerce Validation]

We can't 'coerce' 'Expr's because the @v@ parameter is considered to have a
nominal role, due to datatypes like 'Comprehension'. We only ever use @v@ in
as a phantom in 'Expr', so 'unsafeCoerce :: Expr v a -> Expr '[] a' is safe.

-}
instance Validated e => Validated (ExprF e) where; unvalidated = to unsafeCoerce

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
  , _unsafeListCompValue :: Comprehension VIdentity expr v a -- ] spaces
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

instance
  (Validated expr, Validated (VariantV vs expr), CtorV vs ExprF) =>
  AsExpr (VariantV vs expr) expr where

  _Ident = _CtorV' @ExprF ._Ident
  _Tuple = _CtorV' @ExprF ._Tuple
  _None = _CtorV' @ExprF ._None
  _Call = _CtorV' @ExprF ._Call
  _List = _CtorV' @ExprF ._List
  _Bool = _CtorV' @ExprF ._Bool
  _String = _CtorV' @ExprF ._String
  _Unit = _CtorV' @ExprF ._Unit
  _Lambda = _CtorV' @ExprF ._Lambda
  _Yield = _CtorV' @ExprF ._Yield
  _YieldFrom = _CtorV' @ExprF ._YieldFrom
  _Ternary = _CtorV' @ExprF ._Ternary
  _ListComp = _CtorV' @ExprF ._ListComp
  _Dict = _CtorV' @ExprF ._Dict
  _DictComp = _CtorV' @ExprF ._DictComp
  _Set = _CtorV' @ExprF ._Set
  _SetComp = _CtorV' @ExprF ._SetComp
  _Deref = _CtorV' @ExprF ._Deref
  _Subscript = _CtorV' @ExprF ._Subscript
  _Binary = _CtorV' @ExprF ._Binary
  _Unary = _CtorV' @ExprF ._Unary
  _Not = _CtorV' @ExprF ._Not
  _Generator = _CtorV' @ExprF ._Generator
  _Await = _CtorV' @ExprF ._Await
  _Parens = _CtorV' @ExprF ._Parens
  _Ellipsis = _CtorV' @ExprF ._Ellipsis

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

newtype Expr35 v a = Expr35 { unExpr35 :: VFix (VariantV '[ExprF, PyInt35]) v a }
makeWrapped ''Expr35

instance Validated Expr35 where; unvalidated = to unsafeCoerce

py35
  :: VFunctor b
  => (forall expr. Validated expr => Prism (ExprF expr v a) (ExprF expr '[] a) (b expr v a) (b expr '[] a))
  -> Prism (Expr35 v a) (Expr35 '[] a) (b Expr35 v a) (b Expr35 '[] a)
py35 p = _Wrapped._Wrapped._CtorV' @ExprF .p.iso (vfmap Expr35) (vfmap unExpr35)

instance AsExpr Expr35 Expr35 where
  _Ident = _Wrapped._Wrapped._CtorV' @ExprF ._Ident
  _Tuple = py35 _Tuple
  _None = py35 _None
  _Call = py35 _Call
  _List = py35 _List
  _Bool = py35 _Bool
  _String = py35 _String
  _Unit = py35 _Unit
  _Lambda = py35 _Lambda
  _Yield = py35 _Yield
  _YieldFrom = py35 _YieldFrom
  _Ternary = py35 _Ternary
  _ListComp = py35 _ListComp
  _Dict = py35 _Dict
  _DictComp = py35 _DictComp
  _Set = py35 _Set
  _SetComp = py35 _SetComp
  _Deref = py35 _Deref
  _Subscript = py35 _Subscript
  _Binary = py35 _Binary
  _Unary = py35 _Unary
  _Not = py35 _Not
  _Generator = py35 _Generator
  _Await = py35 _Await
  _Ellipsis = py35 _Ellipsis
  _Parens = py35 _Parens

newtype Expr36 v a = Expr36 { unExpr36 :: VFix (VariantV '[ExprF, PyInt36]) v a }

exprFromInteger
  :: ( AsPyInt (e (VFix e)) int
     , AsIntLiteral (int ()) f ()
     , AsExpr (VFix e) (VFix e)
     )
  => (forall a. NonEmpty a -> f a)
  -> Integer
  -> Raw (VFix e)
exprFromInteger l n
  | n >= 0 =
    _Int #
    MkInt
      (Ann ())
      (_IntLiteralDec # (Ann (), integralDecDigits n ^?! _Right.to l))
      []
  | otherwise =
      _Unary #
      MkUnary
        (Ann ())
        (Negate (Ann ()) [])
        (_Int #
         MkInt
          (Ann ())
          (_IntLiteralDec # (Ann (), integralDecDigits (-n) ^?! _Right.to l))
          [])

exprNegate :: AsExpr (VFix e) (VFix e) => Raw (VFix e) -> Raw (VFix e)
exprNegate = review _Unary . MkUnary (Ann ()) (Negate (Ann ()) [])

exprPlus
  :: ( HasTrailingWhitespace (Raw (VFix e))
     , AsExpr (VFix e) (VFix e)
     )
  => Raw (VFix e)
  -> Raw (VFix e)
  -> Raw (VFix e)
exprPlus a =
  review _Binary .
  MkBinary
    (Ann ())
    (a & trailingWhitespace .~ [Space])
    (Plus (Ann ()) [Space])

exprMultiply
  :: ( HasTrailingWhitespace (Raw (VFix e))
     , AsExpr (VFix e) (VFix e)
     )
  => Raw (VFix e)
  -> Raw (VFix e)
  -> Raw (VFix e)
exprMultiply a =
  review _Binary .
  MkBinary
    (Ann ())
    (a & trailingWhitespace .~ [Space])
    (Multiply (Ann ()) [Space])

exprMinus
  :: ( HasTrailingWhitespace (Raw (VFix e))
     , AsExpr (VFix e) (VFix e)
     )
  => Raw (VFix e)
  -> Raw (VFix e)
  -> Raw (VFix e)
exprMinus a =
  review _Binary .
  MkBinary
    (Ann ())
    (a & trailingWhitespace .~ [Space])
    (Minus (Ann ()) [Space])

instance Num (Raw Expr35) where
  fromInteger = Expr35 . exprFromInteger id
  negate = Expr35 . exprNegate . unExpr35
  (+) (Expr35 a) = Expr35 . exprPlus a . unExpr35
  (*) (Expr35 a) = Expr35 . exprMultiply a . unExpr35
  (-) (Expr35 a) = Expr35 . exprMinus a . unExpr35
  signum = undefined
  abs = undefined

{-

instance Num (Raw (VFix (VariantV vs))) where
  fromInteger n
    | n >= 0 =
      _Int # (Ann (), _IntLiteralDec # (Ann (), integralDecDigits n ^?! _Right), [])
    | otherwise =
        _Unary #
        MkUnary
          (Ann ())
          (Negate (Ann ()) [])
          (_Int #
           ( Ann ()
           , _IntLiteralDec # (Ann (), integralDecDigits (-n) ^?! _Right)
           , []
           ))

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

instance HasExprs Expr35 Expr35 where
  _Exprs = id

instance HasExprs Expr36 Expr36 where
  _Exprs = id

-}
