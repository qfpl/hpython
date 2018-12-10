{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language InstanceSigs, ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Optics
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Optics for manipulating Python syntax trees

-}

module Language.Python.Optics
  ( module Language.Python.Optics.Validated
    -- * Indentation
  , module Language.Python.Optics.Indents
    -- * Newlines
  , module Language.Python.Optics.Newlines
    -- * Simple statements
    -- ** Assignment
  , assignTargets
    -- * Compound statements
    -- ** Function defintions
  , _Fundef
    -- ** Class defintions
  , _ClassDef
    -- ** @while@ statements
  , _While
    -- ** @for@ statements
  , _For
    -- ** @with@ statements
  , _With
    -- ** @if@ statements
  , _If
  , _Elif
    -- ** @try@ statements
  , _TryExcept
  , _TryFinally
  , _Finally
  , _Except
  , AsTry(..)
    -- ** @else@
  , _Else
    -- * Parameters
  , _PositionalParam
  , _KeywordParam
  , _UnnamedStarParam
  , _StarParam
    -- * Expressions
    -- ** Identifiers
  , _Ident
    -- ** @None@
  , _None
    -- ** Function calls
  , _Call
    -- ** Tuples
  , _Tuple
  , _TupleUnpack
  , tupleItems
    -- ** Lists
  , _List
  , _ListUnpack
  , listItems
  )
where

import Control.Lens.Getter ((^.), view)
import Control.Lens.Iso (Iso', iso, from)
import Control.Lens.Traversal (Traversal)
import Control.Lens.Prism (Prism, prism)

import Language.Python.Optics.Indents
import Language.Python.Optics.Newlines
import Language.Python.Optics.Validated
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Types
import Language.Python.Syntax.Whitespace

_TupleUnpack :: Prism (TupleItem v a) (TupleItem '[] a) (TupleUnpack v a) (TupleUnpack '[] a)
_TupleUnpack =
  prism
    (\(MkTupleUnpack a b c d) -> TupleUnpack a b c d)
    (\case
       TupleUnpack a b c d -> Right $ MkTupleUnpack a b c d
       a -> Left $ a ^. unvalidated)

_Tuple :: Prism (Expr v a) (Expr '[] a) (Tuple v a) (Tuple '[] a)
_Tuple =
  prism
    (\(MkTuple a b c d) -> Tuple a b c d)
    (\case
        Tuple a b c d -> Right (MkTuple a b c d)
        a -> Left $ a ^. unvalidated)

tupleItems :: Traversal (Tuple v a) (Tuple '[] a) (TupleItem v a) (TupleItem '[] a)
tupleItems f (MkTuple a b c d) =
  (\b' d' -> MkTuple a b' c d') <$>
  f b <*>
  (traverse.traverse) f d

_ListUnpack :: Prism (ListItem v a) (ListItem '[] a) (ListUnpack v a) (ListUnpack '[] a)
_ListUnpack =
  prism
    (\(MkListUnpack a b c d) -> ListUnpack a b c d)
    (\case
       ListUnpack a b c d -> Right $ MkListUnpack a b c d
       a -> Left $ a ^. unvalidated)

_List :: Prism (Expr v a) (Expr '[] a) (List v a) (List '[] a)
_List =
  prism
    (\(MkList a b c d) -> List a b c d)
    (\case
        List a b c d -> Right (MkList a b c d)
        a -> Left $ a ^. unvalidated)

listItems :: Traversal (List v a) (List '[] a) (ListItem v a) (ListItem '[] a)
listItems f (MkList a b c d) =
  (\c' -> MkList a b c' d) <$>
  (traverse.traverse) f c

_None :: Prism (Expr v a) (Expr '[] a) (None v a) (None '[] a)
_None =
  prism
    (\(MkNone a b) -> None a b)
    (\case
        None a b -> Right (MkNone a b)
        a -> Left $ a ^. unvalidated)

_KeywordParam
  :: Prism
       (Param v a)
       (Param '[] a)
       (KeywordParam v a)
       (KeywordParam '[] a)
_KeywordParam =
  prism
    (\(MkKeywordParam a b c d e) -> KeywordParam a b c d e)
    (\case
        KeywordParam a b c d e -> Right (MkKeywordParam a b c d e)
        a -> Left $ a ^. unvalidated)

_PositionalParam
  :: Prism
       (Param v a)
       (Param '[] a)
       (PositionalParam v a)
       (PositionalParam '[] a)
_PositionalParam =
  prism
    (\(MkPositionalParam a b c) -> PositionalParam a b c)
    (\case
        PositionalParam a b c -> Right (MkPositionalParam a b c)
        a -> Left $ a ^. unvalidated)

_StarParam
  :: Prism
       (Param v a)
       (Param '[] a)
       (StarParam v a)
       (StarParam '[] a)
_StarParam =
  prism
    (\(MkStarParam a b c d) -> StarParam a b c d)
    (\case
        StarParam a b c d -> Right (MkStarParam a b c d)
        a -> Left $ a ^. unvalidated)

_UnnamedStarParam
  :: Prism
       (Param v a)
       (Param '[] a)
       (UnnamedStarParam v a)
       (UnnamedStarParam '[] a)
_UnnamedStarParam =
  prism
    (\(MkUnnamedStarParam a b) -> UnnamedStarParam a b)
    (\case
        UnnamedStarParam a b -> Right (MkUnnamedStarParam a b)
        a -> Left $ a ^. unvalidated)

_Fundef
  :: Prism
       (Statement v a)
       (Statement '[] a)
       (Fundef v a)
       (Fundef '[] a)
_Fundef =
  prism
    (\(MkFundef idnt a b c d e f g h i j) ->
       CompoundStatement (Fundef idnt a b c d e f g h i j))
    (\case
        CompoundStatement (Fundef idnt a b c d e f g h i j) ->
          Right $ MkFundef idnt a b c d e f g h i j
        a -> Left $ a ^. unvalidated)

_While
  :: Prism
       (Statement v a)
       (Statement '[] a)
       (While v a)
       (While '[] a)
_While =
  prism
    (\(MkWhile a b c d e f) ->
       CompoundStatement (While a b c d e $ view _Else <$> f))
    (\case
        CompoundStatement (While a b c d e f) ->
          Right . MkWhile a b c d e $ view (from _Else) <$> f
        a -> Left $ a ^. unvalidated)

_Else :: Iso' (Else v a) (Indents a, [Whitespace], Suite v a)
_Else = iso (\(MkElse a b c) -> (a, b, c)) (\(a, b, c) -> MkElse a b c)

_Elif :: Iso' (Elif v a) (Indents a, [Whitespace], Expr v a, Suite v a)
_Elif = iso (\(MkElif a b c d) -> (a, b, c, d)) (\(a, b, c, d) -> MkElif a b c d)

_Finally :: Iso' (Finally v a) (Indents a, [Whitespace], Suite v a)
_Finally = iso (\(MkFinally a b c) -> (a, b, c)) (\(a, b, c) -> MkFinally a b c)

_Except :: Iso' (Except v a) (Indents a, [Whitespace], Maybe (ExceptAs v a), Suite v a)
_Except = iso (\(MkExcept a b c d) -> (a, b, c, d)) (\(a, b, c, d) -> MkExcept a b c d)

_If :: Prism (Statement v a) (Statement '[] a) (If v a) (If '[] a)
_If =
  prism
    (\(MkIf a b c d e f g) ->
       CompoundStatement (If a b c d e (view _Elif <$> f) (view _Else <$> g)))
    (\case
        CompoundStatement (If a b c d e f g) ->
          Right $ MkIf a b c d e (view (from _Elif) <$> f) (view (from _Else) <$> g)
        a -> Left $ a ^. unvalidated)

class AsTry s where
  _Try :: Prism (Statement v a) (Statement '[] a) (s v a) (s '[] a)

instance AsTry TryExcept where
  _Try = _TryExcept

instance AsTry TryFinally where
  _Try = _TryFinally

_TryExcept :: Prism (Statement v a) (Statement '[] a) (TryExcept v a) (TryExcept '[] a)
_TryExcept =
  prism
    (\(MkTryExcept a b c d e f g) ->
       CompoundStatement $
       TryExcept a b c d (view _Except <$> e) (view _Else <$> f) (view _Finally <$> g))
    (\case
        CompoundStatement (TryExcept a b c d e f g) ->
          Right $
          MkTryExcept a b c d
            (view (from _Except) <$> e)
            (view (from _Else) <$> f)
            (view (from _Finally) <$> g)
        a -> Left $ a ^. unvalidated)

_TryFinally :: Prism (Statement v a) (Statement '[] a) (TryFinally v a) (TryFinally '[] a)
_TryFinally =
  prism
    (\(MkTryFinally a b c d e) ->
       CompoundStatement $ (\(x, y, z) -> TryFinally a b c d x y z) (e ^. _Finally))
    (\case
        CompoundStatement (TryFinally a b c d e f g) ->
          Right $ MkTryFinally a b c d ((e, f, g) ^. from _Finally)
        a -> Left $ a ^. unvalidated)

_For :: Prism (Statement v a) (Statement '[] a) (For v a) (For '[] a)
_For =
  prism
    (\(MkFor a b c d e f g h i) ->
       CompoundStatement (For a b c d e f g h (view _Else <$> i)))
    (\case
        CompoundStatement (For a b c d e f g h i) ->
          Right $ MkFor a b c d e f g h (view (from _Else) <$> i)
        a -> Left $ a ^. unvalidated)

_Call :: Prism (Expr v a) (Expr '[] a) (Call v a) (Call '[] a)
_Call =
  prism
    (\(MkCall a b c d e) -> Call a b c d e)
    (\case
        Call a b c d e -> Right $ MkCall a b c d e
        a -> Left $ a ^. unvalidated)

_ClassDef :: Prism (Statement v a) (Statement '[] a) (ClassDef v a) (ClassDef '[] a)
_ClassDef =
  prism
    (\(MkClassDef a b c d e f g) -> CompoundStatement $ ClassDef a b c d e f g)
    (\case
        CompoundStatement (ClassDef a b c d e f g) -> Right $ MkClassDef a b c d e f g
        a -> Left $ a ^. unvalidated)

_With :: Prism (Statement v a) (Statement '[] a) (With v a) (With '[] a)
_With =
  prism
    (\(MkWith a b c d e f) -> CompoundStatement $ With a b c d e f)
    (\case
        CompoundStatement (With a b c d e f) -> Right $ MkWith a b c d e f
        a -> Left $ a ^. unvalidated)

_Ident :: Prism (Expr v a) (Expr '[] a) (Ident v a) (Ident '[] a)
_Ident =
  prism
    Ident
    (\case
        Ident a -> Right a
        a -> Left $ a ^. unvalidated)

assignTargets :: Traversal (Expr v a) (Expr '[] a) (Ident v a) (Ident '[] a)
assignTargets f e =
  case e of
    List a b c d -> (\c' -> List a b c' d) <$> (traverse.traverse._Exprs.assignTargets) f c
    Parens a b c d -> (\c' -> Parens a b c' d) <$> assignTargets f c
    Ident a -> Ident <$> f a
    Tuple a b c d ->
      (\b' d' -> Tuple a b' c d') <$>
      (_Exprs.assignTargets) f b <*>
      (traverse.traverse._Exprs.assignTargets) f d
    Unit{} -> pure $ e ^. unvalidated
    Lambda{} -> pure $ e ^. unvalidated
    Yield{} -> pure $ e ^. unvalidated
    YieldFrom{} -> pure $ e ^. unvalidated
    Ternary{} -> pure $ e ^. unvalidated
    ListComp{} -> pure $ e ^. unvalidated
    Deref{} -> pure $ e ^. unvalidated
    Subscript{} -> pure $ e ^. unvalidated
    Call{} -> pure $ e ^. unvalidated
    None{} -> pure $ e ^. unvalidated
    Ellipsis{} -> pure $ e ^. unvalidated
    BinOp{} -> pure $ e ^. unvalidated
    UnOp{} -> pure $ e ^. unvalidated
    Int{} -> pure $ e ^. unvalidated
    Float{} -> pure $ e ^. unvalidated
    Imag{} -> pure $ e ^. unvalidated
    Bool{} -> pure $ e ^. unvalidated
    String{} -> pure $ e ^. unvalidated
    Not{} -> pure $ e ^. unvalidated
    DictComp{} -> pure $ e ^. unvalidated
    Dict{} -> pure $ e ^. unvalidated
    SetComp{} -> pure $ e ^. unvalidated
    Set{} -> pure $ e ^. unvalidated
    Generator{} -> pure $ e ^. unvalidated
    Await{} -> pure $ e ^. unvalidated
