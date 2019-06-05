{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language InstanceSigs, ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Optics
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Optics for manipulating Python syntax trees

-}

module Language.Python.Optics
  ( -- * Identifiers
    module Language.Python.Optics.Idents
    -- * Indentation
  , module Language.Python.Optics.Indents
    -- * Newlines
  , module Language.Python.Optics.Newlines
    -- * Simple statements
    -- ** Assignment
  , assignTargets
    -- * Compound statements
  , HasCompoundStatement(..)
    -- ** Function definitions
  , HasFundef(..)
    -- ** Class defintions
  , HasClassDef(..)
    -- ** @while@ statements
  , HasWhile(..)
    -- ** @for@ statements
  , HasFor(..)
    -- ** @with@ statements
  , HasWith(..)
    -- ** @if@ statements
  , HasIf(..)
  , _Elif
    -- ** @try@ statements
  , HasTryExcept(..)
  , HasTryFinally(..)
  , _Finally
  , _Except
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
import Control.Lens.Traversal (Traversal')
import Control.Lens.Prism (Choice, Prism', prism)

import Language.Python.Optics.Idents
import Language.Python.Optics.Indents
import Language.Python.Optics.Newlines
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Types
import Language.Python.Syntax.Whitespace

_TupleUnpack :: Prism' (TupleItem a) (TupleUnpack a)
_TupleUnpack =
  prism
    (\(MkTupleUnpack a b c d) -> TupleUnpack a b c d)
    (\case
       TupleUnpack a b c d -> Right $ MkTupleUnpack a b c d
       a -> Left a)

_Tuple :: Prism' (Expr a) (Tuple a)
_Tuple =
  prism
    (\(MkTuple a b c d) -> Tuple a b c d)
    (\case
        Tuple a b c d -> Right (MkTuple a b c d)
        a -> Left a)

tupleItems :: Traversal' (Tuple a) (TupleItem a)
tupleItems f (MkTuple a b c d) =
  (\b' d' -> MkTuple a b' c d') <$>
  f b <*>
  (traverse.traverse) f d

_ListUnpack :: Prism' (ListItem a) (ListUnpack a)
_ListUnpack =
  prism
    (\(MkListUnpack a b c d) -> ListUnpack a b c d)
    (\case
       ListUnpack a b c d -> Right $ MkListUnpack a b c d
       a -> Left a)

_List :: Prism' (Expr a) (List a)
_List =
  prism
    (\(MkList a b c d) -> List a b c d)
    (\case
        List a b c d -> Right (MkList a b c d)
        a -> Left a)

listItems :: Traversal' (List a) (ListItem a)
listItems f (MkList a b c d) =
  (\c' -> MkList a b c' d) <$>
  (traverse.traverse) f c

_None :: Prism' (Expr a) (None a)
_None =
  prism
    (\(MkNone a b) -> None a b)
    (\case
        None a b -> Right (MkNone a b)
        a -> Left a)

_KeywordParam :: Prism' (Param a) (KeywordParam a)
_KeywordParam =
  prism
    (\(MkKeywordParam a b c d e) -> KeywordParam a b c d e)
    (\case
        KeywordParam a b c d e -> Right (MkKeywordParam a b c d e)
        a -> Left a)

_PositionalParam :: Prism' (Param a) (PositionalParam a)
_PositionalParam =
  prism
    (\(MkPositionalParam a b c) -> PositionalParam a b c)
    (\case
        PositionalParam a b c -> Right (MkPositionalParam a b c)
        a -> Left a)

_StarParam :: Prism' (Param a) (StarParam a)
_StarParam =
  prism
    (\(MkStarParam a b c d) -> StarParam a b c d)
    (\case
        StarParam a b c d -> Right (MkStarParam a b c d)
        a -> Left a)

_UnnamedStarParam :: Prism' (Param a) (UnnamedStarParam a)
_UnnamedStarParam =
  prism
    (\(MkUnnamedStarParam a b) -> UnnamedStarParam a b)
    (\case
        UnnamedStarParam a b -> Right (MkUnnamedStarParam a b)
        a -> Left a)

class HasCompoundStatement s where
  _CompoundStatement :: Prism' (s a) (CompoundStatement a)

instance HasCompoundStatement CompoundStatement where
  _CompoundStatement = id

instance HasCompoundStatement Statement where
  _CompoundStatement =
    prism
      CompoundStatement
      (\case
          CompoundStatement a -> Right a
          a -> Left a)

class HasFundef s where
  _Fundef :: Prism' (s a) (Fundef a)

instance HasFundef Fundef where
  _Fundef = id

instance HasFundef CompoundStatement where
  _Fundef =
    prism
      (\(MkFundef idnt a b c d e f g h i j) ->
         Fundef idnt a b c d e f g h i j)
      (\case
          Fundef idnt a b c d e f g h i j ->
            Right $ MkFundef idnt a b c d e f g h i j
          a -> Left a)

instance HasFundef Statement where
  _Fundef = _CompoundStatement._Fundef

class HasWhile s where
  _While :: Prism' (s a) (While a)

instance HasWhile While where
  _While = id

instance HasWhile CompoundStatement where
  _While =
    prism
      (\(MkWhile a b c d e f) ->
        While a b c d e $ view _Else <$> f)
      (\case
          While a b c d e f ->
            Right . MkWhile a b c d e $ view (from _Else) <$> f
          a -> Left a)

instance HasWhile Statement where
  _While = _CompoundStatement._While

_Else :: Iso' (Else a) (Indents a, [Whitespace], Suite a)
_Else = iso (\(MkElse a b c) -> (a, b, c)) (\(a, b, c) -> MkElse a b c)

_Elif :: Iso' (Elif a) (Indents a, [Whitespace], Expr a, Suite a)
_Elif = iso (\(MkElif a b c d) -> (a, b, c, d)) (\(a, b, c, d) -> MkElif a b c d)

_Finally :: Iso' (Finally a) (Indents a, [Whitespace], Suite a)
_Finally = iso (\(MkFinally a b c) -> (a, b, c)) (\(a, b, c) -> MkFinally a b c)

_Except :: Iso' (Except a) (Indents a, [Whitespace], Maybe (ExceptAs a), Suite a)
_Except = iso (\(MkExcept a b c d) -> (a, b, c, d)) (\(a, b, c, d) -> MkExcept a b c d)

class HasIf s where
  _If :: Prism' (s a) (If a)

instance HasIf If where
  _If = id

instance HasIf CompoundStatement where
  _If =
    prism
      (\(MkIf a b c d e f g) ->
        If a b c d e (view _Elif <$> f) (view _Else <$> g))
      (\case
          If a b c d e f g ->
            Right $ MkIf a b c d e (view (from _Elif) <$> f) (view (from _Else) <$> g)
          a -> Left a)

instance HasIf Statement where
  _If = _CompoundStatement._If

class HasTryExcept s where
  _TryExcept :: Prism' (s a) (TryExcept a)

instance HasTryExcept TryExcept where
  _TryExcept = id

instance HasTryExcept CompoundStatement where
  _TryExcept =
    prism
      (\(MkTryExcept a b c d e f g) ->
        TryExcept a b c d (view _Except <$> e) (view _Else <$> f) (view _Finally <$> g))
      (\case
          TryExcept a b c d e f g ->
            Right $
            MkTryExcept a b c d
              (view (from _Except) <$> e)
              (view (from _Else) <$> f)
              (view (from _Finally) <$> g)
          a -> Left a)

instance HasTryExcept Statement where
  _TryExcept = _CompoundStatement._TryExcept

class HasTryFinally s where
  _TryFinally :: Prism' (s a) (TryFinally a)

instance HasTryFinally TryFinally where
  _TryFinally = id

instance HasTryFinally CompoundStatement where
  _TryFinally =
    prism
      (\(MkTryFinally a b c d e) ->
        (\(x, y, z) -> TryFinally a b c d x y z) (e ^. _Finally))
      (\case
          TryFinally a b c d e f g ->
            Right $ MkTryFinally a b c d ((e, f, g) ^. from _Finally)
          a -> Left a)

instance HasTryFinally Statement where
  _TryFinally = _CompoundStatement._TryFinally

class HasFor s where
  _For :: Prism' (s a) (For a)

instance HasFor For where
  _For = id

instance HasFor CompoundStatement where
  _For =
    prism
      (\(MkFor a b c d e f g h i) ->
        For a b c d e f g h (view _Else <$> i))
      (\case
          For a b c d e f g h i ->
            Right $ MkFor a b c d e f g h (view (from _Else) <$> i)
          a -> Left a)

instance HasFor Statement where
  _For = _CompoundStatement._For

_Call :: Prism' (Expr a) (Call a)
_Call =
  prism
    (\(MkCall a b c d e) -> Call a b c d e)
    (\case
        Call a b c d e -> Right $ MkCall a b c d e
        a -> Left a)

class HasClassDef s where
  _ClassDef :: Prism' (s a) (ClassDef a)

instance HasClassDef ClassDef where
  _ClassDef = id

instance HasClassDef CompoundStatement where
  _ClassDef =
    prism
      (\(MkClassDef a b c d e f g) -> ClassDef a b c d e f g)
      (\case
          ClassDef a b c d e f g -> Right $ MkClassDef a b c d e f g
          a -> Left a)

instance HasClassDef Statement where
  _ClassDef = _CompoundStatement._ClassDef

class HasWith s where
  _With :: Prism' (s a) (With a)

instance HasWith With where
  _With = id

instance HasWith CompoundStatement where
  _With =
    prism
      (\(MkWith a b c d e f) -> With a b c d e f)
      (\case
          With a b c d e f -> Right $ MkWith a b c d e f
          a -> Left a)

instance HasWith Statement where
  _With = _CompoundStatement._With

-- |
-- A faux-Prism for matching on the @Ident@ constructor of an 'Expr'.
--
-- It's not a Prism because:
--
-- When 'Control.Lens.Fold.preview'ing, it discards the 'Expr'\'s annotation, and when
-- 'Control.Lens.Review.review'ing, it re-constructs an annotation from the supplied 'Language.Python.Syntax.Ident.Ident'
--
-- @'_Ident' :: 'Prism' ('Expr' v a) ('Expr' '[] a) ('Ident' v a) ('Ident' '[] a)@
_Ident :: (Choice p, Applicative f) => p (Ident a) (f (Ident a)) -> p (Expr a) (f (Expr a))
_Ident =
  prism
    (\i -> Ident (i ^. annot) i)
    (\case
        Ident _ a -> Right a
        a -> Left a)

-- | 'Traversal' targeting the variables that would modified as a result of an assignment
--
-- Here are some examples of assignment targets:
--
-- @
-- a = b
-- ^
-- @
--
-- @
-- (a, b, c) = d
--  ^  ^  ^
-- @
--
-- @
-- [a, b, *c] = d
--  ^  ^   ^
-- @
--
-- These expressions have variables on the left hand side of the @=@, but those variables
-- don't count as assignment targets:
--
-- @
-- a[b] = c
-- @
--
-- @
-- a(b) = c
-- @
--
-- @
-- {a: b} = c
-- @
assignTargets :: Traversal' (Expr a) (Ident a)
assignTargets f e =
  case e of
    List a b c d -> (\c' -> List a b c' d) <$> (traverse.traverse._Exprs.assignTargets) f c
    Parens a b c d -> (\c' -> Parens a b c' d) <$> assignTargets f c
    Ident a b -> Ident a <$> f b
    Tuple a b c d ->
      (\b' d' -> Tuple a b' c d') <$>
      (_Exprs.assignTargets) f b <*>
      (traverse.traverse._Exprs.assignTargets) f d
    Unit{} -> pure e
    Lambda{} -> pure e
    Yield{} -> pure e
    YieldFrom{} -> pure e
    Ternary{} -> pure e
    ListComp{} -> pure e
    Deref{} -> pure e
    Subscript{} -> pure e
    Call{} -> pure e
    None{} -> pure e
    Ellipsis{} -> pure e
    BinOp{} -> pure e
    UnOp{} -> pure e
    Int{} -> pure e
    Float{} -> pure e
    Imag{} -> pure e
    Bool{} -> pure e
    String{} -> pure e
    Not{} -> pure e
    DictComp{} -> pure e
    Dict{} -> pure e
    SetComp{} -> pure e
    Set{} -> pure e
    Generator{} -> pure e
    Await{} -> pure e
