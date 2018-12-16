{-# language DataKinds #-}
{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

{-|
Module      : Language.Python.Optics.Indents
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Optics.Indents where

import Control.Lens.Traversal (Traversal')

import Language.Python.Syntax
import Language.Python.Internal.Token

-- | 'Traversal'' targeting the indent-chunks in a structure
--
-- e.g.
--
-- This is one indent chunk:
--
-- @
-- def a():
--     pass
--     if b:
--         pass
-- ^^^^
-- @
--
-- and this is another
--
-- @
-- def a():
--     pass
--     if b:
--         pass
--     ^^^^
-- @
_Indent :: HasIndents s a => Traversal' s [Whitespace]
_Indent = _Indents.indentsValue.traverse.indentWhitespaces

class HasIndents s a | s -> a where
  -- | 'Traversal'' targeting the indentation inside a structure
  --
  -- Note: whitespace inside \'enclosed forms\' (such as lists or tuples) is not
  -- considered indentation.
  --
  -- e.g.
  --
  -- In the following code, there is only one chunk of indentation:
  --
  -- @
  -- def a():
  --     [ b
  --     , c
  --     , d
  --     ]
  -- @
  --
  -- it's here:
  --
  -- @
  -- def a():
  --     [ b
  -- ^^^^
  --     , c
  --     , d
  --     ]
  -- @
  --
  -- The rest is whitespace that is internal to the list.
  _Indents :: Traversal' s (Indents a)

instance HasIndents (PyToken a) a where
  _Indents f (TkIndent a i) = TkIndent a <$> f i
  _Indents f (TkLevel a i) = TkLevel a <$> f i
  _Indents _ a = pure a

instance HasIndents (Fundef '[] a) a where
  _Indents fun (MkFundef a b c d e f g h i j k) =
    (\b' c' -> MkFundef a b' c' d e f g h i j) <$>
    (traverse._Indents) fun b <*>
    fun c <*>
    _Indents fun k

instance HasIndents (For '[] a) a where
  _Indents fun (MkFor a b c d e f g h i) =
    (\b' -> MkFor a b' c d e f g) <$>
    fun b <*>
    _Indents fun h <*>
    (traverse._Indents) fun i

instance HasIndents (TryFinally '[] a) a where
  _Indents fun (MkTryFinally a b c d e) =
    (\b' -> MkTryFinally a b' c) <$>
    fun b <*>
    _Indents fun d <*>
    _Indents fun e

instance HasIndents (TryExcept '[] a) a where
  _Indents fun (MkTryExcept a b c d e f g) =
    (\b' -> MkTryExcept a b' c) <$>
    fun b <*>
    _Indents fun d <*>
    (traverse._Indents) fun e <*>
    (traverse._Indents) fun f <*>
    (traverse._Indents) fun g

instance HasIndents (Except '[] a) a where
  _Indents fun (MkExcept a b c d) =
    (\a' -> MkExcept a' b c) <$>
    fun a <*>
    _Indents fun d

instance HasIndents (Finally '[] a) a where
  _Indents fun (MkFinally a b c) =
    (\a' -> MkFinally a' b) <$>
    fun a <*>
    _Indents fun c

instance HasIndents (If '[] a) a where
  _Indents fun (MkIf a b c d e f g) =
    (\b' -> MkIf a b' c d) <$>
    fun b <*>
    _Indents fun e <*>
    (traverse._Indents) fun f <*>
    (traverse._Indents) fun g

instance HasIndents (While '[] a) a where
  _Indents fun (MkWhile a b c d e f) =
    (\b' -> MkWhile a b' c d) <$>
    fun b <*>
    _Indents fun e <*>
    (traverse._Indents) fun f

instance HasIndents (Elif '[] a) a where
  _Indents fun (MkElif a b c d) =
    (\a' -> MkElif a' b c) <$>
    fun a <*>
    _Indents fun d

instance HasIndents (Else '[] a) a where
  _Indents f (MkElse a b c) = MkElse <$> f a <*> pure b <*> _Indents f c

instance HasIndents (SmallStatement '[] a) a where
  _Indents _ (MkSmallStatement a b c d e) =
    pure $ MkSmallStatement a b c d e

instance HasIndents (Statement '[] a) a where
  _Indents f (SmallStatement idnt a) = SmallStatement <$> f idnt <*> _Indents f a
  _Indents f (CompoundStatement c) = CompoundStatement <$> _Indents f c

instance HasIndents (Block '[] a) a where
  _Indents = _Statements._Indents

instance HasIndents (Suite '[] a) a where
  _Indents _ (SuiteOne a b c) = pure $ SuiteOne a b c
  _Indents f (SuiteMany a b c d e) = SuiteMany a b c d <$> _Indents f e

instance HasIndents (Decorator '[] a) a where
  _Indents fun (Decorator a b c d e f g) =
    (\b' -> Decorator a b' c d e f g) <$>
    fun b

instance HasIndents (ClassDef '[] a) a where
  _Indents fun (MkClassDef a b c d e f g) =
    (\b' c' -> MkClassDef a b' c' d e f) <$>
    (traverse._Indents) fun b <*>
    fun c <*>
    _Indents fun g

instance HasIndents (With '[] a) a where
  _Indents fun (MkWith a b c d e f) =
    (\b' -> MkWith a b' c d e) <$>
    fun b <*>
    _Indents fun f

instance HasIndents (CompoundStatement '[] a) a where
  _Indents fun s =
    case s of
      Fundef a decos idnt asyncWs b c d e f g h ->
        (\decos' idnt' -> Fundef a decos' idnt' asyncWs b c d e f g) <$>
        (traverse._Indents) fun decos <*>
        fun idnt <*>
        _Indents fun h
      If a idnt b c d elifs e ->
        (\idnt' -> If a idnt' b c) <$>
        fun idnt <*>
        _Indents fun d <*>
        traverse
          (\(idnt, a, b, c) ->
             (\idnt'  -> (,,,) idnt' a b) <$>
             fun idnt <*>
             _Indents fun c)
          elifs <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          e
      While a idnt b c d e ->
        (\idnt' -> While a idnt' b c) <$>
        fun idnt <*>
        _Indents fun d <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          e
      TryExcept a idnt b c d e f ->
        (\idnt' -> TryExcept a idnt' b) <$>
        fun idnt <*>
        _Indents fun c <*>
        traverse
          (\(idnt, a, b, c) ->
             (\idnt' -> (,,,) idnt' a b) <$>
             fun idnt <*>
             _Indents fun c)
          d <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          e <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          f
      TryFinally a idnt b c idnt2 d e ->
        (\idnt' c' idnt2' -> TryFinally a idnt' b c' idnt2' d) <$>
        fun idnt <*>
        _Indents fun c <*>
        fun idnt2 <*>
        _Indents fun e
      For a idnt asyncWs b c d e f g ->
        (\idnt' -> For a idnt' asyncWs b c d e) <$>
        fun idnt <*>
        _Indents fun f <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          g
      ClassDef a decos idnt b c d e ->
        (\decos' idnt' -> ClassDef a decos' idnt' b c d) <$>
        traverse (_Indents fun) decos <*>
        fun idnt <*>
        _Indents fun e
      With a b asyncWs c d e ->
        (\b' -> With a b' asyncWs c d) <$>
        fun b <*>
        _Indents fun e
