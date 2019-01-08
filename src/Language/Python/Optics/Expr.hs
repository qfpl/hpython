{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
module Language.Python.Optics.Expr where

import Control.Lens.Getter ((^.))
import Control.Lens.Prism (Prism, prism)
import Control.Lens.Wrapped (_Wrapped)

import Language.Python.Optics.Validated
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Types

class AsExpr outer inner | outer -> inner where
  _Ident :: Prism (outer v a) (outer '[] a) (Ident v a) (Ident '[] a)
  _Int :: Prism (outer v a) (outer '[] a) (PyInt v a) (PyInt '[] a)
  _Float :: Prism (outer v a) (outer '[] a) (PyFloat v a) (PyFloat '[] a)
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

instance AsExpr Expr Expr where
  _Ident =
    _Wrapped .
    prism
      Ident
      (\case
          Ident a -> Right a
          a -> Left $ a ^. unvalidated)
  _Tuple =
    _Wrapped .
    prism
      (\(MkTuple a b c d) -> Tuple a b c d)
      (\case
          Tuple a b c d -> Right (MkTuple a b c d)
          a -> Left $ a ^. unvalidated)

  _None =
    _Wrapped .
    prism
      (\(MkNone a b) -> None a b)
      (\case
          None a b -> Right (MkNone a b)
          a -> Left $ a ^. unvalidated)

  _Call =
    _Wrapped .
    prism
      (\(MkCall a b c d e) -> Call a b c d e)
      (\case
          Call a b c d e -> Right $ MkCall a b c d e
          a -> Left $ a ^. unvalidated)

  _List =
    _Wrapped .
    prism
      (\(MkList a b c d) -> List a b c d)
      (\case
          List a b c d -> Right (MkList a b c d)
          a -> Left $ a ^. unvalidated)
