{-# language DataKinds #-}
{-# language FlexibleContexts, UndecidableInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Language.Python.Syntax.Expr.Optics where

import Control.Lens.Prism (Choice, Prism)
import Control.Lens.Wrapped (_Wrapped)

import Data.VFix
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Expr.Types

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
  _Bool :: Prism (outer v a) (outer '[] a) (PyBool inner v a) (PyBool inner '[] a)
  _String :: Prism (outer v a) (outer '[] a) (PyString inner v a) (PyString inner '[] a)
  _Unit :: Prism (outer v a) (outer '[] a) (Unit inner v a) (Unit inner '[] a)
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
  _None :: Prism (outer v a) (outer '[] a) (None inner v a) (None inner '[] a)
  _Ellipsis :: Prism (outer v a) (outer '[] a) (Ellipsis inner v a) (Ellipsis inner '[] a)
  _Call :: Prism (outer v a) (outer '[] a) (Call inner v a) (Call inner '[] a)
  _List
    :: Prism
        (outer v a)
        (outer '[] a)
        (List inner v a)
        (List inner '[] a)

instance (AsExpr (expr (VFix expr)) (VFix expr')) => AsExpr (VFix expr) (VFix expr') where
  _Ident = _Wrapped._Ident
  _Tuple = _Wrapped._Tuple
  _None = _Wrapped._None
  _Call = _Wrapped._Call
  _List = _Wrapped._List
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

