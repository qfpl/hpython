{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language UndecidableInstances #-}
module Language.Python.Optics.Expr where

import Control.Lens.Getter ((^.))
import Control.Lens.Prism (Choice, Prism, prism)
import Control.Lens.Wrapped (_Wrapped)

import Data.VFix
import Language.Python.Optics.Validated
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Types

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
