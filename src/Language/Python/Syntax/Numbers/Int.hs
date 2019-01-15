{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# language FunctionalDependencies #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving, QuantifiedConstraints, UndecidableInstances #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}

{-|
Module      : Language.Python.Syntax.Numbers.Int
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Python integer syntax
-}

module Language.Python.Syntax.Numbers.Int
  ( -- * Classy Prism
    AsPyInt(..)
    -- * Datatypes
  , PyInt35
  , PyInt36
  , PyInt(..)
    -- *** Lenses
  , intAnn
  , intValue
  , intWhitespace
  )
where

import Control.Lens.Getter (to)
import Control.Lens.Iso (coerced)
import Control.Lens.Prism (Prism)
import Control.Lens.TH (makeLenses)
import Control.Lens.Wrapped (_Wrapped)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Data.VFix
import Data.VariantV
import Language.Python.Optics.Validated
import Language.Python.Optics.Version
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Numbers.IntLiteral
import Language.Python.Syntax.Whitespace

class AsPyInt s int | s -> int where
  _Int
    :: Prism
         (s v a)
         (s '[] a)
         (PyInt int expr v a)
         (PyInt int expr '[] a)

instance
  (Validated (VariantV vs expr), CtorV vs (PyInt f)) =>
  AsPyInt (VariantV vs expr) f where
  _Int = _CtorV' @(PyInt f) . coerced

instance AsPyInt (e (VFix e)) f => AsPyInt (VFix e) f where
  _Int = _Wrapped._Int

data PyInt f (expr :: [*] -> * -> *) (v :: [*]) (a :: *)
  = MkInt
  { _intAnn :: Ann a
  , _intValue :: f a
  , _intWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PyInt

instance Validated e => Validated (PyInt f e) where; unvalidated = to unsafeCoerce

instance HasTrailingWhitespace (PyInt f expr v a) where
  trailingWhitespace = intWhitespace

instance HasAnn (PyInt f expr v) where
  annot = intAnn

instance Upgrade (f a) (f' a) => Upgrade (PyInt f expr v a) (PyInt f' expr v a) where
  upgrade (MkInt a b c) = MkInt a (upgrade b) c
  downgrade (MkInt a b c) = (\b' -> MkInt a b' c) <$> downgrade b

type PyInt35 = PyInt IntLiteral35
type PyInt36 = PyInt IntLiteral36
