{-# language DataKinds #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# language FunctionalDependencies #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving, QuantifiedConstraints, UndecidableInstances #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}

{-|
Module      : Language.Python.Syntax.Numbers.Imag
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Python imaginary number syntax
-}

module Language.Python.Syntax.Numbers.Imag
  ( -- * Classy Prism
    AsImag(..)
    -- * Datatypes
    -- ** 3.5
  , Imag35(..)
    -- *** Lenses
  , imag35Ann
  , imag35Value
  , imag35Whitespace
    -- ** 3.6
  , Imag36(..)
    -- *** Lenses
  , imag36Ann
  , imag36Value
  , imag36Whitespace
  )
where

import Control.Lens.Lens (Lens')
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makeLenses)
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Language.Python.Optics.Version
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Numbers.ImagLiteral
import Language.Python.Syntax.Whitespace


class AsImag s imag v a | s -> v a, imag -> v a where
  _Imag :: Prism' (s v a) (Ann a, imag v a, [Whitespace])

data Imag35 (v :: [*]) (a :: *)
  = MkImag35
  { _imag35Ann :: Ann a
  , _imag35Value :: ImagLiteral35 a
  , _imag35Whitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Imag35

instance HasAnn (Imag35 v) where
  annot :: forall a. Lens' (Imag35 v a) (Ann a)
  annot = typed @(Ann a)

instance Upgrade (Imag35 v a) (Imag35 v a) where
  upgrade = id
  downgrade = Just


data Imag36 (v :: [*]) (a :: *)
  = MkImag36
  { _imag36Ann :: Ann a
  , _imag36Value :: ImagLiteral35 a
  , _imag36Whitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''Imag36

instance HasAnn (Imag36 v) where
  annot :: forall a. Lens' (Imag36 v a) (Ann a)
  annot = typed @(Ann a)

instance Upgrade (Imag36 v a) (Imag36 v a) where
  upgrade = id
  downgrade = Just

instance Upgrade (Imag35 v a) (Imag36 v a) where
  upgrade (MkImag35 a b c) = MkImag36 a (upgrade b) c
  downgrade (MkImag36 a b c) = (\b' -> MkImag35 a b' c) <$> downgrade b
