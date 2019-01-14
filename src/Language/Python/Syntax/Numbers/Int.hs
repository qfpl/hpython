{-# language DataKinds, KindSignatures #-}
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
    -- ** 3.5
  , PyInt35(..)
    -- *** Lenses
  , int35Ann
  , int35Value
  , int35Whitespace
    -- ** 3.6
  , PyInt36(..)
    -- *** Lenses
  , int36Ann
  , int36Value
  , int36Whitespace
  )
where

import Control.Lens.Lens (Lens')
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makeLenses)
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Language.Python.Optics.Version
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Numbers.IntLiteral
import Language.Python.Syntax.Whitespace

class AsPyInt s int v a | s -> v a, int -> v a where
  _Int :: Prism' (s v a) (Ann a, int v a, [Whitespace])

data PyInt35 (v :: [*]) (a :: *)
  = MkInt35
  { _int35Ann :: Ann a
  , _int35Value :: IntLiteral35 a
  , _int35Whitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PyInt35

instance HasAnn (PyInt35 v) where
  annot :: forall a. Lens' (PyInt35 v a) (Ann a)
  annot = typed @(Ann a)

instance Upgrade (PyInt35 v a) (PyInt35 v a) where
  upgrade = id
  downgrade = Just


data PyInt36 (v :: [*]) (a :: *)
  = MkInt36
  { _int36Ann :: Ann a
  , _int36Value :: IntLiteral36 a
  , _int36Whitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PyInt36

instance HasAnn (PyInt36 v) where
  annot :: forall a. Lens' (PyInt36 v a) (Ann a)
  annot = typed @(Ann a)

instance Upgrade (PyInt36 v a) (PyInt36 v a) where
  upgrade = id
  downgrade = Just

instance Upgrade (PyInt35 v a) (PyInt36 v a) where
  upgrade (MkInt35 a b c) = MkInt36 a (upgrade b) c
  downgrade (MkInt36 a b c) = (\b' -> MkInt35 a b' c) <$> downgrade b