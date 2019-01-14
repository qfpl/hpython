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
Module      : Language.Python.Syntax.Numbers.Float
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Python float syntax
-}

module Language.Python.Syntax.Numbers.Float
  ( -- * Classy Prism
    AsPyFloat(..)
    -- * Datatypes
    -- ** 3.5
  , PyFloat35(..)
    -- *** Lenses
  , float35Ann
  , float35Value
  , float35Whitespace
    -- ** 3.6
  , PyFloat36(..)
    -- *** Lenses
  , float36Ann
  , float36Value
  , float36Whitespace
  )
where

import Control.Lens.Lens (Lens')
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makeLenses)
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Language.Python.Optics.Version
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Numbers.FloatLiteral
import Language.Python.Syntax.Whitespace

class AsPyFloat s float v a | s -> v a, float -> v a where
  _Float :: Prism' (s v a) (Ann a, float v a, [Whitespace])

data PyFloat35 (v :: [*]) (a :: *)
  = MkFloat35
  { _float35Ann :: Ann a
  , _float35Value :: FloatLiteral35 a
  , _float35Whitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PyFloat35

instance HasAnn (PyFloat35 v) where
  annot :: forall a. Lens' (PyFloat35 v a) (Ann a)
  annot = typed @(Ann a)

instance Upgrade (PyFloat35 v a) (PyFloat35 v a) where
  upgrade = id
  downgrade = Just

data PyFloat36 (v :: [*]) (a :: *)
  = MkFloat36
  { _float36Ann :: Ann a
  , _float36Value :: FloatLiteral36 a
  , _float36Whitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLenses ''PyFloat36

instance HasAnn (PyFloat36 v) where
  annot :: forall a. Lens' (PyFloat36 v a) (Ann a)
  annot = typed @(Ann a)

instance Upgrade (PyFloat36 v a) (PyFloat36 v a) where
  upgrade = id
  downgrade = Just

instance Upgrade (PyFloat35 v a) (PyFloat36 v a) where
  upgrade (MkFloat35 a b c) = MkFloat36 a (upgrade b) c
  downgrade (MkFloat36 a b c) = (\b' -> MkFloat35 a b' c) <$> downgrade b
