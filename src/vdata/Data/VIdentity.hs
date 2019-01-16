{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Data.VIdentity where

import Control.Lens.TH (makeWrapped)
import GHC.Generics (Generic)

import Data.VFoldable
import Data.VFunctor
import Data.VTraversable

newtype VIdentity (f :: [*] -> * -> *) (v :: [*]) (a :: *)
  = VIdentity
  { unVIdentity :: f v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance VFunctor VIdentity where
  vfmap f (VIdentity a) = VIdentity (f a)

instance VFoldable VIdentity where
  vfoldMap f (VIdentity a) = f a

instance VTraversable VIdentity where
  vtraverse f (VIdentity a) = VIdentity <$> f a


makeWrapped ''VIdentity
