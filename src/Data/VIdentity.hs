{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Data.VIdentity where

import Control.Lens.TH (makeWrapped)

import Data.VTraversable

newtype VIdentity (f :: [*] -> * -> *) (v :: [*]) (a :: *)
  = VIdentity
  { unVIdentity :: f v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance VTraversable VIdentity where
  vtraverse f (VIdentity a) = VIdentity <$> f a

makeWrapped ''VIdentity