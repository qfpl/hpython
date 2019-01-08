{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DataKinds, KindSignatures #-}
module Data.VIdentity where

import Data.VTraversable

newtype VIdentity (f :: [*] -> * -> *) (v :: [*]) (a :: *)
  = VIdentity
  { unVIdentity :: f v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance VTraversable VIdentity where
  vtraverse f (VIdentity a) = VIdentity <$> f a