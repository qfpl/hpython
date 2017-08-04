{-# language DataKinds, FlexibleInstances, GADTs, KindSignatures, TypeOperators #-}
module Data.Separated.Repeated where

import Papa

import GHC.TypeLits

data Repeated :: Nat -> * -> * where
  None :: Repeated 0 a
  Once :: a -> Repeated n a -> Repeated (n + 1) a

instance Functor (Repeated n) where
  fmap _ None = None
  fmap f (Once a rest) = Once (f a) (fmap f rest)

instance Foldable (Repeated n) where
  foldMap _ None = mempty
  foldMap f (Once a rest) = f a `mappend` foldMap f rest

instance Traversable (Repeated n) where
  traverse _ None = pure None
  traverse f (Once a rest) = Once <$> f a <*> traverse f rest
