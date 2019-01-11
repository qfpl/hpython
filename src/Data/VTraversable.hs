{-# language DataKinds, KindSignatures #-}
module Data.VTraversable where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))

import Data.VFoldable
import Data.VFunctor

class
  (VFunctor h, VFoldable h) =>
  VTraversable (h :: ([*] -> * -> *) -> [*] -> * -> *)

  where

  vtraverse :: Applicative m => (f v a -> m (g v a)) -> h f v a -> m (h g v a)

vfmapDefault
  :: VTraversable h
  => (f v a -> g v a)
  -> h f v a -> h g v a
vfmapDefault f = runIdentity . vtraverse (Identity . f)

vfoldMapDefault
  :: (VTraversable h, Monoid m)
  => (f v a -> m)
  -> h f v a -> m
vfoldMapDefault f = getConst . vtraverse (Const . f)
