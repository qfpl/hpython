{-# language DataKinds, KindSignatures #-}
module Data.VFoldable where

class VFoldable (h :: ([*] -> * -> *) -> [*] -> * -> *) where
  vfoldMap :: Monoid m => (f v a -> m) -> h f v a -> m
