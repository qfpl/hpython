{-# language DataKinds, KindSignatures #-}
module Data.VFunctor where

class VFunctor (h :: ([*] -> * -> *) -> [*] -> * -> *) where
  vfmap :: (f v a -> g v a) -> h f v a -> h g v a
