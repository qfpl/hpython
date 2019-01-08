{-# language DataKinds, KindSignatures #-}
module Data.VTraversable where

class VTraversable (h :: ([*] -> * -> *) -> [*] -> * -> *) where
  vtraverse :: Applicative m => (f v a -> m (g v a)) -> h f v a -> m (h g v a)