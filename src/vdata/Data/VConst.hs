{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DataKinds, KindSignatures #-}
module Data.VConst where

newtype VConst x (v :: [*]) (a :: *) = VConst { unVConst :: x }
  deriving (Functor, Foldable, Traversable)
