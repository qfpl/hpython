{-# language LambdaCase #-}
module Data.Functor.Sum.Lens where

import Papa hiding (Sum)
import Data.Functor.Sum

_InL :: Prism (Sum f g a) (Sum f' g a) (f a) (f' a)
_InL =
  prism
    InL
    (\case
        InL a -> Right a
        InR a -> Left $ InR a)

_InR :: Prism (Sum f g a) (Sum f g' a) (g a) (g' a)
_InR =
  prism
    InR
    (\case
        InR a -> Right a
        InL a -> Left $ InL a)
