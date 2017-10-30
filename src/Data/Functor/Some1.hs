{-# language ExistentialQuantification #-}
module Data.Functor.Some1 where

import Papa
import Data.Functor.Classes

data Some1 f = forall a. Some1 { getSome1 :: f a }

instance Eq1 f => Eq (Some1 f) where
  Some1 a == Some1 b = liftEq (==) a b


