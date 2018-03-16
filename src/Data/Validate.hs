{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Data.Validate where

import Control.Lens.TH
import Data.Semigroup

data Validate e a
  = Failure e
  | Success a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup e => Applicative (Validate e) where
  pure = Success
  Success f <*> Success a = Success (f a)
  Success _ <*> Failure e = Failure e
  Failure e <*> a =
    Failure $ case a of
      Success _ -> e
      Failure e' -> e <> e'

makePrisms ''Validate
