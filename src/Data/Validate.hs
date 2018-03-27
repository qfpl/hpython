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

bindValidate :: Validate e a -> (a -> Validate e b) -> Validate e b
bindValidate (Failure e) _ = Failure e
bindValidate (Success a) f = f a

makePrisms ''Validate
