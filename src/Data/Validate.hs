{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Data.Validate where

import Control.Lens.TH (makePrisms)
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Semigroup (Semigroup(..))

data Validate e a
  = Failure e
  | Success a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor Validate where
  bimap f _ (Failure e) = Failure $ f e
  bimap _ g (Success e) = Success $ g e

instance Bifoldable Validate where
  bifoldMap f _ (Failure e) = f e
  bifoldMap _ g (Success e) = g e

instance Bitraversable Validate where
  bitraverse f _ (Failure e) = Failure <$> f e
  bitraverse _ g (Success e) = Success <$> g e

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

fromEither :: Either a b -> Validate a b
fromEither (Left a) = Failure a
fromEither (Right a) = Success a

validate :: (e -> r) -> (a -> r) -> Validate e a -> r
validate f _ (Failure a) = f a
validate _ g (Success a) = g a

makePrisms ''Validate
