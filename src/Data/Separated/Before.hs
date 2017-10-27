{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell #-}
module Data.Separated.Before where

import Papa

import Data.Deriving

data Before s a = Before s a
  deriving (Eq, Ord, Foldable, Functor, Traversable, Show)

deriveEq1 ''Before
deriveShow1 ''Before
deriveOrd1 ''Before
deriveRead1 ''Before

instance Bifunctor Before where
  bimap f g (Before s a) = Before (f s) (g a)

before :: Lens (Before s a) (Before t b) (s, a) (t, b)
before = lens (\(Before s a) -> (s, a)) (\Before{} (t, b) -> Before t b)
