{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Python.Syntax.Ann where

newtype Ann a = Ann { getAnn :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)