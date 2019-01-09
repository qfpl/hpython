{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
Module      : Language.Python.Syntax.Ann
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.Ann where

-- | Used to mark annotations in data structures, which helps with generic deriving
newtype Ann a = Ann { getAnn :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)