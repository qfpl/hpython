{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-|
Module      : Language.Python.Syntax.Punctuation
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

These types are used throughout the syntax tree to help preserve formatting.
-}

module Language.Python.Syntax.Punctuation where

import Control.Lens.Lens (lens)

import Language.Python.Syntax.Whitespace

-- | A period character, possibly followed by some whitespace.
data Dot = Dot [Whitespace]
  deriving (Eq, Show)

instance HasTrailingWhitespace Dot where
  trailingWhitespace =
    lens (\(Dot ws) -> ws) (\_ ws -> Dot ws)

-- | The venerable comma separator
newtype Comma =
  Comma [Whitespace]
  deriving (Eq, Show)

instance HasTrailingWhitespace Comma where
  trailingWhitespace =
    lens (\(Comma ws) -> ws) (\_ ws -> Comma ws)

newtype Colon
  = Colon [Whitespace]
  deriving (Eq, Show)

instance HasTrailingWhitespace Colon where
  trailingWhitespace =
    lens (\(Colon ws) -> ws) (\_ ws -> Colon ws)

data Semicolon a
  = Semicolon a [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (Semicolon a) where
  trailingWhitespace =
    lens (\(Semicolon _ ws) -> ws) (\(Semicolon a _) ws -> Semicolon a ws)
