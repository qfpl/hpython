{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}

{-|
Module      : Language.Python.Syntax.Punctuation
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

These types are used throughout the syntax tree to help preserve formatting.
-}

module Language.Python.Syntax.Punctuation where

import Control.Lens.Lens (Lens', lens)
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Whitespace

-- | A period character, possibly followed by some whitespace.
newtype Dot = MkDot [Whitespace]
  deriving (Eq, Show)

instance HasTrailingWhitespace Dot where
  trailingWhitespace =
    lens (\(MkDot ws) -> ws) (\_ ws -> MkDot ws)

-- | The venerable comma separator
newtype Comma = MkComma [Whitespace]
  deriving (Eq, Show, Generic)

instance HasTrailingWhitespace Comma where
  trailingWhitespace =
    lens (\(MkComma ws) -> ws) (\_ ws -> MkComma ws)

newtype Colon = MkColon [Whitespace]
  deriving (Eq, Show, Generic)

instance HasTrailingWhitespace Colon where
  trailingWhitespace =
    lens (\(MkColon ws) -> ws) (\_ ws -> MkColon ws)

data Semicolon a = MkSemicolon (Ann a) [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn Semicolon where
  annot :: forall a. Lens' (Semicolon a) (Ann a)
  annot = typed @(Ann a)

instance HasTrailingWhitespace (Semicolon a) where
  trailingWhitespace =
    lens (\(MkSemicolon _ ws) -> ws) (\(MkSemicolon a _) ws -> MkSemicolon a ws)

newtype Equals
  = MkEquals [Whitespace]
  deriving (Eq, Show)

instance HasTrailingWhitespace Equals where
  trailingWhitespace =
    lens (\(MkEquals ws) -> ws) (\_ ws -> MkEquals ws)

newtype At
  = MkAt [Whitespace]
  deriving (Eq, Show)

instance HasTrailingWhitespace At where
  trailingWhitespace =
    lens (\(MkAt ws) -> ws) (\_ ws -> MkAt ws)
