{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Python.Internal.Syntax.ModuleNames where

import Data.List.NonEmpty (NonEmpty)

import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Whitespace

data RelativeModuleName v a
  = RelativeWithName [Dot] (ModuleName v a)
  | Relative (NonEmpty Dot)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Dot = Dot [Whitespace]
  deriving (Eq, Show)

data ModuleName v a
  = ModuleNameOne a (Ident v a)
  | ModuleNameMany a (Ident v a) [Whitespace] [Whitespace] (ModuleName v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
