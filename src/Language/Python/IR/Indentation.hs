{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.IR.Indentation where

import Papa
import Data.Deriving
import Language.Python.AST.Symbols

data Indent a
  = Indent
  { _indent_value :: [WhitespaceChar]
  , _indent_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Dedent a
  = Dedent
  { _dedent_value :: [WhitespaceChar]
  , _dedent_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''Indent
deriveEq1 ''Indent
deriveShow1 ''Indent

makeLenses ''Dedent
deriveEq1 ''Dedent
deriveShow1 ''Dedent
