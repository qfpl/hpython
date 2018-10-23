{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
Module      : Language.Python.Internal.Syntax.Module
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.Module where

import Language.Python.Internal.Syntax.Statement
import Language.Python.Internal.Syntax.Whitespace

-- | A Python 'Module', which is stored as a sequence of statements.
-- A module corresponds to one source file of Python code.
data Module v a
  = ModuleEmpty
  | ModuleBlankFinal (Blank a)
  | ModuleBlank (Blank a) Newline (Module v a)
  | ModuleStatement (Statement v a) (Module v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasStatements Module where
  _Statements f = go
    where
      go ModuleEmpty = pure ModuleEmpty
      go (ModuleBlankFinal a) = pure $ ModuleBlankFinal a
      go (ModuleBlank a b c) = ModuleBlank a b <$> go c
      go (ModuleStatement a b) = ModuleStatement <$> f a <*> go b
