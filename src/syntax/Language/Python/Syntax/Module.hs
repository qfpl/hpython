{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Language.Python.Syntax.Module
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.Module
  ( Module (ModuleEmpty, ModuleBlankFinal, ModuleBlank, ModuleStatement)
  )
where

import GHC.Generics (Generic)

import Language.Python.Optics.Idents (HasIdents)
import Language.Python.Optics.Exprs
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Whitespace

-- | A Python 'Module', which is stored as a sequence of statements.
-- A module corresponds to one source file of Python code.
data Module v a
  = ModuleEmpty
  | ModuleBlankFinal (Blank a)
  | ModuleBlank (Blank a) Newline (Module v a)
  | ModuleStatement (Statement v a) (Module v a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasIdents Module

instance HasStatements Module where
  _Statements f = go
    where
      go ModuleEmpty = pure ModuleEmpty
      go (ModuleBlankFinal a) = pure $ ModuleBlankFinal a
      go (ModuleBlank a b c) = ModuleBlank a b <$> go c
      go (ModuleStatement a b) = ModuleStatement <$> f a <*> go b

instance HasExprs Module Expr where
  _Exprs = _Statements._Exprs
