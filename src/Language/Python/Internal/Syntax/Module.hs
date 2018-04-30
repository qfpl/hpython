{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Python.Internal.Syntax.Module where

import Control.Lens.TH (makeWrapped)
import Control.Lens.Prism (_Right)
import Control.Lens.Wrapped (_Wrapped)

import Language.Python.Internal.Syntax.Statement
import Language.Python.Internal.Syntax.Whitespace

newtype Module v a = Module [Either ([Whitespace], Newline) (Statement v a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasStatements Module where
  _Statements = _Wrapped.traverse._Right

makeWrapped ''Module
