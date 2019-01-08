{-# language DataKinds #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Language.Python.Optics.Exprs where

import Control.Lens.Traversal (Traversal)

-- | 'Control.Lens.Traversal.Traversal' over all the expressions in a term
class HasExprs s expr | s -> expr where
  _Exprs :: Traversal (s v a) (s '[] a) (expr v a) (expr '[] a)
