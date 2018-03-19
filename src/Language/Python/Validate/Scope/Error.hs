{-# language DataKinds, KindSignatures #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Language.Python.Validate.Scope.Error where

import Control.Lens.TH
import Language.Python.Internal.Syntax

data ScopeError (v :: [*]) a
  = FoundNonlocal a
  | FoundGlobal a
  | FoundDel a
  | FoundDynamic a (Ident v a)
  | NotInScope (Ident v a)
  deriving (Eq, Show)

makeClassyPrisms ''ScopeError
