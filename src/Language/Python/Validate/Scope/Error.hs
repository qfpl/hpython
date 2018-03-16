{-# language DataKinds, KindSignatures #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Language.Python.Validate.Scope.Error where

import Control.Lens.TH
import Language.Python.Internal.Syntax

data ScopeError (v :: [*]) a
  = FoundNonlocal a
  | FoundGlobal a
  | NotInScope (Ident v a)

makeClassyPrisms ''ScopeError
