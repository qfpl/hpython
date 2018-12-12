{-# language DataKinds, KindSignatures #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{-|
Module      : Language.Python.Validate.Scope.Error
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Scope.Error where

import Control.Lens.TH
import Language.Python.Syntax.Ident

data ScopeError a
  = FoundNonlocal a
  | FoundGlobal a
  | DeletedIdent a
  | FoundDynamic a (Ident '[] a)
  | NotInScope (Ident '[] a)
  | BadShadowing (Ident '[] a)
  deriving (Eq, Show)

makeClassyPrisms ''ScopeError