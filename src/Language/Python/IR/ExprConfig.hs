{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Python.IR.ExprConfig where

import Papa
import Data.Singletons.TH

data DefinitionContext = TopLevel | FunDef FunType
  deriving (Eq, Show)

data FunType = Normal | Async
  deriving (Eq, Show)

data AtomType = Assignable | NotAssignable
  deriving (Eq, Show)

genSingletons [''DefinitionContext, ''FunType, ''AtomType]
promoteEqInstances [''DefinitionContext, ''FunType, ''AtomType]

data ExprConfig atomType ctxt
  = ExprConfig
  { _atomType :: SAtomType atomType
  , _definitionContext :: SDefinitionContext ctxt
  }

makeLenses ''ExprConfig
