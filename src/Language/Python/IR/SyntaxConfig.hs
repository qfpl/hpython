{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Python.IR.SyntaxConfig where

import Papa

data ExprContext = TopLevel | FunDef FunType
data SExprContext :: ExprContext -> * where
  STopLevel :: SExprContext 'TopLevel
  SFunDef :: SFunType f -> SExprContext ('FunDef f)

data FunType = Normal | Async
data SFunType :: FunType -> * where
  SNormal :: SFunType 'Normal
  SAsync :: SFunType 'Async

data AtomType = Assignable | NotAssignable
data SAtomType :: AtomType -> * where
  SAssignable :: SAtomType 'Assignable
  SNotAssignable :: SAtomType 'NotAssignable

data SyntaxConfig atomType ctxt
  = SyntaxConfig
  { _atomType :: SAtomType atomType
  , _exprContext :: SExprContext ctxt
  }
makeLenses ''SyntaxConfig
