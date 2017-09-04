{-# LANGUAGE TemplateHaskell #-}
module Language.Python.Parser.IR.SyntaxConfig where

import Papa
import Language.Python.AST

data SyntaxConfig atomType ctxt
  = SyntaxConfig
  { _atomType :: SAtomType atomType
  , _exprContext :: SExprContext ctxt
  }
makeLenses ''SyntaxConfig
