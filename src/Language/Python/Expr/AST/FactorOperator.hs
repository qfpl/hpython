module Language.Python.Expr.AST.FactorOperator where

import Papa

data FactorOperator
  = FactorNeg
  | FactorPos
  | FactorInv
  deriving (Eq, Ord, Show)
