module Language.Python.AST.FactorOperator where

import Papa

data FactorOperator
  = FactorNeg
  | FactorPos
  | FactorInv
  deriving (Eq, Show)
