module Language.Python.AST.FactorOperator where

import Papa

data FactorOp
  = FactorNeg
  | FactorPos
  | FactorInv
  deriving (Eq, Show)
