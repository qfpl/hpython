module Language.Python.Expr.AST.TermOperator where

import Papa

data TermOperator
  = TermMult
  | TermAt
  | TermFloorDiv
  | TermDiv
  | TermMod
  deriving (Eq, Show)
