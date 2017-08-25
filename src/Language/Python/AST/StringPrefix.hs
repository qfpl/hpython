module Language.Python.AST.StringPrefix where

import Papa

data StringPrefix
  = StringPrefix_r
  | StringPrefix_u
  | StringPrefix_R
  | StringPrefix_U
  deriving (Eq, Show)
