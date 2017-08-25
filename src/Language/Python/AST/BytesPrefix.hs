module Language.Python.AST.BytesPrefix where

import Papa

data BytesPrefix
  = BytesPrefix_b
  | BytesPrefix_B
  | BytesPrefix_br
  | BytesPrefix_Br
  | BytesPrefix_bR
  | BytesPrefix_BR
  | BytesPrefix_rb
  | BytesPrefix_rB
  | BytesPrefix_Rb
  | BytesPrefix_RB
  deriving (Eq, Show)
