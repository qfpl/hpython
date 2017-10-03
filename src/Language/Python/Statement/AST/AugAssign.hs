module Language.Python.Statement.AST.AugAssign where

import Papa

data AugAssign
  = PlusEquals
  | MinusEquals
  | StarEquals
  | AtEquals
  | SlashEquals
  | PercentEquals
  | AmphersandEquals
  | PipeEquals
  | CaretEquals
  | ShiftLeftEquals
  | ShiftRightEquals
  | DoubleStarEquals
  | DoubleSlashEquals
  deriving (Eq, Show)
