module Language.Python.Import.Graph where

data Graph v a
  = Graph
  { _gModule :: Module
  }