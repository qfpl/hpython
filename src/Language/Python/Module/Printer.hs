module Language.Python.Module.Printer where

import Papa
import Text.PrettyPrint

import Language.Python.Module.AST
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols
import Language.Python.Statement.Printer

module' :: Ord a => Module a -> Doc
module' (Module s _ ) =
  foldMapOf
    (_Wrapped.folded)
    (sumElim (newlineChar . getConst) (fold . statement))
    s
