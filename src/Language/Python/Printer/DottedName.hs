module Language.Python.Printer.DottedName where

import Papa
import Text.PrettyPrint hiding ((<>))

import Language.Python.AST.DottedName
import Language.Python.Printer.Combinators
import Language.Python.Printer.Identifier
import Language.Python.Printer.Symbols

dottedName :: DottedName a -> Doc
dottedName (DottedName h t _) =
  identifier h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' dot) identifier)
    t
