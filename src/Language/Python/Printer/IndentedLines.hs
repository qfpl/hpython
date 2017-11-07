module Language.Python.Printer.IndentedLines where

import Papa
import Text.PrettyPrint hiding ((<>))

import Language.Python.AST.IndentedLines
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols

indentedLines
  :: (comment a -> Doc)
  -> (f a -> Doc)
  -> IndentedLines comment f a
  -> Doc
indentedLines c f i =
  foldMapOf
    (_Wrapped.folded)
    (sumElim c (beforeF (foldMap indentationChar) f))
    (getIndentedLines i)
