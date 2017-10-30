module Language.Python.Printer.IndentedLines where

import Papa
import Text.PrettyPrint hiding ((<>))

import Language.Python.AST.IndentedLines
import Language.Python.Printer.Symbols

indentedLines :: (a -> Doc) -> IndentedLines a -> Doc
indentedLines f i =
  foldMap
    (\(a, b) -> foldMap indentationChar a <> f b)
    (getIndentedLines i)
