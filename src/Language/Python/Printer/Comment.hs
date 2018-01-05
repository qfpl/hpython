module Language.Python.Printer.Comment where

import Papa
import Data.Text (unpack)
import Text.PrettyPrint hiding ((<>), dot)

import Language.Python.AST.Comment
import Language.Python.Printer.Combinators
import Language.Python.Printer.Identifier
import Language.Python.Printer.Symbols

comment :: Comment a -> Doc
comment (Comment h _) =
  text "#" <>
  text (unpack h)
