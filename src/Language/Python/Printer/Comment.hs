module Language.Python.Printer.Comment where

import Papa
import Data.Text (unpack)
import Text.PrettyPrint hiding ((<>))

import Language.Python.AST.Comment

comment :: Comment a -> Doc
comment (Comment h _) =
  text "#" <>
  text (unpack h)
