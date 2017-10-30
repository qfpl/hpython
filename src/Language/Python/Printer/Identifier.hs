module Language.Python.Printer.Identifier where

import Papa
import Data.Text
import Text.PrettyPrint

import Language.Python.AST.Identifier

identifier :: Identifier a -> Doc
identifier i = i ^. identifier_value . to unpack . to text
