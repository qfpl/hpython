module Language.Python.Printer.Keywords where

import Text.PrettyPrint hiding ((<>), comma, colon)

import Language.Python.AST.Keywords

kAwait :: KAwait -> Doc
kAwait _ = text "await"

kOr :: KOr -> Doc
kOr _ = text "or"

kAnd :: KAnd -> Doc
kAnd _ = text "and"

kNot :: KNot -> Doc
kNot _ = text "not"
