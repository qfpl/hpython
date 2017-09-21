module Language.Python.Parser.Keywords where

import Papa

import Text.Trifecta
import Language.Python.AST.Keywords

kOr :: CharParsing m => m KOr
kOr = string "or" $> KOr

kAnd :: CharParsing m => m KAnd
kAnd = string "and" $> KAnd

kAs :: CharParsing m => m KAs
kAs = string "as" $> KAs
