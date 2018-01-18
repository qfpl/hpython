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

kFrom :: CharParsing m => m KFrom
kFrom = string "from" $> KFrom

kIf :: CharParsing m => m KIf
kIf = string "if" $> KIf

kIn :: CharParsing m => m KIn
kIn = string "in" $> KIn

kFor :: CharParsing m => m KFor
kFor = string "for" $> KFor
