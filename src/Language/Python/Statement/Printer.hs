module Language.Python.Statement.Printer where

import Papa
import Text.PrettyPrint hiding ((<>), semicolon)

import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols
import Language.Python.Statement.AST

statement :: Statement lctxt ectxt a -> Doc
statement s =
  case s of
    StatementSimple v _ -> simpleStatement v
    StatementCompound v _ -> compoundStatement v

simpleStatement :: SimpleStatement lctxt ectxt a -> Doc
simpleStatement (SimpleStatement h t s n _) =
  smallStatement h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' semicolon) smallStatement)
    t <>
  foldMap (whitespaceBefore semicolon) s <>
  whitespaceBefore newlineChar n

smallStatement :: SmallStatement lctxt ectxt a -> Doc
smallStatement s = _

compoundStatement :: CompoundStatement lctxt ectxt a -> Doc
compoundStatement = _
