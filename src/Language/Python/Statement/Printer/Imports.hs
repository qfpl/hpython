module Language.Python.Statement.Printer.Imports where

import Papa
import Text.PrettyPrint hiding ((<>), comma)

import Language.Python.Printer.Combinators
import Language.Python.Printer.DottedName
import Language.Python.Printer.Keywords
import Language.Python.Printer.Identifier
import Language.Python.Printer.Symbols
import Language.Python.Statement.AST.Imports

importStatement :: ImportStatement a -> Doc
importStatement s =
  case s of
    ImportStatementName v _ -> importName v
    ImportStatementFrom v _ -> importFrom v

importName :: ImportName a -> Doc
importName (ImportName v _) =
  text "import" <>
  whitespaceBeforeF dottedAsNames v

dottedAsNames :: DottedAsNames a -> Doc
dottedAsNames (DottedAsNames h t _) =
  dottedAsName h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' comma) dottedAsName)
    t

dottedAsName :: DottedAsName a -> Doc
dottedAsName (DottedAsName l r _) =
  dottedName l <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' kAs) identifier)
    r

importFrom :: ImportFrom a -> Doc
importFrom (ImportFrom f i _) =
  text "from" <>
  sumElim
    (whitespaceAfterF $
     sumElim
      (whitespaceBeforeF dottedName)
      (beforeF (foldMap . betweenWhitespace' $ either dot ellipsis) dottedName))
    (foldMapOf (_Wrapped.folded) $ either dot ellipsis)
    f <>
  text "import" <>
  sumElim
    (whitespaceBeforeF $
     sumElim
      (asterisk.getConst)
      (betweenF leftParen rightParen (betweenWhitespace'F importAsNames)))
    (whitespaceBeforeF importAsNames)
  i

importAsNames :: ImportAsNames a -> Doc
importAsNames (ImportAsNames h t c _) =
  importAsName h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' comma) importAsName)
    t <>
  foldMap (betweenWhitespace' comma) c

importAsName :: ImportAsName a -> Doc
importAsName (ImportAsName l r _) =
  identifier l <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' kAs) identifier)
    r
