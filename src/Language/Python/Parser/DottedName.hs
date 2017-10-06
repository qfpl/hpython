module Language.Python.Parser.DottedName where

import Papa
import Text.Trifecta hiding (Unspaced, dot)

import Language.Python.AST.DottedName
import Language.Python.Parser.Combinators
import Language.Python.Parser.Identifier
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols

import Text.Parser.Unspaced

dottedName :: DeltaParsing m => Unspaced m (DottedName SrcInfo)
dottedName =
  annotated $
  DottedName <$>
  identifier <*>
  manyF (beforeF (betweenWhitespace dot) identifier)
