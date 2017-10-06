module Language.Python.Statement.Parser.Imports where

import Papa
import Data.Functor.Sum
import Text.Trifecta hiding (Unspaced(..), comma, dot, colon)

import Language.Python.Parser.Combinators
import Language.Python.Parser.DottedName
import Language.Python.Parser.Identifier
import Language.Python.Parser.Keywords
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols
import Language.Python.Statement.AST.Imports

import Text.Parser.Unspaced

importStatement :: DeltaParsing m => Unspaced m (ImportStatement SrcInfo)
importStatement =
  annotated $
  try (ImportStatementName <$> importName) <|>
  (ImportStatementFrom <$> importFrom)

dottedAsNames :: DeltaParsing m => Unspaced m (DottedAsNames SrcInfo)
dottedAsNames =
  annotated $
  DottedAsNames <$>
  dottedAsName <*>
  manyF (beforeF (betweenWhitespace comma) dottedAsName)

dottedAsName :: DeltaParsing m => Unspaced m (DottedAsName SrcInfo)
dottedAsName =
  annotated $
  DottedAsName <$>
  dottedName <*>
  optionalF (try $ beforeF (betweenWhitespace1 kAs) identifier)

importName :: DeltaParsing m => Unspaced m (ImportName SrcInfo)
importName =
  annotated $
  ImportName <$>
  whitespaceBefore1F dottedAsNames

importFrom :: DeltaParsing m => Unspaced m (ImportFrom SrcInfo)
importFrom =
  annotated $
  ImportFrom <$>
  (string "from" *> fromPart) <*>
  (string "import" *> importPart)
  where
    dotOrEllipsis = (Left <$> try dot) <|> (Right <$> ellipsis)
    fromPart =
      (InL <$>
        try (beforeF (many $ betweenWhitespace dotOrEllipsis) dottedName)) <|>
      (InR . Const <$>
        some1 dotOrEllipsis)

    importPart =
      whitespaceBefore1F $
      try (InL . InL . Const <$> asterisk) <|>
      try
        (InL . InR <$>
          betweenF leftParen rightParen (betweenWhitespaceF importAsNames)) <|>
       (InR <$> importAsNames)

importAsNames :: DeltaParsing m => Unspaced m (ImportAsNames SrcInfo)
importAsNames =
  annotated $
  ImportAsNames <$>
  importAsName <*>
  manyF (beforeF (betweenWhitespace comma) importAsName) <*>
  optional (try $ betweenWhitespace comma)

importAsName :: DeltaParsing m => Unspaced m (ImportAsName SrcInfo)
importAsName =
  annotated $
  ImportAsName <$>
  identifier <*>
  optionalF (try $ beforeF (betweenWhitespace1 kAs) identifier)
