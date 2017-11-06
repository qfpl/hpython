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
  (ImportStatementName <$> importName) <|>
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
  (string "import" *>
   whitespaceBefore1F dottedAsNames)

importFrom :: DeltaParsing m => Unspaced m (ImportFrom SrcInfo)
importFrom =
  annotated $
  ImportFrom <$>
  (string "from" *> fromPart) <*>
  (string "import" *> importPart)
  where
    dotOrEllipsis = (Left <$> try dot) <|> (Right <$> ellipsis)
    fromPart =
      try (InL <$>
        whitespaceAfter1F
        (try (InL <$> whitespaceBefore1F dottedName) <|>
         (InR <$>
          beforeF (some1 $ betweenWhitespace dotOrEllipsis) dottedName))) <|>
      (InR <$> betweenWhitespaceF (Const <$> some1 dotOrEllipsis))

    importPart =
      try (InL <$>
        whitespaceBeforeF
          (try (InL . Const <$> asterisk) <|>
           (InR <$>
            betweenF leftParen rightParen (betweenWhitespaceF importAsNames)))) <|>
       (InR <$> whitespaceBefore1F importAsNames)

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
