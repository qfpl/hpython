module Language.Python.Statement.Parser.Imports where

import Papa
import Data.Functor.Sum
import Text.Trifecta hiding (Unspaced(..), comma, dot, colon)
import Text.Parser.LookAhead

import Language.Python.Parser.Combinators
import Language.Python.Parser.DottedName
import Language.Python.Parser.Identifier
import Language.Python.Parser.Keywords
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols
import Language.Python.Statement.AST.Imports

import Text.Parser.Unspaced

importStatement :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (ImportStatement SrcInfo)
importStatement =
  annotated $
  (ImportStatementName <$> importName) <|>
  (ImportStatementFrom <$> importFrom)

dottedAsNames :: DeltaParsing m => Unspaced m (DottedAsNames SrcInfo)
dottedAsNames =
  annotated $
  DottedAsNames <$>
  dottedAsName <*>
  manyF
    (beforeF (try $ betweenWhitespace comma) dottedAsName)

dottedAsName :: DeltaParsing m => Unspaced m (DottedAsName SrcInfo)
dottedAsName =
  annotated $
  DottedAsName <$>
  dottedName <*>
  optionalF
    (beforeF (try $ betweenWhitespace1 kAs) identifier)

importName :: DeltaParsing m => Unspaced m (ImportName SrcInfo)
importName =
  annotated $
  ImportName <$>
  (string "import" *>
   whitespaceBefore1F dottedAsNames)

importFrom :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (ImportFrom SrcInfo)
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
      (InL <$>
        beforeF (try $ many whitespaceChar <* lookAhead (char '*' <|> char '('))
          ((InL . Const <$> asterisk) <|>
           (InR <$>
            betweenF
              leftParen
              rightParen
              (between'F (many anyWhitespaceChar) $ importAsNames anyWhitespaceChar)))) <|>
      (InR <$> whitespaceBefore1F (importAsNames whitespaceChar))

importAsNames
  :: DeltaParsing m
  => Unspaced m ws
  -> Unspaced m (ImportAsNames ws SrcInfo)
importAsNames ws =
  annotated $
  ImportAsNames <$>
  importAsName ws <*>
  manyF
    (beforeF
      (try $
       between' (many ws) comma <*
       notFollowedBy (void newlineChar <|> void (char '#') <|> void (char ';')))
      (importAsName ws)) <*>
  optional (try $ between' (many ws) comma)

importAsName
  :: DeltaParsing m
  => Unspaced m ws
  -> Unspaced m (ImportAsName ws SrcInfo)
importAsName ws =
  annotated $
  ImportAsName <$>
  identifier <*>
  optionalF (beforeF (try $ between'1 ws kAs) identifier)
