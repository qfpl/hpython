module Language.Python.Parser.Identifier where

import Papa
import Data.Text (pack)
import Text.Trifecta

import Language.Python.AST.Identifier
import Language.Python.Parser.SrcInfo

identifier :: DeltaParsing m => m (Identifier SrcInfo)
identifier =
  annotated $
  Identifier . pack <$>
  liftA2 (:) idStart (many $ try idContinue)

idStart :: CharParsing m => m Char
idStart = try letter <|> char '_'

idContinue :: CharParsing m => m Char
idContinue = try idStart <|> digit
