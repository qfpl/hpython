module Language.Python.Parser.Identifier where

import Papa
import Text.Trifecta
import qualified Data.HashSet as HashSet
import qualified Text.Parser.Token.Highlight as Highlight

import Language.Python.AST.Identifier
import Language.Python.Parser.SrcInfo

idStyle :: CharParsing m => IdentifierStyle m
idStyle =
  IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = idStart
  , _styleLetter = idContinue
  , _styleReserved =
    HashSet.fromList
    [ "False"
    , "None"
    , "True"
    , "and"
    , "as"
    , "assert"
    , "break"
    , "class"
    , "continue"
    , "def"
    , "del"
    , "elif"
    , "else"
    , "except"
    , "finally"
    , "for"
    , "from"
    , "global"
    , "if"
    , "import"
    , "in"
    , "is"
    , "lambda"
    , "nonlocal"
    , "not"
    , "or"
    , "pass"
    , "raise"
    , "return"
    , "try"
    , "while"
    , "with"
    , "yield"
    ]
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

identifier :: DeltaParsing m => m (Identifier SrcInfo)
identifier = annotated $ Identifier <$> ident idStyle

idStart :: CharParsing m => m Char
idStart = try letter <|> char '_'

idContinue :: CharParsing m => m Char
idContinue = try idStart <|> digit
