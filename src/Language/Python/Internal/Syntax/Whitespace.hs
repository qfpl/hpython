module Language.Python.Internal.Syntax.Whitespace where

import Data.Functor (($>))
import Control.Applicative ((<|>))
import Text.Trifecta (CharParsing, char)

data Newline
  = CR
  | LF
  | CRLF
  deriving (Eq, Show)

newline :: CharParsing m => m Newline
newline =
  char '\n' $> LF <|>
  char '\r' *> (char '\n' $> CRLF <|> pure CR)

data Whitespace
  = Space
  | Tab
  | Continued Newline [Whitespace]
  | Newline Newline
  deriving (Eq, Show)
