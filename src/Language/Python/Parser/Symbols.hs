module Language.Python.Parser.Symbols where

import Papa hiding (Plus, Space)
import Text.Parser.Char
import Text.Parser.Combinators

import Language.Python.AST.Symbols

newlineChar :: CharParsing m => m NewlineChar
newlineChar =
  ((char '\r' $> CR) <|>
  (char '\n' $> LF) <|>
  (string "\r\n" $> CRLF)) <?> "newline"

leftParen :: CharParsing m => m LeftParen
leftParen = char '(' $> LeftParen

rightParen :: CharParsing m => m RightParen
rightParen = char ')' $> RightParen

doubleAsterisk :: CharParsing m => m DoubleAsterisk
doubleAsterisk = string "**" $> DoubleAsterisk

asterisk :: CharParsing m => m Asterisk
asterisk = char '*' $> Asterisk

colon :: CharParsing m => m Colon
colon = char ':' $> Colon

semicolon :: CharParsing m => m Semicolon
semicolon = char ';' $> Semicolon

tripleDoublequote :: CharParsing m => m TripleDoubleQuote
tripleDoublequote = string "\"\"\"" $> TripleDoubleQuote

tripleSinglequote :: CharParsing m => m TripleSingleQuote
tripleSinglequote = string "'''" $> TripleSingleQuote

doubleQuote :: CharParsing m => m DoubleQuote
doubleQuote = char '"' $> DoubleQuote

singleQuote :: CharParsing m => m SingleQuote
singleQuote = char '\'' $> SingleQuote

zero :: CharParsing m => m Zero
zero = char '0' $> Zero

oO :: CharParsing m => m (Either Char_o Char_O)
oO =
  try (fmap Left $ char 'o' $> Char_o) <|>
  fmap Right (char 'O' $> Char_O)

xX :: CharParsing m => m (Either Char_x Char_X)
xX =
  try (fmap Left $ char 'x' $> Char_x) <|>
  fmap Right (char 'X' $> Char_X)

bB :: CharParsing m => m (Either Char_b Char_B)
bB =
  try (fmap Left $ char 'b' $> Char_b) <|>
  fmap Right (char 'B' $> Char_B)

eE :: CharParsing m => m (Either Char_e Char_E)
eE =
  try (fmap Left $ char 'e' $> Char_e) <|>
  fmap Right (char 'E' $> Char_E)

plus :: CharParsing m => m Plus
plus = char '+' $> Plus

minus :: CharParsing m => m Minus
minus = char '-' $> Minus

jJ :: CharParsing m => m (Either Char_j Char_J)
jJ =
  try (fmap Left $ char 'j' $> Char_j) <|>
  fmap Right (char 'J' $> Char_J)

comma :: CharParsing m => m Comma
comma = char ',' $> Comma

dot :: CharParsing m => m Dot
dot = char '.' $> Dot

whitespaceChar :: CharParsing m => m WhitespaceChar
whitespaceChar =
  (char ' ' $> Space) <|>
  (char '\t' $> Tab) <|>
  fmap Continued (char '\\' *> newlineChar)

equals :: CharParsing m => m Equals
equals = char '=' $> Equals

ellipsis :: CharParsing m => m Ellipsis
ellipsis = string "..." $> Ellipsis

rightArrow :: CharParsing m => m RightArrow
rightArrow = string "->" $> RightArrow

formFeed :: CharParsing m => m FormFeed
formFeed = char '\f' $> FormFeed

indentationChar :: CharParsing m => m IndentationChar
indentationChar =
  ((char ' ' $> IndentSpace) <|>
  (char '\t' $> IndentTab) <|>
  (IndentContinued <$> (char '\\' *> try newlineChar) <*>
   many indentationChar)) <?> "indentation character"
  
