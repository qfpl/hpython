module Language.Python.Expr.Parser.EscapeSeq (parseEscapeSeq) where

import Papa
import Data.Digit
import Text.Parser.Char
import Text.Parser.Combinators

import Language.Python.Expr.AST.EscapeSeq

parseEscapeSeq :: CharParsing m => m EscapeSeq
parseEscapeSeq = char '\\' *> (escapeSequence <?> "escape sequence")
  where
    escapeSequence =
      try (string "newline" $> Slash_newline) <|>
      try (string "\\" $> Slash_backslash) <|>
      try (string "'" $> Slash_singlequote) <|>
      try (string "\"" $> Slash_doublequote) <|>
      try (string "a" $> Slash_a) <|>
      try (string "f" $> Slash_f) <|>
      try (string "b" $> Slash_b) <|>
      try (string "n" $> Slash_n) <|>
      try (string "r" $> Slash_r) <|>
      try (string "t" $> Slash_t) <|>
      try (string "v" $> Slash_v) <|>
      try (Slash_octal <$> some1 parseOctal) <|>
      Slash_hex <$> (char 'x' *> parseHeXaDeCiMaL) <*> some1 parseHeXaDeCiMaL

