module Language.Python.AST.EscapeSeq
  ( EscapeSeq(..)
  , _Escape
  , escapeSeq
  ) where

import Papa
import Data.Digit
import Text.Parser.Char
import Text.Trifecta

import Language.Python.AST.Digits

data EscapeSeq
  = Slash_newline
  | Slash_backslash
  | Slash_singlequote
  | Slash_doublequote
  | Slash_a
  | Slash_f
  | Slash_b
  | Slash_n
  | Slash_r
  | Slash_t
  | Slash_v
  | Slash_octal (NonEmpty OctDigit)
  | Slash_hex HexDigit (NonEmpty HexDigit)
  deriving (Eq, Show)

escapeSeq :: CharParsing m => m EscapeSeq
escapeSeq =
  char '\\' *>
  (try (string "newline" $> Slash_newline) <|>
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
   (Slash_hex <$> parseHeXaDeCiMaL <*> some1 parseHeXaDeCiMaL))

_Escape :: Prism' String EscapeSeq
_Escape =
  prism'
    (\a -> '\\' :
      case a of
        Slash_newline -> "newline"
        Slash_backslash -> "\\"
        Slash_singlequote -> "'"
        Slash_doublequote -> "\""
        Slash_a -> "a"
        Slash_f -> "f"
        Slash_b -> "b"
        Slash_n -> "n"
        Slash_r -> "r"
        Slash_t -> "t"
        Slash_v -> "v"
        Slash_octal oct -> foldMap printOctDigit oct
        Slash_hex h hs -> 'x' : printHexDigit h <> foldMap printHexDigit hs)
    (\val -> case parseString escapeSeq mempty val of
        Success res -> Just res
        Failure _ -> Nothing)
