module Language.Python.Expr.AST.EscapeSeq where

import Papa
import Data.Digit
import Text.Trifecta

import Language.Python.Expr.AST.Digits

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
  deriving (Eq, Show, Ord)

isEscapeChar :: Char -> Bool
isEscapeChar c =
  case c of
    '\\' -> True
    '\'' -> True
    '"' -> True
    'a' -> True
    'f' -> True
    'b' -> True
    'n' -> True
    'r' -> True
    't' -> True
    'v' -> True
    _ -> False

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
    (\val -> case val of
        "" -> Nothing
        '\\' : rest -> case rest of
          "newline" -> Just Slash_newline
          "\\" -> Just Slash_backslash
          "'" -> Just Slash_singlequote
          "\"" -> Just Slash_doublequote
          "a" -> Just Slash_a
          "f" -> Just Slash_f
          "b" -> Just Slash_b
          "n" -> Just Slash_n
          "r" -> Just Slash_r
          "t" -> Just Slash_t
          "v" -> Just Slash_v
          _ -> case parseString (try oct <|> hex) mempty rest of
            Success a -> Just a
            Failure _ -> Nothing
            where
              oct = Slash_octal <$> some1 parseOctal
              hex = Slash_hex <$> (char 'x' *> parseHeXaDeCiMaL) <*> some1 parseHeXaDeCiMaL
        _ -> Nothing)
