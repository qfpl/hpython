module Language.Python36.Parse.Digits where

import Control.Applicative (optional, many)
import Text.Parser.Char (CharParsing, char)

import Language.Python36.Syntax.Digits
import Language.Python.Syntax.Punctuation

parseDigits :: CharParsing m => m a -> m (Digits a)
parseDigits a = consDigits <$> a <*> parseDigitsTail a

parseDigitsTail :: CharParsing m => m a -> m (DigitsTail a)
parseDigitsTail a =
  DigitsTail <$>
  many ((,) <$> optional (MkUnderscore <$ char '_') <*> a)
