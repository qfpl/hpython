module Language.Python35.Parse.Digits where

import Control.Applicative (many)
import Text.Parser.Char (CharParsing)

import Language.Python35.Syntax.Digits

parseDigits :: CharParsing m => m a -> m (Digits a)
parseDigits a = consDigits <$> a <*> parseDigitsTail a

parseDigitsTail :: CharParsing m => m a -> m (DigitsTail a)
parseDigitsTail a = DigitsTail <$> many a
