module Language.Python.Parser.StringContent where

import Papa
import Data.Maybe
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead

import Language.Python.AST.StringContent

import Language.Python.AST.Symbols
import Language.Python.Parser.EscapeSeq

parseStringContentSingle
  :: ( AsChar a
     , CharParsing m
     , LookAheadParsing m
     )
  => m a
  -> m b
  -> m (StringContent SingleQuote a)
parseStringContentSingle p term =
  fromJust . fromList <$>
  manyTill
    (try (Left <$> parseEscapeSeq) <|>
     try (Right <$> p))
    (lookAhead term)

parseStringContentDouble
  :: ( AsChar a
     , CharParsing m
     , LookAheadParsing m
     )
  => m a
  -> m b
  -> m (StringContent DoubleQuote a)
parseStringContentDouble p term =
  fromJust . fromList <$>
  manyTill
    (try (Left <$> parseEscapeSeq) <|>
     try (Right <$> p))
    (lookAhead term)
