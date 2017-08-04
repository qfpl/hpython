{-# language LambdaCase #-}
module Language.Python.AST.LongStringChar
  ( LongStringChar
  , _LongStringChar
  , _longStringChar_value
  ) where

import Papa

newtype LongStringChar
  = LongStringChar
  { _longStringChar_value :: Char
  } deriving (Eq, Show)

_LongStringChar :: Prism' Char LongStringChar
_LongStringChar =
  prism'
  _longStringChar_value
  (\case
      '\n' -> Nothing
      '\\' -> Nothing
      c -> Just $ LongStringChar c)
