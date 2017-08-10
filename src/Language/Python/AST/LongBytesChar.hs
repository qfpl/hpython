{-# language LambdaCase #-}
module Language.Python.AST.LongBytesChar
  ( LongBytesChar
  , _LongBytesChar
  , _longStringChar_value
  ) where

import Papa

newtype LongBytesChar
  = LongBytesChar
  { _longStringChar_value :: Char
  } deriving (Eq, Show)

_LongBytesChar :: Prism' Char LongBytesChar
_LongBytesChar =
  prism'
  _longStringChar_value
  (\case
      '\\' -> Nothing
      c
        | isAscii c -> Just $ LongBytesChar c
        | otherwise -> Nothing)
