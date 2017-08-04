{-# language GADTs, StandaloneDeriving, LambdaCase #-}
module Language.Python.AST.ShortBytesChar
  ( ShortBytesChar
  , _ShortBytesCharSingle
  , _ShortBytesCharDouble
  , _shortStringChar_value
  )
  where

import Papa

import Language.Python.AST.Symbols (SingleQuote, DoubleQuote)

data ShortBytesChar inside where
  ShortBytesCharSingle :: Char -> ShortBytesChar SingleQuote
  ShortBytesCharDouble :: Char -> ShortBytesChar DoubleQuote

_shortStringChar_value :: ShortBytesChar inside -> Char
_shortStringChar_value (ShortBytesCharSingle c) = c
_shortStringChar_value (ShortBytesCharDouble c) = c

_ShortBytesCharSingle :: Prism' Char (ShortBytesChar SingleQuote)
_ShortBytesCharSingle =
  prism'
  _shortStringChar_value
  (\case
      '\n' -> Nothing
      '\\' -> Nothing
      '\'' -> Nothing
      c -> Just $ ShortBytesCharSingle c)

_ShortBytesCharDouble :: Prism' Char (ShortBytesChar DoubleQuote)
_ShortBytesCharDouble =
  prism'
  _shortStringChar_value
  (\case
      '\n' -> Nothing
      '\\' -> Nothing
      '"' -> Nothing
      c
        | isAscii c -> Just $ ShortBytesCharDouble c
        | otherwise -> Nothing)
  
deriving instance Eq (ShortBytesChar inside)
deriving instance Show (ShortBytesChar inside)
