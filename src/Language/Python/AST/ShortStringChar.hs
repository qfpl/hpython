{-# language GADTs, StandaloneDeriving, LambdaCase #-}
module Language.Python.AST.ShortStringChar
  ( ShortStringChar
  , _ShortStringCharSingle
  , _ShortStringCharDouble
  , _shortStringChar_value
  )
  where

import Papa

import Language.Python.AST.Symbols (SingleQuote, DoubleQuote)

data ShortStringChar inside where
  ShortStringCharSingle :: Char -> ShortStringChar SingleQuote
  ShortStringCharDouble :: Char -> ShortStringChar DoubleQuote

_shortStringChar_value :: ShortStringChar inside -> Char
_shortStringChar_value (ShortStringCharSingle c) = c
_shortStringChar_value (ShortStringCharDouble c) = c

_ShortStringCharSingle :: Prism' Char (ShortStringChar SingleQuote)
_ShortStringCharSingle =
  prism'
  _shortStringChar_value
  (\case
      '\n' -> Nothing
      '\'' -> Nothing
      c -> Just $ ShortStringCharSingle c)

_ShortStringCharDouble :: Prism' Char (ShortStringChar DoubleQuote)
_ShortStringCharDouble =
  prism'
  _shortStringChar_value
  (\case
      '\n' -> Nothing
      '"' -> Nothing
      c -> Just $ ShortStringCharDouble c)
  
deriving instance Eq (ShortStringChar inside)
deriving instance Show (ShortStringChar inside)
