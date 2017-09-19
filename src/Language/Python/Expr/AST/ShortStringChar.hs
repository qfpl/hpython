{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
module Language.Python.Expr.AST.ShortStringChar
  ( ShortStringChar
  , _ShortStringCharSingle
  , _ShortStringCharDouble
  , _shortStringChar_value
  , parseShortStringCharSingle
  , parseShortStringCharDouble
  )
  where

import Prelude (error)
import Papa
import Data.CharSet as CharSet
import Data.CharSet.Common as CharSet
import GHC.Stack
import Text.Parser.Char

import Language.Python.AST.Symbols (SingleQuote, DoubleQuote)
import Language.Python.Expr.AST.StringContent

data ShortStringChar inside where
  ShortStringCharSingle :: Char -> ShortStringChar SingleQuote
  ShortStringCharDouble :: Char -> ShortStringChar DoubleQuote

instance AsChar (ShortStringChar SingleQuote) where
  _Char = _ShortStringCharSingle

instance AsChar (ShortStringChar DoubleQuote) where
  _Char = _ShortStringCharDouble

_shortStringChar_value :: ShortStringChar inside -> Char
_shortStringChar_value (ShortStringCharSingle c) = c
_shortStringChar_value (ShortStringCharDouble c) = c

_ShortStringCharSingle :: Prism' Char (ShortStringChar SingleQuote)
_ShortStringCharSingle =
  prism'
  _shortStringChar_value
  (\case
      '\n' -> Nothing
      '\r' -> Nothing
      '\'' -> Nothing
      '\\' -> Nothing
      '\0' -> Nothing
      c -> Just $ ShortStringCharSingle c)

_ShortStringCharDouble :: Prism' Char (ShortStringChar DoubleQuote)
_ShortStringCharDouble =
  prism'
  _shortStringChar_value
  (\case
      '\n' -> Nothing
      '\r' -> Nothing
      '"' -> Nothing
      '\\' -> Nothing
      '\0' -> Nothing
      c -> Just $ ShortStringCharDouble c)
  
deriving instance Eq (ShortStringChar inside)
deriving instance Show (ShortStringChar inside)

parseShortStringCharSingle
  :: ( HasCallStack
     , CharParsing m
     ) => m (ShortStringChar SingleQuote)
parseShortStringCharSingle =
  (\c -> fromMaybe (error $ show c) $ c ^? _ShortStringCharSingle) <$>
  oneOfSet
    (CharSet.ascii CharSet.\\ CharSet.fromList "\n\r'\\\0")

parseShortStringCharDouble
  :: ( HasCallStack
     , CharParsing m
     ) => m (ShortStringChar DoubleQuote)
parseShortStringCharDouble =
  (\c -> fromMaybe (error $ show c) $ c ^? _ShortStringCharDouble) <$>
  oneOfSet
    (CharSet.ascii CharSet.\\ CharSet.fromList "\n\r\"\\\0")
