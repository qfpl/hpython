{-# language LambdaCase #-}
module Language.Python.Expr.AST.LongBytesChar
  ( LongBytesChar
  , _LongBytesChar
  , _longStringChar_value
  , LongBytesCharFinal
  , _LongBytesCharFinalSingle
  , _LongBytesCharFinalDouble
  , _longStringCharFinal_value
  , parseLongBytesChar
  , parseLongBytesCharFinalSingle
  , parseLongBytesCharFinalDouble
  ) where

import Prelude (error)
import Papa
import Data.CharSet as CharSet
import Data.CharSet.Common as CharSet
import GHC.Stack
import Text.Parser.Char

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.StringContent

newtype LongBytesChar
  = LongBytesChar
  { _longStringChar_value :: Char
  } deriving (Eq, Show)

instance AsChar LongBytesChar where
  _Char = _LongBytesChar

_LongBytesChar :: Prism' Char LongBytesChar
_LongBytesChar =
  prism'
  _longStringChar_value
  (\case
      '\0' -> Nothing
      c
        | isAscii c -> Just $ LongBytesChar c
        | otherwise -> Nothing)

newtype LongBytesCharFinal a
  = LongBytesCharFinal
  { _longStringCharFinal_value :: Char
  } deriving (Eq, Show)

_LongBytesCharFinalSingle :: Prism' Char (LongBytesCharFinal SingleQuote)
_LongBytesCharFinalSingle =
  prism'
  _longStringCharFinal_value
  (\case
      '\0' -> Nothing
      '\\' -> Nothing
      '\'' -> Nothing
      c
        | isAscii c -> Just $ LongBytesCharFinal c
        | otherwise -> Nothing)

_LongBytesCharFinalDouble :: Prism' Char (LongBytesCharFinal DoubleQuote)
_LongBytesCharFinalDouble =
  prism'
  _longStringCharFinal_value
  (\case
      '\0' -> Nothing
      '\\' -> Nothing
      '"' -> Nothing
      c
        | isAscii c -> Just $ LongBytesCharFinal c
        | otherwise -> Nothing)

parseLongBytesCharFinalSingle
  :: ( HasCallStack
     , CharParsing m
     )
  => m (LongBytesCharFinal SingleQuote)
parseLongBytesCharFinalSingle =
  (\c -> fromMaybe (error $ show c) $ c ^? _LongBytesCharFinalSingle) <$>
  oneOfSet (CharSet.ascii CharSet.\\ CharSet.fromList "\\'\0")

parseLongBytesCharFinalDouble
  :: ( HasCallStack
     , CharParsing m
     )
  => m (LongBytesCharFinal DoubleQuote)
parseLongBytesCharFinalDouble =
  (\c -> fromMaybe (error $ show c) $ c ^? _LongBytesCharFinalDouble) <$>
  oneOfSet (CharSet.ascii CharSet.\\ CharSet.fromList "\\\"\0")

parseLongBytesChar
  :: ( HasCallStack
     , CharParsing m
     )
  => m LongBytesChar
parseLongBytesChar =
  (\c -> fromMaybe (error $ show c) $ c ^? _LongBytesChar) <$>
  oneOfSet CharSet.ascii
