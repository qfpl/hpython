{-# language LambdaCase #-}
module Language.Python.AST.LongStringChar
  ( LongStringChar
  , _LongStringChar
  , _longStringChar_value
  , LongStringCharFinal
  , _LongStringCharFinalSingle
  , _LongStringCharFinalDouble
  , _longStringCharFinal_value
  , parseLongStringCharFinalSingle
  , parseLongStringCharFinalDouble
  ) where

import Prelude (error)
import Papa
import Data.CharSet as CharSet
import Data.CharSet.Common as CharSet
import GHC.Stack
import Language.Python.AST.Symbols
import Text.Parser.Char

newtype LongStringChar
  = LongStringChar
  { _longStringChar_value :: Char
  } deriving (Eq, Show)

_LongStringChar :: Prism' Char LongStringChar
_LongStringChar =
  prism'
  _longStringChar_value
  (\case
      '\0' -> Nothing
      c -> Just $ LongStringChar c)

newtype LongStringCharFinal a
  = LongStringCharFinal
  { _longStringCharFinal_value :: Char
  } deriving (Eq, Show)

_LongStringCharFinalSingle :: Prism' Char (LongStringCharFinal SingleQuote)
_LongStringCharFinalSingle =
  prism'
  _longStringCharFinal_value
  (\case
      '\0' -> Nothing
      '\\' -> Nothing
      '\'' -> Nothing
      c -> Just $ LongStringCharFinal c)

_LongStringCharFinalDouble :: Prism' Char (LongStringCharFinal DoubleQuote)
_LongStringCharFinalDouble =
  prism'
  _longStringCharFinal_value
  (\case
      '\0' -> Nothing
      '\\' -> Nothing
      '"' -> Nothing
      c -> Just $ LongStringCharFinal c)

parseLongStringCharFinalSingle
  :: ( HasCallStack
     , CharParsing m
     )
  => m (LongStringCharFinal SingleQuote)
parseLongStringCharFinalSingle =
  (\c -> fromMaybe (error $ show c) $ c ^? _LongStringCharFinalSingle) <$>
  oneOfSet (CharSet.ascii CharSet.\\ CharSet.fromList "\\'\0")

parseLongStringCharFinalDouble
  :: ( HasCallStack
     , CharParsing m
     ) => m (LongStringCharFinal DoubleQuote)
parseLongStringCharFinalDouble =
  (\c -> fromMaybe (error $ show c) $ c ^? _LongStringCharFinalDouble) <$>
  oneOfSet (CharSet.ascii CharSet.\\ CharSet.fromList "\\\"\0")
