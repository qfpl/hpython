{-# language GADTs, StandaloneDeriving, LambdaCase #-}
module Language.Python.AST.ShortBytesChar
  ( ShortBytesChar
  , _ShortBytesCharSingle
  , _ShortBytesCharDouble
  , _shortStringChar_value
  , parseShortBytesCharSingle
  , parseShortBytesCharDouble
  )
  where

import Prelude (error)
import Papa
import Data.CharSet as CharSet
import Data.CharSet.Common as CharSet
import GHC.Stack
import Text.Parser.Char
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
      '\r' -> Nothing
      '\'' -> Nothing
      '\\' -> Nothing
      '\0' -> Nothing
      c -> Just $ ShortBytesCharSingle c)

_ShortBytesCharDouble :: Prism' Char (ShortBytesChar DoubleQuote)
_ShortBytesCharDouble =
  prism'
  _shortStringChar_value
  (\case
      '\n' -> Nothing
      '\r' -> Nothing
      '"' -> Nothing
      '\\' -> Nothing
      '\0' -> Nothing
      c
        | isAscii c -> Just $ ShortBytesCharDouble c
        | otherwise -> Nothing)
  
deriving instance Eq (ShortBytesChar inside)
deriving instance Show (ShortBytesChar inside)

parseShortBytesCharSingle
  :: ( HasCallStack
     , CharParsing m
     ) => m (ShortBytesChar SingleQuote)
parseShortBytesCharSingle =
  (\c -> fromMaybe (error $ show c) $ c ^? _ShortBytesCharSingle) <$>
  oneOfSet
    (CharSet.ascii CharSet.\\ CharSet.fromList "\n\r'\\\0")

parseShortBytesCharDouble
  :: ( HasCallStack
     , CharParsing m
     ) => m (ShortBytesChar DoubleQuote)
parseShortBytesCharDouble =
  (\c -> fromMaybe (error $ show c) $ c ^? _ShortBytesCharDouble) <$>
  oneOfSet
    (CharSet.ascii CharSet.\\ CharSet.fromList "\n\r\"\\\0")
