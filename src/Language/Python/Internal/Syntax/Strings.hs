{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Internal.Syntax.Strings
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.Strings where

import Control.Lens.Lens (lens)
import Control.Lens.TH (makeLensesFor)
import Data.Digit.Octal (OctDigit)
import Data.Digit.Hexadecimal.MixedCase (HeXDigit(..))
import Data.Maybe (isJust)
import Data.Text (Text)

import Language.Python.Internal.Syntax.Whitespace

data QuoteType
  = SingleQuote
  | DoubleQuote
  deriving (Eq, Ord, Show)

data StringType
  = ShortString
  | LongString
  deriving (Eq, Ord, Show)

data StringPrefix
  = Prefix_u
  | Prefix_U
  deriving (Eq, Ord, Show)

data RawStringPrefix
  = Prefix_r
  | Prefix_R
  deriving (Eq, Ord, Show)

data BytesPrefix
  = Prefix_b
  | Prefix_B
  deriving (Eq, Ord, Show)

data RawBytesPrefix
  = Prefix_br
  | Prefix_Br
  | Prefix_bR
  | Prefix_BR
  | Prefix_rb
  | Prefix_rB
  | Prefix_Rb
  | Prefix_RB
  deriving (Eq, Ord, Show)

hasStringPrefix :: StringLiteral a -> Bool
hasStringPrefix RawStringLiteral{} = True
hasStringPrefix RawBytesLiteral{} = True
hasStringPrefix (StringLiteral _ a _ _ _ _) = isJust a
hasStringPrefix BytesLiteral{} = True

data StringLiteral a
  = RawStringLiteral
  { _stringLiteralAnn :: a
  , _unsafeRawStringLiteralPrefix :: RawStringPrefix
  , _stringLiteralStringType :: StringType
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | StringLiteral
  { _stringLiteralAnn :: a
  , _unsafeStringLiteralPrefix :: Maybe StringPrefix
  , _stringLiteralStringType :: StringType
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | RawBytesLiteral
  { _stringLiteralAnn :: a
  , _unsafeRawBytesLiteralPrefix :: RawBytesPrefix
  , _stringLiteralStringType :: StringType
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | BytesLiteral
  { _stringLiteralAnn :: a
  , _unsafeBytesLiteralPrefix :: BytesPrefix
  , _stringLiteralStringType :: StringType
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (StringLiteral a) where
  trailingWhitespace =
    lens
      (\case
          RawStringLiteral _ _ _ _ _ ws -> ws
          StringLiteral _ _ _ _ _ ws -> ws
          RawBytesLiteral _ _ _ _ _ ws -> ws
          BytesLiteral _ _ _ _ _ ws -> ws)
      (\s ws -> case s of
          StringLiteral a b c d e _ -> StringLiteral a b c d e ws
          RawStringLiteral a b c d e _ -> RawStringLiteral a b c d e ws
          BytesLiteral a b c d e _ -> BytesLiteral a b c d e ws
          RawBytesLiteral a b c d e _ -> RawBytesLiteral a b c d e ws)

data PyChar
  = Char_newline
  | Char_octal OctDigit OctDigit OctDigit
  | Char_hex HeXDigit HeXDigit
  | Char_uni16
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
  | Char_uni32
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
  | Char_esc_bslash
  | Char_esc_singlequote
  | Char_esc_doublequote
  | Char_esc_a
  | Char_esc_b
  | Char_esc_f
  | Char_esc_n
  | Char_esc_r
  | Char_esc_t
  | Char_esc_v
  | Char_lit Char
  deriving (Eq, Ord, Show)

isEscape :: PyChar -> Bool
isEscape c =
  case c of
    Char_newline -> True
    Char_octal{} -> True
    Char_hex{} -> True
    Char_uni16{} -> True
    Char_uni32{} -> True
    Char_esc_bslash -> True
    Char_esc_singlequote -> True
    Char_esc_doublequote -> True
    Char_esc_a -> True
    Char_esc_b -> True
    Char_esc_f -> True
    Char_esc_n -> True
    Char_esc_r -> True
    Char_esc_t -> True
    Char_esc_v -> True
    Char_lit{} -> False

fromHaskellString :: String -> [PyChar]
fromHaskellString "" = []
fromHaskellString (c:cs) =
  (case c of
    '\\' -> Char_esc_bslash
    '\'' -> Char_esc_singlequote
    '\"' -> Char_esc_doublequote
    '\a' -> Char_esc_a
    '\b' -> Char_esc_b
    '\f' -> Char_esc_f
    '\n' -> Char_esc_n
    '\r' -> Char_esc_r
    '\t' -> Char_esc_t
    '\v' -> Char_esc_v
    '\0' -> Char_hex HeXDigit0 HeXDigit0
    _ -> Char_lit c) :
  fromHaskellString cs

showStringPrefix :: StringPrefix -> Text
showStringPrefix sp =
  case sp of
    Prefix_u -> "u"
    Prefix_U -> "U"

showRawStringPrefix :: RawStringPrefix -> Text
showRawStringPrefix sp =
  case sp of
    Prefix_r -> "r"
    Prefix_R -> "R"

showBytesPrefix :: BytesPrefix -> Text
showBytesPrefix sp =
  case sp of
    Prefix_b -> "b"
    Prefix_B -> "B"

showRawBytesPrefix :: RawBytesPrefix -> Text
showRawBytesPrefix sp =
  case sp of
    Prefix_br -> "br"
    Prefix_Br -> "Br"
    Prefix_bR -> "bR"
    Prefix_BR -> "BR"
    Prefix_rb -> "rb"
    Prefix_rB -> "rB"
    Prefix_Rb -> "Rb"
    Prefix_RB -> "RB"

showQuoteType :: QuoteType -> Char
showQuoteType qt =
  case qt of
    DoubleQuote -> '\"'
    SingleQuote -> '\''

makeLensesFor [("_stringLiteralValue", "stringLiteralValue")] ''StringLiteral