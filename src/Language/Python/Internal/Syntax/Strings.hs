{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
module Language.Python.Internal.Syntax.Strings where

import Control.Lens.Lens (lens)
import Data.Digit.Octal (OctDigit)
import Data.Digit.HeXaDeCiMaL (HeXDigit(..))

import Language.Python.Internal.Syntax.Whitespace
import Language.Python.Internal.Syntax.Strings.Raw

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

data StringLiteral a
  = LongRawStringLiteral
  { _stringLiteralAnn :: a
  , _unsafeRawStringLiteralPrefix :: RawStringPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _unsafeLongRawStringLiteralValue :: LongRawString [Char]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | ShortRawStringLiteral
  { _stringLiteralAnn :: a
  , _unsafeRawStringLiteralPrefix :: RawStringPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _unsafeShortRawStringLiteralValue :: ShortRawString [Char]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | StringLiteral
  { _stringLiteralAnn :: a
  , _unsafeStringLiteralPrefix :: Maybe StringPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralType :: StringType
  , _unsafeStringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | LongRawBytesLiteral
  { _stringLiteralAnn :: a
  , _unsafeRawBytesLiteralPrefix :: RawBytesPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _unsafeLongRawBytesLiteralValue :: LongRawString [Char]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | ShortRawBytesLiteral
  { _stringLiteralAnn :: a
  , _unsafeRawBytesLiteralPrefix :: RawBytesPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _unsafeShortRawBytesLiteralValue :: ShortRawString [Char]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | BytesLiteral
  { _stringLiteralAnn :: a
  , _unsafeBytesLiteralPrefix :: BytesPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralType :: StringType
  , _unsafeBytesLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (StringLiteral a) where
  trailingWhitespace =
    lens
      (\case
          LongRawStringLiteral _ _ _ _ ws -> ws
          ShortRawStringLiteral _ _ _ _ ws -> ws
          StringLiteral _ _ _ _ _ ws -> ws
          LongRawBytesLiteral _ _ _ _ ws -> ws
          ShortRawBytesLiteral _ _ _ _ ws -> ws
          BytesLiteral _ _ _ _ _ ws -> ws)
      (\s ws -> case s of
          StringLiteral a b c d e _ -> StringLiteral a b c d e ws
          LongRawStringLiteral a b c d _ -> LongRawStringLiteral a b c d ws
          ShortRawStringLiteral a b c d _ -> ShortRawStringLiteral a b c d ws
          BytesLiteral a b c d e _ -> BytesLiteral a b c d e ws
          LongRawBytesLiteral a b c d _ -> LongRawBytesLiteral a b c d ws
          ShortRawBytesLiteral a b c d _ -> ShortRawBytesLiteral a b c d ws)

data PyChar
  = Char_newline
  | Char_octal OctDigit OctDigit
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
    -- '\0' -> Char_hex HeXDigit0 HeXDigit0
    _ -> Char_lit c) :
  fromHaskellString cs
