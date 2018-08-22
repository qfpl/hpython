{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
module Language.Python.Internal.Syntax.Strings where

import Control.Lens.Lens (lens)
import Data.Digit.Octal (OctDigit)
import Data.Digit.HeXaDeCiMaL (HeXDigit)

import Language.Python.Internal.Syntax.Whitespace
import Language.Python.Internal.Syntax.Strings.Raw

data StringPrefix
  = Prefix_u
  | Prefix_U
  deriving (Eq, Show)

data RawStringPrefix
  = Prefix_r
  | Prefix_R
  deriving (Eq, Show)

data BytesPrefix
  = Prefix_b
  | Prefix_B
  deriving (Eq, Show)

data RawBytesPrefix
  = Prefix_br
  | Prefix_Br
  | Prefix_bR
  | Prefix_BR
  | Prefix_rb
  | Prefix_rB
  | Prefix_Rb
  | Prefix_RB
  deriving (Eq, Show)

data StringLiteral a
  = RawStringLiteral
  { _stringLiteralAnn :: a
  , _unsafeRawStringLiteralPrefix :: RawStringPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralType :: StringType
  , _unsafeRawStringLiteralValue :: RawString [Char]
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
  | RawBytesLiteral
  { _stringLiteralAnn :: a
  , _unsafeRawBytesLiteralPrefix :: RawBytesPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralType :: StringType
  , _unsafeRawBytesLiteralValue :: RawString [Char]
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
  deriving (Eq, Show, Functor, Foldable, Traversable)

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

data QuoteType
  = SingleQuote
  | DoubleQuote
  deriving (Eq, Show)

data StringType
  = ShortString
  | LongString
  deriving (Eq, Show)

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
  deriving (Eq, Show)
