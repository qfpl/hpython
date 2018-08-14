{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
module Language.Python.Internal.Syntax.Strings where

import Control.Lens.Lens (lens)
import Data.Digit.Octal (OctDigit)
import Data.Digit.HeXaDeCiMaL (HeXDigit)

import Language.Python.Internal.Syntax.Whitespace

data StringLiteral a
  = StringLiteral
  { _stringLiteralAnn :: a
  , _unsafeStringLiteralPrefix :: Maybe StringPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralType :: StringType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | BytesLiteral
  { _stringLiteralAnn :: a
  , _unsafeBytesLiteralPrefix :: BytesPrefix
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralType :: StringType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (StringLiteral a) where
  trailingWhitespace =
    lens
      (\case
          StringLiteral _ _ _ _ _ ws -> ws
          BytesLiteral _ _ _ _ _ ws -> ws)
      (\s ws -> case s of
          StringLiteral a b c d e _ -> StringLiteral a b c d e ws
          BytesLiteral a b c d e _ -> BytesLiteral a b c d e ws)

data QuoteType
  = SingleQuote
  | DoubleQuote
  deriving (Eq, Show)

data StringType
  = ShortString
  | LongString
  deriving (Eq, Show)

data StringPrefix
  = Prefix_r
  | Prefix_R
  | Prefix_u
  | Prefix_U
  deriving (Eq, Show)

data BytesPrefix
  = Prefix_b
  | Prefix_B
  | Prefix_br
  | Prefix_Br
  | Prefix_bR
  | Prefix_BR
  | Prefix_rb
  | Prefix_rB
  | Prefix_Rb
  | Prefix_RB
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
