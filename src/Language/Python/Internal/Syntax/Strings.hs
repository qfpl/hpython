module Language.Python.Internal.Syntax.Strings where

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
