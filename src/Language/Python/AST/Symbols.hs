{-# language DataKinds #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
module Language.Python.AST.Symbols where

import Papa

data Backslash = Backslash deriving (Eq, Ord, Show)
data SingleQuote = SingleQuote deriving (Eq, Ord, Show)
data DoubleQuote = DoubleQuote deriving (Eq, Ord, Show)
data TripleSingleQuote = TripleSingleQuote deriving (Eq, Ord, Show)
data TripleDoubleQuote = TripleDoubleQuote deriving (Eq, Ord, Show)
data Plus = Plus deriving (Eq, Ord, Show)
data Minus = Minus deriving (Eq, Ord, Show)
data Char_e = Char_e deriving (Eq, Ord, Show)
data Char_E = Char_E deriving (Eq, Ord, Show)
data Char_o = Char_o deriving (Eq, Ord, Show)
data Char_O = Char_O deriving (Eq, Ord, Show)
data Char_x = Char_x deriving (Eq, Ord, Show)
data Char_X = Char_X deriving (Eq, Ord, Show)
data Char_b = Char_b deriving (Eq, Ord, Show)
data Char_B = Char_B deriving (Eq, Ord, Show)
data Char_j = Char_j deriving (Eq, Ord, Show)
data Char_J = Char_J deriving (Eq, Ord, Show)
data Zero = Zero deriving (Eq, Ord, Show)
data LeftParen = LeftParen deriving (Eq, Ord, Show)
data RightParen = RightParen deriving (Eq, Ord, Show)
data NewlineChar = CR | LF | CRLF deriving (Eq, Ord, Show)
data WhitespaceChar = Space | Tab | Continued NewlineChar deriving (Eq, Ord, Show)
data IndentationChar
  = IndentSpace
  | IndentTab
  | IndentContinued NewlineChar [IndentationChar]
  deriving (Eq, Show, Ord)
data Comma = Comma deriving (Eq, Ord, Show)
data Hash = Hash deriving (Eq, Ord, Show)
data Colon = Colon deriving (Eq, Ord, Show)
data Asterisk = Asterisk deriving (Eq, Ord, Show)
data DoubleAsterisk = DoubleAsterisk deriving (Eq, Ord, Show)
data Pipe = Pipe deriving (Eq, Ord, Show)
data Caret = Caret deriving (Eq, Ord, Show)
data Ampersand = Ampersand deriving (Eq, Ord, Show)
data DoubleLT = DoubleLT deriving (Eq, Ord, Show)
data DoubleGT = DoubleGT deriving (Eq, Ord, Show)
data Equals = Equals deriving (Eq, Ord, Show)
data Semicolon = Semicolon deriving (Eq, Ord, Show)
data Dot = Dot deriving (Eq, Ord, Show)
data Ellipsis = Ellipsis deriving (Eq, Ord, Show)
data RightArrow = RightArrow deriving (Eq, Ord, Show)
data FormFeed = FormFeed deriving (Eq, Ord, Show)
