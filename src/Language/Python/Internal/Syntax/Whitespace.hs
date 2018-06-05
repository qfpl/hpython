module Language.Python.Internal.Syntax.Whitespace where

data Newline
  = CR
  | LF
  | CRLF
  deriving (Eq, Show)

data Whitespace
  = Space
  | Tab
  | Continued Newline [Whitespace]
  | Newline Newline
  deriving (Eq, Show)
