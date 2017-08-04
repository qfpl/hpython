module Language.Python.AST.BytesEscapeSeq
  ( BytesEscapeSeq
  , _BytesEscapeSeq
  , _bytesEscapeSeq_value
  )
  where

import Papa

newtype BytesEscapeSeq
  = BytesEscapeSeq
  { _bytesEscapeSeq_value :: Char
  } deriving (Eq, Show)

_BytesEscapeSeq :: Prism' Char BytesEscapeSeq
_BytesEscapeSeq =
  prism'
  _bytesEscapeSeq_value
  (\c -> if isAscii c then Just $ BytesEscapeSeq c else Nothing)
