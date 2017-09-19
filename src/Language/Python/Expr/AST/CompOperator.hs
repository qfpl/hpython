{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.CompOperator where

import Papa

import Language.Python.AST.Symbols

data CompOperator
  = CompLT
  { _compLT_spaceBefore :: [WhitespaceChar]
  , _compLT_spaceAfter :: [WhitespaceChar]
  }
  | CompGT
  { _compGT_spaceBefore :: [WhitespaceChar]
  , _compGT_spaceAfter :: [WhitespaceChar]
  }
  | CompEq
  { _compEq_spaceBefore :: [WhitespaceChar]
  , _compEq_spaceAfter :: [WhitespaceChar]
  }
  | CompGEq
  { _compGEq_spaceBefore :: [WhitespaceChar]
  , _compGEq_spaceAfter :: [WhitespaceChar]
  }
  | CompLEq
  { _compLEq_spaceBefore :: [WhitespaceChar]
  , _compLEq_spaceAfter :: [WhitespaceChar]
  }
  | CompNEq
  { _compNEq_spaceBefore :: [WhitespaceChar]
  , _compNEq_spaceAfter :: [WhitespaceChar]
  }
  | CompIs
  { _compIs_spaceBefore :: NonEmpty WhitespaceChar
  , _compIs_spaceAfter :: NonEmpty WhitespaceChar
  }
  | CompIsNot
  { _compIsNot_spaceBefore :: NonEmpty WhitespaceChar
  , _compIsNot_spaceBetween :: NonEmpty WhitespaceChar
  , _compIsNot_spaceAfter :: NonEmpty WhitespaceChar
  }
  | CompIn
  { _compIn_spaceBefore :: NonEmpty WhitespaceChar
  , _compIn_spaceAfter :: NonEmpty WhitespaceChar
  }
  | CompNotIn
  { _compNotIn_spaceBefore :: NonEmpty WhitespaceChar
  , _compNotIn_spaceBetween :: NonEmpty WhitespaceChar
  , _compNotIn_spaceAfter :: NonEmpty WhitespaceChar
  }
  deriving (Eq, Show)

makeLenses ''CompOperator
