{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.CompOperator where

import Papa
import Data.Deriving

import Language.Python.AST.Symbols

data CompOperator
  = CompLT
  | CompGT
  | CompEq
  | CompGEq
  | CompLEq
  | CompNEq
  | CompIs
  { _compIs_spaceAfter :: WhitespaceChar
  }
  | CompIsNot
  { _compIsNot_spaceBetween :: NonEmpty WhitespaceChar
  , _compIsNot_spaceAfter :: WhitespaceChar
  }
  | CompIn
  { _compIn_spaceAfter :: WhitespaceChar
  }
  | CompNotIn
  { _compNotIn_spaceBetween :: NonEmpty WhitespaceChar
  , _compNotIn_spaceAfter :: WhitespaceChar
  }
  deriving (Eq, Show)

makeLenses ''CompOperator
