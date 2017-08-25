{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.CompOperator where

import Papa
import Data.Deriving

import Language.Python.AST.Symbols

data CompOperator a
  = CompLT
  { _comp_ann :: a
  }
  | CompGT
  { _comp_ann :: a
  }
  | CompEq
  { _comp_ann :: a
  }
  | CompGEq
  { _comp_ann :: a
  }
  | CompLEq
  { _comp_ann :: a
  }
  | CompNEq
  { _comp_ann :: a
  }
  | CompIs
  { _compIs_spaceAfter :: WhitespaceChar
  , _comp_ann :: a
  }
  | CompIsNot
  { _compIsNot_spaceBetween :: NonEmpty WhitespaceChar
  , _compIsNot_spaceAfter :: WhitespaceChar
  , _comp_ann :: a
  }
  | CompIn
  { _compIn_spaceAfter :: WhitespaceChar
  , _comp_ann :: a
  }
  | CompNotIn
  { _compNotIn_spaceBetween :: NonEmpty WhitespaceChar
  , _compNotIn_spaceAfter :: WhitespaceChar
  , _comp_ann :: a
  }
  deriving (Eq, Show)

makeLenses ''CompOperator
deriveEq1 ''CompOperator
deriveShow1 ''CompOperator
