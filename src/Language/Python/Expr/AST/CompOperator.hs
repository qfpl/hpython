{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.CompOperator where

import Papa

data CompOperator ws
  = CompLT
  { _compLT_spaceBefore :: [ws]
  , _compLT_spaceAfter :: [ws]
  }
  | CompGT
  { _compGT_spaceBefore :: [ws]
  , _compGT_spaceAfter :: [ws]
  }
  | CompEq
  { _compEq_spaceBefore :: [ws]
  , _compEq_spaceAfter :: [ws]
  }
  | CompGEq
  { _compGEq_spaceBefore :: [ws]
  , _compGEq_spaceAfter :: [ws]
  }
  | CompLEq
  { _compLEq_spaceBefore :: [ws]
  , _compLEq_spaceAfter :: [ws]
  }
  | CompNEq
  { _compNEq_spaceBefore :: [ws]
  , _compNEq_spaceAfter :: [ws]
  }
  | CompIs
  { _compIs_spaceBefore :: NonEmpty ws
  , _compIs_spaceAfter :: NonEmpty ws
  }
  | CompIsNot
  { _compIsNot_spaceBefore :: NonEmpty ws
  , _compIsNot_spaceBetween :: NonEmpty ws
  , _compIsNot_spaceAfter :: NonEmpty ws
  }
  | CompIn
  { _compIn_spaceBefore :: NonEmpty ws
  , _compIn_spaceAfter :: NonEmpty ws
  }
  | CompNotIn
  { _compNotIn_spaceBefore :: NonEmpty ws
  , _compNotIn_spaceBetween :: NonEmpty ws
  , _compNotIn_spaceAfter :: NonEmpty ws
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''CompOperator
