{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.Integer where

import Papa
import Data.Deriving
import Data.Separated.Before

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.Digits

data Integer' a
  = IntegerDecimal
  { _integerDecimal_value
    :: Either (NonZeroDigit, [Digit]) (NonEmpty Zero)
  , _integer_ann :: a
  }
  | IntegerOct
  { _integerOct_value
    :: Before
         (Either Char_o Char_O)
         (NonEmpty OctDigit)
  , _integer_ann :: a
  }
  | IntegerHex
  { _integerHex_value
    :: Before
         (Either Char_x Char_X)
         (NonEmpty HexDigit)
  , _integer_ann :: a
  }
  | IntegerBin
  { _integerBin_value
    :: Before
         (Either Char_b Char_B)
         (NonEmpty BinDigit)
  , _integer_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''Integer'
deriveEq1 ''Integer'
deriveOrd1 ''Integer'
deriveShow1 ''Integer'
