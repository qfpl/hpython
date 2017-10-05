{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.Float where

import Papa
import Data.Deriving
import Data.Functor.Compose
import Data.Orphans.NonEmpty
import Data.Separated.Before

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.Digits

data Float' a
  = FloatNoDecimal
  { _floatNoDecimal_base :: NonEmpty Digit
  , _float_exponent
    :: Maybe (Before (Either Char_e Char_E) (NonEmpty Digit))
  , _float_ann :: a
  }
  | FloatDecimalNoBase
  { _floatDecimalNoBase_fraction :: NonEmpty Digit
  , _float_exponent
    :: Maybe (Before (Either Char_e Char_E) (NonEmpty Digit))
  , _float_ann :: a
  }
  | FloatDecimalBase
  { _floatDecimalBase_base :: NonEmpty Digit
  , _floatDecimalBase_fraction :: Compose Maybe NonEmpty Digit
  , _float_exponent
    :: Maybe (Before (Either Char_e Char_E) (NonEmpty Digit))
  , _float_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq ''Float'
deriveShow ''Float'
deriveEq1 ''Float'
deriveShow1 ''Float'
makeLenses ''Float'
