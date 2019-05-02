{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language TemplateHaskell #-}
module Language.Python.Syntax.Imag where

import Control.Lens.Lens (Lens')
import Control.Lens.Review ((#))
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.Digit.Char (charDecimal)
import Data.Digit.Decimal (DecDigit)
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic, Generic1)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Float
import Language.Python.Syntax.Digits.Sig

-- | Imaginary number literals
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#imaginary-literals>
data ImagLiteral a
  -- | A decimal integer followed by a \'j\'
  --
  -- @12j@
  = ImagLiteralInt
  { _imagLiteralAnn :: Ann a
  , _unsafeImagLiteralIntValue :: Digits DecDigit
  , _imagLiteralUppercase :: Bool
  }
  -- | A float followed by a \'j\'
  --
  -- @12.j@
  --
  -- @12.3j@
  --
  -- @.3j@
  | ImagLiteralFloat
  { _imagLiteralAnn :: Ann a
  , _unsafeImagLiteralFloatValue :: FloatLiteral a
  , _imagLiteralUppercase :: Bool
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
deriveEq1 ''ImagLiteral
deriveOrd1 ''ImagLiteral
deriveShow1 ''ImagLiteral

instance HasAnn ImagLiteral where
  annot :: forall a. Lens' (ImagLiteral a) (Ann a)
  annot = typed @(Ann a)

showImagLiteral :: ImagLiteral a -> String
showImagLiteral (ImagLiteralInt _ ds b) =
  showDigits (charDecimal #) ds <>
  [if b then 'J' else 'j']
showImagLiteral (ImagLiteralFloat _ f b) =
  showFloatLiteral f <> [if b then 'J' else 'j']
