{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Language.Python.Syntax.Float where

import Control.Lens.Lens (Lens')
import Control.Lens.Review ((#))
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.Digit.Char (charDecimal)
import Data.Digit.Decimal (DecDigit)
import Data.Generics.Product.Typed (typed)
import Data.These (These(..))
import GHC.Generics (Generic, Generic1)

import Language.Python.Optics.Idents (HasIdents'(..))
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Digits.Sig

-- | Positive or negative, as in @-7@
data Sign = Pos | Neg deriving (Eq, Ord, Show, Generic)

-- | When a floating point literal is in scientific notation, it includes the character
-- @e@, which can be lower or upper case.
data E = Ee | EE deriving (Eq, Ord, Show, Generic)

-- | The exponent of a floating point literal.
--
-- An @e@, followed by an optional 'Sign', followed by at least one digit.
data FloatExponent = FloatExponent E (Maybe Sign) (Digits DecDigit)
  deriving (Eq, Ord, Show, Generic)

-- | A literal floating point value.
--
-- Eg. @7.63@
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#floating-point-literals>
data FloatLiteral a
  -- | \'Complete\' floats
  --
  -- @12.@
  --
  -- @12.34@
  --
  -- @12.e34@
  --
  -- @12.34e56@
  = FloatLiteralFull
  { _floatLiteralAnn :: Ann a
  , _floatLiteralFullLeft :: Digits DecDigit
  , _floatLiteralFullRight
      :: Maybe (These (Digits DecDigit) FloatExponent)
  }
  -- | Floats that begin with a decimal point
  --
  -- @.12@
  --
  -- @.12e34@
  | FloatLiteralPoint
  { _floatLiteralAnn :: Ann a
  -- . [0-9]+
  , _floatLiteralPointRight :: Digits DecDigit
  -- [ 'e' ['-' | '+'] [0-9]+ ]
  , _floatLiteralPointExponent :: Maybe FloatExponent
  }
  -- | Floats with no decimal points
  --
  -- @12e34@
  | FloatLiteralWhole
  { _floatLiteralAnn :: Ann a
  -- [0-9]+
  , _floatLiteralWholeRight :: Digits DecDigit
  -- [ 'e' ['-' | '+'] [0-9]+ ]
  , _floatLiteralWholeExponent :: FloatExponent
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
deriveEq1 ''FloatLiteral
deriveOrd1 ''FloatLiteral
deriveShow1 ''FloatLiteral

instance HasAnn FloatLiteral where
  annot :: forall a. Lens' (FloatLiteral a) (Ann a)
  annot = typed @(Ann a)

instance HasIdents' (FloatLiteral a) (FloatLiteral a) v a where; _Idents' _ = pure

showFloatExponent :: FloatExponent -> String
showFloatExponent (FloatExponent e s ds) =
  (case e of; EE -> 'E'; Ee -> 'e') :
  foldMap (\case; Pos -> "+"; Neg -> "-") s <>
  showDigits (charDecimal #) ds

showFloatLiteral :: FloatLiteral a -> String
showFloatLiteral (FloatLiteralFull _ a b) =
  showDigits (charDecimal #) a <> "." <>
  foldMap
    (\case
       This x -> showDigits (charDecimal #) x
       That x -> showFloatExponent x
       These x y ->
         showDigits (charDecimal #) x <>
         showFloatExponent y)
    b
showFloatLiteral (FloatLiteralPoint _ a b) =
  '.' :
  showDigits (charDecimal #) a <>
  foldMap showFloatExponent b
showFloatLiteral (FloatLiteralWhole _ a b) =
  showDigits (charDecimal #) a <>
  showFloatExponent b
