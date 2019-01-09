{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Numbers
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Numerical literal values in Python
-}

module Language.Python.Syntax.Numbers
  ( -- * Datatypes
    IntLiteral(..)
  , Sign(..)
  , E(..)
  , FloatExponent(..)
  , FloatLiteral(..)
  , ImagLiteral(..)
    -- * Rendering
    -- | The output of these functions is guaranteed to be valid Python code
  , showIntLiteral
  , showFloatLiteral
  , showFloatExponent
  , showImagLiteral
  )
where

import Control.Lens.Review ((#))
import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Digit.Binary (BinDigit)
import Data.Digit.Char (charHeXaDeCiMaL, charOctal, charBinary, charDecimal)
import Data.Digit.Octal (OctDigit)
import Data.Digit.Decimal (DecDigit)
import Data.Digit.Hexadecimal.MixedCase (HeXDigit)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.These (These(..))
import GHC.Generics (Generic)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

-- | An integer literal value.
--
-- @5@ is an integer literal.
--
-- @6.2@ is a literal but is not an integer
--
-- @x@ might be an integer, but is not a literal
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#integer-literals>
data IntLiteral a
  -- | Decimal
  --
  -- @1234@
  = IntLiteralDec
  { _intLiteralAnn :: a
  , _unsafeIntLiteralDecValue :: NonEmpty DecDigit
  }
  -- | Binary
  --
  -- @0b10110@
  | IntLiteralBin
  { _intLiteralAnn :: a
  , _unsafeIntLiteralBinUppercase :: Bool
  , _unsafeIntLiteralBinValue :: NonEmpty BinDigit
  }
  -- | Octal
  --
  -- @0o1367@
  | IntLiteralOct
  { _intLiteralAnn :: a
  , _unsafeIntLiteralOctUppercase :: Bool
  , _unsafeIntLiteralOctValue :: NonEmpty OctDigit
  }
  -- | Mixed-case hexadecimal
  --
  -- @0x18B4f@
  | IntLiteralHex
  { _intLiteralAnn :: a
  , _unsafeIntLiteralHexUppercase :: Bool
  , _unsafeIntLiteralHexValue :: NonEmpty HeXDigit
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
deriveEq1 ''IntLiteral
deriveOrd1 ''IntLiteral

-- | Positive or negative, as in @-7@
data Sign = Pos | Neg deriving (Eq, Ord, Show, Generic)

-- | When a floating point literal is in scientific notation, it includes the character
-- @e@, which can be lower or upper case.
data E = Ee | EE deriving (Eq, Ord, Show, Generic)

-- | The exponent of a floating point literal.
--
-- An @e@, followed by an optional 'Sign', followed by at least one digit.
data FloatExponent = FloatExponent E (Maybe Sign) (NonEmpty DecDigit)
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
  { _floatLiteralAnn :: a
  , _floatLiteralFullLeft :: NonEmpty DecDigit
  , _floatLiteralFullRight
      :: Maybe (These (NonEmpty DecDigit) FloatExponent)
  }
  -- | Floats that begin with a decimal point
  --
  -- @.12@
  --
  -- @.12e34@
  | FloatLiteralPoint
  { _floatLiteralAnn :: a
  -- . [0-9]+
  , _floatLiteralPointRight :: NonEmpty DecDigit
  -- [ 'e' ['-' | '+'] [0-9]+ ]
  , _floatLiteralPointExponent :: Maybe FloatExponent
  }
  -- | Floats with no decimal points
  --
  -- @12e34@
  | FloatLiteralWhole
  { _floatLiteralAnn :: a
  -- [0-9]+
  , _floatLiteralWholeRight :: NonEmpty DecDigit
  -- [ 'e' ['-' | '+'] [0-9]+ ]
  , _floatLiteralWholeExponent :: FloatExponent
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
deriveEq1 ''FloatLiteral
deriveOrd1 ''FloatLiteral

-- | Imaginary number literals
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#imaginary-literals>
data ImagLiteral a
  -- | A decimal integer followed by a \'j\'
  --
  -- @12j@
  = ImagLiteralInt
  { _imagLiteralAnn :: a
  , _unsafeImagLiteralIntValue :: NonEmpty DecDigit
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
  { _imagLiteralAnn :: a
  , _unsafeImagLiteralFloatValue :: FloatLiteral a
  , _imagLiteralUppercase :: Bool
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
deriveEq1 ''ImagLiteral
deriveOrd1 ''ImagLiteral

showIntLiteral :: IntLiteral a -> Text
showIntLiteral (IntLiteralDec _ n) =
  Text.pack $
  (charDecimal #) <$> NonEmpty.toList n
showIntLiteral (IntLiteralBin _ b n) =
  Text.pack $
  '0' : (if b then 'B' else 'b') : fmap (charBinary #) (NonEmpty.toList n)
showIntLiteral (IntLiteralOct _ b n) =
  Text.pack $
  '0' : (if b then 'O' else 'o') : fmap (charOctal #) (NonEmpty.toList n)
showIntLiteral (IntLiteralHex _ b n) =
  Text.pack $
  '0' : (if b then 'X' else 'x') : fmap (charHeXaDeCiMaL #) (NonEmpty.toList n)

showFloatExponent :: FloatExponent -> Text
showFloatExponent (FloatExponent e s ds) =
  Text.pack $
  (case e of; EE -> 'E'; Ee -> 'e') :
  foldMap (\case; Pos -> "+"; Neg -> "-") s <>
  fmap (charDecimal #) (NonEmpty.toList ds)

showFloatLiteral :: FloatLiteral a -> Text
showFloatLiteral (FloatLiteralFull _ a b) =
  Text.pack (fmap (charDecimal #) (NonEmpty.toList a) <> ".") <>
  foldMap
    (\case
       This x -> Text.pack $ fmap (charDecimal #) (NonEmpty.toList x)
       That x -> showFloatExponent x
       These x y ->
         Text.pack (fmap (charDecimal #) (NonEmpty.toList x)) <>
         showFloatExponent y)
    b
showFloatLiteral (FloatLiteralPoint _ a b) =
  Text.pack ('.' : fmap (charDecimal #) (NonEmpty.toList a)) <>
  foldMap showFloatExponent b
showFloatLiteral (FloatLiteralWhole _ a b) =
  Text.pack (fmap (charDecimal #) (NonEmpty.toList a)) <>
  showFloatExponent b

showImagLiteral :: ImagLiteral a -> Text
showImagLiteral (ImagLiteralInt _ ds b) =
  Text.pack $ fmap (charDecimal #) (NonEmpty.toList ds) ++ [if b then 'J' else 'j']
showImagLiteral (ImagLiteralFloat _ f b) =
  showFloatLiteral f <> Text.singleton (if b then 'J' else 'j')
