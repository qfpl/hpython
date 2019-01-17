{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
module Language.Python.Syntax.Int where

import Control.Lens.Lens (Lens')
import Control.Lens.Review ((#))
import Data.Digit.Binary (BinDigit)
import Data.Digit.Char (charHeXaDeCiMaL, charOctal, charBinary, charDecimal)
import Data.Digit.Hexadecimal.MixedCase (HeXDigit)
import Data.Digit.Octal (OctDigit)
import Data.Digit.Decimal (DecDigit)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..))
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic, Generic1)
import Generic.Data (gliftEq, gliftCompare, gliftShowsPrec)
import Generic.Data.Orphans ()

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Digits.Sig

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
  { _intLiteralAnn :: Ann a
  , _unsafeIntLiteralDecValue :: Digits DecDigit
  }
  -- | Binary
  --
  -- @0b10110@
  | IntLiteralBin
  { _intLiteralAnn :: Ann a
  , _unsafeIntLiteralBinUppercase :: Bool
  , _unsafeIntLiteralBinValue :: Digits BinDigit
  }
  -- | Octal
  --
  -- @0o1367@
  | IntLiteralOct
  { _intLiteralAnn :: Ann a
  , _unsafeIntLiteralOctUppercase :: Bool
  , _unsafeIntLiteralOctValue :: Digits OctDigit
  }
  -- | Mixed-case hexadecimal
  --
  -- @0x18B4f@
  | IntLiteralHex
  { _intLiteralAnn :: Ann a
  , _unsafeIntLiteralHexUppercase :: Bool
  , _unsafeIntLiteralHexValue :: Digits HeXDigit
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
instance Eq1 IntLiteral where; liftEq = gliftEq
instance Ord1 IntLiteral where; liftCompare = gliftCompare
instance Show1 IntLiteral where; liftShowsPrec = gliftShowsPrec

instance HasAnn IntLiteral where
  annot :: forall a. Lens' (IntLiteral a) (Ann a)
  annot = typed @(Ann a)

showIntLiteral :: IntLiteral a -> String
showIntLiteral (IntLiteralDec _ n) =
  showDigits (charDecimal #) n
showIntLiteral (IntLiteralBin _ b n) =
  '0' : (if b then 'B' else 'b') :
  showDigits (charBinary #) n
showIntLiteral (IntLiteralOct _ b n) =
  '0' : (if b then 'O' else 'o') :
  showDigits (charOctal #) n
showIntLiteral (IntLiteralHex _ b n) =
  '0' : (if b then 'X' else 'x') :
  showDigits (charHeXaDeCiMaL #) n
