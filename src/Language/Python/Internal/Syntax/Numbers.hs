{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Syntax.Numbers where

import Data.Deriving (deriveEq1)
import Data.Digit.Binary (BinDigit)
import Data.Digit.Octal (OctDigit)
import Data.Digit.Decimal (DecDigit)
import Data.Digit.HeXaDeCiMaL (HeXDigit)
import Data.List.NonEmpty (NonEmpty)

data IntLiteral a
  = IntLiteralDec
  { _intLiteralAnn :: a
  , _unsafeIntLiteralDecValue :: NonEmpty DecDigit
  }
  | IntLiteralBin
  { _intLiteralAnn :: a
  , _unsafeIntLiteralBinUppercase :: Bool
  , _unsafeIntLiteralBinValue :: NonEmpty BinDigit
  }
  | IntLiteralOct
  { _intLiteralAnn :: a
  , _unsafeIntLiteralOctUppercase :: Bool
  , _unsafeIntLiteralOctValue :: NonEmpty OctDigit
  }
  | IntLiteralHex
  { _intLiteralAnn :: a
  , _unsafeIntLiteralHexUppercase :: Bool
  , _unsafeIntLiteralHexValue :: NonEmpty HeXDigit
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveEq1 ''IntLiteral
