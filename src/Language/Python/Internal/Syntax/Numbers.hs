{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Internal.Syntax.Numbers
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.Numbers where

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

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

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
deriveOrd1 ''IntLiteral

data Sign = Pos | Neg deriving (Eq, Ord, Show)

data FloatExponent = FloatExponent Bool (Maybe Sign) (NonEmpty DecDigit)
  deriving (Eq, Ord, Show)

data FloatLiteral a
  = FloatLiteralFull
  { _floatLiteralAnn :: a
  , _floatLiteralFullLeft :: NonEmpty DecDigit
  , _floatLiteralFullRight
      :: Maybe (These (NonEmpty DecDigit) FloatExponent)
  }
  | FloatLiteralPoint
  { _floatLiteralAnn :: a
  -- . [0-9]+
  , _floatLiteralPointRight :: NonEmpty DecDigit
  -- [ 'e' ['-' | '+'] [0-9]+ ]
  , _floatLiteralPointExponent :: Maybe FloatExponent
  }
  | FloatLiteralWhole
  { _floatLiteralAnn :: a
  -- [0-9]+
  , _floatLiteralWholeRight :: NonEmpty DecDigit
  -- [ 'e' ['-' | '+'] [0-9]+ ]
  , _floatLiteralWholeExponent :: FloatExponent
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveEq1 ''FloatLiteral
deriveOrd1 ''FloatLiteral

data ImagLiteral a
  = ImagLiteralInt
  { _imagLiteralAnn :: a
  , _unsafeImagLiteralIntValue :: NonEmpty DecDigit
  , _imagLiteralUppercase :: Bool
  }
  | ImagLiteralFloat
  { _imagLiteralAnn :: a
  , _unsafeImagLiteralFloatValue :: FloatLiteral a
  , _imagLiteralUppercase :: Bool
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
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
  (if e then 'E' else 'e') :
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
