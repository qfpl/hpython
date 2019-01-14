{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# language FunctionalDependencies #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving, QuantifiedConstraints, UndecidableInstances #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}

{-|
Module      : Language.Python.Syntax.Numbers.IntLiteral
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Python integer literals
-}

module Language.Python.Syntax.Numbers.IntLiteral
  ( -- * Classy Prism
    AsIntLiteral(..)
    -- * Datatypes
  , IntLiteral35(..)
  , IntLiteral36(..)
  , IntLiteral(..)
    -- * Rendering
  , showIntLiteral
  )
where

import Control.Lens.Lens (Lens')
import Control.Lens.Review ((#))
import Control.Lens.TH (makeClassyPrisms, makeWrapped)
import Control.Lens.Wrapped (_Wrapped, _Wrapping)
import Data.Coerce (coerce)
import Data.Digit.Binary (BinDigit)
import Data.Digit.Char (charHeXaDeCiMaL, charOctal, charBinary, charDecimal)
import Data.Digit.Octal (OctDigit)
import Data.Digit.Decimal (DecDigit)
import Data.Digit.Hexadecimal.MixedCase (HeXDigit)
import Data.Functor.Identity (Identity(..))
import Data.Generics.Product.Typed (typed)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as Text

import Language.Python.Optics.Version
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Punctuation

-- | An integer literal value.
--
-- @5@ is an integer literal.
--
-- @6.2@ is a literal but is not an integer
--
-- @x@ might be an integer, but is not a literal
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#integer-literals>
data IntLiteral f a
  -- | Decimal
  --
  -- @1234@
  = IntLiteralDec
  { _intLiteralAnn :: Ann a
  , _unsafeIntLiteralDecValue :: f DecDigit
  }
  -- | Binary
  --
  -- @0b10110@
  | IntLiteralBin
  { _intLiteralAnn :: Ann a
  , _unsafeIntLiteralBinUppercase :: Bool
  , _unsafeIntLiteralBinValue :: f BinDigit
  }
  -- | Octal
  --
  -- @0o1367@
  | IntLiteralOct
  { _intLiteralAnn :: Ann a
  , _unsafeIntLiteralOctUppercase :: Bool
  , _unsafeIntLiteralOctValue :: f OctDigit
  }
  -- | Mixed-case hexadecimal
  --
  -- @0x18B4f@
  | IntLiteralHex
  { _intLiteralAnn :: Ann a
  , _unsafeIntLiteralHexUppercase :: Bool
  , _unsafeIntLiteralHexValue :: f HeXDigit
  }
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (forall x. Eq x => Eq (f x), Eq a) => Eq (IntLiteral f a)
deriving instance (forall x. Show x => Show (f x), Show a) => Show (IntLiteral f a)
makeClassyPrisms ''IntLiteral

htraverseIntLiteral
  :: forall m f g a
   . Applicative m
  => (forall x. f x -> m (g x))
  -> IntLiteral f a
  -> m (IntLiteral g a)
htraverseIntLiteral f il =
  case il of
    IntLiteralDec a b -> IntLiteralDec a <$> f b
    IntLiteralBin a b c -> IntLiteralBin a b <$> f c
    IntLiteralOct a b c -> IntLiteralOct a b <$> f c
    IntLiteralHex a b c -> IntLiteralHex a b <$> f c

hmapIntLiteral
  :: forall f g a
   . (forall x. f x -> g x)
  -> IntLiteral f a
  -> IntLiteral g a
hmapIntLiteral f = runIdentity . htraverseIntLiteral (Identity . f)


newtype IntLiteral35 a
  = IntLiteral35 { unIntLiteral35 :: IntLiteral NonEmpty a }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeWrapped ''IntLiteral35

instance HasAnn IntLiteral35 where
  annot :: forall a. Lens' (IntLiteral35 a) (Ann a)
  annot = _Wrapping IntLiteral35 . typed @(Ann a)

instance AsIntLiteral (IntLiteral35 a) NonEmpty a where; _IntLiteral = _Wrapped

instance Upgrade (IntLiteral35 a) (IntLiteral35 a) where
  upgrade = id
  downgrade = Just

newtype IntLiteral36 a
  = IntLiteral36 { unIntLiteral36 :: IntLiteral Underscored a }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeWrapped ''IntLiteral36

instance HasAnn IntLiteral36 where
  annot :: forall a. Lens' (IntLiteral36 a) (Ann a)
  annot = _Wrapping IntLiteral36 . typed @(Ann a)

instance AsIntLiteral (IntLiteral36 a) Underscored a where; _IntLiteral = _Wrapped

instance Upgrade (IntLiteral36 a) (IntLiteral36 a) where
  upgrade = id
  downgrade = Just

instance Upgrade (IntLiteral35 a) (IntLiteral36 a) where
  upgrade = coerce go
    where
      go :: IntLiteral NonEmpty a -> IntLiteral Underscored a
      go = hmapIntLiteral $ \(x:|xs) -> Underscored x ((Nothing ,) <$> xs)

  downgrade = coerce go
    where
      go :: IntLiteral Underscored a -> Maybe (IntLiteral NonEmpty a)
      go = Just . hmapIntLiteral (\(Underscored x xs) -> x :| fmap snd xs)

showIntLiteral :: (forall x. (x -> Char) -> f x -> [Char]) -> IntLiteral f a -> Text
showIntLiteral f (IntLiteralDec _ n) =
  Text.pack $ f (charDecimal #) n
showIntLiteral f (IntLiteralBin _ b n) =
  Text.pack $
  '0' : (if b then 'B' else 'b') : f (charBinary #) n
showIntLiteral f (IntLiteralOct _ b n) =
  Text.pack $
  '0' : (if b then 'O' else 'o') : f (charOctal #) n
showIntLiteral f (IntLiteralHex _ b n) =
  Text.pack $
  '0' : (if b then 'X' else 'x') : f (charHeXaDeCiMaL #) n
