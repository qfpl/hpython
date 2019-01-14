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
Module      : Language.Python.Syntax.Numbers.FloatLiteral
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Python float literals
-}

module Language.Python.Syntax.Numbers.FloatLiteral
  ( -- * Classy Prism
    AsFloatLiteral(..)
    -- * Datatypes
  , FloatLiteral35(..)
  , FloatLiteral36(..)
  , FloatLiteral(..)
  , Sign(..)
  , E(..)
  , FloatExponent(..)
    -- * Rendering
  , showFloatExponent
  , showFloatLiteral
  )
where

import Control.Lens.Lens (Lens')
import Control.Lens.Review ((#))
import Control.Lens.TH (makeClassyPrisms, makeWrapped)
import Control.Lens.Wrapped (_Wrapped, _Wrapping)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Digit.Char (charDecimal)
import Data.Digit.Decimal (DecDigit)
import Data.Functor.Identity (Identity(..))
import Data.Generics.Product.Typed (typed)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.These (These(..))
import GHC.Generics (Generic)

import qualified Data.Text as Text

import Language.Python.Optics.Version
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Punctuation

-- | Positive or negative, as in @-7@
data Sign = Pos | Neg deriving (Eq, Ord, Show, Generic)

-- | When a floating point literal is in scientific notation, it includes the character
-- @e@, which can be lower or upper case.
data E = Ee | EE deriving (Eq, Ord, Show, Generic)

-- | The exponent of a floating point literal.
--
-- An @e@, followed by an optional 'Sign', followed by at least one digit.
data FloatExponent f = FloatExponent E (Maybe Sign) (f DecDigit)
  deriving Generic
deriving instance (forall x. Eq x => Eq (f x)) => Eq (FloatExponent f)
deriving instance
  (forall x. Eq x => Eq (f x), forall x. Ord x => Ord (f x)) =>
  Ord (FloatExponent f)
deriving instance (forall x. Show x => Show (f x)) => Show (FloatExponent f)

-- | A literal floating point value.
--
-- Eg. @7.63@
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#floating-point-literals>
data FloatLiteral f a
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
  , _floatLiteralFullLeft :: f DecDigit
  , _floatLiteralFullRight
      :: Maybe (These (f DecDigit) (FloatExponent f))
  }
  -- | Floats that begin with a decimal point
  --
  -- @.12@
  --
  -- @.12e34@
  | FloatLiteralPoint
  { _floatLiteralAnn :: Ann a
  -- . [0-9]+
  , _floatLiteralPointRight :: f DecDigit
  -- [ 'e' ['-' | '+'] [0-9]+ ]
  , _floatLiteralPointExponent :: Maybe (FloatExponent f)
  }
  -- | Floats with no decimal points
  --
  -- @12e34@
  | FloatLiteralWhole
  { _floatLiteralAnn :: Ann a
  -- [0-9]+
  , _floatLiteralWholeRight :: f DecDigit
  -- [ 'e' ['-' | '+'] [0-9]+ ]
  , _floatLiteralWholeExponent :: FloatExponent f
  }
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (forall x. Eq x => Eq (f x), Eq a) => Eq (FloatLiteral f a)
deriving instance (forall x. Show x => Show (f x), Show a) => Show (FloatLiteral f a)
makeClassyPrisms ''FloatLiteral

htraverseFloatExponent
  :: forall m f g
   . Applicative m
  => (forall x. f x -> m (g x))
  -> FloatExponent f
  -> m (FloatExponent g)
htraverseFloatExponent f (FloatExponent a b c) =
  FloatExponent a b <$> f c

htraverseFloatLiteral
  :: forall m f g a
   . Applicative m
  => (forall x. f x -> m (g x))
  -> FloatLiteral f a
  -> m (FloatLiteral g a)
htraverseFloatLiteral f fl =
  case fl of
    FloatLiteralFull a b c ->
      (\b' -> FloatLiteralFull a b') <$>
      f b <*>
      traverse (bitraverse f (htraverseFloatExponent f)) c
    FloatLiteralPoint a b c ->
      (\b' -> FloatLiteralPoint a b') <$>
      f b <*>
      traverse (htraverseFloatExponent f) c
    FloatLiteralWhole a b c ->
      (\b' -> FloatLiteralWhole a b') <$>
      f b <*>
      htraverseFloatExponent f c

hmapFloatLiteral
  :: forall f g a
   . (forall x. f x -> g x)
  -> FloatLiteral f a
  -> FloatLiteral g a
hmapFloatLiteral f = runIdentity . htraverseFloatLiteral (Identity . f)


newtype FloatLiteral35 a
  = FloatLiteral35 { unFloatLiteral35 :: FloatLiteral NonEmpty a }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeWrapped ''FloatLiteral35

instance HasAnn FloatLiteral35 where
  annot :: forall a. Lens' (FloatLiteral35 a) (Ann a)
  annot = _Wrapping FloatLiteral35 . typed @(Ann a)

instance Upgrade (FloatLiteral35 a) (FloatLiteral35 a) where
  upgrade = id
  downgrade = Just

instance AsFloatLiteral (FloatLiteral35 a) NonEmpty a where; _FloatLiteral = _Wrapped

newtype FloatLiteral36 a
  = FloatLiteral36 { unFloatLiteral36 :: FloatLiteral Underscored a }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeWrapped ''FloatLiteral36

instance HasAnn FloatLiteral36 where
  annot :: forall a. Lens' (FloatLiteral36 a) (Ann a)
  annot = _Wrapping FloatLiteral36 . typed @(Ann a)

instance Upgrade (FloatLiteral36 a) (FloatLiteral36 a) where
  upgrade = id
  downgrade = Just

instance Upgrade (FloatLiteral35 a) (FloatLiteral36 a) where
  upgrade = coerce go
    where
      go :: FloatLiteral NonEmpty a -> FloatLiteral Underscored a
      go = hmapFloatLiteral $ \(x:|xs) -> Underscored x ((Nothing ,) <$> xs)

  downgrade = coerce go
    where
      go :: FloatLiteral Underscored a -> Maybe (FloatLiteral NonEmpty a)
      go = Just . hmapFloatLiteral (\(Underscored x xs) -> x :| fmap snd xs)

instance AsFloatLiteral (FloatLiteral36 a) Underscored a where; _FloatLiteral = _Wrapped

showFloatExponent
  :: (forall x. (x -> Char) -> f x -> [Char])
  -> FloatExponent f
  -> Text
showFloatExponent f (FloatExponent e s ds) =
  Text.pack $
  (case e of; EE -> 'E'; Ee -> 'e') :
  foldMap (\case; Pos -> "+"; Neg -> "-") s <>
  f (charDecimal #) ds

showFloatLiteral
  :: (forall x. (x -> Char) -> f x -> [Char])
  -> FloatLiteral f a
  -> Text
showFloatLiteral f (FloatLiteralFull _ a b) =
  Text.pack (f (charDecimal #) a <> ".") <>
  foldMap
    (\case
       This x -> Text.pack $ f (charDecimal #) x
       That x -> showFloatExponent f x
       These x y ->
         Text.pack (f (charDecimal #) x) <>
         showFloatExponent f y)
    b
showFloatLiteral f (FloatLiteralPoint _ a b) =
  Text.pack ('.' : f (charDecimal #) a) <>
  foldMap (showFloatExponent f) b
showFloatLiteral f (FloatLiteralWhole _ a b) =
  Text.pack (f (charDecimal #) a) <>
  showFloatExponent f b
