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
Module      : Language.Python.Syntax.Numbers.ImagLiteral
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Python imaginary number literals
-}

module Language.Python.Syntax.Numbers.ImagLiteral
  ( -- * Classy Prism
    AsImagLiteral(..)
    -- * Datatypes
  , ImagLiteral35(..)
  , ImagLiteral36(..)
  , ImagLiteral(..)
    -- * Rendering
  , showImagLiteral
  )
where

import Control.Lens.Lens (Lens')
import Control.Lens.Review ((#))
import Control.Lens.TH (makeClassyPrisms, makeWrapped)
import Control.Lens.Wrapped (_Wrapped, _Wrapping)
import Data.Digit.Char (charDecimal)
import Data.Digit.Decimal (DecDigit)
import Data.Generics.Product.Typed (typed)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as Text

import Language.Python.Optics.Version
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Punctuation

import Language.Python.Syntax.Numbers.FloatLiteral

-- | Imaginary number literals
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#imaginary-literals>
data ImagLiteral f a
  -- | A decimal integer followed by a \'j\'
  --
  -- @12j@
  = ImagLiteralInt
  { _imagLiteralAnn :: Ann a
  , _unsafeImagLiteralIntValue :: f DecDigit
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
  , _unsafeImagLiteralFloatValue :: FloatLiteral f a
  , _imagLiteralUppercase :: Bool
  }
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (forall x. Eq x => Eq (f x), Eq a) => Eq (ImagLiteral f a)
deriving instance (forall x. Show x => Show (f x), Show a) => Show (ImagLiteral f a)
makeClassyPrisms ''ImagLiteral


newtype ImagLiteral35 a
  = ImagLiteral35 { unImagLiteral35 :: ImagLiteral NonEmpty a }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeWrapped ''ImagLiteral35

instance HasAnn ImagLiteral35 where
  annot :: forall a. Lens' (ImagLiteral35 a) (Ann a)
  annot = _Wrapping ImagLiteral35 . typed @(Ann a)

instance Upgrade (ImagLiteral35 a) (ImagLiteral35 a) where
  upgrade = id
  downgrade = Just

instance AsImagLiteral (ImagLiteral35 a) NonEmpty a where; _ImagLiteral = _Wrapped

newtype ImagLiteral36 a
  = ImagLiteral36 { unImagLiteral36 :: ImagLiteral Underscored a }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeWrapped ''ImagLiteral36

instance HasAnn ImagLiteral36 where
  annot :: forall a. Lens' (ImagLiteral36 a) (Ann a)
  annot = _Wrapping ImagLiteral36 . typed @(Ann a)

instance AsImagLiteral (ImagLiteral36 a) Underscored a where; _ImagLiteral = _Wrapped


showImagLiteral
  :: (forall x. (x -> Char) -> f x -> [Char])
  -> ImagLiteral f a
  -> Text
showImagLiteral f (ImagLiteralInt _ ds b) =
  Text.pack $ f (charDecimal #) ds ++ [if b then 'J' else 'j']
showImagLiteral f (ImagLiteralFloat _ a b) =
  showFloatLiteral f a <> Text.singleton (if b then 'J' else 'j')
