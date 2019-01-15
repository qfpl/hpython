{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
module Language.Python.Syntax.Dicts where

import Control.Lens.Getter ((^.), to)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~))
import Data.Function ((&))
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Data.VFoldable
import Data.VFunctor
import Data.VTraversable
import Language.Python.Optics.Exprs
import Language.Python.Optics.Validated
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Whitespace

instance Validated e => Validated (DictItem e) where; unvalidated = to unsafeCoerce

-- | @a : b@ or @**a@
--
-- Used to construct dictionaries, e.g. @{ 1: a, 2: b, **c }@
--
-- https://docs.python.org/3/reference/expressions.html#dictionary-displays
data DictItem expr (v :: [*]) a
  = DictItem
  { _dictItemAnn :: Ann a
  , _unsafeDictItemKey :: expr v a
  , _unsafeDictItemColon :: Colon
  , _unsafeDictItemValue :: expr v a
  }
  | DictUnpack
  { _dictItemAnn :: Ann a
  , _unsafeDictItemUnpackWhitespace :: [Whitespace]
  , _unsafeDictItemUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (DictItem expr v) where
  annot :: forall a. Lens' (DictItem expr v a) (Ann a)
  annot = typed @(Ann a)

instance VFunctor DictItem where; vfmap = vfmapDefault
instance VFoldable DictItem where; vfoldMap = vfoldMapDefault
instance VTraversable DictItem where
  vtraverse f (DictItem a b c d) = (\b' -> DictItem a b' c) <$> f b <*> f d
  vtraverse f (DictUnpack a b c) = DictUnpack a b <$> f c

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (DictItem expr v a) where
  trailingWhitespace =
    lens
      (\(DictItem _ _ _ a) -> a ^. trailingWhitespace)
      (\(DictItem a b c d) ws -> DictItem a b c (d & trailingWhitespace .~ ws))

instance HasExprs expr expr => HasExprs (DictItem expr) expr where
  _Exprs f (DictItem a b c d) = (\b' -> DictItem a b' c) <$> f b <*> f d
  _Exprs f (DictUnpack a b c) = DictUnpack a b <$> f c
