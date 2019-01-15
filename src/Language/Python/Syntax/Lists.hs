{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
module Language.Python.Syntax.Lists where

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
import Language.Python.Syntax.Whitespace

instance Validated e => Validated (ListItem e) where; unvalidated = to unsafeCoerce

-- | @a@ or @*a@
--
-- Used to construct lists, e.g. @[ 1, 'x', **c ]@
--
-- https://docs.python.org/3/reference/expressions.html#list-displays
data ListItem expr (v :: [*]) a
  = ListItem
  { _listItemAnn :: Ann a
  , _unsafeListItemValue :: expr v a
  }
  | ListUnpack
  { _listItemAnn :: Ann a
  , _unsafeListUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeListUnpackWhitespace :: [Whitespace]
  , _unsafeListUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (ListItem expr v) where
  annot :: forall a. Lens' (ListItem expr v a) (Ann a)
  annot = typed @(Ann a)

instance VFunctor ListItem where; vfmap = vfmapDefault
instance VFoldable ListItem where; vfoldMap = vfoldMapDefault
instance VTraversable ListItem where
  vtraverse f (ListItem a b) = ListItem a <$> f b
  vtraverse f (ListUnpack a b c d) = ListUnpack a b c <$> f d

instance HasExprs expr expr => HasExprs (ListItem expr) expr where
  _Exprs f (ListItem a b) = ListItem a <$> f b
  _Exprs f (ListUnpack a b c d) = ListUnpack a b c <$> f d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (ListItem expr v a) where
  trailingWhitespace =
    lens
      (\case
          ListItem _ a -> a ^. trailingWhitespace
          ListUnpack _ [] _ a -> a ^. trailingWhitespace
          ListUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           ListItem b c -> ListItem b $ c & trailingWhitespace .~ ws
           ListUnpack b [] d e -> ListUnpack b [] d $ e & trailingWhitespace .~ ws
           ListUnpack b ((c, _) : rest) e f -> ListUnpack b ((c, ws) : rest) e f)
