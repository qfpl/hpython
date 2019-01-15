{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
module Language.Python.Syntax.Sets where

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

instance Validated e => Validated (SetItem e) where; unvalidated = to unsafeCoerce

-- | @a@ or @*a@
--
-- Used to construct sets, e.g. @{ 1, 'x', **c }@
--
-- https://docs.python.org/3/reference/expressions.html#set-displays
data SetItem expr (v :: [*]) a
  = SetItem
  { _setItemAnn :: Ann a
  , _unsafeSetItemValue :: expr v a
  }
  | SetUnpack
  { _setItemAnn :: Ann a
  , _unsafeSetUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeSetUnpackWhitespace :: [Whitespace]
  , _unsafeSetUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (SetItem expr v) where
  annot :: forall a. Lens' (SetItem expr v a) (Ann a)
  annot = typed @(Ann a)

instance VFunctor SetItem where; vfmap = vfmapDefault
instance VFoldable SetItem where; vfoldMap = vfoldMapDefault
instance VTraversable SetItem where
  vtraverse f (SetItem a b) = SetItem a <$> f b
  vtraverse f (SetUnpack a b c d) = SetUnpack a b c <$> f d

instance HasExprs expr expr => HasExprs (SetItem expr) expr where
  _Exprs f (SetItem a b) = SetItem a <$> f b
  _Exprs f (SetUnpack a b c d) = SetUnpack a b c <$> f d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (SetItem expr v a) where
  trailingWhitespace =
    lens
      (\case
          SetItem _ a -> a ^. trailingWhitespace
          SetUnpack _ [] _ a -> a ^. trailingWhitespace
          SetUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           SetItem b c -> SetItem b $ c & trailingWhitespace .~ ws
           SetUnpack b [] d e -> SetUnpack b [] d $ e & trailingWhitespace .~ ws
           SetUnpack b ((c, _) : rest) e f -> SetUnpack b ((c, ws) : rest) e f)
