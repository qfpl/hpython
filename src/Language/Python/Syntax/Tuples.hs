{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
module Language.Python.Syntax.Tuples where

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

instance Validated e => Validated (TupleItem e) where; unvalidated = to unsafeCoerce

-- | @a@ or @*a@
--
-- Used to construct tuples, e.g. @(1, 'x', **c)@
data TupleItem expr (v :: [*]) a
  = TupleItem
  { _tupleItemAnn :: Ann a
  , _unsafeTupleItemValue :: expr v a
  }
  | TupleUnpack
  { _tupleItemAnn :: Ann a
  , _unsafeTupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeTupleUnpackWhitespace :: [Whitespace]
  , _unsafeTupleUnpackValue :: expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (TupleItem expr v) where
  annot :: forall a. Lens' (TupleItem expr v a) (Ann a)
  annot = typed @(Ann a)

instance VFunctor TupleItem where; vfmap = vfmapDefault
instance VFoldable TupleItem where; vfoldMap = vfoldMapDefault
instance VTraversable TupleItem where
  vtraverse f (TupleItem a b) = TupleItem a <$> f b
  vtraverse f (TupleUnpack a b c d) = TupleUnpack a b c <$> f d

instance HasExprs expr expr => HasExprs (TupleItem expr) expr where
  _Exprs f (TupleItem a b) = TupleItem a <$> f b
  _Exprs f (TupleUnpack a b c d) = TupleUnpack a b c <$> f d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (TupleItem expr v a) where
  trailingWhitespace =
    lens
      (\case
          TupleItem _ a -> a ^. trailingWhitespace
          TupleUnpack _ [] _ a -> a ^. trailingWhitespace
          TupleUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           TupleItem b c -> TupleItem b $ c & trailingWhitespace .~ ws
           TupleUnpack b [] d e -> TupleUnpack b [] d $ e & trailingWhitespace .~ ws
           TupleUnpack b ((c, _) : rest) e f -> TupleUnpack b ((c, ws) : rest) e f)
