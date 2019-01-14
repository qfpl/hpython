{-# language DataKinds, KindSignatures, PolyKinds #-}
{-# language FlexibleInstances, FlexibleContexts #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language InstanceSigs #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Data.VariantV where

import Control.Lens.Prism (Prism', prism')
import Data.Functor.Classes (Eq1(..), eq1, Show1(..), showsPrec1)
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

data VariantV (vs :: [[*] -> k -> *]) (v :: [*]) (a :: k)
  = VariantV {-# unpack #-} !Word Any
type role VariantV representational representational representational

{-# inline elimVV #-}
elimVV
  :: forall a as v x r
   . (a v x -> r)
  -> (VariantV as v x -> r)
  -> VariantV (a ': as) v x -> r
elimVV f g (VariantV tag a) =
  if tag == 0
  then f (unsafeCoerce a :: a v x)
  else g (VariantV (tag-1) a)

{-# inline widenVV #-}
widenVV :: VariantV as v x -> VariantV (a ': as) vv x
widenVV (VariantV tag a) = VariantV (tag+1) a

{-# inline absurdVV #-}
absurdVV :: VariantV '[] v a -> b
absurdVV _ = error "absurdV1: absurd!"

instance Functor (VariantV '[] v) where
  {-# inline fmap #-}
  fmap _ = absurdVV

instance Foldable (VariantV '[] v) where
  {-# inline foldMap #-}
  foldMap _ = absurdVV

instance Traversable (VariantV '[] v) where
  {-# inline traverse #-}
  traverse _ = absurdVV

instance (Functor (a v), Functor (VariantV as v)) => Functor (VariantV (a ': as) v) where
  {-# inline fmap #-}
  fmap f =
    elimVV
      (injV . fmap f)
      (widenVV . fmap f)

instance (Foldable (a v), Foldable (VariantV as v)) => Foldable (VariantV (a ': as) v) where
  {-# inline foldMap #-}
  foldMap f =
    elimVV
      (foldMap f)
      (foldMap f)

instance (Traversable (a v), Traversable (VariantV as v)) => Traversable (VariantV (a ': as) v) where
  {-# inline traverse #-}
  traverse f =
    elimVV
      (fmap injV . traverse f)
      (fmap widenVV . traverse f)

instance Show1 (VariantV '[] v) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec _ _ _ = absurdVV

instance (Show1 (a v), Show1 (VariantV as v)) => Show1 (VariantV (a ': as) v) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec a b c =
    elimVV
      (\val ->
         showParen (c > 10) $
         showString "VariantV " .
         liftShowsPrec a b 11 val)
      (liftShowsPrec a b c)

instance Eq1 (VariantV '[] v) where
  {-# inline liftEq #-}
  liftEq _ = absurdVV

instance (Eq1 (a v), Eq1 (VariantV as v)) => Eq1 (VariantV (a ': as) v) where
  {-# inline liftEq #-}
  liftEq :: (x -> y -> Bool) -> VariantV (a ': as) v x -> VariantV (a ': as) v y -> Bool
  liftEq f (VariantV 0 a) (VariantV 0 b) =
    liftEq f (unsafeCoerce a :: a v x) (unsafeCoerce b :: a v y)
  liftEq _ (VariantV 0 _) _ = False
  liftEq _ _ (VariantV 0 _) = False
  liftEq f (VariantV n a) (VariantV m b) =
    liftEq f
      (VariantV (n-1) a :: VariantV as v x)
      (VariantV (m-1) b :: VariantV as v y)

instance (Eq1 (VariantV as v), Eq a) => Eq (VariantV as v a) where
  {-# inline (==) #-}
  (==) = eq1

instance (Show1 (VariantV as v), Show a) => Show (VariantV as v a) where
  {-# inline showsPrec #-}
  showsPrec = showsPrec1

class CtorV as g | as -> g where
  injV :: g v a -> VariantV as v a
  prjV :: VariantV as v a -> Maybe (g v a)

instance {-# overlapping #-} CtorV (v ': vs) v where
  {-# inline injV #-}
  injV = VariantV 0 . unsafeCoerce

  {-# inline prjV #-}
  prjV = elimVV Just $ const Nothing

instance {-# overlappable #-} CtorV vs b =>  CtorV (v ': vs) b where
  {-# inline injV #-}
  injV = widenVV . injV

  {-# inline prjV #-}
  prjV :: forall a as g v x. CtorV as g => VariantV (a ': as) v x -> Maybe (g v x)
  prjV (VariantV tag a) = prjV (VariantV (tag-1) a :: VariantV as v x)

{-# inline _CtorV #-}
_CtorV :: CtorV vs g => Prism' (VariantV vs v a) (g v a)
_CtorV = prism' injV prjV
