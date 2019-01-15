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

data VariantV
  (vs :: [([*] -> k -> *) -> [*] -> k -> *])
  (expr :: [*] -> k -> *)
  (v :: [*])
  (a :: k)
  = VariantV {-# unpack #-} !Word Any
type role VariantV
  representational representational representational representational

{-# inline elimVV #-}
elimVV
  :: forall a expr as v x r
   . (a expr v x -> r)
  -> (VariantV as expr v x -> r)
  -> VariantV (a ': as) expr v x -> r
elimVV f g (VariantV tag a) =
  if tag == 0
  then f (unsafeCoerce a :: a expr v x)
  else g (VariantV (tag-1) a)

{-# inline widenVV #-}
widenVV :: VariantV as expr v x -> VariantV (a ': as) expr v x
widenVV (VariantV tag a) = VariantV (tag+1) a

{-# inline absurdVV #-}
absurdVV :: VariantV '[] expr v a -> b
absurdVV _ = error "absurdV1: absurd!"

instance Functor (VariantV '[] expr v) where
  {-# inline fmap #-}
  fmap _ = absurdVV

instance Foldable (VariantV '[] expr v) where
  {-# inline foldMap #-}
  foldMap _ = absurdVV

instance Traversable (VariantV '[] expr v) where
  {-# inline traverse #-}
  traverse _ = absurdVV

instance
  (Functor (a expr v), Functor (VariantV as expr v)) => Functor (VariantV (a ': as) expr v) where
  {-# inline fmap #-}
  fmap f =
    elimVV
      (injV . fmap f)
      (widenVV . fmap f)

instance
  (Foldable (a expr v), Foldable (VariantV as expr v)) =>
  Foldable (VariantV (a ': as) expr v) where
  {-# inline foldMap #-}
  foldMap f =
    elimVV
      (foldMap f)
      (foldMap f)

instance
  (Traversable (a expr v), Traversable (VariantV as expr v)) => Traversable (VariantV (a ': as) expr v) where
  {-# inline traverse #-}
  traverse f =
    elimVV
      (fmap injV . traverse f)
      (fmap widenVV . traverse f)

instance Show1 (VariantV '[] expr v) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec _ _ _ = absurdVV

instance
  (Show1 (a expr v), Show1 (VariantV as expr v)) => Show1 (VariantV (a ': as) expr v) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec a b c =
    elimVV
      (\val ->
         showParen (c > 10) $
         showString "VariantV " .
         liftShowsPrec a b 11 val)
      (liftShowsPrec a b c)

instance Eq1 (VariantV '[] expr v) where
  {-# inline liftEq #-}
  liftEq _ = absurdVV

instance
  (Eq1 (a expr v), Eq1 (VariantV as expr v)) => Eq1 (VariantV (a ': as) expr v) where
  {-# inline liftEq #-}
  liftEq
    :: (x -> y -> Bool)
    -> VariantV (a ': as) expr v x
    -> VariantV (a ': as) expr v y
    -> Bool
  liftEq f (VariantV 0 a) (VariantV 0 b) =
    liftEq f (unsafeCoerce a :: a expr v x) (unsafeCoerce b :: a expr v y)
  liftEq _ (VariantV 0 _) _ = False
  liftEq _ _ (VariantV 0 _) = False
  liftEq f (VariantV n a) (VariantV m b) =
    liftEq f
      (VariantV (n-1) a :: VariantV as expr v x)
      (VariantV (m-1) b :: VariantV as expr v y)

instance (Eq1 (VariantV as expr v), Eq a) => Eq (VariantV as expr v a) where
  {-# inline (==) #-}
  (==) = eq1

instance (Show1 (VariantV as expr v), Show a) => Show (VariantV as expr v a) where
  {-# inline showsPrec #-}
  showsPrec = showsPrec1

class CtorV as g | as -> g where
  injV :: g expr v a -> VariantV as expr v a
  prjV :: VariantV as expr v a -> Maybe (g expr v a)

instance {-# overlapping #-} CtorV (v ': vs) v where
  {-# inline injV #-}
  injV = VariantV 0 . unsafeCoerce

  {-# inline prjV #-}
  prjV = elimVV Just $ const Nothing

instance {-# overlappable #-} CtorV vs b =>  CtorV (v ': vs) b where
  {-# inline injV #-}
  injV = widenVV . injV

  {-# inline prjV #-}
  prjV :: forall a expr as g v x. CtorV as g => VariantV (a ': as) expr v x -> Maybe (g expr v x)
  prjV (VariantV tag a) = prjV (VariantV (tag-1) a :: VariantV as expr v x)

{-# inline _CtorV #-}
_CtorV
  :: forall g vs expr v a
   . CtorV vs g
  => Prism' (VariantV vs expr v a) (g expr v a)
_CtorV = prism' injV prjV
