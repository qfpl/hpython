{-# language GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Generators.Sized where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import GHC.Stack (HasCallStack)

sizedBind
  :: MonadGen m
  => m a
  -> (a -> m b)
  -> m b
sizedBind ma f =
  Gen.sized $ \n -> do
    aSize <- Gen.integral (Range.constant 0 n)
    a <- Gen.resize aSize ma
    Gen.resize (max 0 $ n - aSize) $ f a

sized2M
  :: MonadGen m
  => (a -> b -> m c)
  -> m a
  -> m b
  -> m c
sized2M f ma mb =
  sizedBind ma $ \a ->
  sizedBind mb $ \b ->
  f a b

sized2
  :: MonadGen m
  => (a -> b -> c)
  -> m a
  -> m b
  -> m c
sized2 f = sized2M (\a b -> pure $ f a b)

sized3M
  :: MonadGen m
  => (a -> b -> c -> m d)
  -> m a
  -> m b
  -> m c
  -> m d
sized3M f ma mb mc =
  sized2M (\a b -> sizedBind mc $ \c -> f a b c) ma mb

sized3
  :: MonadGen m
  => (a -> b -> c -> d)
  -> m a
  -> m b
  -> m c
  -> m d
sized3 f = sized3M (\a b c -> pure $ f a b c)

sized4M
  :: MonadGen m
  => (a -> b -> c -> d -> m e)
  -> m a
  -> m b
  -> m c
  -> m d
  -> m e
sized4M f ma mb mc md =
  sized3M (\a b c -> sizedBind md $ \d -> f a b c d) ma mb mc

sized4
  :: MonadGen m
  => (a -> b -> c -> d -> e)
  -> m a
  -> m b
  -> m c
  -> m d
  -> m e
sized4 f = sized4M (\a b c d -> pure $ f a b c d)

sizedList :: MonadGen m => m a -> m [a]
sizedList ma = go
  where
    go =
      sized2 (:)
        ma
        (Gen.sized $ \n -> if n == 0 then pure [] else go)

sizedNonEmpty :: MonadGen m => m a -> m (NonEmpty a)
sizedNonEmpty ma = sized2 (:|) ma (sizedList ma)

sizedMaybe :: MonadGen m => m a -> m (Maybe a)
sizedMaybe ma =
  Gen.sized $ \n ->
    if n == 0 then pure Nothing else Gen.maybe (Gen.resize (max 0 $ n-1) ma)

sizedRecursive :: (HasCallStack, MonadGen m) => [m a] -> [m a] -> m a
sizedRecursive bases recs =
  Gen.sized $ \n ->
  Gen.scale (max 0 . subtract 1) $
  case bases of
    [] -> Gen.choice recs
    _ | n == 0 -> Gen.choice bases
      | otherwise -> Gen.choice $ bases <> recs
