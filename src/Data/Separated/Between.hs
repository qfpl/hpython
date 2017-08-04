{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
module Data.Separated.Between where

import Papa

import Data.Eq.Deriving
import Text.Show.Deriving

data Between s t a = Between s a t
  deriving (Eq, Foldable, Functor, Traversable, Show)

deriveEq1 ''Between
deriveShow1 ''Between

between :: Lens (Between s s' a) (Between t t' b) (s, a, s') (t, b, t')
between =
  lens
  (\(Between s a s') -> (s, a, s'))
  (\Between{} (t, b, t') -> Between t b t')

newtype Between' s a = Between' { _getBetween :: Between s s a }
  deriving (Eq, Foldable, Functor, Traversable, Show)

deriveEq1 ''Between'
deriveShow1 ''Between'

instance Bifunctor Between' where
  bimap f g (Between' b) =
    Between' (b & between._1 %~ f & between._2 %~ g & between._3 %~ f)

instance Bifoldable Between' where
  bifoldMap f g b =
    f (b ^. between'._1) `mappend`
    g (b ^. between'._2) `mappend`
    f (b ^. between'._3)
    
instance Bitraversable Between' where
  bitraverse f g b =
    fmap Between' $
      Between <$>
      f (b ^. between'._1) <*>
      g (b ^. between'._2) <*>
      f (b ^. between'._3)

instance Wrapped (Between' s a) where
  type Unwrapped (Between' s a) = Between s s a

  _Wrapped' = iso _getBetween Between'

instance Rewrapped (Between' s a) t where

between' :: Lens (Between' s a) (Between' t b) (s, a, s) (t, b, t)
between' = _Wrapped.between
