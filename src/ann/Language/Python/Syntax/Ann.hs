{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language UndecidableInstances #-}
-- {-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Ann
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.Ann
  ( Ann(..)
  , annot_
  , HasAnn(..)
  )
where

import Control.Lens.Lens (Lens')
-- import Control.Lens.TH (makeWrapped)
import Control.Lens.Wrapped (_Wrapped)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..))
import Data.Semigroup (Semigroup)
import Data.Monoid (Monoid)
import GHC.Generics (Generic, Generic1)
import Generic.Data (gliftEq, gliftCompare, gliftShowsPrec)
import Generic.Data.Orphans ()

import qualified Control.Lens.Iso
import qualified Control.Lens.Wrapped

import Data.VFix

-- | Used to mark annotations in data structures, which helps with generic deriving
newtype Ann a = Ann { getAnn :: a }
  deriving
    ( Eq, Ord, Show
    , Functor, Foldable, Traversable
    , Semigroup, Monoid
    , Generic, Generic1
    )

-- | Classy 'Lens'' for things that are annotated
class HasAnn s where
  annot :: Lens' (s a) (Ann a)

instance HasAnn (e (VFix e) v) => HasAnn (VFix e v) where
  annot = _Wrapped.annot

-- | Get an annotation and forget the wrapper
annot_ :: HasAnn s => Lens' (s a) a
annot_ = annot._Wrapped

-- makeWrapped ''Ann
instance Ann a_amxX ~ t_amxW =>
          Control.Lens.Wrapped.Rewrapped (Ann a_agsh) t_amxW
instance Control.Lens.Wrapped.Wrapped (Ann a_agsh) where
  type Unwrapped (Ann a_agsh) = a_agsh
  _Wrapped'
    = (Control.Lens.Iso.iso (\ (Ann x_amxV) -> x_amxV)) Ann

instance Eq1 Ann where; liftEq = gliftEq
instance Ord1 Ann where; liftCompare = gliftCompare
instance Show1 Ann where; liftShowsPrec = gliftShowsPrec
