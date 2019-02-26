{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

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
import Control.Lens.TH (makeWrapped)
import Control.Lens.Wrapped (_Wrapped)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.Semigroup (Semigroup)
import Data.Monoid (Monoid)

-- | Used to mark annotations in data structures, which helps with generic deriving
newtype Ann a = Ann { getAnn :: a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Semigroup, Monoid)

-- | Classy 'Lens'' for things that are annotated
class HasAnn s where
  annot :: Lens' (s a) (Ann a)

instance HasAnn Ann where
  annot = id

-- | Get an annotation and forget the wrapper
annot_ :: HasAnn s => Lens' (s a) a
annot_ = annot._Wrapped

makeWrapped ''Ann
deriveEq1 ''Ann
deriveOrd1 ''Ann
deriveShow1 ''Ann
