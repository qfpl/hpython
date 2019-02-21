{-# language DataKinds, PolyKinds, DefaultSignatures #-}
{-# language AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

{-|
Module      : Language.Python.Optics.Validated
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Optics.Validated (Validated(..), demoted) where

import Control.Lens.Getter (Getter, to)
import Data.Coerce (Coercible, coerce)

import Data.Type.Set (Member, Delete)

-- | A type class for things for which we can strip the validation information.
-- This can help types line up when they need to, for example to put many
-- things of various validation statuses together in a list.
class Validated (s :: [*] -> * -> *) where
  unvalidated :: Getter (s v a) (s '[] a)
  default unvalidated :: Coercible (s v a) (s '[] a) => Getter (s v a) (s '[] a)
  unvalidated = to coerce

  demoted_ :: Member v vs => Getter (s vs a) (s (Delete v vs) a)

demoted ::
  forall v s vs a.
  (Validated s, Member v vs) =>
  Getter (s vs a) (s (Delete v vs) a)
demoted = demoted_ @s @v