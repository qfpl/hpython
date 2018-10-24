{-# language DataKinds, PolyKinds, DefaultSignatures #-}

{-|
Module      : Language.Python.Optics.Validated
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Optics.Validated where

import Control.Lens.Getter (Getter, to)
import Data.Coerce (Coercible, coerce)

-- | A type class for things for which we can strip the validation information.
-- This can help types line up when they need to, for example to put many
-- things of various validation statuses together in a list.
class Validated (s :: [*] -> * -> *) where
  unvalidated :: Getter (s v a) (s '[] a)
  default unvalidated :: Coercible (s v a) (s '[] a) => Getter (s v a) (s '[] a)
  unvalidated = to coerce
