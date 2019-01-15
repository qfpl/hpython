{-# language DataKinds, PolyKinds, DefaultSignatures #-}
{-# language ExplicitForAll #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-|
Module      : Language.Python.Optics.Validated
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Optics.Validated
  ( Validated(..)
  , _CtorV'
  )
where

import Control.Lens.Getter ((^.), Getter, to)
import Control.Lens.Prism (Prism, prism)
import Data.Coerce (Coercible, coerce)
import Unsafe.Coerce (unsafeCoerce)

import Data.VFix
import Data.VariantV

-- | A type class for things for which we can strip the validation information.
-- This can help types line up when they need to, for example to put many
-- things of various validation statuses together in a list.
class Validated (s :: [*] -> * -> *) where
  unvalidated :: Getter (s v a) (s '[] a)
  default unvalidated :: Coercible (s v a) (s '[] a) => Getter (s v a) (s '[] a)
  unvalidated = to coerce

instance Validated (f (VFix f)) => Validated (VFix f) where
  unvalidated = to unsafeCoerce

instance Validated (VariantV '[] expr) where
  unvalidated = to absurdVV

instance
  (Validated (a expr), Validated (VariantV as expr)) => Validated (VariantV (a ': as) expr) where
  unvalidated = to unsafeCoerce

_CtorV'
  :: forall g vs expr v a
   . (CtorV vs g, Validated (VariantV vs expr))
  => Prism
       (VariantV vs expr v a)
       (VariantV vs expr '[] a)
       (g expr v a)
       (g expr '[] a)
_CtorV' = prism injV (\a -> maybe (Left $ a ^. unvalidated) Right $ prjV a)