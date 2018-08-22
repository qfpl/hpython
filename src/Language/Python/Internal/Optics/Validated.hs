{-# language DataKinds, PolyKinds, DefaultSignatures #-}
module Language.Python.Internal.Optics.Validated where

import Control.Lens.Getter (Getter, to)
import Data.Coerce (Coercible, coerce)

class Validated (s :: [*] -> * -> *) where
  unvalidated :: Getter (s v a) (s '[] a)
  default unvalidated :: Coercible (s v a) (s '[] a) => Getter (s v a) (s '[] a)
  unvalidated = to coerce
