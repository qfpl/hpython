module Data.Functor.Sum.Lens where

import Papa hiding (Sum)
import Data.Functor.Sum

_InL :: Prism' (Sum f g a) (f a)
_InL =
  prism
    InL
    (\a -> case a of
        InL a' -> Right a'
        InR _ -> Left a)

_InR :: Prism' (Sum f g a) (g a)
_InR =
  prism
    InR
    (\a -> case a of
        InR a' -> Right a'
        InL _ -> Left a)
