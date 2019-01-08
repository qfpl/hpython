{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Data.VFix where

import Control.Lens.Plated (Plated(..))
import Control.Lens.TH (makeWrapped)

import Data.VTraversable

newtype VFix (f :: ([*] -> * -> *) -> ([*] -> * -> *)) (v :: [*]) (a :: *)
  = VIn { vout :: f (VFix f) v a }

instance VTraversable f => Plated (VFix f '[] a) where
  plate f (VIn a) = VIn <$> vtraverse f a

makeWrapped ''VFix