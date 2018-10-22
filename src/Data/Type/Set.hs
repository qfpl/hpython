{-# language DataKinds, TypeFamilies, TypeOperators #-}
{-# language FlexibleInstances, MultiParamTypeClasses, PolyKinds #-}

{-|
Module      : Data.Type.Set
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Data.Type.Set where

type family Nub t where
  Nub '[] = '[]
  Nub '[e] = '[e]
  Nub (e ': e ': s) = Nub (e ': s)
  Nub (e ': f ': s) = e ': Nub (f ': s)

class Member a s where
instance {-# OVERLAPS #-} Member a (a ': s) where
instance {-# OVERLAPPABLE #-} Member a s => Member a (b ': s) where
