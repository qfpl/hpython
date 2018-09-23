{-# language DataKinds, TypeFamilies, TypeOperators #-}
{-# language FlexibleInstances, MultiParamTypeClasses, PolyKinds #-}
module Data.Type.Set where

type family Nub t where
  Nub '[] = '[]
  Nub '[e] = '[e]
  Nub (e ': e ': s) = Nub (e ': s)
  Nub (e ': f ': s) = e ': Nub (f ': s)

class Member a s where
instance {-# OVERLAPS #-} Member a (a ': s) where
instance {-# OVERLAPPABLE #-} Member a s => Member a (b ': s) where
