{-# language DataKinds, TypeFamilies, TypeOperators #-}
{-# language FlexibleInstances, MultiParamTypeClasses, PolyKinds #-}

{-|
Module      : Data.Type.Set
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

This module defines some helpful set-like functions for working with type-level lists.
-}

module Data.Type.Set (Delete, Nub, Member) where

-- | Remove adjacent equal elements from a type-level list
type family Nub t where
  Nub '[] = '[]
  Nub '[e] = '[e]
  Nub (e ': e ': s) = Nub (e ': s)
  Nub (e ': f ': s) = e ': Nub (f ': s)

-- | Delete an element from a type-level list
type family Delete (a :: k) (as :: [k]) :: [k] where
  Delete a '[] = '[]
  Delete a (a ': as) = Delete a as
  Delete a (b ': as) = b ': Delete a as

-- | Determine whether type @a@ is a member of type-level list @s@
class Member a s where
instance {-# OVERLAPS #-} Member a (a ': s) where
instance {-# OVERLAPPABLE #-} Member a s => Member a (b ': s) where
