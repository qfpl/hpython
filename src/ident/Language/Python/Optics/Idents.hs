{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-|
Module      : Language.Python.Optics.Idents
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Optics.Idents
  (HasIdents(..), HasIdents'(..))
where

import Control.Lens.Iso (iso)
import Control.Lens.Traversal (Traversal)
import Control.Lens.Wrapped (_Wrapped)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics

import Data.VFix
import Data.VIdentity
import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comment
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Whitespace

-- | 'Traversal' that targets all the 'Ident's in a structure
class HasIdents s where
  _Idents :: Traversal (s v a) (s '[] a) (Ident v a) (Ident '[] a)
  default _Idents
    :: forall v a l m
    . ( Generic (s v a)
      , Generic (s '[] a)
      , Rep (s v a) ~ l
      , Rep (s '[] a) ~ m
      , GHasIdents l m v a
      )
    => Traversal (s v a) (s '[] a) (Ident v a) (Ident '[] a)
  _Idents = iso from to . gidents @l @m @v @a

-- |
-- Helper class for deriving 'HasIdents'. You shouldn't need to
-- use this.
class HasIdents' s t v a where
  _Idents' :: Traversal s t (Ident v a) (Ident '[] a)

instance HasIdents s => HasIdents' (s v a) (s '[] a) v a where
  _Idents' = _Idents
instance HasIdents expr => HasIdents (VIdentity expr)
instance HasIdents Ident where; _Idents = id

instance HasIdents (expr (VFix expr)) => HasIdents (VFix expr) where
  _Idents = _Wrapped._Idents

class GHasIdents s t v a where
  gidents :: Traversal (s x) (t x) (Ident v a) (Ident '[] a)

instance (GHasIdents a c v x, GHasIdents b d v x) => GHasIdents (a :+: b) (c :+: d) v x where
  gidents f (L1 a) = L1 <$> gidents f a
  gidents f (R1 a) = R1 <$> gidents f a

instance (GHasIdents a c v x, GHasIdents b d v x) => GHasIdents (a :*: b) (c :*: d) v x where
  gidents f (a :*: b) = (:*:) <$> gidents f a <*> gidents f b

instance GHasIdents U1 U1 v x where
  gidents _ U1 = pure U1

instance GHasIdents V1 V1 v x where
  gidents _ !_ = undefined

instance GHasIdents a b v x => GHasIdents (M1 i t a) (M1 i' t' b) v x where
  gidents f (M1 a) = M1 <$> gidents f a

instance {-# overlapping #-} HasIdents s => GHasIdents (K1 i (s v a)) (K1 i (s '[] a)) v a where
  gidents f (K1 a) = K1 <$> _Idents f a

instance {-# overlappable #-} HasIdents' a b v x => GHasIdents (K1 i a) (K1 i b) v x where
  gidents f (K1 a) = K1 <$> _Idents' f a

-- redundant instances

instance HasIdents' (Ann a) (Ann a) v a where; _Idents' _ = pure
instance HasIdents' Whitespace Whitespace v a where; _Idents' _ = pure
instance HasIdents' Newline Newline v a where; _Idents' _ = pure
instance HasIdents' (Blank a) (Blank a) v a where; _Idents' _ = pure
instance HasIdents' Colon Colon v a where; _Idents' _ = pure
instance HasIdents' At At v a where; _Idents' _ = pure
instance HasIdents' (Semicolon a) (Semicolon a) v a where; _Idents' _ = pure
instance HasIdents' (Comment a) (Comment a) v a where; _Idents' _ = pure
instance HasIdents' (Indents a) (Indents a) v a where; _Idents' _ = pure
instance HasIdents' Dot Dot v a where; _Idents' _ = pure
instance HasIdents' Equals Equals v a where; _Idents' _ = pure
instance HasIdents' Comma Comma v a where; _Idents' _ = pure
instance HasIdents' Bool Bool v a where; _Idents' _ = pure
instance HasIdents' a b v x => HasIdents' [a] [b] v x where; _Idents' = traverse._Idents'
instance HasIdents' a b v x => HasIdents' (NonEmpty a) (NonEmpty b) v x where; _Idents' = traverse._Idents'
instance HasIdents' a b v x => HasIdents' (Maybe a) (Maybe b) v x where; _Idents' = traverse._Idents'
instance HasIdents' a b v x => HasIdents' (CommaSep a) (CommaSep b) v x where; _Idents' = traverse._Idents'
instance HasIdents' a b v x => HasIdents' (CommaSep1 a) (CommaSep1 b) v x where; _Idents' = traverse._Idents'
instance HasIdents' a b v x => HasIdents' (CommaSep1' a) (CommaSep1' b) v x where; _Idents' = traverse._Idents'
instance (HasIdents' a b v x, HasIdents' c d v x) => HasIdents' (a, c) (b, d) v x where
  _Idents' f (a, b) = (,) <$> _Idents' f a <*> _Idents' f b
instance (HasIdents' a b v x, HasIdents' c d v x, HasIdents' e f v x) => HasIdents' (a, c, e) (b, d, f) v x where
  _Idents' f (a, b, c) =
    (,,) <$>
    _Idents' f a <*>
    _Idents' f b <*>
    _Idents' f c
instance (HasIdents' a b v x, HasIdents' c d v x, HasIdents' e f v x, HasIdents' g h v x) => HasIdents' (a, c, e, g) (b, d, f, h) v x where
  _Idents' f (a, b, c, d) =
    (,,,) <$>
    _Idents' f a <*>
    _Idents' f b <*>
    _Idents' f c <*>
    _Idents' f d
instance (HasIdents' a b v x, HasIdents' c d v x) => HasIdents' (Either a c) (Either b d) v x where
  _Idents' f (Left a) = Left <$> _Idents' f a
  _Idents' f (Right a) = Right <$>_Idents' f a
