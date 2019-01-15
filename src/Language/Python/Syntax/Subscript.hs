{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language LambdaCase #-}
module Language.Python.Syntax.Subscript where

import Control.Lens.Getter ((^.))
import Control.Lens.Lens (lens)
import Control.Lens.Setter ((.~))
import Control.Lens.Tuple (_2)
import Data.Function ((&))
import GHC.Generics (Generic)

import Data.VFoldable
import Data.VFunctor
import Data.VTraversable
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Whitespace

-- | Syntax for things that can be used as subscripts (inside the square brackets)
--
-- e.g.
--
-- @a[b]@
--
-- @a[:]@
--
-- @a[b:]@
--
-- @a[:b]@
--
-- @a[b:c]@
--
-- @a[b:c:d]@
--
-- https://docs.python.org/3/reference/expressions.html#subscriptions
data SubscriptItem expr (v :: [*]) a
  = SubscriptExpr (expr v a)
  | SubscriptSlice
      -- [expr]
      (Maybe (expr v a))
      -- ':' <spaces>
      Colon
      -- [expr]
      (Maybe (expr v a))
      -- [':' [expr]]
      (Maybe (Colon, Maybe (expr v a)))
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance VFunctor SubscriptItem where; vfmap = vfmapDefault
instance VFoldable SubscriptItem where; vfoldMap = vfoldMapDefault
instance VTraversable SubscriptItem where
  vtraverse f (SubscriptExpr a) = SubscriptExpr <$> f a
  vtraverse f (SubscriptSlice a b c d) =
    (\a' c' -> SubscriptSlice a' b c') <$>
    traverse f a <*>
    traverse f c <*>
    (traverse._2.traverse) f d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (SubscriptItem expr v a) where
  trailingWhitespace =
    lens
      (\case
          SubscriptExpr e -> e ^. trailingWhitespace
          SubscriptSlice _ b c d ->
            case d of
              Nothing ->
                case c of
                  Nothing -> b ^. trailingWhitespace
                  Just e -> e ^. trailingWhitespace
              Just (e, f) ->
                case f of
                  Nothing -> e ^. trailingWhitespace
                  Just g -> g ^. trailingWhitespace)
      (\x ws ->
         case x of
          SubscriptExpr e -> SubscriptExpr $ e & trailingWhitespace .~ ws
          SubscriptSlice a b c d ->
            (\(b', c', d') -> SubscriptSlice a b' c' d') $
            case d of
              Nothing ->
                case c of
                  Nothing -> (MkColon ws, c, d)
                  Just e -> (b, Just $ e & trailingWhitespace .~ ws, d)
              Just (e, f) ->
                case f of
                  Nothing -> (b, c, Just (MkColon ws, f))
                  Just g -> (b, c, Just (e, Just $ g & trailingWhitespace .~ ws)))
