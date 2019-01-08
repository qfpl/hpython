{-|

Comprehensions https://docs.python.org/3/reference/expressions.html#grammar-token-comprehension

-}

{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DataKinds, KindSignatures #-}
module Language.Python.Syntax.Comprehension where

import Control.Lens.Cons (_last)
import Control.Lens.Fold ((^?!))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (lens)
import Control.Lens.Prism (_Left, _Right)
import Control.Lens.Setter ((.~))
import Control.Lens.Traversal (failing)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Function ((&))

import Data.VTraversable
import Language.Python.Syntax.Whitespace

-- | A condition inside a comprehension, e.g. @[x for x in xs if even(x)]@
data CompIf expr (v :: [*]) a
  = CompIf a [Whitespace] (expr v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance VTraversable CompIf where
  vtraverse f (CompIf a b c) = CompIf a b <$> f c

instance HasTrailingWhitespace (e v a) => HasTrailingWhitespace (CompIf e v a) where
  trailingWhitespace =
    lens
      (\(CompIf _ _ a) -> a ^. trailingWhitespace)
      (\(CompIf a b c) ws -> CompIf a b $ c & trailingWhitespace .~ ws)

-- | A nested comprehesion, e.g. @[(x, y) for x in xs for y in ys]@
data CompFor expr (v :: [*]) a
  = CompFor a [Whitespace] (expr v a) [Whitespace] (expr v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance VTraversable CompFor where
  vtraverse f (CompFor a b c d e) = (\c' -> CompFor a b c' d) <$> f c <*> f e

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (CompFor expr v a) where
  trailingWhitespace =
    lens
      (\(CompFor _ _ _ _ a) -> a ^. trailingWhitespace)
      (\(CompFor a b c d e) ws -> CompFor a b c d $ e & trailingWhitespace .~ ws)

-- | A Python for comprehension, such as
--
-- @
-- x for y in z
-- @
data Comprehension h expr (v :: [*]) a
  = Comprehension a
      (h expr v a)
      (CompFor expr v a)
      [Either (CompFor expr v a) (CompIf expr v a)] -- ^ <expr> <comp_for> (comp_for | comp_if)*
  deriving (Eq, Show)

instance VTraversable h => VTraversable (Comprehension h) where
  vtraverse f (Comprehension a b c d) =
    Comprehension a <$>
    vtraverse f b <*>
    vtraverse f c <*>
    traverse (bitraverse (vtraverse f) (vtraverse f)) d

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (Comprehension h expr v a) where
  trailingWhitespace =
    lens
      (\(Comprehension _ _ a b) ->
         case b of
           [] -> a ^. trailingWhitespace
           _ -> b ^?! _last.failing (_Left.trailingWhitespace) (_Right.trailingWhitespace))
      (\(Comprehension a b c d) ws ->
         case d of
           [] -> Comprehension a b (c & trailingWhitespace .~ ws) d
           _ ->
             Comprehension a b c
               (d &
                _last.failing (_Left.trailingWhitespace) (_Right.trailingWhitespace) .~ ws))

instance (Functor (expr v), Functor (h expr v)) => Functor (Comprehension h expr v) where
  fmap f (Comprehension a b c d) =
    Comprehension (f a) (fmap f b) (fmap f c) (fmap (bimap (fmap f) (fmap f)) d)

instance (Foldable (expr v), Foldable (h expr v)) => Foldable (Comprehension h expr v) where
  foldMap f (Comprehension a b c d) =
    f a <> foldMap f b <> foldMap f c <> foldMap (bifoldMap (foldMap f) (foldMap f)) d

instance (Traversable (expr v), Traversable (h expr v)) => Traversable (Comprehension h expr v) where
  traverse f (Comprehension a b c d) =
    Comprehension <$>
    f a <*>
    traverse f b <*>
    traverse f c <*>
    traverse (bitraverse (traverse f) (traverse f)) d