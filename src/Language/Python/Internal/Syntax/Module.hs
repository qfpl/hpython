{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Language.Python.Internal.Syntax.Module where

import Control.Lens.Fold (foldMapOf, folded)
import Control.Lens.Setter (over, mapped)
import Control.Lens.TH (makeWrapped)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Tuple (_1)
import Control.Lens.Prism (_Right)
import Control.Lens.Wrapped (_Wrapped)
import Data.Bifunctor (bimap)
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)

import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.Statement
import Language.Python.Internal.Syntax.Whitespace

newtype Module v a
  = Module
  { unModule :: [Either (Indents a, Maybe Comment, Maybe Newline) (Statement v a)]
  } deriving (Eq, Show)

instance HasStatements Module where
  _Statements = _Wrapped.traverse._Right

instance Functor (Module v) where
  fmap f (Module m) = Module $ fmap (bimap (over (_1.mapped) f) (fmap f)) m

instance Foldable (Module v) where
  foldMap f (Module m) = foldMap (bifoldMap (foldMapOf (_1.folded) f) (foldMap f)) m

instance Traversable (Module v) where
  traverse f (Module m) =
    Module <$> traverse (bitraverse (traverseOf (_1.traverse) f) (traverse f)) m

makeWrapped ''Module
