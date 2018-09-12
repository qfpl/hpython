{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Language.Python.Internal.Syntax.Module where

import Control.Lens.Fold (foldMapOf)
import Control.Lens.TH (makeWrapped)
import Control.Lens.Prism (_Right)
import Control.Lens.Setter (over)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Tuple (_1)
import Control.Lens.Wrapped (_Wrapped)
import Data.Bifunctor (bimap)
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)

import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.Statement
import Language.Python.Internal.Syntax.Whitespace

newtype Module v a
  = Module
  { unModule :: [Either (a, [Whitespace], Maybe Comment, Maybe Newline) (Statement v a)]
  } deriving (Eq, Show)

instance HasStatements Module where
  _Statements = _Wrapped.traverse._Right

instance Functor (Module v) where
  fmap f = Module . fmap (bimap (over _1 f) (fmap f)) . unModule

instance Foldable (Module v) where
  foldMap f = foldMap (bifoldMap (foldMapOf _1 f) (foldMap f)) . unModule

instance Traversable (Module v) where
  traverse f = fmap Module . traverse (bitraverse (traverseOf _1 f) (traverse f)) . unModule

makeWrapped ''Module
