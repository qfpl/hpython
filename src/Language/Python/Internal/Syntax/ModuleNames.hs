{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
module Language.Python.Internal.Syntax.ModuleNames where

import Control.Lens.Cons (_last)
import Control.Lens.Fold ((^?!))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (lens)
import Control.Lens.Setter ((.~))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Whitespace

data RelativeModuleName v a
  = RelativeWithName [Dot] (ModuleName v a)
  | Relative (NonEmpty Dot)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (RelativeModuleName v a) where
  trailingWhitespace =
    lens
      (\case
          RelativeWithName _ mn -> mn ^. trailingWhitespace
          Relative (a :| as) -> (a : as) ^?! _last.trailingWhitespace)
      (\a ws -> case a of
          RelativeWithName x mn -> RelativeWithName x (mn & trailingWhitespace .~ ws)
          Relative (a :| as) ->
            Relative .
            NonEmpty.fromList $
            (a : as) & _last.trailingWhitespace .~ ws)

data Dot = Dot [Whitespace]
  deriving (Eq, Show)

instance HasTrailingWhitespace Dot where
  trailingWhitespace =
    lens (\(Dot ws) -> ws) (\_ ws -> Dot ws)

data ModuleName v a
  = ModuleNameOne a (Ident v a)
  | ModuleNameMany a (Ident v a) [Whitespace] (ModuleName v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

_moduleNameAnn :: ModuleName v a -> a
_moduleNameAnn (ModuleNameOne a _) = a
_moduleNameAnn (ModuleNameMany a _ _ _) = a

makeModuleName :: Ident v a -> [([Whitespace], Ident v a)] -> ModuleName v a
makeModuleName i [] = ModuleNameOne (_identAnnotation i) i
makeModuleName i ((a, b) : as) =
  ModuleNameMany (_identAnnotation i) i a $
  makeModuleName b as

instance HasTrailingWhitespace (ModuleName v a) where
  trailingWhitespace =
    lens
      (\case
          ModuleNameOne _ i -> i ^. trailingWhitespace
          ModuleNameMany _ _ _ mn -> mn ^. trailingWhitespace)
      (\mn ws -> case mn of
          ModuleNameOne a b -> ModuleNameOne a (b & trailingWhitespace .~ ws)
          ModuleNameMany a b d mn ->
            ModuleNameMany a (coerce b) d (mn & trailingWhitespace .~ ws))
