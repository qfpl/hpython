{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Syntax.ModuleNames
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Module names, including those qualified by packages.

See <https://docs.python.org/3.5/tutorial/modules.html#packages>
-}

module Language.Python.Syntax.ModuleNames
  ( ModuleName (..)
  , RelativeModuleName (..)
  , makeModuleName
  , _moduleNameAnn
  )
where

import Control.Lens.Cons (_last)
import Control.Lens.Fold ((^?!))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (lens)
import Control.Lens.Setter ((.~))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic)

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Whitespace

-- | @.a.b@
--
-- @.@
--
-- @...@
--
--See <https://docs.python.org/3.5/tutorial/modules.html#intra-package-references>
data RelativeModuleName v a
  = RelativeWithName (Ann a) [Dot] (ModuleName v a)
  | Relative (Ann a) (NonEmpty Dot)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasTrailingWhitespace (RelativeModuleName v a) where
  trailingWhitespace =
    lens
      (\case
          RelativeWithName _ _ mn -> mn ^. trailingWhitespace
          Relative _ (a :| as) -> (a : as) ^?! _last.trailingWhitespace)
      (\a ws -> case a of
          RelativeWithName ann x mn -> RelativeWithName ann x (mn & trailingWhitespace .~ ws)
          Relative ann (a :| as) ->
            Relative ann .
            NonEmpty.fromList $
            (a : as) & _last.trailingWhitespace .~ ws)

-- | A module name. It can be a single segment, or a sequence of them which
-- are implicitly separated by period character.
--
-- @a@
--
-- @a.b@
data ModuleName v a
  = ModuleNameOne (Ann a) (Ident v a)
  | ModuleNameMany (Ann a) (Ident v a) Dot (ModuleName v a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | Get the annotation from a 'ModuleName'
_moduleNameAnn :: ModuleName v a -> a
_moduleNameAnn (ModuleNameOne a _) = getAnn a
_moduleNameAnn (ModuleNameMany a _ _ _) = getAnn a

-- | Convenience constructor for 'ModuleName'
makeModuleName :: Ident v a -> [([Whitespace], Ident v a)] -> ModuleName v a
makeModuleName i [] = ModuleNameOne (Ann $ _identAnn i) i
makeModuleName i ((a, b) : as) =
  ModuleNameMany (Ann $ _identAnn i) i (MkDot a) $
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
