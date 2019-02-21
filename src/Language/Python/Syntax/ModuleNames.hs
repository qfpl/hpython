{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

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
  ( ModuleName(..)
  , RelativeModuleName(..)
  , makeModuleName
  , moduleNameInit
  , moduleNameSnoc
  , moduleNameCat
  , moduleNameAppend
  , unfoldModuleName
  , sameModuleName
  )
where

import Control.Lens.Cons (_last)
import Control.Lens.Fold ((^?!))
import Control.Lens.Getter ((^.), to, getting)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~))
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Function ((&))
import Data.Generics.Product.Typed (typed)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Punctuation
import Language.Python.Optics.Validated (Validated(..))
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
instance Validated RelativeModuleName where
  unvalidated = to unsafeCoerce
  demoted_ = to unsafeCoerce

instance HasAnn (RelativeModuleName v) where
  annot :: forall a. Lens' (RelativeModuleName v a) (Ann a)
  annot = typed @(Ann a)

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
instance Validated ModuleName where
  unvalidated = to unsafeCoerce
  demoted_ = to unsafeCoerce
deriveEq1 ''ModuleName
deriveOrd1 ''ModuleName

instance HasAnn (ModuleName v) where
  annot :: forall a. Lens' (ModuleName v a) (Ann a)
  annot = typed @(Ann a)

-- |
-- Split a 'ModuleName' into an optional prefix along with its last segment
--
-- >>> moduleNameInit "a" []
-- (Nothing, (.., "a"))
--
-- >>> moduleNameInit "a" [([], "b"), ([], "c")]
-- (Just (ModuleNameMany .. "a" .. (ModuleNameOne .. "b"), Just ..), (.., "c"))
moduleNameInit :: ModuleName v a -> (Maybe (ModuleName v a, Dot), (Ann a, Ident v a))
moduleNameInit (ModuleNameOne a b) = (Nothing, (a, b))
moduleNameInit (ModuleNameMany a b c d) =
  first
    (Just . maybe (ModuleNameOne a b, c) (\(e, f) -> (ModuleNameMany a b c e, f)))
    (moduleNameInit d)

-- | Prepend an 'Ident' onto the first segment of a 'ModuleName'
--
-- >>> moduleNameCat "a" (ModuleNameOne .. "b")
-- ModuleNameOne .. "ab"
--
-- >>> moduleNameCat "a" (ModuleNameMany .. "b" .. (ModuleNameOne .. "c"), Just ..)
-- ModuleNameOne .. "ab.c"
moduleNameCat :: Semigroup a => Ident v a -> ModuleName v a -> ModuleName v a
moduleNameCat a b =
  case b of
    ModuleNameOne c d -> ModuleNameOne (a ^. annot <> c) (a <> d)
    ModuleNameMany c d e f -> ModuleNameMany (a ^. annot <> c) (a <> d) e f

-- | Append some segments to a 'ModuleName'
moduleNameAppend :: ModuleName v a -> (Dot, ModuleName v a) -> ModuleName v a
moduleNameAppend (ModuleNameOne a b) (c, d) =
  ModuleNameMany a b c d
moduleNameAppend (ModuleNameMany a b c d) e =
  ModuleNameMany a b c (moduleNameAppend d e)

-- | Append a segments to a 'ModuleName'
moduleNameSnoc :: ModuleName v a -> (Dot, Ann a, Ident v a) -> ModuleName v a
moduleNameSnoc a (b, c, d) = moduleNameAppend a (b, ModuleNameOne c d)

-- | Convenience constructor for 'ModuleName'
makeModuleName :: Ident v a -> [([Whitespace], Ident v a)] -> ModuleName v a
makeModuleName i [] = ModuleNameOne (_identAnn i) i
makeModuleName i ((a, b) : as) =
  ModuleNameMany (_identAnn i) i (MkDot a) $
  makeModuleName b as

-- |
-- View a 'ModuleName' as a module path followed by a module name
unfoldModuleName :: ModuleName v a -> ([Ident v a], Ident v a)
unfoldModuleName = go id
  where
    go f (ModuleNameOne _ a) = (f [], a)
    go f (ModuleNameMany _ a _ b) = go (f . (a :)) b

-- | Check two 'ModuleName's for equality, ignoring annotations and whitespace
sameModuleName :: ModuleName v a -> ModuleName v' a' -> Bool
sameModuleName (ModuleNameOne _ i) (ModuleNameOne _ i') =
  i ^. getting identValue == i' ^. getting identValue
sameModuleName (ModuleNameMany _ i _ rest) (ModuleNameMany _ i' _ rest') =
  i ^. getting identValue == i' ^. getting identValue &&
  sameModuleName rest rest'
sameModuleName _ _ = False

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
