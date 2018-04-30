{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
module Language.Python.Internal.Syntax.ModuleNames where

import Control.Lens.Getter ((^.), getting)
import Control.Lens.Lens (lens)
import Control.Lens.Setter ((.~))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)

import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Token
import Language.Python.Internal.Syntax.Whitespace

data RelativeModuleName v a
  = RelativeWithName [Dot] (ModuleName v a)
  | Relative (NonEmpty Dot) [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Token (RelativeModuleName v a) (RelativeModuleName '[] a) where
  unvalidate = coerce
  whitespaceAfter =
    lens
      (\case
          RelativeWithName _ mn -> mn ^. getting whitespaceAfter
          Relative _ ws -> ws)
      (\a ws -> case a of
          RelativeWithName x mn -> RelativeWithName x (mn & whitespaceAfter .~ ws)
          Relative x _ -> Relative x ws)

  startChar (RelativeWithName [] mn) = startChar mn
  startChar (RelativeWithName (_:_) mn) = '.'
  startChar (Relative _ _) = '.'

  endChar (RelativeWithName _ mn) = endChar mn
  endChar (Relative _ _) = '.'

data Dot = Dot [Whitespace]
  deriving (Eq, Show)

data ModuleName v a
  = ModuleNameOne a (Ident v a)
  | ModuleNameMany a (Ident v a) [Whitespace] (ModuleName v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Token (ModuleName v a) (ModuleName '[] a) where
  unvalidate = coerce
  whitespaceAfter =
    lens
      (\case
          ModuleNameOne _ i -> i ^. getting whitespaceAfter
          ModuleNameMany _ _ _ mn -> mn ^. getting whitespaceAfter)
      (\mn ws -> case mn of
          ModuleNameOne a b -> ModuleNameOne a (b & whitespaceAfter .~ ws)
          ModuleNameMany a b d mn ->
            ModuleNameMany a (unvalidate b) d (mn & whitespaceAfter .~ ws))

  startChar (ModuleNameOne _ i) = startChar i
  startChar (ModuleNameMany _ i _ _) = startChar i

  endChar (ModuleNameOne _ i) = endChar i
  endChar (ModuleNameMany _ _ _ mn) = endChar mn
