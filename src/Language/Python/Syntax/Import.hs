{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language DataKinds #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Import
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Syntax used in import statements

https://docs.python.org/3.5/reference/simple_stmts.html#the-import-statement
-}

module Language.Python.Syntax.Import
  ( ImportAs(..)
  , ImportTargets(..)
    -- * Lenses
  , importAsAnn
  , importAsName
  , importAsQual
  )
where

import Control.Lens.Getter ((^.), getting)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Prism (_Just)
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_2)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Whitespace

-- | Some data optionally followed by @as <ident>@
--
-- Used in:
--
-- @import a as b@
--
-- @from a import b as c, d as e@
--
-- @from a import (b as c, d as e)@
data ImportAs e a
  = ImportAs
  { _importAsAnn :: Ann a
  , _importAsName :: e a
  , _importAsQual :: Maybe (NonEmpty Whitespace, Ident a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

makeLenses ''ImportAs

instance HasAnn (ImportAs e) where
  annot :: forall a. Lens' (ImportAs e a) (Ann a)
  annot = importAsAnn

instance HasTrailingWhitespace (e a) => HasTrailingWhitespace (ImportAs e a) where
  trailingWhitespace =
    lens
      (\(ImportAs _ a b) ->
         maybe (a ^. getting trailingWhitespace) (^. _2.trailingWhitespace) b)
      (\(ImportAs x a b) ws ->
         ImportAs
           x
           (maybe (a & trailingWhitespace .~ ws) (const a) b)
           (b & _Just._2.trailingWhitespace .~ ws))

-- | The targets of a @from ... import ...@ statement
data ImportTargets a
  -- | @from x import *@
  = ImportAll (Ann a) [Whitespace]
  -- | @from x import a, b, c@
  | ImportSome (Ann a) (CommaSep1 (ImportAs Ident a))
  -- | @from x import (a, b, c)@
  | ImportSomeParens
      (Ann a)
      -- ( spaces
      [Whitespace]
      -- imports as
      (CommaSep1' (ImportAs Ident a))
      -- ) spaces
      [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn ImportTargets where
  annot :: forall a. Lens' (ImportTargets a) (Ann a)
  annot = typed @(Ann a)

instance HasTrailingWhitespace (ImportTargets a) where
  trailingWhitespace =
    lens
      (\case
          ImportAll _ ws -> ws
          ImportSome _ cs -> cs ^. trailingWhitespace
          ImportSomeParens _ _ _ ws -> ws)
      (\ts ws ->
         case ts of
           ImportAll a _ -> ImportAll a ws
           ImportSome a cs -> ImportSome a (cs & trailingWhitespace .~ ws)
           ImportSomeParens x a b _ -> ImportSomeParens x a b ws)