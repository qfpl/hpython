{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Syntax.Import
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Syntax used in import statements

https://docs.python.org/3.5/reference/simple_stmts.html#the-import-statement
-}

module Language.Python.Syntax.Import where

import Control.Lens.Getter ((^.), getting)
import Control.Lens.Lens (lens)
import Control.Lens.Prism (_Just)
import Control.Lens.Setter ((.~))
import Control.Lens.Tuple (_2)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)

import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Whitespace

-- | Some data optionally followed by @as <ident>@
--
-- Used in:
--
-- @import a as b@
-- @from a import b as c, d as e@
-- @from a import (b as c, d as e)@
data ImportAs e v a
  = ImportAs a (e a) (Maybe (NonEmpty Whitespace, Ident v a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

importAsAnn :: ImportAs e v a -> a
importAsAnn (ImportAs a _ _) = a

instance HasTrailingWhitespace (e a) => HasTrailingWhitespace (ImportAs e v a) where
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
data ImportTargets v a
  -- | @from x import *@
  = ImportAll a [Whitespace]
  -- | @from x import a, b, c@
  | ImportSome a (CommaSep1 (ImportAs (Ident v) v a))
  -- | @from x import (a, b, c)@
  | ImportSomeParens
      a
      -- ( spaces
      [Whitespace]
      -- imports as
      (CommaSep1' (ImportAs (Ident v) v a))
      -- ) spaces
      [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (ImportTargets v a) where
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
