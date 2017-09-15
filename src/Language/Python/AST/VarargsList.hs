{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.VarargsList where

import Papa hiding (Sum)
import Data.Deriving
import Data.Separated.Before
import Data.Separated.Between
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum

import Language.Python.AST.Identifier
import Language.Python.AST.Symbols

data VarargsList test a
  = VarargsListAll
  { _varargsListAll_head :: VarargsListArg test a
  , _varargsListAll_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (VarargsListArg test))
         a
  , _varargsListAll_rest
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Compose
             Maybe
             (Sum
               (VarargsListStarPart test)
               VarargsListDoublestarArg)))
         a
  , _varargsList_ann :: a
  }
  | VarargsListArgsKwargs
  { _varargsListArgsKwargs_value
      :: Sum
           (VarargsListStarPart test)
           VarargsListDoublestarArg
           a
  , _varargsList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 test, Eq a) => Eq (VarargsList test a)
deriving instance (Show1 test, Show a) => Show (VarargsList test a)

data VarargsListStarPart test a
  = VarargsListStarPartEmpty
  { _varargsListStarPart_ann :: a
  }
  | VarargsListStarPart
  { _varargsListStarPart_starred
    :: Compose
         (Before (Between' [WhitespaceChar] Asterisk))
         Identifier
         a
  , _varargsListStarPart_defaults
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (VarargsListArg test))
         a
  , _varargsListStarPart_kwargs
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           VarargsListDoublestarArg)
         a
  , _varargsListStarPart_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 test, Eq a) => Eq (VarargsListStarPart test a)
deriving instance (Show1 test, Show a) => Show (VarargsListStarPart test a)

data VarargsListDoublestarArg a
  = VarargsListDoublestarArg
  { _varargsListDoublestarArg_value
    :: Compose
         (Between' [WhitespaceChar])
         Identifier
         a
  , _varargsListDoublestarArg_ann :: a
  }
  deriving (Eq, Functor, Foldable, Show, Traversable)

data VarargsListArg test a
  = VarargsListArg
  { _varargsListArg_left :: Identifier a
  , _varargsListArg_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Equals))
           test)
         a
  , _varargsListArg_ann :: a
  }
  deriving (Eq, Functor, Foldable, Show, Traversable)

deriveEq1 ''VarargsListArg
deriveShow1 ''VarargsListArg
makeLenses ''VarargsListArg

deriveEq1 ''VarargsListStarPart
deriveShow1 ''VarargsListStarPart
makeLenses ''VarargsListStarPart

deriveEq1 ''VarargsListDoublestarArg
deriveShow1 ''VarargsListDoublestarArg
makeLenses ''VarargsListDoublestarArg

deriveEq1 ''VarargsList
deriveShow1 ''VarargsList
makeLenses ''VarargsList
