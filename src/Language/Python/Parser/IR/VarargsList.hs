{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.Parser.IR.VarargsList where

import Papa hiding (Sum)
import Data.Deriving
import Data.Separated.Before
import Data.Separated.Between
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum

import Language.Python.AST.Symbols
import Language.Python.AST.VarargsList
  ( VarargsListArg
  , VarargsListStarPart
  , VarargsListDoublestarArg
  )

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
               (VarargsListDoublestarArg test))))
         a
  , _varargsList_ann :: a
  }
  | VarargsListArgsKwargs
  { _varargsListArgsKwargs_value
      :: Sum
           (VarargsListStarPart test)
           (VarargsListDoublestarArg test)
           a
  , _varargsList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 test, Eq a) => Eq (VarargsList test a)
deriving instance (Show1 test, Show a) => Show (VarargsList test a)

deriveEq1 ''VarargsList
deriveShow1 ''VarargsList
makeLenses ''VarargsList
