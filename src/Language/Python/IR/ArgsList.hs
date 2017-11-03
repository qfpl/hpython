{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.IR.ArgsList where

import Papa hiding (Sum)
import Data.Deriving
import Data.Separated.Before
import Data.Separated.Between
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum

import Language.Python.AST.Symbols
import Language.Python.AST.ArgsList
  ( ArgsListArg
  , ArgsListStarPart
  , ArgsListDoublestarArg
  )

data ArgsList name test a
  = ArgsListAll
  { _argsListAll_head :: ArgsListArg name test a
  , _argsListAll_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (ArgsListArg name test))
         a
  , _argsListAll_rest
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Compose
             Maybe
             (Sum
               (ArgsListStarPart name test)
               (ArgsListDoublestarArg name test))))
         a
  , _argsList_ann :: a
  }
  | ArgsListArgsKwargs
  { _argsListArgsKwargs_value
      :: Sum
           (ArgsListStarPart name test)
           (ArgsListDoublestarArg name test)
           a
  , _argsList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 test, Eq a, Eq1 name, Eq (name a)) => Eq (ArgsList name test a)
deriving instance (Show1 test, Show a, Show1 name, Show (name a)) => Show (ArgsList name test a)
deriving instance (Ord1 test, Ord a, Ord1 name, Ord (name a)) => Ord (ArgsList name test a)

deriveEq1 ''ArgsList
deriveShow1 ''ArgsList
deriveOrd1 ''ArgsList
makeLenses ''ArgsList
