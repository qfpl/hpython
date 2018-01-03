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

data ArgsList ws name test a
  = ArgsListAll
  { _argsListAll_head :: ArgsListArg ws name test a
  , _argsListAll_tail
    :: Compose
         []
         (Compose
           (Before (Between' [ws] Comma))
           (ArgsListArg ws name test))
         a
  , _argsListAll_rest
    :: Compose
         Maybe
         (Compose
           (Before (Between' [ws] Comma))
           (Compose
             Maybe
             (Sum
               (ArgsListStarPart ws name test)
               (ArgsListDoublestarArg ws name test))))
         a
  , _argsList_ann :: a
  }
  | ArgsListArgsKwargs
  { _argsListArgsKwargs_value
      :: Sum
           (ArgsListStarPart ws name test)
           (ArgsListDoublestarArg ws name test)
           a
  , _argsList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq ws, Eq1 test, Eq a, Eq1 name, Eq (name a)) => Eq (ArgsList ws name test a)
deriving instance (Show ws, Show1 test, Show a, Show1 name, Show (name a)) => Show (ArgsList ws name test a)
deriving instance (Ord ws, Ord1 test, Ord a, Ord1 name, Ord (name a)) => Ord (ArgsList ws name test a)

deriveEq1 ''ArgsList
deriveShow1 ''ArgsList
deriveOrd1 ''ArgsList
makeLenses ''ArgsList
