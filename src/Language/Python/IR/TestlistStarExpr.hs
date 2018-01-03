{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}

module Language.Python.IR.TestlistStarExpr where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Symbols

data TestlistStarExpr ws test starExpr a
  = TestlistStarExpr
  { _testlistStarExpr_head
    :: Sum (test ws) (starExpr ws) a
  , _testlistStarExpr_tail
    :: Compose
         []
         (Compose
           (Before (Between' [ws] Comma))
           (Sum (test ws) (starExpr ws)))
         a
  , _testlistStarExpr_comma :: Maybe (Between' [ws] Comma)
  , _testlistStarExpr_ann :: a
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

makeLenses ''TestlistStarExpr

instance (Eq1 (test ws), Eq1 (starExpr ws), Eq ws) =>
  Eq1 (TestlistStarExpr ws test starExpr) where
  liftEq = $(makeLiftEq ''TestlistStarExpr)

instance (Show1 (test ws), Show1 (starExpr ws), Show ws) =>
  Show1 (TestlistStarExpr ws test starExpr) where
  liftShowsPrec = $(makeLiftShowsPrec ''TestlistStarExpr)

instance (Ord1 (test ws), Ord1 (starExpr ws), Ord ws) =>
  Ord1 (TestlistStarExpr ws test starExpr) where
  liftCompare = $(makeLiftCompare ''TestlistStarExpr)
