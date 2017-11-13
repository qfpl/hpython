{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}

module Language.Python.IR.TestlistStarExpr where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Symbols

data TestlistStarExpr test starExpr a
  = TestlistStarExpr
  { _testlistStarExpr_head
    :: Sum test starExpr a
  , _testlistStarExpr_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum test starExpr))
         a
  , _testlistStarExpr_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _testlistStarExpr_ann :: a
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

makeLenses ''TestlistStarExpr
deriveEq1 ''TestlistStarExpr
deriveShow1 ''TestlistStarExpr
deriveOrd1 ''TestlistStarExpr
