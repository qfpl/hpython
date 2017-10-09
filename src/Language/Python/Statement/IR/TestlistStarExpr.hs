{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}

module Language.Python.Statement.IR.TestlistStarExpr where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Symbols
import Language.Python.Expr.IR

data TestlistStarExpr a
  = TestlistStarExpr
  { _testlistStarExpr_head
    :: Sum Test StarExpr a
  , _testlistStarExpr_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum Test StarExpr))
         a
  , _testlistStarExpr_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _testlistStarExpr_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''TestlistStarExpr
deriveEq1 ''TestlistStarExpr
deriveShow1 ''TestlistStarExpr
