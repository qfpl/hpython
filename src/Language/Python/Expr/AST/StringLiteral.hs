{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.StringLiteral where

import Papa hiding (Sum)

import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Before

import Language.Python.Expr.AST.LongString
import Language.Python.Expr.AST.ShortString
import Language.Python.Expr.AST.StringPrefix

data StringLiteral a
  = StringLiteral
  { _stringLiteral_value
    :: Compose
         (Before (Maybe StringPrefix))
         (Sum ShortString LongString)
         a
  , _stringLiteral_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''StringLiteral
deriveOrd1 ''StringLiteral
deriveShow1 ''StringLiteral
makeLenses ''StringLiteral
