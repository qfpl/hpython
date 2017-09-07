{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.StringLiteral where

import Papa hiding (Sum)

import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Before

import Language.Python.AST.LongString
import Language.Python.AST.ShortString
import Language.Python.AST.StringPrefix

data StringLiteral a
  = StringLiteral
  { _stringLiteral_value
    :: Compose
         (Before (Maybe StringPrefix))
         (Sum ShortString LongString)
         a
  , _stringLiteral_ann :: a
  } deriving (Functor, Foldable, Traversable)

deriveEq ''StringLiteral
deriveShow ''StringLiteral
deriveEq1 ''StringLiteral
deriveShow1 ''StringLiteral
makeLenses ''StringLiteral
