{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}
module Language.Python.Module.IR where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Between

import Language.Python.Statement.IR
import Language.Python.AST.Comment
import Language.Python.AST.Symbols

data Module a
  = Module
  { _module_statements
    :: Compose
         []
         (Sum
           (Compose
             (Between [WhitespaceChar] NewlineChar)
             (Compose Maybe Comment))
           Statement)
         a
  , _module_ann :: a
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

deriveEq1 ''Module
deriveOrd1 ''Module
deriveShow1 ''Module
makeLenses ''Module
