{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}
module Language.Python.Statement.AST.DottedName where

import Papa
import Data.Deriving
import Data.Functor.Compose
import Data.Separated.Before
import Data.Separated.Between
import Language.Python.AST.Identifier
import Language.Python.AST.Symbols

data DottedName a
  = DottedName
  { _dottedName_head :: Identifier a
  , _dottedName_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Dot))
           Identifier)
         a
  , _dottedName_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''DottedName
deriveEq1 ''DottedName
deriveShow1 ''DottedName
