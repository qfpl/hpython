{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.Imag where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.Digits
import Language.Python.Expr.AST.Float

data Imag a
  = Imag
  { _imag_value
    :: Compose
         (After (Either Char_j Char_J))
         (Sum Float' (Const (NonEmpty Digit)))
         a
  , _imag_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

makeLenses ''Imag
deriveEq ''Imag
deriveShow ''Imag
deriveEq1 ''Imag
deriveShow1 ''Imag
