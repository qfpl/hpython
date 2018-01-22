{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.LongString where

import Papa
import Data.Deriving

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.LongStringChar
import Language.Python.Expr.AST.StringContent

-- | Between three quotes
data LongString a
  = LongStringSingleEmpty
  { _longString_ann :: a
  }
  | LongStringDoubleEmpty
  { _longString_ann :: a
  }
  | LongStringSingle
  { _longStringSingle_value
    :: StringContent SingleQuote LongStringChar
  , _longString_ann :: a
  }
  | LongStringDouble
  { _longStringDouble_value
    :: StringContent DoubleQuote LongStringChar
  , _longString_ann :: a
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

deriveEq1 ''LongString
deriveShow1 ''LongString
deriveOrd1 ''LongString
makeLenses ''LongString
