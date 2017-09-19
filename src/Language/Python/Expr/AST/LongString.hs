{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.LongString where

import Papa
import Data.Deriving

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.EscapeSeq
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
  deriving (Functor, Foldable, Traversable)

deriveEq ''LongString
deriveShow ''LongString
deriveEq1 ''LongString
deriveShow1 ''LongString
makeLenses ''LongString
