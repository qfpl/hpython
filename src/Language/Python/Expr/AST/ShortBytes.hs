{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.ShortBytes where

import Papa
import Data.Deriving

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.ShortBytesChar
import Language.Python.Expr.AST.StringContent

data ShortBytes a
  = ShortBytesSingle
  { _shortBytesSingle_value
    :: StringContent SingleQuote (ShortBytesChar SingleQuote)
  , _shortBytes_ann :: a
  }
  | ShortBytesDouble
  { _shortBytesDouble_value
    :: StringContent DoubleQuote (ShortBytesChar DoubleQuote)
  , _shortBytes_ann :: a
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

deriveEq1 ''ShortBytes
deriveShow1 ''ShortBytes
deriveOrd1 ''ShortBytes
makeLenses ''ShortBytes
