{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.ShortString where

import Papa
import Data.Deriving

import Language.Python.AST.EscapeSeq
import Language.Python.AST.ShortStringChar
import Language.Python.AST.Symbols
import Language.Python.AST.StringContent

-- | Strings between one single or double quote
data ShortString a
  = ShortStringSingle
  { _shortStringSingle_value
    :: StringContent SingleQuote (ShortStringChar SingleQuote)
  , _shortString_ann :: a
  }
  | ShortStringDouble
  { _shortStringDouble_value
    :: StringContent DoubleQuote (ShortStringChar DoubleQuote)
  , _shortString_ann :: a
  } deriving (Functor, Foldable, Traversable)

deriveEq ''ShortString
deriveShow ''ShortString
deriveEq1 ''ShortString
deriveShow1 ''ShortString
makeLenses ''ShortString
