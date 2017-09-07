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

-- | Strings between one single or double quote
data ShortString a
  = ShortStringSingle
  { _shortStringSingle_value
    :: [Either (ShortStringChar SingleQuote) EscapeSeq]
  , _shortString_ann :: a
  }
  | ShortStringDouble
  { _shortStringDouble_value
    :: [Either (ShortStringChar DoubleQuote) EscapeSeq]
  , _shortString_ann :: a
  } deriving (Functor, Foldable, Traversable)

deriveEq ''ShortString
deriveShow ''ShortString
deriveEq1 ''ShortString
deriveShow1 ''ShortString
makeLenses ''ShortString
