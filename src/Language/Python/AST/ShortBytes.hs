{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.ShortBytes where

import Papa
import Data.Deriving

import Language.Python.AST.EscapeSeq
import Language.Python.AST.ShortBytesChar
import Language.Python.AST.Symbols
import Language.Python.AST.TripleString

data ShortBytes a
  = ShortBytesSingle
  { _shortBytesSingle_value
    :: TripleStringContent SingleQuote (ShortBytesChar SingleQuote)
  , _shortBytes_ann :: a
  }
  | ShortBytesDouble
  { _shortBytesDouble_value
    :: TripleStringContent DoubleQuote (ShortBytesChar DoubleQuote)
  , _shortBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq ''ShortBytes
deriveShow ''ShortBytes
deriveEq1 ''ShortBytes
deriveShow1 ''ShortBytes
makeLenses ''ShortBytes
