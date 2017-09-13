{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.LongString where

import Papa
import Data.Deriving

import Language.Python.AST.EscapeSeq
import Language.Python.AST.LongStringChar
import Language.Python.AST.Symbols
import Language.Python.AST.TripleString

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
    :: TripleStringContent SingleQuote LongStringChar
  , _longString_ann :: a
  }
  | LongStringDouble
  { _longStringDouble_value
    :: TripleStringContent DoubleQuote LongStringChar
  , _longString_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq ''LongString
deriveShow ''LongString
deriveEq1 ''LongString
deriveShow1 ''LongString
makeLenses ''LongString
