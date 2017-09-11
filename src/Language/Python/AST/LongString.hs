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

-- | Between three quotes
data LongString a
  = LongStringSingleEmpty
  { _longString_ann :: a
  }
  | LongStringDoubleEmpty
  { _longString_ann :: a
  }
  | LongStringSingle
  { _longStringSingle_init
    :: [Either LongStringChar EscapeSeq]
  , _longStringSingle_last
    :: Either (LongStringCharFinal SingleQuote) EscapeSeq
  , _longString_ann :: a
  }
  | LongStringDouble
  { _longStringDouble_init
    :: [Either LongStringChar EscapeSeq]
  , _longStringDouble_last
    :: Either (LongStringCharFinal DoubleQuote) EscapeSeq
  , _longString_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq ''LongString
deriveShow ''LongString
deriveEq1 ''LongString
deriveShow1 ''LongString
makeLenses ''LongString
