{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.LongString where

import Papa
import Data.Deriving

import Language.Python.AST.EscapeSeq
import Language.Python.AST.LongStringChar

-- | Between three quotes
data LongString a
  = LongStringSingle
  { _longStringSingle_value
    :: [Either LongStringChar EscapeSeq]
  , _longStringSingle_ann :: a
  }
  | LongStringDouble
  { _longStringDouble_value
    :: [Either LongStringChar EscapeSeq]
  , _longStringDouble_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq ''LongString
deriveShow ''LongString
deriveEq1 ''LongString
deriveShow1 ''LongString
makeLenses ''LongString
