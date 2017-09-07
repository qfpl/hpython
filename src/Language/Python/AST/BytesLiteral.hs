{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.BytesLiteral where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Sum

import Language.Python.AST.BytesPrefix
import Language.Python.AST.LongBytes
import Language.Python.AST.ShortBytes

data BytesLiteral a
  = BytesLiteral
  { _bytesLiteral_prefix :: BytesPrefix
  , _bytesLiteral_value :: Sum ShortBytes LongBytes a
  , _bytesLiteral_ann :: a
  } deriving (Functor, Foldable, Traversable)

deriveEq ''BytesLiteral
deriveShow ''BytesLiteral
deriveEq1 ''BytesLiteral
deriveShow1 ''BytesLiteral
makeLenses ''BytesLiteral
