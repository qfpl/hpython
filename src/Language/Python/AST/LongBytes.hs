{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.LongBytes where

import Papa
import Data.Deriving

import Language.Python.AST.EscapeSeq

-- | Between triple quotes
data LongBytes a
  = LongBytesSingle
  { _longBytesSingle_value
    :: [Either Char EscapeSeq]
  , _longBytes_ann :: a
  }
  | LongBytesDouble
  { _longBytesDouble_value
    :: [Either Char EscapeSeq]
  , _longBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq ''LongBytes
deriveShow ''LongBytes
deriveEq1 ''LongBytes
deriveShow1 ''LongBytes
makeLenses ''LongBytes
