{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.LongBytes where

import Papa
import Data.Deriving

import Language.Python.AST.LongBytesChar
import Language.Python.AST.EscapeSeq
import Language.Python.AST.Symbols
import Language.Python.AST.StringContent

-- | Between triple quotes
data LongBytes a
  = LongBytesSingleEmpty 
  { _longBytes_ann :: a
  }
  | LongBytesDoubleEmpty
  { _longBytes_ann :: a
  }
  | LongBytesSingle
  { _longBytesSingle_value
    :: StringContent SingleQuote LongBytesChar
  , _longBytes_ann :: a
  }
  | LongBytesDouble
  { _longBytesDouble_value
    :: StringContent DoubleQuote LongBytesChar
  , _longBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq ''LongBytes
deriveShow ''LongBytes
deriveEq1 ''LongBytes
deriveShow1 ''LongBytes
makeLenses ''LongBytes
