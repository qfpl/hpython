{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Expr.AST.LongBytes where

import Papa
import Data.Deriving

import Language.Python.AST.Symbols
import Language.Python.Expr.AST.LongBytesChar
import Language.Python.Expr.AST.EscapeSeq
import Language.Python.Expr.AST.StringContent

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
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

deriveEq1 ''LongBytes
deriveShow1 ''LongBytes
deriveOrd1 ''LongBytes
makeLenses ''LongBytes
