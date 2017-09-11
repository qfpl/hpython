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

-- | Between triple quotes
data LongBytes a
  = LongBytesSingleEmpty 
  { _longBytes_ann :: a
  }
  | LongBytesDoubleEmpty
  { _longBytes_ann :: a
  }
  | LongBytesSingle
  { _longBytesSingle_init
    :: [Either LongBytesChar EscapeSeq]
  , _longBytesSingle_last
    :: Either (LongBytesCharFinal SingleQuote) EscapeSeq
  , _longBytes_ann :: a
  }
  | LongBytesDouble
  { _longBytesDouble_init
    :: [Either LongBytesChar EscapeSeq]
  , _longBytesDouble_last
    :: Either (LongBytesCharFinal DoubleQuote) EscapeSeq
  , _longBytes_ann :: a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq ''LongBytes
deriveShow ''LongBytes
deriveEq1 ''LongBytes
deriveShow1 ''LongBytes
makeLenses ''LongBytes
