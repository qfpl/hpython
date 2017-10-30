{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.Identifier where

import Papa
import Data.Deriving
import Data.Text (Text)

import Language.Python.AST.IsArgList

data Identifier a
  = Identifier
  { _identifier_value :: Text
  , _identifier_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Ord)

makeLenses ''Identifier

instance HasName Identifier where
  name = identifier_value

deriveEq1 ''Identifier
deriveOrd1 ''Identifier
deriveShow1 ''Identifier
