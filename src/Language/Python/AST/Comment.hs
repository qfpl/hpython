{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.Comment where

import Papa
import Data.Deriving
import Data.Text (Text)

data Comment a
  = Comment
  { _comment_text :: Text
  , _comment_ann :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''Comment
deriveEq1 ''Comment
deriveShow1 ''Comment
