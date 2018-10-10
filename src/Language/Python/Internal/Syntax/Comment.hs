{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Syntax.Comment where

import Data.Deriving (deriveEq1, deriveOrd1)

data Comment a
  = MkComment
  { _commentAnn :: a
  , _commentValue :: String
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''Comment
deriveOrd1 ''Comment
