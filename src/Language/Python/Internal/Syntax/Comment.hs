{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Internal.Syntax.Comment
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

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
