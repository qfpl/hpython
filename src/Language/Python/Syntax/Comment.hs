{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Comment
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.Comment where

import Data.Deriving (deriveEq1, deriveOrd1)

-- | A Python single-line comment, such as on the following line:
--
-- @
-- y = x + 4 # add four to the value of x
-- @
--
-- In this case, the structure parsed would be
--
-- @
-- MkComment () " add four to the value of x"
-- @
--
-- with the hash being inferred, and the space after the hash being preserved.
--
-- Python does not have multi-line comments. There is a common convention of
-- using a multi-line string expression as a multi-line comment, since a
-- string expression is a no-op statement. Such multi-line comments are
-- __NOT__ represented with this data type, but rather as normal
-- string expressions (since that's what they are).
data Comment a
  = MkComment
  { _commentAnn :: a
  , _commentValue :: String
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''Comment
deriveOrd1 ''Comment
