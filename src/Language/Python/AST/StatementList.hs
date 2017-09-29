{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
module Language.Python.AST.StatementList
  ( IndentedLines
  , IndentationError(..)
  )
  where

import Papa hiding (cons, snoc)
import qualified Papa as P (cons)
import GHC.Natural

import Language.Python.AST.Symbols

data Nested a
  = Unnested a
  | Nested (IndentedLines a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype IndentedLines a = IndentedLines [([IndentationChar], Nested a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | This traversal does not preserve the invariant that I want
nestedIndentations_ :: Traversal' (Nested a) [IndentationChar]
nestedIndentations_ f a = pure a
nestedIndentations_ f (Nested is) = Nested <$> indentations_ f is

-- | This traversal does not preserve the invariant that I want
indentations_ :: Traversal' (IndentedLines a) [IndentationChar]
indentations_ f (IndentedLines t) =
  IndentedLines <$>
  traverse (\(a, b) -> (,) <$> f a <*> traverseOf nestedIndentations_ f b) t

data IndentationError
  = TabsAndSpacesBadlyMixed
  | ExpectedLevel IndentLevel Natural

data IndentLevel
  = GEq Natural
  | Eq Natural

indentLevel :: [IndentationChar] -> Natural
indentLevel = go 0
  where
    go count [] = count
    go count (IndentContinued _ _ : _) = count
    go count (IndentTab : rest) = go (count + max 1 ((8 - (count `mod` 8)) `mod` 8)) rest
    go count (IndentSpace : rest) = go (count + 1) rest

minIndentLevel :: [IndentationChar] -> Natural
minIndentLevel = go 0
  where
    go count [] = count
    go count (IndentContinued _ _ : _) = count
    go count (IndentTab : rest) = go (count + 1) rest
    go count (IndentSpace : rest) = go (count + 1) rest

maxIndentLevel :: [IndentationChar] -> Natural
maxIndentLevel = go 0
  where
    go count [] = count
    go count (IndentContinued _ _ : _) = count
    go count (IndentTab : rest) = go (count + 8) rest
    go count (IndentSpace : rest) = go (count + 1) rest

-- | If this throws an overflow exception, it's because the result of dropping would
-- begin 'inside' an expanded tab
dropIndentation :: Natural -> [IndentationChar] -> [IndentationChar]
dropIndentation n = go n
  where
    go 0 res = res
    go _ [] = []
    go target (IndentSpace : rest) = go (target-1) rest
    go target (IndentTab : rest) =
      let amount = max 1 ((8 - ((n - target) `mod` 8)) `mod` 8)
      in go (target-amount) rest
    go _ (IndentContinued _ _ : rest) = rest
