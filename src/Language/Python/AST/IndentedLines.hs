{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language PolyKinds #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.IndentedLines
  ( IndentedLines
  , getIndentedLines
  , IndentationError(..)
  , Indentable(..)
  , mkIndentationLines
  , getIndentLevel
  , getMinIndentLevel
  , getMaxIndentLevel
  )
  where

import Papa
import Data.Deriving
import Data.Functor.Classes
import Data.Orphans.NonEmpty
import GHC.Natural

import Language.Python.AST.Symbols

newtype IndentedLines a
  = IndentedLines
  { getIndentedLines :: NonEmpty (NonEmpty IndentationChar, a)
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

getIndentLevel :: IndentedLines a -> Natural
getIndentLevel (IndentedLines ((i, _) :| _)) = indentLevel i

getMinIndentLevel :: IndentedLines a -> Natural
getMinIndentLevel (IndentedLines ((i, _) :| _)) = minIndentLevel i

getMaxIndentLevel :: IndentedLines a -> Natural
getMaxIndentLevel (IndentedLines ((i, _) :| _)) = maxIndentLevel i

data IndentationError a
  = TabError a
  | ExpectedLevel IndentLevel Natural a
  deriving (Functor, Foldable, Traversable, Eq, Show, Ord)

data IndentLevel = Eq | Gt
  deriving (Eq, Show, Ord)

indentLevel :: NonEmpty IndentationChar -> Natural
indentLevel (h:|t) = go 0 (h:t)
  where
    go count [] = count
    go count (IndentContinued _ _ : _) = count
    go count (IndentTab : rest) = go (count + max 1 (8 - (count `mod` 8))) rest
    go count (IndentSpace : rest) = go (count + 1) rest

minIndentLevel :: NonEmpty IndentationChar -> Natural
minIndentLevel (h:|t) = go 0 (h:t)
  where
    go count [] = count
    go count (IndentContinued _ _ : _) = count
    go count (IndentTab : rest) = go (count + 1) rest
    go count (IndentSpace : rest) = go (count + 1) rest

maxIndentLevel :: NonEmpty IndentationChar -> Natural
maxIndentLevel (h:|t) = go 0 (h:t)
  where
    go count [] = count
    go count (IndentContinued _ _ : _) = count
    go count (IndentTab : rest) = go (count + 8) rest
    go count (IndentSpace : rest) = go (count + 1) rest

class Indentable (s :: a -> b -> * -> *) (s' :: a -> b -> * -> *) | s -> s' where
  indentation :: Traversal' (s lctxt ctxt a) (IndentedLines (s' lctxt ctxt a))

validateSubIndentation
  :: Natural
  -> Natural
  -> Natural
  -> IndentedLines a
  -> Either (b -> IndentationError b) (IndentedLines a)
validateSubIndentation parentLevel parentMinLevel parentMaxLevel il =
  let
    minLevel = getMinIndentLevel il
    maxLevel = getMaxIndentLevel il
    level = getIndentLevel il
  in
    if (minLevel <= parentMinLevel) || (maxLevel <= parentMaxLevel)
    then Left TabError
    else
      if level <= parentLevel
      then Left $ ExpectedLevel Gt parentLevel
      else Right il

validateIndentation
  :: Natural
  -> Natural
  -> Natural
  -> (NonEmpty IndentationChar, a)
  -> Either (b -> IndentationError b) (NonEmpty IndentationChar, a)
validateIndentation desired desiredMinLevel desiredMaxLevel (i, a) = 
  let
    minLevel = minIndentLevel i
    maxLevel = maxIndentLevel i
    level = indentLevel i
  in
    if (minLevel /= desiredMinLevel) || (maxLevel /= desiredMaxLevel)
    then Left TabError
    else
      if level /= desired
      then Left $ ExpectedLevel Eq desired
      else Right (i, a)

mkIndentationLines
  :: NonEmpty (NonEmpty IndentationChar, s lctxt ctxt a)
  -> Either (b -> IndentationError b) (IndentedLines (s lctxt ctxt a))
mkIndentationLines ls@((i, _) :| _) =
  let
    minLevel = minIndentLevel i
    maxLevel = maxIndentLevel i
    level = indentLevel i
  in fmap IndentedLines
    (for ls $ validateIndentation level minLevel maxLevel) >>=
    validateSubIndentation level minLevel maxLevel

deriveEq1 ''IndentedLines
deriveShow1 ''IndentedLines

deriveEq1 ''IndentationError
deriveShow1 ''IndentationError
