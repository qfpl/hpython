{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language PolyKinds #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.IndentedLines
  ( IndentedLines
  , getIndentedLines
  , IndentationError(..)
  , mkIndentedLines
  , getIndentLevel
  , getMinIndentLevel
  , getMaxIndentLevel
  )
  where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Functor.Sum.Lens
import qualified Data.List.NonEmpty as NE
import Data.Separated.Before
import GHC.Natural

import Language.Python.AST.Symbols

newtype IndentedLines (comment :: * -> *) f a
  = IndentedLines
  { getIndentedLines
    :: Compose
         NonEmpty
         (Sum
            comment
            (Compose
              (Before (NonEmpty IndentationChar))
              f))
         a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 comment, Eq1 f, Eq a) => Eq (IndentedLines comment f a)
deriving instance (Show1 comment, Show1 f, Show a) => Show (IndentedLines comment f a)
deriving instance (Ord1 comment, Ord1 f, Ord a) => Ord (IndentedLines comment f a)

getIndentLevel :: IndentedLines comment f a -> Maybe Natural
getIndentLevel (IndentedLines as) =
  as ^? _Wrapped.folded._InR._Wrapped.before._1.to indentLevel

getMinIndentLevel :: IndentedLines comment f a -> Maybe Natural
getMinIndentLevel (IndentedLines as) =
  as ^? _Wrapped.folded._InR._Wrapped.before._1.to minIndentLevel

getMaxIndentLevel :: IndentedLines comment f a -> Maybe Natural
getMaxIndentLevel (IndentedLines as) =
  as ^? _Wrapped.folded._InR._Wrapped.before._1.to maxIndentLevel

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

mkIndentedLines
  :: Compose
       NonEmpty
       (Sum
         comment
         (Compose
           (Before (NonEmpty IndentationChar))
           f))
       a
  -> Either
       (b -> IndentationError b)
       (IndentedLines comment f a)
mkIndentedLines ls =
  case ls ^? _Wrapped.folded._InR._Wrapped.before._1 of
    Nothing ->
      case NE.tail $ getCompose ls of
        [] -> Right $ IndentedLines ls
        a:as ->
          IndentedLines .
          over _Wrapped (NE.cons . NE.head $ getCompose ls) .
          getIndentedLines <$>
          mkIndentedLines (Compose $ a :| as)
    Just i ->
      let
        minLevel = minIndentLevel i
        maxLevel = maxIndentLevel i
        level = indentLevel i
      in
        IndentedLines <$>
        traverseOf
          (_Wrapped.traverse._InR._Wrapped.before)
          (validateIndentation level minLevel maxLevel)
          ls

deriveEq1 ''IndentedLines
deriveOrd1 ''IndentedLines
deriveShow1 ''IndentedLines

deriveEq1 ''IndentationError
deriveOrd1 ''IndentationError
deriveShow1 ''IndentationError
