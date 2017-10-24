module Language.Python.Parser.ArgumentList where

import Text.Trifecta as P hiding (Unspaced(..))

import Language.Python.IR.ArgumentList

import Text.Parser.Unspaced

keywordItem
  :: DeltaParsing m
  => Unspaced m (name a)
  -> Unspaced m (expr a)
  -> Unspaced m (KeywordItem name expr a)
keywordItem _name _expr = _

keywordsArguments
  :: DeltaParsing m
  => Unspaced m (name a)
  -> Unspaced m (expr a)
  -> Unspaced m (KeywordsArguments name expr a)
keywordsArguments _name _expr = _

positionalArguments
  :: DeltaParsing m
  => Unspaced m (expr a)
  -> Unspaced m (PositionalArguments expr a)
positionalArguments _expr = _

starredAndKeywords
  :: DeltaParsing m
  => Unspaced m (name a)
  -> Unspaced m (expr a)
  -> Unspaced m (StarredAndKeywords name expr a)
starredAndKeywords _name _expr = _

argumentList
  :: DeltaParsing m
  => Unspaced m (name a)
  -> Unspaced m (expr a)
  -> Unspaced m (ArgumentList name expr a)
argumentList _name _expr = _
