{-# language GADTs #-}
{-# language RankNTypes #-}
module Language.Python.Printer.ArgumentList where

import Text.PrettyPrint
import Language.Python.AST.ArgumentList

keywordItem
  :: (forall x. name x -> Doc)
  -> (forall as dctxt x. expr as dctxt x -> Doc)
  -> KeywordItem name expr as dctxt a
  -> Doc
keywordItem _name _expr (KeywordItem l r _) = _

keywordsArguments
  :: (forall x. name x -> Doc)
  -> (forall as dctxt x. expr as dctxt x -> Doc)
  -> KeywordsArguments name expr as dctxt a
  -> Doc
keywordsArguments _name _expr (KeywordsArguments h t _) = _

positionalArguments
  :: (forall as dctxt x. expr as dctxt x -> Doc)
  -> PositionalArguments expr as dctxt a
  -> Doc
positionalArguments _expr (PositionalArguments h t _) = _

starredAndKeywords
  :: (forall x. name x -> Doc)
  -> (forall as dctxt x. expr as dctxt x -> Doc)
  -> StarredAndKeywords name expr as dctxt a
  -> Doc
starredAndKeywords _name _expr (StarredAndKeywords h t _) = _

argumentList
  :: (forall x. name x -> Doc)
  -> (forall as dctxt x. expr as dctxt x -> Doc)
  -> ArgumentList name expr as dctxt a
  -> Doc
argumentList _name _expr a = _
