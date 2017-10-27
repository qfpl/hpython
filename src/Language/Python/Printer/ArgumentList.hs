{-# language DataKinds #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
module Language.Python.Printer.ArgumentList where

import Prelude (error)
import Papa
import Text.PrettyPrint hiding ((<>), comma)

import Language.Python.AST.ArgumentList
import Language.Python.IR.ExprConfig
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols

keywordItem
  :: (forall x. name x -> Doc)
  -> (forall as' dctxt' x. expr as' dctxt' x -> Doc)
  -> KeywordItem name expr as dctxt a
  -> Doc
keywordItem _name _expr (KeywordItem l r _) =
  whitespaceAfterF _name l <>
  text "=" <>
  whitespaceBeforeF _expr r

keywordsArguments
  :: (forall x. name x -> Doc)
  -> (forall as' dctxt' x. expr as' dctxt' x -> Doc)
  -> KeywordsArguments name expr as dctxt a
  -> Doc
keywordsArguments _name _expr (KeywordsArguments h t _) =
  sumElim
    (keywordItem _name _expr)
    (beforeF (betweenWhitespace' doubleAsterisk) _expr)
    h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF
      (betweenWhitespace' comma)
      (sumElim
        (keywordItem _name _expr)
        (beforeF (betweenWhitespace' doubleAsterisk) _expr)))
    t

positionalArguments
  :: (forall as' dctxt' x. expr as' dctxt' x -> Doc)
  -> PositionalArguments expr as dctxt a
  -> Doc
positionalArguments _expr (PositionalArguments h t _) =
  beforeF (foldMap $ betweenWhitespace' asterisk) _expr h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF
      (betweenWhitespace' comma)
      (beforeF (foldMap $ betweenWhitespace' asterisk) _expr))
    t

starredAndKeywords
  :: (forall x. name x -> Doc)
  -> (forall as' dctxt' x. expr as' dctxt' x -> Doc)
  -> StarredAndKeywords name expr as dctxt a
  -> Doc
starredAndKeywords _name _expr (StarredAndKeywords h t _) =
  sumElim
    (beforeF (betweenWhitespace' asterisk) _expr)
    (keywordItem _name _expr)
    h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF
      (betweenWhitespace' comma)
      (sumElim
        (beforeF (betweenWhitespace' asterisk) _expr)
        (keywordItem _name _expr)))
    t

argumentList
  :: (forall x. name x -> Doc)
  -> (forall as' dctxt' x. expr as' dctxt' x -> Doc)
  -> ArgumentList name expr 'NotAssignable dctxt a
  -> Doc
argumentList _name _expr e =
  Just e &
    (outside _ArgumentListAll .~
      (\(a, b, c, d, _) ->
         positionalArguments _expr a <>
         foldMapOf
           (_Wrapped.folded)
           (beforeF (betweenWhitespace' comma) (starredAndKeywords _name _expr))
           b <>
         foldMapOf
           (_Wrapped.folded)
           (beforeF (betweenWhitespace' comma) (keywordsArguments _name _expr))
           c <>
         foldMap (betweenWhitespace' comma) d) $
     outside _ArgumentListUnpacking .~
      (\(a, b, c, _) ->
         starredAndKeywords _name _expr a <>
         foldMapOf
           (_Wrapped.folded)
           (beforeF (betweenWhitespace' comma) (keywordsArguments _name _expr))
           b <>
         foldMap (betweenWhitespace' comma) c) $
     outside _ArgumentListKeywords .~
      (\(a, b, _) ->
         keywordsArguments _name _expr a <>
         foldMap (betweenWhitespace' comma) b) $
     error "incomplete pattern")
