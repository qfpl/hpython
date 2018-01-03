{-# language DataKinds #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
module Language.Python.Printer.ArgumentList where

import Prelude (error)
import Papa
import Text.PrettyPrint hiding ((<>), comma)

import Language.Python.AST.ArgumentList
import Language.Python.AST.IsArgList
import Language.Python.IR.ExprConfig
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols

keywordItem
  :: (name a -> Doc)
  -> (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> KeywordItem name expr as dctxt a
  -> Doc
keywordItem _name _expr (KeywordItem l r _) =
  anyWhitespaceAfterF _name l <>
  text "=" <>
  anyWhitespaceBeforeF (_expr anyWhitespaceChar) r

keywordsArguments
  :: (name a -> Doc)
  -> (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> KeywordsArguments name expr as dctxt a
  -> Doc
keywordsArguments _name _expr (KeywordsArguments h t _) =
  sumElim
    (keywordItem _name _expr)
    (beforeF (betweenAnyWhitespace' doubleAsterisk) (_expr anyWhitespaceChar))
    h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF
      (betweenAnyWhitespace' comma)
      (sumElim
        (keywordItem _name _expr)
        (beforeF (betweenAnyWhitespace' doubleAsterisk) (_expr anyWhitespaceChar))))
    t

positionalArguments
  :: (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> PositionalArguments expr as dctxt a
  -> Doc
positionalArguments _expr (PositionalArguments h t _) =
  beforeF (foldMap $ betweenAnyWhitespace' asterisk) (_expr anyWhitespaceChar) h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF
      (betweenAnyWhitespace' comma)
      (beforeF (foldMap $ betweenAnyWhitespace' asterisk) (_expr anyWhitespaceChar)))
    t

starredAndKeywords
  :: (name a -> Doc)
  -> (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> StarredAndKeywords name expr as dctxt a
  -> Doc
starredAndKeywords _name _expr (StarredAndKeywords h t _) =
  sumElim
    (beforeF (betweenAnyWhitespace' asterisk) (_expr anyWhitespaceChar))
    (keywordItem _name _expr)
    h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF
      (betweenAnyWhitespace' comma)
      (sumElim
        (beforeF (betweenAnyWhitespace' asterisk) (_expr anyWhitespaceChar))
        (keywordItem _name _expr)))
    t

argumentList
  :: HasName name
  => (name a -> Doc)
  -> (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> ArgumentList name expr 'NotAssignable dctxt a
  -> Doc
argumentList _name _expr e =
  Just e &
    (outside _ArgumentListAll .~
      (\(a, b, c, d, _) ->
         positionalArguments _expr a <>
         foldMapOf
           (_Wrapped.folded)
           (beforeF (betweenAnyWhitespace' comma) (starredAndKeywords _name _expr))
           b <>
         foldMapOf
           (_Wrapped.folded)
           (beforeF (betweenAnyWhitespace' comma) (keywordsArguments _name _expr))
           c <>
         foldMap (betweenAnyWhitespace' comma) d) $
     outside _ArgumentListUnpacking .~
      (\(a, b, c, _) ->
         starredAndKeywords _name _expr a <>
         foldMapOf
           (_Wrapped.folded)
           (beforeF (betweenAnyWhitespace' comma) (keywordsArguments _name _expr))
           b <>
         foldMap (betweenAnyWhitespace' comma) c) $
     outside _ArgumentListKeywords .~
      (\(a, b, _) ->
         keywordsArguments _name _expr a <>
         foldMap (betweenAnyWhitespace' comma) b) $
     error "incomplete pattern")
