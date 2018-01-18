{-# language DataKinds #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
module Language.Python.Printer.ArgumentList where

import Prelude (error)
import Papa hiding (argument)
import Text.PrettyPrint hiding ((<>), comma, equals)

import Language.Python.AST.ArgumentList
import Language.Python.AST.IsArgList hiding (Argument)
import Language.Python.IR.ExprConfig
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols

argument
  :: HasName name
  => (val a -> Doc)
  -> (name a -> Doc)
  -> (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> Argument val name expr dctxt a
  -> Doc
argument _val _name _expr a =
  case a of
    ArgumentPositional a _ -> _val a
    ArgumentKeyword a b c _ ->
      _name a <>
      between' (foldMap anyWhitespaceChar) equals b <>
      _expr anyWhitespaceChar c
    ArgumentStar a b _ ->
      after (foldMap anyWhitespaceChar) asterisk a <>
      _val b
    ArgumentDoublestar a b _ ->
      after (foldMap anyWhitespaceChar) doubleAsterisk a <>
      _val b

argumentList
  :: HasName name
  => (val a -> Doc)
  -> (name a -> Doc)
  -> (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> ArgumentList val name expr 'NotAssignable dctxt a
  -> Doc
argumentList _val _name _expr e =
  Just e &
    (outside _ArgumentList .~
      (\(a, b, c, d, _) ->
         argument _val _name _expr a <>
         foldMapOf
           (_Wrapped.folded._Wrapped)
           (before (between' (foldMap anyWhitespaceChar) comma) (argument _val _name _expr))
           b <>
         foldMap anyWhitespaceChar c <>
         foldMap (after (foldMap anyWhitespaceChar) comma) d) $
     error "incomplete pattern")
