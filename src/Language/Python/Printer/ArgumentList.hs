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
  => (name a -> Doc)
  -> (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> Argument name expr dctxt a
  -> Doc
argument _name _expr a =
  case a of
    ArgumentPositional a _ -> _expr anyWhitespaceChar a
    ArgumentKeyword a b c _ ->
      _name a <>
      between' (foldMap anyWhitespaceChar) equals b <>
      _expr anyWhitespaceChar c
    ArgumentStar a b _ ->
      after (foldMap anyWhitespaceChar) asterisk a <>
      _expr anyWhitespaceChar b
    ArgumentDoublestar a b _ ->
      after (foldMap anyWhitespaceChar) doubleAsterisk a <>
      _expr anyWhitespaceChar b

argumentList
  :: HasName name
  => (name a -> Doc)
  -> (forall as' ws' dctxt'. (ws' -> Doc) -> expr ws' as' dctxt' a -> Doc)
  -> ArgumentList name expr 'NotAssignable dctxt a
  -> Doc
argumentList _name _expr e =
  Just e &
    (outside _ArgumentList .~
      (\(a, b, c, d, _) ->
         argument _name _expr a <>
         foldMapOf
           (_Wrapped.folded._Wrapped)
           (before (between' (foldMap anyWhitespaceChar) comma) (argument _name _expr))
           b <>
         foldMap anyWhitespaceChar c <>
         foldMap (after (foldMap anyWhitespaceChar) comma) d) $
     error "incomplete pattern")
