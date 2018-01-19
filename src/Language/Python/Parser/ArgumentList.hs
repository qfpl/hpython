{-# language RankNTypes #-}
module Language.Python.Parser.ArgumentList where

import Papa hiding (argument)

import Text.Trifecta hiding (Unspaced(..), comma)

import Language.Python.AST.Symbols
import Language.Python.IR.ArgumentList
import Language.Python.Parser.Combinators
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols

import Text.Parser.Unspaced

argument
  :: ( Functor arg
     , Functor name
     , Functor val
     , Functor (expr AnyWhitespaceChar)
     , DeltaParsing m
     )
  => Unspaced m (arg SrcInfo)
  -> Unspaced m (val SrcInfo)
  -> Unspaced m (name SrcInfo)
  -> (forall ws. Unspaced m ws -> Unspaced m (expr ws SrcInfo))
  -> Unspaced m (Argument arg val name expr SrcInfo)
argument _arg _val _name _expr =
  annotated $
  argKey <|>
  argPos <|>
  argDoublestar <|>
  argStar
  where
    argPos =
      ArgumentPositional <$> _arg
    argKey =
      try
        (ArgumentKeyword <$>
         _name <*>
         between' (many anyWhitespaceChar) equals) <*>
      _expr anyWhitespaceChar
    argStar =
      ArgumentStar <$>
      after (many anyWhitespaceChar) asterisk <*>
      _val
    argDoublestar =
      ArgumentDoublestar <$>
      try (after (many anyWhitespaceChar) doubleAsterisk) <*>
      _val

argumentList
  :: ( Functor arg
     , Functor name
     , Functor val
     , Functor (expr AnyWhitespaceChar)
     , DeltaParsing m
     )
  => Unspaced m (arg SrcInfo)
  -> Unspaced m (val SrcInfo)
  -> Unspaced m (name SrcInfo)
  -> (forall ws. Unspaced m ws -> Unspaced m (expr ws SrcInfo))
  -> Unspaced m (ArgumentList arg val name expr SrcInfo)
argumentList _arg _val _name _expr =
  annotated $
  ArgumentList <$>
  argument _arg _val _name _expr <*>
  manyF
    (beforeF
       (try $ betweenAnyWhitespace comma <* notFollowedBy (char ')'))
       (argument _arg _val _name _expr)) <*>
  many anyWhitespaceChar <*>
  optional (anyWhitespaceAfter comma)
