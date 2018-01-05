{-# language RankNTypes #-}
module Language.Python.Parser.ArgumentList where

import Papa hiding (argument)

import Data.Functor.Sum
import Text.Trifecta hiding (Unspaced(..), comma)

import Language.Python.AST.Symbols
import Language.Python.IR.ArgumentList
import Language.Python.Parser.Combinators
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols

import Text.Parser.Unspaced

argument
  :: ( Functor name
     , Functor (expr AnyWhitespaceChar)
     , DeltaParsing m
     )
  => Unspaced m (name SrcInfo)
  -> (forall ws. Unspaced m ws -> Unspaced m (expr ws SrcInfo))
  -> Unspaced m (Argument name expr SrcInfo)
argument _name _expr =
  annotated $
  argPos <|>
  argKey <|>
  argStar <|>
  argDoublestar
  where
    argPos =
      try $
      ArgumentPositional <$>
        (_expr anyWhitespaceChar <* notFollowedBy (many anyWhitespaceChar *> equals))
    argKey =
      ArgumentKeyword <$>
      _name <*>
      between' (many anyWhitespaceChar) equals <*>
      _expr anyWhitespaceChar
    argStar =
      try $
      ArgumentStar <$>
      after (many anyWhitespaceChar) (asterisk <* notFollowedBy asterisk) <*>
      _expr anyWhitespaceChar
    argDoublestar =
      try $
      ArgumentDoublestar <$>
      after (many anyWhitespaceChar) doubleAsterisk <*>
      _expr anyWhitespaceChar

argumentList
  :: ( Functor name
     , Functor (expr AnyWhitespaceChar)
     , DeltaParsing m
     )
  => Unspaced m (name SrcInfo)
  -> (forall ws. Unspaced m ws -> Unspaced m (expr ws SrcInfo))
  -> Unspaced m (ArgumentList name expr SrcInfo)
argumentList _name _expr =
  annotated $
  ArgumentList <$>
  argument _name _expr <*>
  manyF
    (try $
     beforeF
       (betweenAnyWhitespace comma)
       (argument _name _expr)) <*>
  optional (anyWhitespaceBefore comma)
