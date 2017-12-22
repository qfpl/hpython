module Language.Python.Parser.ArgumentList where

import Papa

import Data.Functor.Sum
import Text.Trifecta hiding (Unspaced(..), comma)

import Language.Python.IR.ArgumentList
import Language.Python.Parser.Combinators
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols

import Text.Parser.Unspaced

keywordItem
  :: ( Functor name
     , Functor expr
     , DeltaParsing m
     )
  => Unspaced m (name SrcInfo)
  -> Unspaced m (expr SrcInfo)
  -> Unspaced m (KeywordItem name expr SrcInfo)
keywordItem _name _expr =
  annotated $
  KeywordItem <$>
  anyWhitespaceAfterF _name <*
  equals <*>
  anyWhitespaceBeforeF _expr

keywordsArguments
  :: ( Functor name
     , Functor expr
     , DeltaParsing m
     )
  => Unspaced m (name SrcInfo)
  -> Unspaced m (expr SrcInfo)
  -> Unspaced m (KeywordsArguments name expr SrcInfo)
keywordsArguments _name _expr =
  annotated $
  KeywordsArguments <$>
  keywordOrDoublestar <*>
  manyF (try $ beforeF (betweenAnyWhitespace comma) keywordOrDoublestar)
  where
    keywordOrDoublestar = 
      try (InL <$> keywordItem _name _expr) <|>
      (InR <$> beforeF (betweenAnyWhitespace doubleAsterisk) _expr)

positionalArguments
  :: ( Functor expr
     , DeltaParsing m
     )
  => Unspaced m (expr SrcInfo)
  -> Unspaced m (PositionalArguments expr SrcInfo)
positionalArguments _expr =
  annotated $
  PositionalArguments <$>
  beforeF
    (optional . try $ betweenAnyWhitespace asterisk)
    (_expr <* notFollowedBy (try $ many whitespaceChar *> char '=')) <*>
  manyF
    (try $
     beforeF
       (betweenAnyWhitespace comma)
       (beforeF (optional . try $ betweenAnyWhitespace asterisk) _expr))

starredAndKeywords
  :: ( Functor name
     , Functor expr
     , DeltaParsing m
     )
  => Unspaced m (name SrcInfo)
  -> Unspaced m (expr SrcInfo)
  -> Unspaced m (StarredAndKeywords name expr SrcInfo)
starredAndKeywords _name _expr =
  annotated $
  StarredAndKeywords <$>
  starOrKeyword <*>
  manyF (beforeF (betweenAnyWhitespace comma) starOrKeyword)
  where
    starOrKeyword =
      try (InL <$> beforeF (betweenAnyWhitespace asterisk) _expr) <|>
      (InR <$> keywordItem _name _expr)

argumentList
  :: ( Functor name
     , Functor expr
     , DeltaParsing m
     )
  => Unspaced m (name SrcInfo)
  -> Unspaced m (expr SrcInfo)
  -> Unspaced m (ArgumentList name expr SrcInfo)
argumentList _name _expr =
  try argumentListAll <|>
  try argumentListUnpacking <|>
  argumentListKeywords
  where
    argumentListAll =
      annotated $
      ArgumentListAll <$>
      positionalArguments _expr <*>
      optionalF
        (try $
         beforeF
           (betweenAnyWhitespace comma)
           (starredAndKeywords _name _expr)) <*>
      optionalF
        (try $
         beforeF
           (betweenAnyWhitespace comma)
           (keywordsArguments _name _expr)) <*>
      optional (try $ betweenAnyWhitespace comma)

    argumentListUnpacking =
      annotated $
      ArgumentListUnpacking <$>
      starredAndKeywords _name _expr <*>
      optionalF
        (try $
         beforeF (betweenAnyWhitespace comma) (keywordsArguments _name _expr)) <*>
      optional (try $ betweenAnyWhitespace comma)

    argumentListKeywords =
      annotated $
      ArgumentListKeywords <$>
      keywordsArguments _name _expr <*>
      optional (try $ betweenAnyWhitespace comma)
