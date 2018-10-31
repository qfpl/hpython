{-# language BangPatterns #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Internal.Render.Correction
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

There are configurations of the core syntax tree which won't print to valid Python
if we printed them naively. Many of these we catch in the
'Language.Python.Validation.Syntax' phase, because those mistakes correspond to
some Python syntax error. In other cases, the mistakes are more benign and have
a "resonable correction" which doesn't break the "print-parse idempotence" law.

This module is where such corrections are defined
-}

module Language.Python.Internal.Render.Correction where

import Control.Lens.Fold (hasn't)
import Control.Lens.Getter ((^.))
import Control.Lens.Plated (transform)
import Control.Lens.Setter ((.~))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Numbers
import Language.Python.Internal.Syntax.Strings
import Language.Python.Internal.Token
import Language.Python.Syntax.Whitespace

correctSpaces :: (PyToken () -> Text) -> [PyToken ()] -> [PyToken ()]
correctSpaces f =
  transform $
  \case
    a : b : rest
      | isIdentifierChar (Text.last $ f a)
      , isIdentifierChar (Text.head $ f b)
      -> a : TkSpace () : b : rest
    a@(TkFloat (FloatLiteralFull _ _ Nothing)) : b : rest
      | isIdentifierChar (Text.head $ f b) -> a : TkSpace () : b : rest
    a -> a

correctNewlines :: [PyToken ()] -> [PyToken ()]
correctNewlines =
  transform $
  \case
    TkNewline CR () : TkNewline LF () : rest ->
      TkNewline CRLF () : TkNewline LF () : rest
    TkContinued CR () : TkNewline LF () : rest ->
      TkContinued CRLF () : TkNewline LF () : rest
    a -> a

-- |
-- Two non-typed single-quoted strings cannot be lexically
-- adjacent, because this would be a parse error
--
-- eg. '''' or """"
--
-- we correct for this by inserting a single space where required
-- '' '' or "" ""
correctAdjacentStrings :: NonEmpty (StringLiteral a) -> NonEmpty (StringLiteral a)
correctAdjacentStrings (a :| []) = a :| []
correctAdjacentStrings (a:|b:cs) =
  if
    _stringLiteralQuoteType a == _stringLiteralQuoteType b &&
    _stringLiteralStringType a == _stringLiteralStringType b &&
    null (a ^. trailingWhitespace) &&
    not (hasStringPrefix b)
  then
    NonEmpty.cons (a & trailingWhitespace .~ [Space]) (correctAdjacentStrings $ b :| cs)
  else
    NonEmpty.cons a (correctAdjacentStrings $ b :| cs)

quoteChar :: QuoteType -> PyChar
quoteChar qt =
  case qt of
    SingleQuote -> Char_esc_singlequote
    DoubleQuote -> Char_esc_doublequote

quote :: QuoteType -> Char
quote qt =
  case qt of
    DoubleQuote -> '\"'
    SingleQuote -> '\''

-- | When a backslash character, precedes an escape sequence it needs to be escaped
-- so that it doesn't interfere with the backslash that begins the escape sequence.
--
-- For example:
--
-- @['Char_lit' \'\\\\\', Char_esc_n]@ would naively render to \'\\\\n\', which
-- would parse to @['Char_esc_bslash', 'Char_lit' \'n\']@, breaking the
-- @parse . print@ identity
correctBackslashEscapes :: [PyChar] -> [PyChar]
correctBackslashEscapes [] = []
correctBackslashEscapes [x] = [x]
correctBackslashEscapes (x:y:ys) =
  case x of
    Char_lit '\\'
      -- if the next character is an escape sequence, then the current backslash
      -- must be escaped
      | isEscape y -> Char_esc_bslash : y : correctBackslashEscapes ys
      | Char_lit c <- y ->
        case c of
          '\\' -> Char_esc_bslash : correctBackslashEscapes ys
          '\'' -> Char_esc_bslash : correctBackslashEscapes ys
          '\"' -> Char_esc_bslash : correctBackslashEscapes ys
          -- if we print out ['\', 'u'] then the parser will think it's beginning a
          -- unicode point
          'u' -> Char_esc_bslash : y : correctBackslashEscapes ys
          'U' -> Char_esc_bslash : y : correctBackslashEscapes ys
          -- same for 'x' and hex values
          'x' -> Char_esc_bslash : y : correctBackslashEscapes ys
          _ -> x : correctBackslashEscapes (y : ys)
    _ -> x : correctBackslashEscapes (y : ys)

correctBackslashes :: [PyChar] -> [PyChar]
correctBackslashes [] = []
correctBackslashes [x] =
  case x of
    Char_lit '\\' -> [Char_esc_bslash]
    _ -> [x]
correctBackslashes (x:y:ys) =
  case x of
    Char_lit '\\'
      -- if the next character is an escape sequence, then the current backslash
      -- must be escaped
      | Char_esc_bslash <- y -> Char_esc_bslash : y : correctBackslashes ys
    _ -> x : correctBackslashes (y : ys)

-- | @(as, bs) = span p xs@
-- @bs@ is the longest suffix that satisfies the predicate, and @as@ is the
-- prefix up to that point
--
-- It's like the reverse of 'span'
naps :: (a -> Maybe b) -> [a] -> ([a], [b])
naps p = go (,) (,)
  where
    go _ r [] = r [] []
    go l r (x:xs) =
      go
        (\res res' -> l (x:res) res')
        (\res res' ->
           case p x of
             Just x' -> r res (x':res')
             Nothing -> l (x:res) res')
        xs

correctBackslashEscapesRaw :: [PyChar] -> [PyChar]
correctBackslashEscapesRaw [] = []
correctBackslashEscapesRaw [x] = [x]
correctBackslashEscapesRaw (x:y:ys) =
  case x of
    Char_lit '\\' ->
      case y of
        Char_esc_doublequote -> Char_esc_bslash : y : correctBackslashEscapesRaw ys
        Char_esc_singlequote -> Char_esc_bslash : y : correctBackslashEscapesRaw ys
        _ -> x : correctBackslashEscapesRaw (y : ys)
    _ -> x : correctBackslashEscapesRaw (y : ys)

correctBackslashesRaw :: [PyChar] -> [PyChar]
correctBackslashesRaw ps =
  let
    (as, bs) =
      naps
        (\a ->
           case a of
             Char_lit '\\' -> Just a
             Char_esc_bslash -> Just a
             _ -> Nothing)
        ps
  in
    if even (numSlashes bs)
    then ps
    else
      as <> (Char_lit '\\' : bs)
  where
    numSlashes :: [PyChar] -> Int
    numSlashes [] = 0
    numSlashes (Char_lit '\\' : xs) = 1 + numSlashes xs
    numSlashes (Char_esc_bslash : xs) = 2 + numSlashes xs
    numSlashes _ = undefined

-- | Every quote in a string of a particular quote type should be escaped
correctQuotes :: QuoteType -> [PyChar] -> [PyChar]
correctQuotes qt =
  fmap
    (case qt of
       DoubleQuote -> \case; Char_lit '"' -> Char_esc_doublequote; c -> c
       SingleQuote -> \case; Char_lit '\'' -> Char_esc_singlequote; c -> c)

-- | Every quote in short raw string that isn't preceded by
-- a backslash should be escaped
correctQuotesRaw :: QuoteType -> [PyChar] -> [PyChar]
correctQuotesRaw _ [] = []
correctQuotesRaw qt [x] =
  case x of
    Char_lit c | quote qt == c -> [quoteChar qt]
    _ -> [x]
correctQuotesRaw qt (x:y:ys) =
  case x of
    Char_lit c | q == c -> go (qc:y:ys)
    _ -> go (x:y:ys)
  where
    qc = quoteChar qt
    q = quote qt

    go [] = []
    go [x] = [x]
    go (x:y:ys) =
      case x of
        Char_lit '\\' -> x : go (y:ys)
        _ ->
          case y of
            Char_lit c | q == c -> x : go (qc:ys)
            _ -> x : go (y:ys)

-- | Literal quotes at the beginning and end of a long (non-raw) string should be escaped
correctInitialFinalQuotesLong :: QuoteType -> [PyChar] -> [PyChar]
correctInitialFinalQuotesLong qt = correctFinalQuotes . correctInitialQuotes
  where
    qc = quoteChar qt
    q = quote qt

    -- | Literal quotes at the end of a long (non-raw) string should be escaped
    correctFinalQuotes :: [PyChar] -> [PyChar]
    correctFinalQuotes = snd . go
      where
        go [] = (True, [])
        go (c:cs) =
          case go cs of
            (b, cs') ->
              if b && c == Char_lit q
              then (True, qc : cs')
              else (False, c : cs')

    -- | Every third literal quote at the beginning of a long (non-raw) string should
    -- be escaped
    correctInitialQuotes :: [PyChar] -> [PyChar]
    correctInitialQuotes = go (0::Int)
      where
        go !_ [] = []
        go !n (c:cs) =
          if c == Char_lit q
          then
            if n == 2
            then qc : go (n+1 `mod` 3) cs
            else c : go (n+1 `mod` 3) cs
          else c : cs

-- | It's possible that successive statements have no newlines in between
-- them. This would cause them to be displayed on the same line. In every line where
-- this would be the case, we explicitly insert a line-feed character.
correctTrailingNewline :: HasTrailingNewline s => Bool -> s v a -> s v a
correctTrailingNewline False s =
  if hasn't trailingNewline s
  then setTrailingNewline s LF
  else s
correctTrailingNewline True s = s
