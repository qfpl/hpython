{- |

This module provides a datatype for Python's "raw strings" - strings
that are prefixed with an 'r'. Raw strings are specified here:

https://docs.python.org/3.5/reference/lexical_analysis.html#string-and-bytes-literals

In spite of Python's noble intentions to treat backslashes as first-class citizens, the
lexer still occasionally lets its latent character-ism out.

In particular, a raw string cannot end in an odd number of backslashes.

@r"\\\\"@ means "a string consisting of two backslash characters", but there is no way to
represent "a string consisting of three backslash characters" using raw string syntax.

Thus, a 'RawString' is "a sequence of of 'Char's ending in an even number of
backslash characters"

-}

{-# language BangPatterns, FlexibleContexts, PatternSynonyms, UndecidableInstances #-}
module Language.Python.Internal.Syntax.Strings.Raw (RawString, _RawString) where

import Control.Lens.Cons (Cons, uncons)
import Control.Lens.Empty (AsEmpty, pattern Empty)
import Control.Lens.Prism (Prism', prism')
import Data.Semigroup (Semigroup(..))

data RawString s = RawString s !Int
  deriving (Eq, Show)

instance (AsEmpty s, Cons s s Char Char, Semigroup s) => Semigroup (RawString s) where
  RawString s n <> RawString s' n' | onlyNSlashes s' (2 * n') = RawString (s <> s') (n + n')
    where
      onlyNSlashes str !x =
        case uncons str of
          Nothing -> True
          Just (c, rest) ->
            case c of
              '\\' ->
                case x of
                  0 -> False
                  _ -> onlyNSlashes rest (x-1)
              _ -> False
  RawString s _ <> RawString s' n = RawString (s <> s') n

instance (AsEmpty s, Cons s s Char Char, Semigroup s, Monoid s) => Monoid (RawString s) where
  mempty = RawString Empty 0
  mappend = (<>)

_RawString :: (AsEmpty s, Cons s s Char Char) => Prism' s (RawString s)
_RawString = prism' (\(RawString s _) -> s) toRaw
  where
    toRaw whole = go whole 0
      where
        go a !n =
          case uncons a of
            Just (c, rest) ->
              case c of
                '\\' -> go rest (n+1)
                _ -> go rest 0
            Nothing ->
              let (q, r) = quotRem n 2
              in
                if r == 0
                then Just $ RawString whole q
                else Nothing
