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
module Language.Python.Internal.Syntax.Strings.Raw
  ( LongRawString, _LongRawString
  , ShortRawString, _ShortRawString
  )
where

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

newtype LongRawString s = LongRawString (RawString s)
  deriving (Eq, Show)

instance (AsEmpty s, Cons s s Char Char, Semigroup s, Monoid s) => Monoid (LongRawString s) where
  mempty = LongRawString mempty
  mappend (LongRawString a) (LongRawString b) = LongRawString $ mappend a b

_LongRawString :: (AsEmpty s, Cons s s Char Char) => Prism' s (LongRawString s)
_LongRawString = prism' (\(LongRawString (RawString s _)) -> s) toRaw
  where
    toRaw whole = go whole 0
      where
        go a !n =
          case uncons a of
            Just (c, rest) ->
              case c of
                '\\' -> go rest (n+1)
                '\0' -> Nothing
                _ -> go rest 0
            Nothing ->
              let (q, r) = quotRem n 2
              in
                if r == 0
                then Just . LongRawString $ RawString whole q
                else Nothing

newtype ShortRawString s = ShortRawString (RawString s)
  deriving (Eq, Show)

instance (AsEmpty s, Cons s s Char Char, Semigroup s, Monoid s) => Monoid (ShortRawString s) where
  mempty = ShortRawString mempty
  mappend (ShortRawString a) (ShortRawString b) = ShortRawString $ mappend a b

_ShortRawString :: (AsEmpty s, Cons s s Char Char) => Prism' s (ShortRawString s)
_ShortRawString = prism' (\(ShortRawString (RawString s _)) -> s) toRaw
  where
    toRaw whole = go whole 0
      where
        go a !n =
          case uncons a of
            Just (c, rest) ->
              case c of
                '\\' -> go rest (n+1)
                '\n' -> Nothing
                '\r' -> Nothing
                '\0' -> Nothing
                _ -> go rest 0
            Nothing ->
              let (q, r) = quotRem n 2
              in
                if r == 0
                then Just . ShortRawString $ RawString whole q
                else Nothing
