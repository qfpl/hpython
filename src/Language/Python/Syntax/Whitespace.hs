{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}
{-# language TypeFamilies #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Whitespace
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.Whitespace
  ( Newline(..)
  , Whitespace(..)
  , Blank(..)
  , HasTrailingWhitespace(..)
  , HasTrailingNewline(..)
    -- * Indentation
  , IndentLevel, getIndentLevel, indentLevel, absoluteIndentLevel
  , Indent(..), indentWhitespaces, indentIt, dedentIt
  , Indents(..), indentsValue, indentsAnn, subtractStart
  )
where

import Control.Lens.Iso (Iso', iso, from)
import Control.Lens.Getter ((^.), view)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (Traversal')
import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.FingerTree (FingerTree, Measured(..), fromList)
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Monoid, Endo(..), Dual(..))
import Data.Semigroup (Semigroup, (<>))
import GHC.Exts (IsList(..))

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Syntax.Comment (Comment)

-- | A newline is either a carriage return, a line feed, or a carriage return
-- followed by a line feed.
data Newline = CR | LF | CRLF deriving (Eq, Ord, Show)

-- | Whitespace is either a space, a tab, a newline that continues the
-- logical line ('Continued'), a newline that ends the logical line ('Newline'),
-- or a 'Comment'.
--
-- Despite not literally being whitespace, comments inside enclosed forms
-- are treated as whitespace. See <https://docs.python.org/3.5/reference/lexical_analysis.html#implicit-line-joining>
--
-- Example and counterexample of comments as whitespace
--
-- @
--( 1 +
--  # here's a comment
-- 2 +
-- 3 # another comment
--)
-- @
--
-- @
-- x = 5 + 5
-- # this line is not considered whitespace
-- y = x * 2
-- @
--
-- @
-- [ 1
-- , 2 # I'm whitespace
-- , 3
-- # also whitespace
-- ]
-- @
data Whitespace
  = Space
  | Tab
  | Continued Newline [Whitespace]
  | Newline Newline
  | Comment (Comment ())
  deriving (Eq, Ord, Show)

-- | An important convention in hpython that helps unambiguously track
-- whitespace in the syntax tree is for each syntactic element to own the
-- whitespace which immediately follows it. They do /not/ own the whitespace
-- which precedes them, as this would cause ambiguity.
--
-- This type class lets us access this trailing whitespace in many different
-- types throughout hpython.
class HasTrailingWhitespace s where
  trailingWhitespace :: Lens' s [Whitespace]

instance HasTrailingWhitespace a => HasTrailingWhitespace (NonEmpty a) where
  trailingWhitespace =
    lens
      (view trailingWhitespace . NonEmpty.last)
      (\(x :| xs) ws ->
         case xs of
           [] -> (x & trailingWhitespace .~ ws) :| xs
           x' : xs' -> NonEmpty.cons x $ (x' :| xs') & trailingWhitespace .~ ws)

-- | A newline that may be following a statement-containing thing
--
-- Some forms /always/ have a trailing newline, which is why this class isn't just
-- @trailingNewline :: 'Lens'' (s v a) ('Maybe' 'Newline')@
class HasTrailingNewline (s :: [*] -> * -> *) where
  trailingNewline :: Traversal' (s v a) Newline
  setTrailingNewline :: s v a -> Newline -> s v a

-- | Lines which are "blank", meaning that they contain, if anything, only
-- whitespace and/or a comment.
data Blank a
  = Blank
  { _blankAnn :: a
  , _blankWhitespaces :: [Whitespace]
  , _blankComment :: Maybe (Comment a)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Python has rules regarding the expansion of tabs into spaces and how to
-- go about computing indentation after this is done.
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#indentation>
--
-- This data structure implements those rules as a monoid.
newtype IndentLevel
  = IndentLevel
  { appIndentLevel
    :: Maybe Int -> Dual (Endo (Bool, Int))
  }
  deriving (Semigroup, Monoid)

indentLevel :: Indent -> Int
indentLevel = getIndentLevel . measure . unIndent

getIndentLevel :: IndentLevel -> Int
getIndentLevel il =
  snd $
  appEndo (getDual (appIndentLevel il Nothing)) (False, 0)

absoluteIndentLevel :: Int -> Indent -> Int
absoluteIndentLevel n il =
  snd $
  appEndo (getDual (appIndentLevel (measure $ unIndent il) $ Just n)) (False, 0)

instance Measured IndentLevel Whitespace where
  measure e =
    IndentLevel $
    \absolute -> Dual . Endo $
    \(b, !i) ->
    case e of
      Space -> (b, if b then i else i+1)
      Tab -> (b, if b then i else maybe (i + 8 - rem i 8) (+i) absolute)
      Continued{} -> (True, i)
      Newline{} -> error "Newline does not have an IndentLevel"
      Comment{} -> error "Comment does not have an IndentLevel"

newtype Indent
  = MkIndent
  { unIndent :: FingerTree IndentLevel Whitespace
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

instance IsList Indent where
  type Item Indent = Whitespace
  toList = view indentWhitespaces
  fromList = view $ from indentWhitespaces

-- | Indent some indentation by a chunk
indentIt :: [Whitespace] -> Indents a -> Indents a
indentIt ws (Indents a b) = Indents (ws ^. from indentWhitespaces : a) b

-- | Deent some indentation by a chunk
dedentIt :: Indents a -> Indents a
dedentIt i@(Indents [] _) = i
dedentIt (Indents (_:b) c) = Indents b c

-- | An 'Indent' is isomorphic to a list of 'Whitespace'
indentWhitespaces :: Iso' Indent [Whitespace]
indentWhitespaces =
  iso (Data.Foldable.toList . unIndent) (MkIndent . Data.FingerTree.fromList)

-- | Subtract the first argument from the beginning of the second
--
-- Returns 'Nothing' if the first list is not a prefix of the second.
subtractStart :: Indents a -> Indents a -> Maybe (Indents a)
subtractStart (Indents a _) (Indents b c) = Indents <$> stripPrefix a b <*> pure c

-- | A possibly annotated list of 'Indent's.
data Indents a
  = Indents
  { _indentsValue :: [Indent]
  , _indentsAnn :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (Indents a) where
  Indents a b <> Indents c d = Indents (a <> c) (b <> d)

makeLenses ''Indents
deriveEq1 ''Indents
deriveOrd1 ''Indents
