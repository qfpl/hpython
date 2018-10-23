{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}
{-# language TypeFamilies #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Internal.Syntax.Whitespace
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.Whitespace
  ( Newline(..)
  , Whitespace(..)
  , Blank(..)
  , HasTrailingWhitespace(..)
  , HasTrailingNewline(..)
  , IndentLevel, getIndentLevel, indentLevel, absoluteIndentLevel
  , Indent(..), indentWhitespaces
  , Indents(..), indentsValue, indentsAnn, subtractStart
  )
where

import Control.Lens.Iso (Iso', iso, from)
import Control.Lens.Getter (view)
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
import Data.Semigroup (Semigroup)
import GHC.Exts (IsList(..))

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax.Comment (Comment)

data Newline = CR | LF | CRLF deriving (Eq, Ord, Show)

data Whitespace
  = Space
  | Tab
  | Continued Newline [Whitespace]
  | Newline Newline
  | Comment (Comment ())
  deriving (Eq, Ord, Show)

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
class HasTrailingNewline (s :: [*] -> * -> *) where
  trailingNewline :: Traversal' (s v a) Newline
  setTrailingNewline :: s v a -> Newline -> s v a

data Blank a
  = Blank
  { _blankAnn :: a
  , _blankWhitespaces :: [Whitespace]
  , _blankComment :: Maybe (Comment a)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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

indentWhitespaces :: Iso' Indent [Whitespace]
indentWhitespaces =
  iso (Data.Foldable.toList . unIndent) (MkIndent . Data.FingerTree.fromList)

-- ^ Subtract the first argument from the beginning of the second
subtractStart :: Indents a -> Indents a -> Maybe (Indents a)
subtractStart (Indents a _) (Indents b c) = Indents <$> stripPrefix a b <*> pure c

data Indents a
  = Indents
  { _indentsValue :: [Indent]
  , _indentsAnn :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''Indents
deriveEq1 ''Indents
deriveOrd1 ''Indents
