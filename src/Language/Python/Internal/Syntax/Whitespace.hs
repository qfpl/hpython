{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}
{-# language TypeFamilies #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Syntax.Whitespace
  ( Newline(..)
  , Whitespace(..)
  , HasTrailingWhitespace(..)
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
import Data.Foldable (toList)
import Data.Function ((&))
import Data.FingerTree (FingerTree, Measured(..), fromList)
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Monoid, Endo(..), Dual(..))
import Data.Semigroup (Semigroup)
import GHC.Exts (IsList(..))

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax.Comment

data Newline
  = CR { _commentBefore :: Maybe Comment }
  | LF { _commentBefore :: Maybe Comment }
  | CRLF { _commentBefore :: Maybe Comment }
  deriving (Eq, Show)

data Whitespace
  = Space
  | Tab
  | Continued Newline [Whitespace]
  | Newline Newline
  deriving (Eq, Show)

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

newtype Indent
  = MkIndent
  { unIndent :: FingerTree IndentLevel Whitespace
  } deriving (Eq, Show, Semigroup, Monoid)

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
