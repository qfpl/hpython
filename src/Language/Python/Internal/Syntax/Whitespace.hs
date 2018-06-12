{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}
{-# language TypeFamilies #-}
module Language.Python.Internal.Syntax.Whitespace
  ( Newline(..)
  , Whitespace(..)
  , lastWhitespace
  , IndentLevel, getIndentLevel, indentLevel, absoluteIndentLevel
  , Indent(..), indentWhitespaces
  )
where

import Control.Lens.Iso (Iso', iso, from)
import Control.Lens.Getter (view)
import Data.Foldable (toList)
import Data.FingerTree (FingerTree, Measured(..), fromList)
import Data.Monoid (Monoid, Endo(..), Dual(..))
import Data.Semigroup (Semigroup)
import GHC.Exts (IsList(..))

data Newline
  = CR
  | LF
  | CRLF
  deriving (Eq, Show)

data Whitespace
  = Space
  | Tab
  | Continued Newline [Whitespace]
  | Newline Newline
  deriving (Eq, Show)

lastWhitespace :: [Whitespace] -> Maybe Whitespace
lastWhitespace [] = Nothing
lastWhitespace (w : ws) =
  case ws of
    [] ->
      case w of
        Continued nl [] -> Just $ Newline nl
        Continued _ ws' -> lastWhitespace ws'
        _ -> Just w
    _ -> lastWhitespace ws

newtype IndentLevel
  = IndentLevel
  { appIndentLevel
    :: Maybe Int -> Dual (Endo (Bool, Int))
  }
  deriving Monoid

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
