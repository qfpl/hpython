{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.IR.ArgumentList where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Symbols

data KeywordItem name expr a
  = KeywordItem
  { _keywordItem_left :: Compose (After [WhitespaceChar]) name a
  , _keywordItem_right
    :: Compose
         (Before [WhitespaceChar])
         expr
         a
  , _keywordItem_ann :: a
  }
deriving instance (Eq1 name, Eq1 expr, Eq a) => Eq (KeywordItem name expr a)
deriving instance (Show1 name, Show1 expr, Show a) => Show (KeywordItem name expr a)
deriving instance (Ord1 name, Ord1 expr, Ord a) => Ord (KeywordItem name expr a)
deriving instance (Functor name, Functor expr) => Functor (KeywordItem name expr)
deriving instance (Foldable name, Foldable expr) => Foldable (KeywordItem name expr)
deriving instance (Traversable name, Traversable expr) => Traversable (KeywordItem name expr)

data KeywordsArguments name expr a
  = KeywordsArguments
  { _keywordsArguments_head
    :: Sum
         (KeywordItem name expr)
         (Compose
           (Before (Between' [WhitespaceChar] DoubleAsterisk))
           expr)
         a
  , _keywordsArguments_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum
             (KeywordItem name expr)
             (Compose
               (Before (Between' [WhitespaceChar] DoubleAsterisk))
               expr)))
         a
  , _keywordsArguments_ann :: a
  }
deriving instance (Eq1 name, Eq1 expr, Eq a) => Eq (KeywordsArguments name expr a)
deriving instance (Show1 name, Show1 expr, Show a) => Show (KeywordsArguments name expr a)
deriving instance (Ord1 name, Ord1 expr, Ord a) => Ord (KeywordsArguments name expr a)
deriving instance (Functor name, Functor expr) => Functor (KeywordsArguments name expr)
deriving instance (Foldable name, Foldable expr) => Foldable (KeywordsArguments name expr)
deriving instance (Traversable name, Traversable expr) => Traversable (KeywordsArguments name expr)

data PositionalArguments expr a
  = PositionalArguments
  { _positionalArguments_head
    :: Compose
        (Before (Maybe (Between' [WhitespaceChar] Asterisk)))
        expr
        a
  , _positionalArguments_tail
    :: Compose
        []
        (Compose
          (Before (Between' [WhitespaceChar] Comma))
          (Compose
            (Before (Maybe (Between' [WhitespaceChar] Asterisk)))
            expr))
        a
  , _positionalArguments_ann :: a
  }
deriving instance (Eq1 expr, Eq a) => Eq (PositionalArguments expr a)
deriving instance (Show1 expr, Show a) => Show (PositionalArguments expr a)
deriving instance (Ord1 expr, Ord a) => Ord (PositionalArguments expr a)
deriving instance Functor expr => Functor (PositionalArguments expr)
deriving instance Foldable expr => Foldable (PositionalArguments expr)
deriving instance Traversable expr => Traversable (PositionalArguments expr)

data StarredAndKeywords name expr a
  = StarredAndKeywords
  { _starredAndKeywords_head
    :: Sum
         (Compose
           (Before (Between' [WhitespaceChar] Asterisk))
           expr)
         (KeywordItem name expr)
         a 
  , _starredAndKeywords_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum
             (Compose
               (Before (Between' [WhitespaceChar] Asterisk))
                 expr)
             (KeywordItem name expr)))
         a
  , _starredAndKeywords_ann :: a
  }
deriving instance (Eq1 name, Eq1 expr, Eq a) => Eq (StarredAndKeywords name expr a)
deriving instance (Ord1 name, Ord1 expr, Ord a) => Ord (StarredAndKeywords name expr a)
deriving instance (Show1 name, Show1 expr, Show a) => Show (StarredAndKeywords name expr a)
deriving instance (Functor name, Functor expr) => Functor (StarredAndKeywords name expr)
deriving instance (Foldable name, Foldable expr) => Foldable (StarredAndKeywords name expr)
deriving instance (Traversable name, Traversable expr) => Traversable (StarredAndKeywords name expr)

data ArgumentList name expr a
  = ArgumentListAll
  { _argumentListAll_positionalArguments
    :: PositionalArguments expr a
  , _argumentListAll_starredAndKeywords
    :: Compose
        Maybe
        (Compose
          (Before (Between' [WhitespaceChar] Comma))
          (StarredAndKeywords name expr))
        a
  , _argumentListAll_keywords
    :: Compose
        Maybe
        (Compose
          (Before (Between' [WhitespaceChar] Comma))
          (KeywordsArguments name expr))
        a
  , _argumentList_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _argumentList_ann :: a
  }
  | ArgumentListUnpacking
  { _argumentListUnpacking_starredAndKeywords
    :: StarredAndKeywords name expr a
  , _argumentListUnpacking_keywords
    :: Compose
        Maybe
        (Compose
          (Before (Between' [WhitespaceChar] Comma))
          (KeywordsArguments name expr))
        a
  , _argumentList_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _argumentList_ann :: a 
  }
  | ArgumentListKeywords
  { _argumentListKeywords_keywords
    :: KeywordsArguments name expr a
  , _argumentList_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _argumentList_ann :: a 
  }
deriving instance (Eq1 name, Eq1 expr, Eq a) => Eq (ArgumentList name expr a)
deriving instance (Show1 name, Show1 expr, Show a) => Show (ArgumentList name expr a)
deriving instance (Ord1 name, Ord1 expr, Ord a) => Ord (ArgumentList name expr a)
deriving instance (Functor name, Functor expr) => Functor (ArgumentList name expr)
deriving instance (Foldable name, Foldable expr) => Foldable (ArgumentList name expr)
deriving instance (Traversable name, Traversable expr) => Traversable (ArgumentList name expr)

$(return [])

instance (Eq1 name, Eq1 expr) => Eq1 (ArgumentList name expr) where
  liftEq = $(makeLiftEq ''ArgumentList)

instance Eq1 expr => Eq1 (PositionalArguments expr) where
  liftEq = $(makeLiftEq ''PositionalArguments)

instance (Eq1 name, Eq1 expr) => Eq1 (StarredAndKeywords name expr) where
  liftEq = $(makeLiftEq ''StarredAndKeywords)

instance (Eq1 name, Eq1 expr) => Eq1 (KeywordsArguments name expr) where
  liftEq = $(makeLiftEq ''KeywordsArguments)

instance (Eq1 name, Eq1 expr) => Eq1 (KeywordItem name expr) where
  liftEq = $(makeLiftEq ''KeywordItem)

instance (Show1 name, Show1 expr) => Show1 (ArgumentList name expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''ArgumentList)

instance Show1 expr => Show1 (PositionalArguments expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''PositionalArguments)

instance (Show1 name, Show1 expr) => Show1 (StarredAndKeywords name expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''StarredAndKeywords)

instance (Show1 name, Show1 expr) => Show1 (KeywordsArguments name expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''KeywordsArguments)

instance (Show1 name, Show1 expr) => Show1 (KeywordItem name expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''KeywordItem)

instance (Ord1 name, Ord1 expr) => Ord1 (ArgumentList name expr) where
  liftCompare = $(makeLiftCompare ''ArgumentList)

instance Ord1 expr => Ord1 (PositionalArguments expr) where
  liftCompare = $(makeLiftCompare ''PositionalArguments)

instance (Ord1 name, Ord1 expr) => Ord1 (StarredAndKeywords name expr) where
  liftCompare = $(makeLiftCompare ''StarredAndKeywords)

instance (Ord1 name, Ord1 expr) => Ord1 (KeywordsArguments name expr) where
  liftCompare = $(makeLiftCompare ''KeywordsArguments)

instance (Ord1 name, Ord1 expr) => Ord1 (KeywordItem name expr) where
  liftCompare = $(makeLiftCompare ''KeywordItem)
