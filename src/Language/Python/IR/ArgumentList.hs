{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module Language.Python.IR.ArgumentList where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.IsArgList hiding (Argument)
import Language.Python.AST.Symbols

data Argument name expr a
  = ArgumentPositional
  { _argumentPositional_value :: expr AnyWhitespaceChar a
  , _argumentPositional_ann :: a
  }
  | ArgumentKeyword
  { _argumentKeyword_left :: name a
  , _argumentKeyword_equals :: Between' [AnyWhitespaceChar] Equals
  , _argumentKeyword_right :: expr AnyWhitespaceChar a
  , _argumentKeyword_ann :: a
  }
  | ArgumentStar
  { _argumentStar_asterisk :: After [AnyWhitespaceChar] Asterisk
  , _argumentStar_value :: expr AnyWhitespaceChar a
  , _argumentStar_ann :: a
  }
  | ArgumentDoublestar
  { _argumentDoublestar_asterisk :: After [AnyWhitespaceChar] DoubleAsterisk
  , _argumentDoublestar_value :: expr AnyWhitespaceChar a
  , _argumentDoublestar_ann :: a
  }
deriving instance (Eq (expr AnyWhitespaceChar a), Eq (name a), Eq a) => Eq (Argument name expr a)
deriving instance (Show (expr AnyWhitespaceChar a), Show (name a), Show a) => Show (Argument name expr a)
deriving instance (Ord (expr AnyWhitespaceChar a), Ord (name a), Ord a) => Ord (Argument name expr a)
deriving instance (Functor name, Functor (expr AnyWhitespaceChar)) => Functor (Argument name expr)
deriving instance (Foldable name, Foldable (expr AnyWhitespaceChar)) => Foldable (Argument name expr)
deriving instance (Traversable name, Traversable (expr AnyWhitespaceChar)) => Traversable (Argument name expr)

data ArgumentList name expr a
  = ArgumentList
  { _argumentList_head :: Argument name expr a
  , _argumentList_tail
    :: Compose
         []
         (Compose
           (Before (Between' [AnyWhitespaceChar] Comma))
           (Argument name expr))
         a
  , _argumentList_whitespace :: [AnyWhitespaceChar]
  , _argumentList_comma :: Maybe (After [AnyWhitespaceChar] Comma)
  , _argumentList_ann :: a
  }
deriving instance (Eq1 (Argument name expr), Eq (expr AnyWhitespaceChar a), Eq (name a), Eq a) => Eq (ArgumentList name expr a)
deriving instance (Show1 (Argument name expr), Show (expr AnyWhitespaceChar a), Show (name a), Show a) => Show (ArgumentList name expr a)
deriving instance (Ord1 (Argument name expr), Ord (expr AnyWhitespaceChar a), Ord (name a), Ord a) => Ord (ArgumentList name expr a)
deriving instance (Functor name, Functor (expr AnyWhitespaceChar)) => Functor (ArgumentList name expr)
deriving instance (Foldable name, Foldable (expr AnyWhitespaceChar)) => Foldable (ArgumentList name expr)
deriving instance (Traversable name, Traversable (expr AnyWhitespaceChar)) => Traversable (ArgumentList name expr)

instance HasName name => IsArgList (ArgumentList name expr a) where
  data KeywordArgument (ArgumentList name expr a)
    = AKeywordArgument
        (name a)
        (Between' [AnyWhitespaceChar] Equals)
        (expr AnyWhitespaceChar a)
        a

  data DoublestarArgument (ArgumentList name expr a)
    = ADoublestarArgument
        (After [AnyWhitespaceChar] DoubleAsterisk)
        (expr AnyWhitespaceChar a)
        a

  data PositionalArgument (ArgumentList name expr a)
    = APositionalArgument
        (Maybe (After [AnyWhitespaceChar] Asterisk))
        (expr AnyWhitespaceChar a)
        a

  argumentName (KeywordArgument (AKeywordArgument n _ _ _)) = Just $ n ^. name
  argumentName _ = Nothing

  arguments (ArgumentList h (Compose as) _ _ _) =
    toArg h : toList (toArg . (^. _Wrapped.before._2) <$> as)
    where
      toArg (ArgumentPositional a b) = PositionalArgument $ APositionalArgument Nothing a b
      toArg (ArgumentKeyword a b c d) = KeywordArgument $ AKeywordArgument a b c d
      toArg (ArgumentStar a b c) = PositionalArgument $ APositionalArgument (Just a) b c
      toArg (ArgumentDoublestar a b c) = DoublestarArgument $ ADoublestarArgument a b c

$(return [])

instance (Eq1 (Argument name expr), Eq1 (expr AnyWhitespaceChar)) => Eq1 (ArgumentList name expr) where
  liftEq = $(makeLiftEq ''ArgumentList)

instance (Show1 (Argument name expr), Show1 (expr AnyWhitespaceChar)) => Show1 (ArgumentList name expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''ArgumentList)

instance (Ord1 (Argument name expr), Ord1 (expr AnyWhitespaceChar)) => Ord1 (ArgumentList name expr) where
  liftCompare = $(makeLiftCompare ''ArgumentList)

instance (Eq1 name, Eq1 (expr AnyWhitespaceChar)) => Eq1 (Argument name expr) where
  liftEq = $(makeLiftEq ''Argument)

instance (Show1 name, Show1 (expr AnyWhitespaceChar)) => Show1 (Argument name expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''Argument)

instance (Ord1 name, Ord1 (expr AnyWhitespaceChar)) => Ord1 (Argument name expr) where
  liftCompare = $(makeLiftCompare ''Argument)
