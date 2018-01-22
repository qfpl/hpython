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

data Argument arg val name expr a
  = ArgumentPositional
  { _argumentPositional_value :: arg a
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
  , _argumentStar_value :: val a
  , _argumentStar_ann :: a
  }
  | ArgumentDoublestar
  { _argumentDoublestar_asterisk :: After [AnyWhitespaceChar] DoubleAsterisk
  , _argumentDoublestar_value :: val a
  , _argumentDoublestar_ann :: a
  }
deriving instance (Eq (expr AnyWhitespaceChar a), Eq (name a), Eq (val a), Eq (arg a), Eq a) => Eq (Argument arg val name expr a)
deriving instance (Show (expr AnyWhitespaceChar a), Show (name a), Show (val a), Show (arg a), Show a) => Show (Argument arg val name expr a)
deriving instance (Ord (expr AnyWhitespaceChar a), Ord (name a), Ord (val a), Ord (arg a), Ord a) => Ord (Argument arg val name expr a)
deriving instance (Functor arg, Functor val, Functor name, Functor (expr AnyWhitespaceChar)) => Functor (Argument arg val name expr)
deriving instance (Foldable arg, Foldable val, Foldable name, Foldable (expr AnyWhitespaceChar)) => Foldable (Argument arg val name expr)
deriving instance (Traversable arg, Traversable val, Traversable name, Traversable (expr AnyWhitespaceChar)) => Traversable (Argument arg val name expr)

data ArgumentList arg val name expr a
  = ArgumentList
  { _argumentList_head :: Argument arg val name expr a
  , _argumentList_tail
    :: Compose
         []
         (Compose
           (Before (Between' [AnyWhitespaceChar] Comma))
           (Argument arg val name expr))
         a
  , _argumentList_whitespace :: [AnyWhitespaceChar]
  , _argumentList_comma :: Maybe (After [AnyWhitespaceChar] Comma)
  , _argumentList_ann :: a
  }
deriving instance (Eq1 (Argument arg val name expr), Eq (expr AnyWhitespaceChar a), Eq (name a), Eq (val a), Eq (arg a), Eq a) => Eq (ArgumentList arg val name expr a)
deriving instance (Show1 (Argument arg val name expr), Show (expr AnyWhitespaceChar a), Show (name a), Show (val a), Show (arg a), Show a) => Show (ArgumentList arg val name expr a)
deriving instance (Ord1 (Argument arg val name expr), Ord (expr AnyWhitespaceChar a), Ord (name a), Ord (val a), Ord (arg a), Ord a) => Ord (ArgumentList arg val name expr a)
deriving instance (Functor arg, Functor val, Functor name, Functor (expr AnyWhitespaceChar)) => Functor (ArgumentList arg val name expr)
deriving instance (Foldable arg, Foldable val, Foldable name, Foldable (expr AnyWhitespaceChar)) => Foldable (ArgumentList arg val name expr)
deriving instance (Traversable arg, Traversable val, Traversable name, Traversable (expr AnyWhitespaceChar)) => Traversable (ArgumentList arg val name expr)

instance HasName name => IsArgList (ArgumentList arg val name expr a) where
  data KeywordArgument (ArgumentList arg val name expr a)
    = AKeywordArgument
        (name a)
        (Between' [AnyWhitespaceChar] Equals)
        (expr AnyWhitespaceChar a)
        a

  data DoublestarArgument (ArgumentList arg val name expr a)
    = ADoublestarArgument
        (After [AnyWhitespaceChar] DoubleAsterisk)
        (val a)
        a

  data PositionalArgument (ArgumentList arg val name expr a)
    = APositionalArgument (arg a) a
    | AStarArgument
        (After [AnyWhitespaceChar] Asterisk)
        (val a)
        a

  argumentName (KeywordArgument (AKeywordArgument n _ _ _)) = Just $ n ^. name
  argumentName _ = Nothing

  arguments (ArgumentList h (Compose as) _ _ _) =
    toArg h : toList (toArg . (^. _Wrapped.before._2) <$> as)
    where
      toArg (ArgumentPositional a b) = PositionalArgument $ APositionalArgument a b
      toArg (ArgumentKeyword a b c d) = KeywordArgument $ AKeywordArgument a b c d
      toArg (ArgumentStar a b c) = PositionalArgument $ AStarArgument a b c
      toArg (ArgumentDoublestar a b c) = DoublestarArgument $ ADoublestarArgument a b c

$(return [])

instance (Eq1 (Argument arg val name expr), Eq1 (expr AnyWhitespaceChar)) => Eq1 (ArgumentList arg val name expr) where
  liftEq = $(makeLiftEq ''ArgumentList)

instance (Show1 (Argument arg val name expr), Show1 (expr AnyWhitespaceChar)) => Show1 (ArgumentList arg val name expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''ArgumentList)

instance (Ord1 (Argument arg val name expr), Ord1 (expr AnyWhitespaceChar)) => Ord1 (ArgumentList arg val name expr) where
  liftCompare = $(makeLiftCompare ''ArgumentList)

instance (Eq1 arg, Eq1 val, Eq1 name, Eq1 (expr AnyWhitespaceChar)) => Eq1 (Argument arg val name expr) where
  liftEq = $(makeLiftEq ''Argument)

instance (Show1 arg, Show1 val, Show1 name, Show1 (expr AnyWhitespaceChar)) => Show1 (Argument arg val name expr) where
  liftShowsPrec = $(makeLiftShowsPrec ''Argument)

instance (Ord1 arg, Ord1 val, Ord1 name, Ord1 (expr AnyWhitespaceChar)) => Ord1 (Argument arg val name expr) where
  liftCompare = $(makeLiftCompare ''Argument)
