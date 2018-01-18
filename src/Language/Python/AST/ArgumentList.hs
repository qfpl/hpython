{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module Language.Python.AST.ArgumentList
  ( ArgumentList
  , Argument(..)
  , mkArgumentList
  , _ArgumentList
  )
where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.IR.ExprConfig
import Language.Python.AST.IsArgList hiding (Argument)
import Language.Python.AST.Symbols

data Argument val name expr (dctxt :: DefinitionContext) a
  = ArgumentPositional
  { _argumentPositional_value :: val a
  , _argumentPositional_ann :: a
  }
  | ArgumentKeyword
  { _argumentKeyword_left :: name a
  , _argumentKeyword_equals :: Between' [AnyWhitespaceChar] Equals
  , _argumentKeyword_right :: expr AnyWhitespaceChar 'NotAssignable dctxt a
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

deriving instance (Eq (name a), Eq (val a), Eq (expr AnyWhitespaceChar 'NotAssignable dctxt a), Eq a) => Eq (Argument val name expr dctxt a)
deriving instance (Show (name a), Show (val a), Show (expr AnyWhitespaceChar 'NotAssignable dctxt a), Show a) => Show (Argument val name expr dctxt a)
deriving instance (Ord (name a), Ord (val a), Ord (expr AnyWhitespaceChar 'NotAssignable dctxt a), Ord a) => Ord (Argument val name expr dctxt a)
deriving instance (Functor (expr AnyWhitespaceChar 'NotAssignable dctxt), Functor name, Functor val) => Functor (Argument val name expr dctxt)
deriving instance (Foldable (expr AnyWhitespaceChar 'NotAssignable dctxt), Foldable name, Foldable val) => Foldable (Argument val name expr dctxt)
deriving instance (Traversable (expr AnyWhitespaceChar 'NotAssignable dctxt), Traversable name, Traversable val) => Traversable (Argument val name expr dctxt)

data ArgumentList val name expr (as :: AtomType) (dctxt :: DefinitionContext) a where
  ArgumentList ::
    { _argumentList_head :: Argument val name expr dctxt a
    , _argumentList_tail
      :: Compose
          []
          (Compose
            (Before (Between' [AnyWhitespaceChar] Comma))
            (Argument val name expr dctxt))
          a
    , _argumentList_whitespace :: [AnyWhitespaceChar]
    , _argumentList_comma :: Maybe (After [AnyWhitespaceChar] Comma)
    , _argumentList_ann :: a
    } -> ArgumentList val name expr 'NotAssignable dctxt a
deriving instance (Eq (val a), Eq (name a), Eq (expr AnyWhitespaceChar as dctxt a), Eq a, Eq1 (Argument val name expr dctxt)) => Eq (ArgumentList val name expr as dctxt a)
deriving instance (Show (val a), Show (name a), Show (expr AnyWhitespaceChar as dctxt a), Show a, Show1 (Argument val name expr dctxt)) => Show (ArgumentList val name expr as dctxt a)
deriving instance (Ord (val a), Ord (name a), Ord (expr AnyWhitespaceChar as dctxt a), Ord a, Ord1 (Argument val name expr dctxt)) => Ord (ArgumentList val name expr as dctxt a)
deriving instance (Functor val, Functor name, Functor (expr AnyWhitespaceChar as dctxt)) => Functor (ArgumentList val name expr as dctxt)
deriving instance (Foldable val, Foldable name, Foldable (expr AnyWhitespaceChar as dctxt)) => Foldable (ArgumentList val name expr as dctxt)
deriving instance (Traversable val, Traversable name, Traversable (expr AnyWhitespaceChar as dctxt)) => Traversable (ArgumentList val name expr as dctxt)

mkArgumentList
  :: HasName name
  => Argument val name expr dctxt a
  -> Compose
       []
        (Compose
          (Before (Between' [AnyWhitespaceChar] Comma))
          (Argument val name expr dctxt))
        a
  -> [AnyWhitespaceChar]
  -> Maybe (After [AnyWhitespaceChar] Comma)
  -> a
  -> Either
       (ArgumentError (ArgumentList val name expr 'NotAssignable dctxt a))
       (ArgumentList val name expr 'NotAssignable dctxt a)
mkArgumentList a b c d e =
  let res = ArgumentList a b c d e
  in validateArgList res

_ArgumentList
  :: HasName name
  => Prism'
       (Maybe (ArgumentList val name expr 'NotAssignable dctxt a))
       ( Argument val name expr dctxt a
       , Compose
           []
           (Compose
             (Before (Between' [AnyWhitespaceChar] Comma))
             (Argument val name expr dctxt))
           a
       , [AnyWhitespaceChar]
       , Maybe (After [AnyWhitespaceChar] Comma)
       , a
       )
_ArgumentList =
  prism'
    (\(a, b, c, d, e) -> mkArgumentList a b c d e ^? _Right)
    (\case
        Just (ArgumentList a b c d e) -> Just (a, b, c, d, e)
        _ -> Nothing)

instance HasName name => IsArgList (ArgumentList val name expr as dctxt a) where
  data KeywordArgument (ArgumentList val name expr as dctxt a)
    = KAKeywordArg
        (name a)
        (Between' [AnyWhitespaceChar] Equals)
        (expr AnyWhitespaceChar 'NotAssignable dctxt a)
        a

  data DoublestarArgument (ArgumentList val name expr as dctxt a)
    = DADoublestarArg
        (After [AnyWhitespaceChar] DoubleAsterisk)
        (val a)
        a

  data PositionalArgument (ArgumentList val name expr as dctxt a)
    = PAPositionalArg
        (Maybe (After [AnyWhitespaceChar] Asterisk))
        (val a)
        a

  argumentName (KeywordArgument (KAKeywordArg a _ _ _)) = Just $ a ^. name
  argumentName _ = Nothing

  arguments l =
    case l of
      ArgumentList h (Compose t) _ _ _ ->
        toArg h : toList (toArg . (^. _Wrapped.before._2) <$> t)
    where
      toArg (ArgumentPositional a b) = PositionalArgument $ PAPositionalArg Nothing a b
      toArg (ArgumentKeyword a b c d) = KeywordArgument $ KAKeywordArg a b c d
      toArg (ArgumentStar a b c) = PositionalArgument $ PAPositionalArg (Just a) b c
      toArg (ArgumentDoublestar a b c) = DoublestarArgument $ DADoublestarArg a b c

$(return [])

instance (Eq1 name, Eq1 (expr AnyWhitespaceChar as dctxt), Eq1 (Argument val name expr dctxt)) => Eq1 (ArgumentList val name expr as dctxt) where
  liftEq = $(makeLiftEq ''ArgumentList)

instance (Show1 name, Show1 (expr AnyWhitespaceChar as dctxt), Show1 (Argument val name expr dctxt)) => Show1 (ArgumentList val name expr as dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''ArgumentList)

instance (Ord1 name, Ord1 (expr AnyWhitespaceChar as dctxt), Ord1 (Argument val name expr dctxt)) => Ord1 (ArgumentList val name expr as dctxt) where
  liftCompare = $(makeLiftCompare ''ArgumentList)

instance (Eq1 val, Eq1 name, Eq1 (expr AnyWhitespaceChar 'NotAssignable dctxt), Eq1 (Argument val name expr dctxt)) => Eq1 (Argument val name expr dctxt) where
  liftEq = $(makeLiftEq ''Argument)

instance (Show1 val, Show1 name, Show1 (expr AnyWhitespaceChar 'NotAssignable dctxt), Show1 (Argument val name expr dctxt)) => Show1 (Argument val name expr dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''Argument)

instance (Ord1 val, Ord1 name, Ord1 (expr AnyWhitespaceChar 'NotAssignable dctxt), Ord1 (Argument val name expr dctxt)) => Ord1 (Argument val name expr dctxt) where
  liftCompare = $(makeLiftCompare ''Argument)
