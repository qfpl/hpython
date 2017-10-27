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
module Language.Python.AST.ArgumentList
  ( KeywordItem(..)
  , KeywordsArguments(..)
  , PositionalArguments(..)
  , StarredAndKeywords(..)
  , ArgumentList
  , mkArgumentListAll
  , mkArgumentListUnpacking
  , mkArgumentListKeywords
  , _ArgumentListAll
  , _ArgumentListUnpacking
  , _ArgumentListKeywords
  )
where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.IR.ExprConfig
import Language.Python.AST.IsArgList
import Language.Python.AST.Symbols

data KeywordItem name expr (as :: AtomType) (dctxt :: DefinitionContext) a where
  KeywordItem ::
    { _keywordItem_left :: Compose (After [WhitespaceChar]) name a
    , _keywordItem_right
      :: Compose
           (Before [WhitespaceChar])
           (expr 'NotAssignable dctxt)
           a
    , _keywordItem_ann :: a
    } -> KeywordItem name expr 'NotAssignable dctxt a
deriving instance (Functor name, Functor (expr as dctxt)) => Functor (KeywordItem name expr as dctxt)
deriving instance (Foldable name, Foldable (expr as dctxt)) => Foldable (KeywordItem name expr as dctxt)
deriving instance (Traversable name, Traversable (expr as dctxt)) => Traversable (KeywordItem name expr as dctxt)

data KeywordsArguments name expr as dctxt a where
  KeywordsArguments ::
    { _keywordsArguments_head
      :: Sum
           (KeywordItem name expr 'NotAssignable dctxt)
           (Compose
             (Before (Between' [WhitespaceChar] DoubleAsterisk))
             (expr 'NotAssignable dctxt))
           a
    , _keywordsArguments_tail
      :: Compose
           []
           (Compose
             (Before (Between' [WhitespaceChar] Comma))
             (Sum
               (KeywordItem name expr 'NotAssignable dctxt)
               (Compose
                 (Before (Between' [WhitespaceChar] DoubleAsterisk))
                 (expr 'NotAssignable dctxt))))
           a
    , _keywordsArguments_ann :: a
    } -> KeywordsArguments name expr 'NotAssignable dctxt a
deriving instance (Functor name, Functor (expr as dctxt)) => Functor (KeywordsArguments name expr as dctxt)
deriving instance (Foldable name, Foldable (expr as dctxt)) => Foldable (KeywordsArguments name expr as dctxt)
deriving instance (Traversable name, Traversable (expr as dctxt)) => Traversable (KeywordsArguments name expr as dctxt)

data PositionalArguments expr (as :: AtomType) (dctxt :: DefinitionContext) a where
  PositionalArguments ::
    { _positionalArguments_head
      :: Compose
          (Before (Maybe (Between' [WhitespaceChar] Asterisk)))
          (expr 'NotAssignable dctxt)
          a
    , _positionalArguments_tail
      :: Compose
          []
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (Compose
              (Before (Maybe (Between' [WhitespaceChar] Asterisk)))
              (expr 'NotAssignable dctxt)))
          a
    , _positionalArguments_ann :: a
    } -> PositionalArguments expr 'NotAssignable dctxt a
deriving instance Functor (expr as dctxt) => Functor (PositionalArguments expr as dctxt)
deriving instance Foldable (expr as dctxt) => Foldable (PositionalArguments expr as dctxt)
deriving instance Traversable (expr as dctxt) => Traversable (PositionalArguments expr as dctxt)

data StarredAndKeywords name expr (as :: AtomType) (dctxt :: DefinitionContext) a where
  StarredAndKeywords ::
    { _starredAndKeywords_head
      :: Sum
           (Compose
             (Before (Between' [WhitespaceChar] Asterisk))
             (expr 'NotAssignable dctxt))
           (KeywordItem name expr 'NotAssignable dctxt)
           a 
    , _starredAndKeywords_tail
      :: Compose
           []
           (Compose
             (Before (Between' [WhitespaceChar] Comma))
             (Sum
               (Compose
                 (Before (Between' [WhitespaceChar] Asterisk))
                   (expr 'NotAssignable dctxt))
               (KeywordItem name expr 'NotAssignable dctxt)))
           a
    , _starredAndKeywords_ann :: a
    } -> StarredAndKeywords name expr 'NotAssignable dctxt a
deriving instance (Functor name, Functor (expr as dctxt)) => Functor (StarredAndKeywords name expr as dctxt)
deriving instance (Foldable name, Foldable (expr as dctxt)) => Foldable (StarredAndKeywords name expr as dctxt)
deriving instance (Traversable name, Traversable (expr as dctxt)) => Traversable (StarredAndKeywords name expr as dctxt)

data ArgumentList name expr (as :: AtomType) (dctxt :: DefinitionContext) a where
  ArgumentListAll ::
    { _argumentListAll_positionalArguments
      :: PositionalArguments expr 'NotAssignable dctxt a
    , _argumentListAll_starredAndKeywords
      :: Compose
          Maybe
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (StarredAndKeywords name expr 'NotAssignable dctxt))
          a
    , _argumentListAll_keywords
      :: Compose
          Maybe
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (KeywordsArguments name expr 'NotAssignable dctxt))
          a
    , _argumentList_comma :: Maybe (Between' [WhitespaceChar] Comma)
    , _argumentList_ann :: a
    } -> ArgumentList name expr 'NotAssignable dctxt a
  ArgumentListUnpacking ::
    { _argumentListUnpacking_starredAndKeywords
      :: StarredAndKeywords name expr 'NotAssignable dctxt a
    , _argumentListUnpacking_keywords
      :: Compose
          Maybe
          (Compose
            (Before (Between' [WhitespaceChar] Comma))
            (KeywordsArguments name expr 'NotAssignable dctxt))
          a
    , _argumentList_comma :: Maybe (Between' [WhitespaceChar] Comma)
    , _argumentList_ann :: a
    } -> ArgumentList name expr 'NotAssignable dctxt a
  ArgumentListKeywords ::
    { _argumentListKeywords_keywords
      :: KeywordsArguments name expr 'NotAssignable dctxt a
    , _argumentList_comma :: Maybe (Between' [WhitespaceChar] Comma)
    , _argumentList_ann :: a
    } -> ArgumentList name expr 'NotAssignable dctxt a
deriving instance (Functor name, Functor (expr as dctxt)) => Functor (ArgumentList name expr as dctxt)
deriving instance (Foldable name, Foldable (expr as dctxt)) => Foldable (ArgumentList name expr as dctxt)
deriving instance (Traversable name, Traversable (expr as dctxt)) => Traversable (ArgumentList name expr as dctxt)

mkArgumentListAll
  :: Ord (name a)
  => PositionalArguments expr 'NotAssignable dctxt a
  -> Compose
       Maybe
       (Compose
         (Before (Between' [WhitespaceChar] Comma))
         (StarredAndKeywords name expr 'NotAssignable dctxt))
       a
  -> Compose
       Maybe
       (Compose
         (Before (Between' [WhitespaceChar] Comma))
         (KeywordsArguments name expr 'NotAssignable dctxt))
       a
  -> Maybe (Between' [WhitespaceChar] Comma)
  -> a
  -> Either
       (ArgumentError (ArgumentList name expr 'NotAssignable dctxt a))
       (ArgumentList name expr 'NotAssignable dctxt a)
mkArgumentListAll a b c d e =
  let res = ArgumentListAll a b c d e
  in validateArgList res

_ArgumentListAll
  :: Ord (name a)
  => Prism'
       (Maybe (ArgumentList name expr 'NotAssignable dctxt a))
       ( PositionalArguments expr 'NotAssignable dctxt a
       , Compose
           Maybe
           (Compose
             (Before (Between' [WhitespaceChar] Comma))
             (StarredAndKeywords name expr 'NotAssignable dctxt))
           a
       , Compose
           Maybe
           (Compose
             (Before (Between' [WhitespaceChar] Comma))
             (KeywordsArguments name expr 'NotAssignable dctxt))
           a
       , Maybe (Between' [WhitespaceChar] Comma)
       , a
       )
_ArgumentListAll =
  prism'
    (\(a, b, c, d, e) ->
       either (const Nothing) Just $ mkArgumentListAll a b c d e)
    (\case
        Just (ArgumentListAll a b c d e) -> Just (a, b, c, d, e)
        _ -> Nothing)

mkArgumentListUnpacking
  :: Ord (name a)
  => StarredAndKeywords name expr 'NotAssignable dctxt a
  -> Compose
       Maybe
       (Compose
         (Before (Between' [WhitespaceChar] Comma))
         (KeywordsArguments name expr 'NotAssignable dctxt))
       a
  -> Maybe (Between' [WhitespaceChar] Comma)
  -> a
  -> Either
       (ArgumentError (ArgumentList name expr 'NotAssignable dctxt a))
       (ArgumentList name expr 'NotAssignable dctxt a)
mkArgumentListUnpacking a b c d =
  let res = ArgumentListUnpacking a b c d
  in validateArgList res

_ArgumentListUnpacking
  :: Ord (name a)
  => Prism'
       (Maybe (ArgumentList name expr 'NotAssignable dctxt a))
       ( StarredAndKeywords name expr 'NotAssignable dctxt a
       , Compose
           Maybe
           (Compose
             (Before (Between' [WhitespaceChar] Comma))
             (KeywordsArguments name expr 'NotAssignable dctxt))
           a
       , Maybe (Between' [WhitespaceChar] Comma)
       , a
       )
_ArgumentListUnpacking =
  prism'
    (\(a, b, c, d) ->
       either (const Nothing) Just $ mkArgumentListUnpacking a b c d)
    (\case
        Just (ArgumentListUnpacking a b c d) -> Just (a, b, c, d)
        _ -> Nothing)

mkArgumentListKeywords
  :: Ord (name a)
  => KeywordsArguments name expr 'NotAssignable dctxt a
  -> Maybe (Between' [WhitespaceChar] Comma)
  -> a
  -> Either
       (ArgumentError (ArgumentList name expr 'NotAssignable dctxt a))
       (ArgumentList name expr 'NotAssignable dctxt a)
mkArgumentListKeywords a b c =
  let res = ArgumentListKeywords a b c
  in validateArgList res

_ArgumentListKeywords
  :: Ord (name a)
  => Prism'
       (Maybe (ArgumentList name expr 'NotAssignable dctxt a))
       ( KeywordsArguments name expr 'NotAssignable dctxt a
       , Maybe (Between' [WhitespaceChar] Comma)
       , a
       )
_ArgumentListKeywords =
  prism'
    (\(a, b, c) -> either (const Nothing) Just $ mkArgumentListKeywords a b c)
    (\case
        Just (ArgumentListKeywords a b c) -> Just (a, b, c)
        _ -> Nothing)

instance IsArgList (ArgumentList name expr as dctxt a) where
  type Name (ArgumentList name expr as dctxt a) = name a

  data KeywordArgument (ArgumentList name expr as dctxt a)
    = KAKeywordArg (KeywordItem name expr as dctxt a)

  data DoublestarArgument (ArgumentList name expr as dctxt a)
    = DADoublestarArg
        (Compose
          (Before (Between' [WhitespaceChar] DoubleAsterisk))
          (expr 'NotAssignable dctxt)
          a)

  data PositionalArgument (ArgumentList name expr as dctxt a)
    = PAPositionalArg
        (Compose
          (Before (Maybe (Between' [WhitespaceChar] Asterisk)))
          (expr 'NotAssignable dctxt)
          a)

  keywordName (KAKeywordArg (KeywordItem ident _ _)) =
    ident ^. _Wrapped.after._2

  arguments l =
    case l of
      ArgumentListAll ps ss ks _ _ ->
        fromPositional ps <>
        maybe [] fromStarredAndKeywords (ss ^? _Wrapped._Just._Wrapped.before._2) <>
        maybe [] fromKeywords (ks ^? _Wrapped._Just._Wrapped.before._2)
      ArgumentListUnpacking ss ks _ _ ->
        fromStarredAndKeywords ss <>
        maybe [] fromKeywords (ks ^? _Wrapped._Just._Wrapped.before._2)
      ArgumentListKeywords ks _ _ ->
        fromKeywords ks
    where
      fromPositional
        :: PositionalArguments expr as dctxt a
        -> [Argument (ArgumentList name expr as dctxt a)]
      fromPositional (PositionalArguments h t _) =
        PositionalArgument (PAPositionalArg h) :
        fmap
          (PositionalArgument . PAPositionalArg)
          (t ^.. _Wrapped.folded._Wrapped.folded)

      fromStarredAndKeywords
        :: StarredAndKeywords name expr as dctxt a
        -> [Argument (ArgumentList name expr as dctxt a)]
      fromStarredAndKeywords (StarredAndKeywords h t _) =
        starOrKeyword h :
        fmap starOrKeyword (t ^.. _Wrapped.folded._Wrapped.folded)
        where
          starOrKeyword x =
            case x of
              InL a ->
                PositionalArgument . PAPositionalArg $
                over (_Wrapped.before._1) Just a
              InR a -> KeywordArgument $ KAKeywordArg a

      fromKeywords
        :: KeywordsArguments name expr as dctxt a
        -> [Argument (ArgumentList name expr as dctxt a)]
      fromKeywords (KeywordsArguments h t _) =
        keyOrUnpack h :
        fmap keyOrUnpack (t ^.. _Wrapped.folded._Wrapped.folded)
        where
          keyOrUnpack x =
            case x of
              InL a -> KeywordArgument $ KAKeywordArg a
              InR a -> DoublestarArgument $ DADoublestarArg a

$(return [])

instance (Eq1 name, Eq1 (expr as dctxt)) => Eq1 (ArgumentList name expr as dctxt) where
  liftEq = $(makeLiftEq ''ArgumentList)

instance Eq1 (expr as dctxt) => Eq1 (PositionalArguments expr as dctxt) where
  liftEq = $(makeLiftEq ''PositionalArguments)

instance (Eq1 name, Eq1 (expr as dctxt)) => Eq1 (StarredAndKeywords name expr as dctxt) where
  liftEq = $(makeLiftEq ''StarredAndKeywords)

instance (Eq1 name, Eq1 (expr as dctxt)) => Eq1 (KeywordsArguments name expr as dctxt) where
  liftEq = $(makeLiftEq ''KeywordsArguments)

instance (Eq1 name, Eq1 (expr as dctxt)) => Eq1 (KeywordItem name expr as dctxt) where
  liftEq = $(makeLiftEq ''KeywordItem)

instance (Show1 name, Show1 (expr as dctxt)) => Show1 (ArgumentList name expr as dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''ArgumentList)

instance Show1 (expr as dctxt) => Show1 (PositionalArguments expr as dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''PositionalArguments)

instance (Show1 name, Show1 (expr as dctxt)) => Show1 (StarredAndKeywords name expr as dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''StarredAndKeywords)

instance (Show1 name, Show1 (expr as dctxt)) => Show1 (KeywordsArguments name expr as dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''KeywordsArguments)

instance (Show1 name, Show1 (expr as dctxt)) => Show1 (KeywordItem name expr as dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''KeywordItem)
