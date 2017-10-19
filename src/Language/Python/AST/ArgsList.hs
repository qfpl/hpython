{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
module Language.Python.AST.ArgsList
  ( HasArgs(..)
  , HasName(..)
  , ArgsError(..)
  , ArgsList
  , mkArgsListAll
  , mkArgsListArgsKwargs
  , _ArgsListAll
  , _ArgsListArgsKwargs
  , _argsListAll_head
  , _argsListAll_tail
  , _argsListAll_rest
  , _argsListArgsKwargs_value
  , _argsList_ann
  , argsListAll_head
  , argsListAll_tail
  , argsListAll_rest
  , argsListArgsKwargs_value
  , argsList_ann
  , ArgsListStarPart(..)
  , argsListStarPart_ann
  , argsListStarPart_starred
  , argsListStarPart_defaults
  , argsListStarPart_kwargs
  , ArgsListDoublestarArg(..)
  , argsListDoublestarArg_value
  , argsListDoublestarArg_ann
  , ArgsListArg(..)
  , argsListArg_left
  , argsListArg_right
  , argsListArg_ann
  )
  where

import Papa hiding (Sum)
import Data.Deriving
import Data.Separated.Before
import Data.Separated.Between
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Functor.Sum.Lens
import Data.Text (Text)

import Language.Python.AST.Identifier
import Language.Python.AST.Symbols

data ArgsError a
  = DuplicateArgumentsError [Identifier a]
  | DefaultBeforeNonDefault [Identifier a]
  deriving (Eq, Show)

data ArgsListArg name test a
  = ArgsListArg
  { _argsListArg_left :: name a
  , _argsListArg_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Equals))
           test)
         a
  , _argsListArg_ann :: a
  }
  deriving (Eq, Functor, Foldable, Show, Traversable)

deriveEq1 ''ArgsListArg
deriveShow1 ''ArgsListArg
makeLenses ''ArgsListArg

duplicates :: (a -> a -> Bool) -> [a] -> [a]
duplicates _ [] = []
duplicates _ [_] = []
duplicates eq (a:as) =
  let (yes, no) = partition (eq a) as
  in
    if null yes
    then duplicates eq no
    else a : duplicates eq no

defaultBeforeNonDefault :: [ArgsListArg name test a] -> Bool
defaultBeforeNonDefault = go False
  where
    go seenDefault [] = seenDefault
    go seenDefault (a:as) =
      case a of
        ArgsListArg _ (Compose r) _ ->
          case r of
            Nothing -> seenDefault || go seenDefault as
            Just _ -> go True as

class HasArgs s where
  -- |
  -- Retrieves all the argument names in a structure, including the names in starred args
  -- and double-starred args.
  argNames :: Fold (s name test a) (name a)

  -- |
  -- Retrieves all the arguments in a structure, *excluding* starred arguments and double-starred
  -- arguments
  args :: Fold (s name test a) (ArgsListArg name test a)

instance HasArgs ArgsListArg where
  argNames f (ArgsListArg a b ann) =
    ArgsListArg <$> f a <*> pure b <*> pure ann

  args f = f

instance HasArgs ArgsListDoublestarArg where
  argNames f (ArgsListDoublestarArg a ann) =
    ArgsListDoublestarArg <$>
    traverseOf (_Wrapped.between'._2) f a <*>
    pure ann

  args _ (ArgsListDoublestarArg a ann) =
    ArgsListDoublestarArg <$>
    pure a <*>
    pure ann

instance HasArgs ArgsListStarPart where
  argNames f (ArgsListStarPart a b c ann) =
    ArgsListStarPart <$>
    traverseOf (_Wrapped.before._2) f a <*>
    traverseOf (_Wrapped.traverse._Wrapped.before._2.argNames) f b <*>
    traverseOf (_Wrapped.traverse._Wrapped.before._2.argNames) f c <*>
    pure ann
  argNames _ (ArgsListStarPartEmpty ann) = pure $ ArgsListStarPartEmpty ann

  args f (ArgsListStarPart a b c ann) =
    ArgsListStarPart <$>
    pure a <*>
    traverseOf (_Wrapped.traverse._Wrapped.before._2.args) f b <*>
    traverseOf (_Wrapped.traverse._Wrapped.before._2.args) f c <*>
    pure ann
  args _ (ArgsListStarPartEmpty ann) = pure $ ArgsListStarPartEmpty ann

class HasName name where
  named :: name a -> Text
  namedIdentifier :: Getter (name a) (Identifier a)

instance HasName name => HasName (ArgsListArg name test) where
  named (ArgsListArg a _ _) = named a
  namedIdentifier = argsListArg_left.namedIdentifier

instance HasName Identifier where
  named = _identifier_value
  namedIdentifier = id

mkArgsListAll
  :: HasName name
  => ArgsListArg name test a
  -> Compose
       []
       (Compose
         (Before
           (Between' [WhitespaceChar] Comma))
           (ArgsListArg name test))
       a
  -> Compose
       Maybe
       (Compose
         (Before
           (Between' [WhitespaceChar] Comma))
           (Compose
             Maybe
             (Sum
               (ArgsListStarPart name test)
               (ArgsListDoublestarArg name test))))
       a
  -> a
  -> Either (ArgsError a) (ArgsList name test a)
mkArgsListAll a bs c ann =
  let allArgNames =
        (a ^.. argNames) <>
        (bs ^.. _Wrapped.folded._Wrapped.before._2.argNames) <>
        (c ^.. _Wrapped.folded._Wrapped.before._2.
               _Wrapped.folded.failing (_InL.argNames) (_InR.argNames))
  in case duplicates ((==) `on` named) allArgNames of
    [] ->
      let allArgs =
            (a ^.. args) <>
            (bs ^.. _Wrapped.folded._Wrapped.before._2.args) <>
            (c ^.. _Wrapped.folded._Wrapped.before._2.
              _Wrapped.folded.failing (_InL.args) (_InR.args))
      in
        if defaultBeforeNonDefault allArgs
        then
          Left .
          DefaultBeforeNonDefault $
          fmap (view namedIdentifier) allArgs
        else Right $ ArgsListAll a bs c ann
    dups ->
      Left .
      DuplicateArgumentsError $
      fmap (view namedIdentifier) dups

_ArgsListAll
  :: HasName name
  => Prism'
       (Maybe (ArgsList name test a))
       ( ArgsListArg name test a
       , Compose
           []
           (Compose
             (Before
               (Between' [WhitespaceChar] Comma))
               (ArgsListArg name test))
           a
       , Compose
           Maybe
           (Compose
             (Before
               (Between' [WhitespaceChar] Comma))
               (Compose
                  Maybe
                  (Sum
                    (ArgsListStarPart name test)
                    (ArgsListDoublestarArg name test))))
           a
       , a
       )
_ArgsListAll =
  prism'
    (\(a, b, c, d) -> case mkArgsListAll a b c d of
        Right a' -> Just a'
        Left _ -> Nothing)
    (\v -> do
        v' <- v
        case v' of
          ArgsListAll a b c d -> pure (a, b, c, d)
          _ -> Nothing)

mkArgsListArgsKwargs
  :: HasName name
  => Sum
       (ArgsListStarPart name test)
       (ArgsListDoublestarArg name test)
       a
  -> a
  -> Either (ArgsError a) (ArgsList name test a)
mkArgsListArgsKwargs a ann =
  let allArgNames =
        a ^.. failing (_InL.argNames) (_InR.argNames)
  in case duplicates ((==) `on` named) allArgNames of
    [] ->
      let allArgs =
            a ^.. failing (_InL.args) (_InR.args)
      in if defaultBeforeNonDefault allArgs
         then
           Left .
           DefaultBeforeNonDefault $
           fmap (view namedIdentifier) allArgs
         else Right $ ArgsListArgsKwargs a ann
    dups ->
      Left .
      DuplicateArgumentsError $
      fmap (view namedIdentifier) dups

_ArgsListArgsKwargs
  :: HasName name
  => Prism'
       (Maybe (ArgsList name test a))
       ( Sum
           (ArgsListStarPart name test)
           (ArgsListDoublestarArg name test)
           a
       , a
       )
_ArgsListArgsKwargs =
  prism'
    (\(a, b) -> case mkArgsListArgsKwargs a b of
        Right a' -> Just a'
        Left _ -> Nothing)
    (\v -> do
        v' <- v
        case v' of
          ArgsListArgsKwargs a b -> pure (a, b)
          _ -> Nothing)

data ArgsList name test a
  = ArgsListAll
  { _argsListAll_head :: ArgsListArg name test a
  , _argsListAll_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (ArgsListArg name test))
         a
  , _argsListAll_rest
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Compose
             Maybe
             (Sum
               (ArgsListStarPart name test)
               (ArgsListDoublestarArg name test))))
         a
  , _argsList_ann :: a
  }
  | ArgsListArgsKwargs
  { _argsListArgsKwargs_value
      :: Sum
           (ArgsListStarPart name test)
           (ArgsListDoublestarArg name test)
           a
  , _argsList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 test, Eq1 name, Eq a, Eq (name a)) => Eq (ArgsList name test a)
deriving instance (Show1 test, Show1 name, Show a, Show (name a)) => Show (ArgsList name test a)

data ArgsListStarPart name test a
  = ArgsListStarPartEmpty
  { _argsListStarPart_ann :: a
  }
  | ArgsListStarPart
  { _argsListStarPart_starred
    :: Compose
         (Before (Between' [WhitespaceChar] Asterisk))
         name
         a
  , _argsListStarPart_defaults
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (ArgsListArg name test))
         a
  , _argsListStarPart_kwargs
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (ArgsListDoublestarArg name test))
         a
  , _argsListStarPart_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 test, Eq1 name, Eq a, Eq (name a)) => Eq (ArgsListStarPart name test a)
deriving instance (Show1 test, Show1 name, Show a, Show (name a)) => Show (ArgsListStarPart name test a)

data ArgsListDoublestarArg name (test :: * -> *) a
  = ArgsListDoublestarArg
  { _argsListDoublestarArg_value
    :: Compose
         (Between' [WhitespaceChar])
         name
         a
  , _argsListDoublestarArg_ann :: a
  }
  deriving (Eq, Functor, Foldable, Show, Traversable)

deriveEq1 ''ArgsListStarPart
deriveShow1 ''ArgsListStarPart
makeLenses ''ArgsListStarPart

deriveEq1 ''ArgsListDoublestarArg
deriveShow1 ''ArgsListDoublestarArg
makeLenses ''ArgsListDoublestarArg

deriveEq1 ''ArgsList
deriveShow1 ''ArgsList
makeLenses ''ArgsList
