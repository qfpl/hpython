{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
module Language.Python.AST.ArgsList
  ( KeywordArgument(..)
  , PositionalArgument(..)
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

import Language.Python.AST.IsArgList
import Language.Python.AST.Symbols

data ArgsListArg ws name test a
  = ArgsListArg
  { _argsListArg_left :: name a
  , _argsListArg_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' [ws] Equals))
           test)
         a
  , _argsListArg_ann :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

mkArgsListAll
  :: HasName name
  => ArgsListArg ws name test a
  -> Compose
       []
       (Compose
         (Before
           (Between' [ws] Comma))
           (ArgsListArg ws name test))
       a
  -> Compose
       Maybe
       (Compose
         (Before
           (Between' [ws] Comma))
           (Compose
             Maybe
             (Sum
               (ArgsListStarPart ws name test)
               (ArgsListDoublestarArg ws name test))))
       a
  -> a
  -> Either (ArgumentError (ArgsList ws name test a)) (ArgsList ws name test a)
mkArgsListAll a bs c ann =
  let res = ArgsListAll a bs c ann
  in validateArgList res

_ArgsListAll
  :: HasName name
  => Prism'
       (Maybe (ArgsList ws name test a))
       ( ArgsListArg ws name test a
       , Compose
           []
           (Compose
             (Before
               (Between' [ws] Comma))
               (ArgsListArg ws name test))
           a
       , Compose
           Maybe
           (Compose
             (Before
               (Between' [ws] Comma))
               (Compose
                  Maybe
                  (Sum
                    (ArgsListStarPart ws name test)
                    (ArgsListDoublestarArg ws name test))))
           a
       , a
       )
_ArgsListAll =
  prism'
    (\(a, b, c, d) -> mkArgsListAll a b c d ^? _Right)
    (\v -> do
        v' <- v
        case v' of
          ArgsListAll a b c d -> pure (a, b, c, d)
          _ -> Nothing)

mkArgsListArgsKwargs
  :: HasName name
  => Sum
       (ArgsListStarPart ws name test)
       (ArgsListDoublestarArg ws name test)
       a
  -> a
  -> Either (ArgumentError (ArgsList ws name test a)) (ArgsList ws name test a)
mkArgsListArgsKwargs a ann =
  let res = ArgsListArgsKwargs a ann
  in validateArgList res

_ArgsListArgsKwargs
  :: HasName name
  => Prism'
       (Maybe (ArgsList ws name test a))
       ( Sum
           (ArgsListStarPart ws name test)
           (ArgsListDoublestarArg ws name test)
           a
       , a)
_ArgsListArgsKwargs =
  prism'
    (\(a, b) -> mkArgsListArgsKwargs a b ^? _Right)
    (\v -> do
        v' <- v
        case v' of
          ArgsListArgsKwargs a b -> pure (a, b)
          _ -> Nothing)

data ArgsList ws name test a
  = ArgsListAll
  { _argsListAll_head :: ArgsListArg ws name test a
  , _argsListAll_tail
    :: Compose
         []
         (Compose
           (Before (Between' [ws] Comma))
           (ArgsListArg ws name test))
         a
  , _argsListAll_rest
    :: Compose
         Maybe
         (Compose
           (Before (Between' [ws] Comma))
           (Compose
             Maybe
             (Sum
               (ArgsListStarPart ws name test)
               (ArgsListDoublestarArg ws name test))))
         a
  , _argsList_ann :: a
  }
  | ArgsListArgsKwargs
  { _argsListArgsKwargs_value
      :: Sum
           (ArgsListStarPart ws name test)
           (ArgsListDoublestarArg ws name test)
           a
  , _argsList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq ws, Eq1 test, Eq1 name, Eq a, Eq (name a)) => Eq (ArgsList ws name test a)
deriving instance (Show ws, Show1 test, Show1 name, Show a, Show (name a)) => Show (ArgsList ws name test a)

instance HasName name => IsArgList (ArgsList ws name test a) where
  data KeywordArgument (ArgsList ws name test a)
    = KAKeywordArg (name a) (test a)

  data DoublestarArgument (ArgsList ws name test a)
    = DADoublestarArg (name a)

  data PositionalArgument (ArgsList ws name test a)
    = PASinglestarArg (name a)
    | PAPositionalArg (name a)

  argumentName (KeywordArgument (KAKeywordArg n _)) = n ^? name
  argumentName (DoublestarArgument (DADoublestarArg n)) = n ^? name
  argumentName (PositionalArgument a) =
    case a of
      PASinglestarArg n -> n ^? name
      PAPositionalArg n -> n ^? name

  arguments =
    \case
        ArgsListAll h t r _ ->
          fromArgslistarg h :
          toListOf (_Wrapped.folded._Wrapped.folded.to fromArgslistarg) t <>
          (toListOf (_Wrapped.folded._Wrapped.folded._Wrapped.folded) r >>=
           starOrDouble)
        ArgsListArgsKwargs a _ -> starOrDouble a
    where
      starOrDouble a =
        case a of
          InL a' -> fromStarpart a'
          InR a' ->
            pure .
            DoublestarArgument .
            DADoublestarArg $
            a' ^. to _argsListDoublestarArg_value._Wrapped.between'._2

      fromArgslistarg a =
        case getCompose $ _argsListArg_right a of
          Nothing ->
            PositionalArgument $ PAPositionalArg (_argsListArg_left a)
          Just r ->
            KeywordArgument $ KAKeywordArg (_argsListArg_left a) (r ^. _Wrapped.before._2)

      fromStarpart a =
        case a of
          ArgsListStarPartEmpty _ -> []
          ArgsListStarPart s d k _ ->
            (s ^. _Wrapped.before._2.to (PositionalArgument . PASinglestarArg)) :
            (d ^.. _Wrapped.folded._Wrapped.before._2.to fromArgslistarg) <>
            (k ^..
               _Wrapped.folded.
               _Wrapped.folded.
               to _argsListDoublestarArg_value.
               _Wrapped.between'._2.
               to (DoublestarArgument . DADoublestarArg))

data ArgsListStarPart ws name test a
  = ArgsListStarPartEmpty
  { _argsListStarPart_ann :: a
  }
  | ArgsListStarPart
  { _argsListStarPart_starred
    :: Compose
         (Before (Between' [ws] Asterisk))
         name
         a
  , _argsListStarPart_defaults
    :: Compose
         []
         (Compose
           (Before (Between' [ws] Comma))
           (ArgsListArg ws name test))
         a
  , _argsListStarPart_kwargs
    :: Compose
         Maybe
         (Compose
           (Before (Between' [ws] Comma))
           (ArgsListDoublestarArg ws name test))
         a
  , _argsListStarPart_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq ws, Eq1 test, Eq1 name, Eq a, Eq (name a)) => Eq (ArgsListStarPart ws name test a)
deriving instance (Show ws, Show1 test, Show1 name, Show a, Show (name a)) => Show (ArgsListStarPart ws name test a)

data ArgsListDoublestarArg ws name (test :: * -> *) a
  = ArgsListDoublestarArg
  { _argsListDoublestarArg_value
    :: Compose
         (Between' [ws])
         name
         a
  , _argsListDoublestarArg_ann :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''ArgsListStarPart
deriveOrd1 ''ArgsListStarPart
deriveShow1 ''ArgsListStarPart
makeLenses ''ArgsListStarPart

deriveEq1 ''ArgsListDoublestarArg
deriveOrd1 ''ArgsListDoublestarArg
deriveShow1 ''ArgsListDoublestarArg
makeLenses ''ArgsListDoublestarArg

deriveEq1 ''ArgsList
deriveOrd1 ''ArgsList
deriveShow1 ''ArgsList
makeLenses ''ArgsList

deriveEq1 ''ArgsListArg
deriveOrd1 ''ArgsListArg
deriveShow1 ''ArgsListArg
makeLenses ''ArgsListArg
