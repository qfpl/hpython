{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.VarargsList
  ( HasArgs(..)
  , DuplicateArgumentsError(..)
  , VarargsList
  , mkVarargsListAll
  , mkVarargsListArgsKwargs
  , _VarargsListAll
  , _VarargsListArgsKwargs
  , _varargsListAll_head
  , _varargsListAll_tail
  , _varargsListAll_rest
  , _varargsListArgsKwargs_value
  , _varargsList_ann
  , varargsListAll_head
  , varargsListAll_tail
  , varargsListAll_rest
  , varargsListArgsKwargs_value
  , varargsList_ann
  , VarargsListStarPart(..)
  , varargsListStarPart_ann
  , varargsListStarPart_starred
  , varargsListStarPart_defaults
  , varargsListStarPart_kwargs
  , VarargsListDoublestarArg(..)
  , varargsListDoublestarArg_value
  , varargsListDoublestarArg_ann
  , VarargsListArg(..)
  , varargsListArg_left
  , varargsListArg_right
  , varargsListArg_ann
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

import Language.Python.AST.Identifier
import Language.Python.AST.Symbols

newtype DuplicateArgumentsError a
  = DuplicateArgumentsError [Identifier a]
  deriving (Eq, Show)

duplicates :: (a -> a -> Bool) -> [a] -> [a]
duplicates eq [] = []
duplicates eq [a] = []
duplicates eq (a:as) =
  let (yes, no) = partition (eq a) as
  in
    if null yes
    then duplicates eq no
    else a : duplicates eq no

class HasArgs s where
  args :: Fold (s a) (Identifier a)

instance HasArgs (VarargsListArg test) where
  args f (VarargsListArg a b ann) =
    VarargsListArg <$> f a <*> pure b <*> pure ann

instance HasArgs VarargsListDoublestarArg where
  args f (VarargsListDoublestarArg a ann) =
    VarargsListDoublestarArg <$>
    traverseOf (_Wrapped.between'._2) f a <*>
    pure ann

instance HasArgs (VarargsListStarPart test) where
  args f (VarargsListStarPart a b c ann) =
    VarargsListStarPart <$>
    traverseOf (_Wrapped.before._2) f a <*>
    traverseOf (_Wrapped.traverse._Wrapped.before._2.args) f b <*>
    traverseOf (_Wrapped.traverse._Wrapped.before._2.args) f c <*>
    pure ann
  args f (VarargsListStarPartEmpty ann) = pure $ VarargsListStarPartEmpty ann

mkVarargsListAll
  :: VarargsListArg test a
  -> Compose
       []
       (Compose
         (Before
           (Between' [WhitespaceChar] Comma))
           (VarargsListArg test))
       a
  -> Compose
       Maybe
       (Compose
         (Before
           (Between' [WhitespaceChar] Comma))
           (Compose
             Maybe
             (Sum
               (VarargsListStarPart test)
               (VarargsListDoublestarArg))))
       a
  -> a
  -> Either (DuplicateArgumentsError a) (VarargsList test a)
mkVarargsListAll a bs c ann =
  let allArgs =
        (a ^.. args) <>
        (bs ^.. _Wrapped.folded._Wrapped.before._2.args) <>
        (c ^.. _Wrapped.folded._Wrapped.before._2.
               _Wrapped.folded.failing (_InL.args) (_InR.args))
  in case duplicates ((==) `on` _identifier_value) allArgs of
    [] -> Right $ VarargsListAll a bs c ann
    dups -> Left $ DuplicateArgumentsError dups

_VarargsListAll
  :: Prism'
       (Maybe (VarargsList test a))
       ( VarargsListArg test a
       , Compose
           []
           (Compose
             (Before
               (Between' [WhitespaceChar] Comma))
               (VarargsListArg test))
           a
       , Compose
           Maybe
           (Compose
             (Before
               (Between' [WhitespaceChar] Comma))
               (Compose
                  Maybe
                  (Sum
                    (VarargsListStarPart test)
                    VarargsListDoublestarArg)))
           a
       , a
       )
_VarargsListAll =
  prism'
    (\(a, b, c, d) -> case mkVarargsListAll a b c d of
        Right a' -> Just a'
        Left _ -> Nothing)
    (\v -> do
        v' <- v
        case v' of
          VarargsListAll a b c d -> pure (a, b, c, d)
          _ -> Nothing)

mkVarargsListArgsKwargs
  :: Sum
       (VarargsListStarPart test)
       VarargsListDoublestarArg
       a
  -> a
  -> Either (DuplicateArgumentsError a) (VarargsList test a)
mkVarargsListArgsKwargs a ann =
  let allArgs =
        a ^.. failing (_InL.args) (_InR.args)
  in case duplicates ((==) `on` _identifier_value) allArgs of
    [] -> Right $ VarargsListArgsKwargs a ann
    dups -> Left $ DuplicateArgumentsError dups

_VarargsListArgsKwargs
  :: Prism'
       (Maybe (VarargsList test a))
       ( Sum
           (VarargsListStarPart test)
           VarargsListDoublestarArg
           a
       , a
       )
_VarargsListArgsKwargs =
  prism'
    (\(a, b) -> case mkVarargsListArgsKwargs a b of
        Right a' -> Just a'
        Left _ -> Nothing)
    (\v -> do
        v' <- v
        case v' of
          VarargsListArgsKwargs a b -> pure (a, b)
          _ -> Nothing)

data VarargsList test a
  = VarargsListAll
  { _varargsListAll_head :: VarargsListArg test a
  , _varargsListAll_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (VarargsListArg test))
         a
  , _varargsListAll_rest
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Compose
             Maybe
             (Sum
               (VarargsListStarPart test)
               VarargsListDoublestarArg)))
         a
  , _varargsList_ann :: a
  }
  | VarargsListArgsKwargs
  { _varargsListArgsKwargs_value
      :: Sum
           (VarargsListStarPart test)
           VarargsListDoublestarArg
           a
  , _varargsList_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 test, Eq a) => Eq (VarargsList test a)
deriving instance (Show1 test, Show a) => Show (VarargsList test a)

data VarargsListStarPart test a
  = VarargsListStarPartEmpty
  { _varargsListStarPart_ann :: a
  }
  | VarargsListStarPart
  { _varargsListStarPart_starred
    :: Compose
         (Before (Between' [WhitespaceChar] Asterisk))
         Identifier
         a
  , _varargsListStarPart_defaults
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (VarargsListArg test))
         a
  , _varargsListStarPart_kwargs
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           VarargsListDoublestarArg)
         a
  , _varargsListStarPart_ann :: a
  }
  deriving (Functor, Foldable, Traversable)
deriving instance (Eq1 test, Eq a) => Eq (VarargsListStarPart test a)
deriving instance (Show1 test, Show a) => Show (VarargsListStarPart test a)

data VarargsListDoublestarArg a
  = VarargsListDoublestarArg
  { _varargsListDoublestarArg_value
    :: Compose
         (Between' [WhitespaceChar])
         Identifier
         a
  , _varargsListDoublestarArg_ann :: a
  }
  deriving (Eq, Functor, Foldable, Show, Traversable)

data VarargsListArg test a
  = VarargsListArg
  { _varargsListArg_left :: Identifier a
  , _varargsListArg_right
    :: Compose
         Maybe
         (Compose
           (Before (Between' [WhitespaceChar] Equals))
           test)
         a
  , _varargsListArg_ann :: a
  }
  deriving (Eq, Functor, Foldable, Show, Traversable)

deriveEq1 ''VarargsListArg
deriveShow1 ''VarargsListArg
makeLenses ''VarargsListArg

deriveEq1 ''VarargsListStarPart
deriveShow1 ''VarargsListStarPart
makeLenses ''VarargsListStarPart

deriveEq1 ''VarargsListDoublestarArg
deriveShow1 ''VarargsListDoublestarArg
makeLenses ''VarargsListDoublestarArg

deriveEq1 ''VarargsList
deriveShow1 ''VarargsList
makeLenses ''VarargsList
