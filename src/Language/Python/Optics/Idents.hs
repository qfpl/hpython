{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables, TypeApplications #-}

{-|
Module      : Language.Python.Optics.Idents
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Optics.Idents (HasIdents(..)) where

import Control.Lens.Iso (iso)
import Control.Lens.Traversal (Traversal')
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics

import Language.Python.Syntax.Ann
import Language.Python.Syntax.AugAssign
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comment
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Import
import Language.Python.Syntax.Module
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace

-- | 'Traversal' that targets all the 'Ident's in a structure
class HasIdents s a | s -> a where
  _Idents :: Traversal' s (Ident a)
  default _Idents
    :: forall l. (Generic s, Rep s ~ l, GHasIdents l a)
    => Traversal' s (Ident a)
  _Idents = iso from to . gidents @l @a

instance HasIdents (CompFor a) a
instance HasIdents (Expr a) a
instance HasIdents (Param a) a
instance HasIdents (e a) a => HasIdents (Comprehension e a) a
instance HasIdents (DictItem a) a
instance HasIdents (SetItem a) a
instance HasIdents (CompIf a) a
instance HasIdents (TupleItem a) a
instance HasIdents (ListItem a) a
instance HasIdents (Subscript a) a
instance HasIdents (Arg a) a
instance HasIdents (Ident a) a where; _Idents = id
instance HasIdents (n a) a => HasIdents (ImportAs n a) a
instance HasIdents (ImportTargets a) a
instance HasIdents (RelativeModuleName a) a
instance HasIdents (ModuleName a) a
instance HasIdents (SimpleStatement a) a
instance HasIdents (SmallStatement a) a
instance HasIdents (Decorator a) a
instance HasIdents (Block a) a
instance HasIdents (Suite a) a
instance HasIdents (ExceptAs a) a
instance HasIdents (WithItem a) a
instance HasIdents (CompoundStatement a) a
instance HasIdents (Statement a) a
instance HasIdents (Module a) a

class GHasIdents s a where
  gidents :: Traversal' (s x) (Ident a)

instance (GHasIdents a x, GHasIdents b x) => GHasIdents (a :+: b) x where
  gidents f (L1 a) = L1 <$> gidents f a
  gidents f (R1 a) = R1 <$> gidents f a

instance (GHasIdents a x, GHasIdents b x) => GHasIdents (a :*: b) x where
  gidents f (a :*: b) = (:*:) <$> gidents f a <*> gidents f b

instance GHasIdents U1 x where
  gidents _ U1 = pure U1

instance GHasIdents V1 x where
  gidents _ !_ = undefined

instance GHasIdents a x => GHasIdents (M1 i t a) x where
  gidents f (M1 a) = M1 <$> gidents f a

instance HasIdents' a x => GHasIdents (K1 i a) x where
  gidents f (K1 a) = K1 <$> _Idents' f a

class HasIdents' s a where; _Idents' :: Traversal' s (Ident a)

instance {-# overlappable #-} HasIdents s a => HasIdents' s a where; _Idents' = _Idents

instance {-# overlapping #-} HasIdents' Whitespace a where; _Idents' _ = pure
instance {-# overlapping #-} HasIdents' Newline a where; _Idents' _ = pure
instance {-# overlapping #-} HasIdents' Colon a where; _Idents' _ = pure
instance {-# overlapping #-} HasIdents' At a where; _Idents' _ = pure
instance {-# overlapping #-} HasIdents' Dot a where; _Idents' _ = pure
instance {-# overlapping #-} HasIdents' Equals a where; _Idents' _ = pure
instance {-# overlapping #-} HasIdents' Comma a where; _Idents' _ = pure
instance {-# overlapping #-} HasIdents' Bool a where; _Idents' _ = pure
instance {-# overlapping #-} HasIdents' a x => HasIdents' [a] x where; _Idents' = traverse._Idents'
instance {-# overlapping #-} HasIdents' a x => HasIdents' (NonEmpty a) x where; _Idents' = traverse._Idents'
instance {-# overlapping #-} HasIdents' a x => HasIdents' (Maybe a) x where; _Idents' = traverse._Idents'
instance {-# overlapping #-} (HasIdents' a x, HasIdents' b x) => HasIdents' (a, b) x where
  _Idents' f (a, b) = (,) <$> _Idents' f a <*> _Idents' f b
instance
  {-# overlapping #-}
  (HasIdents' a x, HasIdents' b x, HasIdents' c x) => HasIdents' (a, b, c) x where
  _Idents' f (a, b, c) =
    (,,) <$>
    _Idents' f a <*>
    _Idents' f b <*>
    _Idents' f c
instance
  {-# overlapping #-}
  (HasIdents' a x, HasIdents' b x, HasIdents' c x, HasIdents' d x) => HasIdents' (a, b, c, d) x where
  _Idents' f (a, b, c, d) =
    (,,,) <$>
    _Idents' f a <*>
    _Idents' f b <*>
    _Idents' f c <*>
    _Idents' f d
instance (HasIdents' a x, HasIdents' c x) => HasIdents' (Either a c) x where
  _Idents' f (Left a) = Left <$> _Idents' f a
  _Idents' f (Right a) = Right <$>_Idents' f a

instance HasIdents (Ann a) a where; _Idents _ = pure
instance HasIdents a x => HasIdents (CommaSep a) x where; _Idents = traverse._Idents
instance HasIdents (BinOp a) a where; _Idents _ = pure
instance HasIdents (IntLiteral a) a where; _Idents _ = pure
instance HasIdents (FloatLiteral a) a where; _Idents _ = pure
instance HasIdents (ImagLiteral a) a where; _Idents _ = pure
instance HasIdents (StringLiteral a) a where; _Idents _ = pure
instance HasIdents (UnOp a) a where; _Idents _ = pure
instance HasIdents (AugAssign a) a where; _Idents _ = pure
instance HasIdents (Blank a) a where; _Idents _ = pure
instance HasIdents (Semicolon a) a where; _Idents _ = pure
instance HasIdents (Comment a) a where; _Idents _ = pure
instance HasIdents (Indents a) a where; _Idents _ = pure
instance HasIdents a x => HasIdents (CommaSep1 a) x where; _Idents = traverse._Idents
instance HasIdents a x => HasIdents (CommaSep1' a) x where; _Idents = traverse._Idents
instance (HasIdents a x, HasIdents c x) => HasIdents (Either a c) x where
  _Idents f (Left a) = Left <$> _Idents f a
  _Idents f (Right a) = Right <$>_Idents f a