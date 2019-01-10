{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}

{-|
Module      : Language.Python.Syntax.AugAssign
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.AugAssign where

import Control.Lens.Lens (Lens', lens)
import Data.Generics.Product.Typed (typed)
import GHC.Generics

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Whitespace

-- | Augmented assignments (PEP 203), such as:
--
-- @
-- x += y
-- @
--
-- or
--
-- @
-- x <<= 8
-- @
--
-- An 'AugAssign' has an 'AugAssignOp' and trailing whitespace. There is an
-- optional annotation, which can simply be @()@ if no annotation is desired.
data AugAssign a
  = MkAugAssign
  { _augAssignAnn :: Ann a
  , _augAssignType :: AugAssignOp
  , _augAssignWhitespace :: [Whitespace]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn AugAssign where
  annot :: forall a. Lens' (AugAssign a) (Ann a)
  annot = typed @(Ann a)

instance HasTrailingWhitespace (AugAssign a) where
  trailingWhitespace =
    lens _augAssignWhitespace (\a b -> a { _augAssignWhitespace = b })

-- | Augmented assignment operators
data AugAssignOp
  -- | @+=@
  = PlusEq
  -- | @-=@
  | MinusEq
  -- | @*=@
  | StarEq
  -- | @\@=@
  | AtEq
  -- | @/=@
  | SlashEq
  -- | @%=@
  | PercentEq
  -- | @&=@
  | AmpersandEq
  -- | @|=@
  | PipeEq
  -- | @^=@
  | CaretEq
  -- | @<<=@
  | ShiftLeftEq
  -- | @>>=@
  | ShiftRightEq
  -- | @**=@
  | DoubleStarEq
  -- | @//=@
  | DoubleSlashEq
  deriving (Eq, Show)
