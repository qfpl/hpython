{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Syntax.Operator.Unary
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Unary operators
-}

module Language.Python.Syntax.Operator.Unary where

import Control.Lens.Lens (Lens', lens)
import Data.Generics.Product.Typed (typed)
import GHC.Generics (Generic)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Whitespace

-- | An 'UnOp' is a unary operator in Python, such as @-@ for negation.
-- An operator is stored with an annotation and its trailing whitespace.
data UnOp a
  -- | @-a@
  = Negate (Ann a) [Whitespace]
  -- | @+a@
  | Positive (Ann a) [Whitespace]
  -- | @~a@
  | Complement (Ann a) [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn UnOp where
  annot :: forall a. Lens' (UnOp a) (Ann a)
  annot = typed @(Ann a)

instance HasTrailingWhitespace (UnOp a) where
  trailingWhitespace =
    lens
      (\case
         Negate _ a -> a
         Positive _ a -> a
         Complement _ a -> a)
      (\op ws ->
         case op of
           Negate a _ -> Negate a ws
           Positive a _ -> Positive a ws
           Complement a _ -> Complement a ws)
