{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Syntax.Operator.Unary
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Unary operators
-}

module Language.Python.Syntax.Operator.Unary where

import Control.Lens.Lens (lens)
import Language.Python.Syntax.Whitespace

-- | An 'UnOp' is a unary operator in Python, such as @-@ for negation.
-- An operator is stored with an annotation and its trailing whitespace.
data UnOp a
  -- | @-a@
  = Negate a [Whitespace]
  -- | @+a@
  | Positive a [Whitespace]
  -- | @~a@
  | Complement a [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable)

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
