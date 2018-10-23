{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Internal.Syntax.UnOp
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.UnOp where

import Control.Lens.Lens (lens)
import Language.Python.Internal.Syntax.Whitespace

data UnOp a
  = Negate a [Whitespace]
  | Positive a [Whitespace]
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
