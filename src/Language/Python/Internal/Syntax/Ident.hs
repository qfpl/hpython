{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
Module      : Language.Python.Internal.Syntax.Ident
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.Ident where

import Control.Lens.Lens (Lens, lens)
import Data.Char (isDigit, isLetter)
import Data.String (IsString(..))

import Language.Python.Optics.Validated (Validated)
import Language.Python.Syntax.Whitespace

data Ident (v :: [*]) a
  = MkIdent
  { _identAnn :: a
  , _identValue :: String
  , _identWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

isIdentifierStart :: Char -> Bool
isIdentifierStart = do
  a <- isLetter
  b <- (=='_')
  pure $ a || b

isIdentifierChar :: Char -> Bool
isIdentifierChar = do
  a <- isIdentifierStart
  b <- isDigit
  pure $ a || b

instance IsString (Ident '[] ()) where
  fromString s = MkIdent () s []

identValue :: Lens (Ident v a) (Ident '[] a) String String
identValue = lens _identValue (\s a -> s { _identValue = a })

identAnn :: Lens (Ident v a) (Ident v a) a a
identAnn = lens _identAnn (\s a -> s { _identAnn = a })

identWhitespace :: Lens (Ident v a) (Ident v a) [Whitespace] [Whitespace]
identWhitespace = lens _identWhitespace (\s a -> s { _identWhitespace = a })

instance HasTrailingWhitespace (Ident v a) where
  trailingWhitespace = lens (\(MkIdent _ _ ws) -> ws) (\(MkIdent a b _) ws -> MkIdent a b ws)

instance Validated Ident where
