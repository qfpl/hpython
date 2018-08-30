{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances #-}
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Python.Internal.Syntax.Ident where

import Control.Lens.Lens (Lens, lens)
import Data.Char (isDigit, isLetter)
import Data.String (IsString(..))

import Language.Python.Internal.Optics.Validated (Validated)
import Language.Python.Internal.Syntax.Whitespace

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

instance HasTrailingWhitespace (Ident v a) where
  trailingWhitespace = lens (\(MkIdent _ _ ws) -> ws) (\(MkIdent a b _) ws -> MkIdent a b ws)

instance Validated Ident where
