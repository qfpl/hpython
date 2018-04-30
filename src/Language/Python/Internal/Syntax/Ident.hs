{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Python.Internal.Syntax.Ident where

import Control.Lens.Lens (Lens, lens)
import Data.Coerce (coerce)
import Data.String (IsString(..))

import Language.Python.Internal.Syntax.Token
import Language.Python.Internal.Syntax.Whitespace

data Ident (v :: [*]) a
  = MkIdent
  { _identAnnotation :: a
  , _identValue :: String
  , _identWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance IsString (Ident '[] ()) where
  fromString s = MkIdent () s []

identValue :: Lens (Ident v a) (Ident '[] a) String String
identValue = lens _identValue (\s a -> s { _identValue = a })

identAnnotation :: Lens (Ident v a) (Ident v a) a a
identAnnotation = lens _identAnnotation (\s a -> s { _identAnnotation = a })

instance Token (Ident v a) (Ident '[] a) where
  unvalidate = coerce
  startChar (MkIdent _ a _) = head a
  endChar (MkIdent _ a _) = last a
  whitespaceAfter = lens (\(MkIdent _ _ ws) -> ws) (\(MkIdent a b _) ws -> MkIdent a b ws)
