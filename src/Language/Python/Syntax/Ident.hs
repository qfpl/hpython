{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
Module      : Language.Python.Syntax.Ident
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.Ident
  ( Ident(..)
    -- * Lenses
  , identAnn
  , identValue
  , identWhitespace
    -- * Extra functions
  , isIdentifierStart
  , isIdentifierChar
  )
where

import Control.Lens.Lens (Lens, lens)
import Data.Char (isDigit, isLetter)
import Data.String (IsString(..))

import Language.Python.Optics.Validated (Validated)
import Language.Python.Syntax.Raw
import Language.Python.Syntax.Whitespace

-- | An identifier. Like many types in hpython, it has an optional annotation
-- and tracks its trailing whitespace.
--
-- 'Raw' 'Ident's have an 'IsString' instance.
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#identifiers>
data Ident (v :: [*]) a
  = MkIdent
  { _identAnn :: a
  , _identValue :: String
  , _identWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Determine whether this character could start a valid identifier
isIdentifierStart :: Char -> Bool
isIdentifierStart = do
  a <- isLetter
  b <- (=='_')
  pure $ a || b

-- | Determine whether this character could be part of a valid identifier
isIdentifierChar :: Char -> Bool
isIdentifierChar = do
  a <- isIdentifierStart
  b <- isDigit
  pure $ a || b

instance IsString (Raw Ident) where
  fromString s = MkIdent () s []

identValue :: Lens (Ident v a) (Ident '[] a) String String
identValue = lens _identValue (\s a -> s { _identValue = a })

identAnn :: Lens (Ident v a) (Ident v a) a a
identAnn = lens _identAnn (\s a -> s { _identAnn = a })

identWhitespace :: Lens (Ident v a) (Ident v a) [Whitespace] [Whitespace]
identWhitespace = lens _identWhitespace (\s ws -> s { _identWhitespace = ws })

instance HasTrailingWhitespace (Ident v a) where
  trailingWhitespace = identWhitespace

instance Validated Ident where
