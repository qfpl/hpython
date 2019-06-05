{-# language DataKinds, KindSignatures #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Ident
Copyright   : (C) CSIRO 2017-2019
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

import Control.Lens.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Data.Char (isDigit, isLetter)
import Data.Generics.Product.Typed (typed)
import Data.String (IsString(..))
import GHC.Generics (Generic)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Whitespace

-- | An identifier. Like many types in hpython, it has an optional annotation
-- and tracks its trailing whitespace.
--
-- 'Raw' 'Ident's have an 'IsString' instance.
--
-- See <https://docs.python.org/3.5/reference/lexical_analysis.html#identifiers>
data Ident a
  = MkIdent
  { _identAnn :: Ann a
  , _identValue :: String
  , _identWhitespace :: [Whitespace]
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

makeLenses ''Ident

instance HasAnn Ident where
  annot :: forall a. Lens' (Ident a) (Ann a)
  annot = typed @(Ann a)

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

instance IsString (Ident ()) where
  fromString s = MkIdent (Ann ()) s []

instance HasTrailingWhitespace (Ident a) where
  trailingWhitespace = identWhitespace
