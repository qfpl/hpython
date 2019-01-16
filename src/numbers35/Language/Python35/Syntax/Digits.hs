{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language TemplateHaskell #-}
module Language.Python35.Syntax.Digits where

import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

newtype Digits a = Digits { unDigits :: NonEmpty a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
deriveEq1 ''Digits
deriveOrd1 ''Digits
deriveShow1 ''Digits

toDigits :: NonEmpty a -> Digits a
toDigits = Digits

fromDigits :: Digits a -> NonEmpty a
fromDigits = unDigits

showDigits :: (a -> Char) -> Digits a -> String
showDigits f = foldr (\a b -> f a : b) [] . unDigits
