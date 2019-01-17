{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language TemplateHaskell #-}
module Language.Python35.Syntax.Digits where

import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic, Generic1)

newtype Digits a = Digits { unDigits :: NonEmpty a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
deriveEq1 ''Digits
deriveOrd1 ''Digits
deriveShow1 ''Digits

newtype DigitsTail a = DigitsTail { unDigitsTail :: [a] }
  deriving (Eq, Ord, Show, Generic)

toDigits :: NonEmpty a -> Digits a
toDigits = Digits

fromDigits :: Digits a -> NonEmpty a
fromDigits = unDigits

consDigits :: a -> DigitsTail a -> Digits a
consDigits a b = Digits $ a :| unDigitsTail b

showDigits :: (a -> Char) -> Digits a -> String
showDigits f = foldr (\a b -> f a : b) [] . unDigits
