{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
module Language.Python36.Syntax.Digits where

import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic, Generic1)

import Language.Python.Syntax.Punctuation

data Digits a = Digits a [(Maybe Underscore, a)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
deriveEq1 ''Digits
deriveOrd1 ''Digits
deriveShow1 ''Digits

newtype DigitsTail a = DigitsTail { unDigitsTail :: [(Maybe Underscore, a)] }
  deriving (Eq, Ord, Show, Generic)

toDigits :: NonEmpty a -> Digits a
toDigits (a:|as) = Digits a $ (Nothing ,) <$> as

fromDigits :: Digits a -> NonEmpty a
fromDigits (Digits a as) = a :| fmap snd as

consDigits :: a -> DigitsTail a -> Digits a
consDigits a b = Digits a $ unDigitsTail b

showDigits :: (a -> Char) -> Digits a -> String
showDigits f (Digits a b) =
  f a : foldMap (\(c, d) -> maybe "" (const "_") c <> [f d]) b
