{-# language RankNTypes #-}
module Language.Python.Printer.Combinators where

import Papa hiding (Sum)
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Separated.After (After(..))
import Data.Separated.Before (Before(..))
import Data.Separated.Between (Between(..), Between'(..))
import Text.PrettyPrint hiding ((<>), comma, colon)

import Language.Python.AST.Symbols
import Language.Python.Printer.Symbols

sumElim :: (forall x. f x -> r) -> (forall x. g x -> r) -> Sum f g a -> r
sumElim f _ (InL a) = f a
sumElim _ g (InR a) = g a

tripled :: (Doc -> Doc) -> Doc -> Doc
tripled f = f . f . f

foldMapF :: (Foldable f, Monoid r) => (g a -> r) -> Compose f g a -> r
foldMapF f = foldMap f . getCompose

before :: Semigroup r => (s -> r) -> (a -> r) -> Before s a -> r
before f g (Before s a) = f s <> g a

beforeF
  :: Semigroup r
  => (s -> r)
  -> (forall x. f x -> r)
  -> Compose (Before s) f a
  -> r
beforeF f g = before f g . getCompose

after :: Semigroup r => (s -> r) -> (a -> r) -> After s a -> r
after f g (After s a) = g a <> f s

afterF
  :: Semigroup r
  => (s -> r)
  -> (forall x. f x -> r)
  -> Compose (After s) f a
  -> r
afterF f g = after f g . getCompose

between
  :: Semigroup r
  => (s -> r)
  -> (t -> r)
  -> (a -> r)
  -> Between s t a
  -> r
between f g h (Between s a t) = f s <> h a <> g t

betweenF
  :: Semigroup r
  => (s -> r)
  -> (t -> r)
  -> (forall x. f x -> r)
  -> Compose (Between s t) f a
  -> r
betweenF f g h = between f g h . getCompose

between'
  :: Semigroup r
  => (s -> r)
  -> (a -> r)
  -> Between' s a
  -> r
between' f g (Between' (Between s a t)) = f s <> g a <> f t


whitespaceAfterF
  :: Foldable g
  => (forall x. f x -> Doc)
  -> Compose (After (g WhitespaceChar)) f a
  -> Doc
whitespaceAfterF f = after (foldMap whitespaceChar) f . getCompose

whitespaceAfter
  :: Foldable g
  => (a -> Doc) -> After (g WhitespaceChar) a -> Doc
whitespaceAfter = after (foldMap whitespaceChar)

whitespaceBefore
  :: Foldable g
  => (a -> Doc)
  -> Before (g WhitespaceChar) a -> Doc
whitespaceBefore = before (foldMap whitespaceChar)

whitespaceBeforeF
  :: Foldable g
  => (forall x. f x -> Doc)
  -> Compose (Before (g WhitespaceChar)) f a
  -> Doc
whitespaceBeforeF f = before (foldMap whitespaceChar) f . getCompose

betweenWhitespace'F
  :: Foldable g
  => (forall x. f x -> Doc)
  -> Compose (Between' (g WhitespaceChar)) f a
  -> Doc
betweenWhitespace'F f = between' (foldMap whitespaceChar) f . getCompose

betweenWhitespace'
  :: Foldable f
  => (a -> Doc) -> Between' (f WhitespaceChar) a -> Doc
betweenWhitespace' = between' (foldMap whitespaceChar)
