module Test.Language.Python.Gen.Combinators where

import Papa hiding (Space)
import Hedgehog
import Data.Functor.Compose
import Data.Separated.After (After(..))
import Data.Separated.Before (Before(..))
import Data.Separated.Between (Between(..), Between'(..))
import Language.Python.AST.Symbols

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genBefore :: MonadGen m => m s -> m a -> m (Before s a)
genBefore = liftA2 Before

genBefore1 :: MonadGen m => m s -> m a -> m (Before (NonEmpty s) a)
genBefore1 a b = liftA2 Before (Gen.nonEmpty (Range.linear 1 10) a) b

genAfter1 :: MonadGen m => m s -> m a -> m (After (NonEmpty s) a)
genAfter1 a b = liftA2 After (Gen.nonEmpty (Range.linear 1 10) a) b

genAfter1F :: MonadGen m => m s -> m (f a) -> m (Compose (After (NonEmpty s)) f a)
genAfter1F a b = Compose <$> genAfter1 a b

genBefore1F :: MonadGen m => m s -> m (f a) -> m (Compose (Before (NonEmpty s)) f a)
genBefore1F a b = Compose <$> genBefore1 a b

genBeforeF
  :: MonadGen m
  => m s
  -> m (f a)
  -> m (Compose (Before s) f a)
genBeforeF ms = fmap Compose . genBefore ms

genAfter
  :: MonadGen m
  => m s -> m a -> m (After s a)
genAfter = liftA2 After

genAfterF
  :: MonadGen m
  => m s
  -> m (f a)
  -> m (Compose (After s) f a)
genAfterF ms = fmap Compose . genAfter ms

genBetween
  :: MonadGen m
  => m s -> m t -> m a -> m (Between s t a)
genBetween ms mt ma = Between <$> ms <*> ma <*> mt

genBetweenF
  :: MonadGen m
  => m s
  -> m t
  -> m (f a)
  -> m (Compose (Between s t) f a)
genBetweenF ms mt = fmap Compose . genBetween ms mt

genBetween'
  :: MonadGen m
  => m s -> m a -> m (Between' s a)
genBetween' ms ma = Between' <$> genBetween ms ms ma

genBetween'F
  :: MonadGen m
  => m s -> m (f a) -> m (Compose (Between' s) f a)
genBetween'F ms ma = Compose <$> genBetween' ms ma

genListF
  :: MonadGen m
  => m (f a) -> m (Compose [] f a)
genListF ma =
  Compose <$>
  Gen.list (Range.linear 0 10) ma

genNonEmptyF
  :: MonadGen m
  => m (f a) -> m (Compose NonEmpty f a)
genNonEmptyF ma =
  Compose <$>
  Gen.nonEmpty (Range.linear 1 10) ma

genMaybeF
  :: MonadGen m
  => m (f a) -> m (Compose Maybe f a)
genMaybeF ma = Compose <$> Gen.maybe ma

genWhitespace1
  :: MonadGen m
  => m (NonEmpty WhitespaceChar)
genWhitespace1 = Gen.nonEmpty (Range.linear 1 10) genWhitespaceChar

genWhitespaceBefore
  :: MonadGen m
  => m a
  -> m (Before [WhitespaceChar] a)
genWhitespaceBefore ma = Before <$> genWhitespace <*> ma

genAnyWhitespaceBefore
  :: MonadGen m
  => m a
  -> m (Before [AnyWhitespaceChar] a)
genAnyWhitespaceBefore ma = Before <$> genAnyWhitespace <*> ma

genWhitespaceBeforeF
  :: MonadGen m
  => m (f a)
  -> m (Compose (Before [WhitespaceChar]) f a)
genWhitespaceBeforeF = fmap Compose . genWhitespaceBefore

genAnyWhitespaceBeforeF
  :: MonadGen m
  => m (f a)
  -> m (Compose (Before [AnyWhitespaceChar]) f a)
genAnyWhitespaceBeforeF = fmap Compose . genAnyWhitespaceBefore

genWhitespaceBefore1
  :: MonadGen m
  => m a
  -> m (Before (NonEmpty WhitespaceChar) a)
genWhitespaceBefore1 ma = Before <$> genWhitespace1 <*> ma

genWhitespaceBefore1F
  :: MonadGen m
  => m (f a)
  -> m (Compose (Before (NonEmpty WhitespaceChar)) f a)
genWhitespaceBefore1F = fmap Compose . genWhitespaceBefore1

genWhitespaceAfter
  :: MonadGen m
  => m a
  -> m (After [WhitespaceChar] a)
genWhitespaceAfter ma = After <$> genWhitespace <*> ma

genAnyWhitespaceAfter
  :: MonadGen m
  => m a
  -> m (After [AnyWhitespaceChar] a)
genAnyWhitespaceAfter ma = After <$> genAnyWhitespace <*> ma

genWhitespaceAfterF
  :: MonadGen m
  => m (f a)
  -> m (Compose (After [WhitespaceChar]) f a)
genWhitespaceAfterF = fmap Compose . genWhitespaceAfter

genAnyWhitespaceAfterF
  :: MonadGen m
  => m (f a)
  -> m (Compose (After [AnyWhitespaceChar]) f a)
genAnyWhitespaceAfterF = fmap Compose . genAnyWhitespaceAfter

genWhitespaceAfter1
  :: MonadGen m
  => m a
  -> m (After (NonEmpty WhitespaceChar) a)
genWhitespaceAfter1 ma = After <$> genWhitespace1 <*> ma

genWhitespaceAfter1F
  :: MonadGen m
  => m (f a)
  -> m (Compose (After (NonEmpty WhitespaceChar)) f a)
genWhitespaceAfter1F = fmap Compose . genWhitespaceAfter1

genWhitespace
  :: MonadGen m
  => m [WhitespaceChar]
genWhitespace = Gen.list (Range.linear 0 10) genWhitespaceChar

genAnyWhitespace
  :: MonadGen m
  => m [AnyWhitespaceChar]
genAnyWhitespace = Gen.list (Range.linear 0 10) genAnyWhitespaceChar

genBetweenWhitespace
  :: MonadGen m
  => m a
  -> m (Between' [WhitespaceChar] a)
genBetweenWhitespace = genBetween' genWhitespace

genBetweenAnyWhitespace
  :: MonadGen m
  => m a
  -> m (Between' [AnyWhitespaceChar] a)
genBetweenAnyWhitespace = genBetween' genAnyWhitespace

genBetweenWhitespaceF
  :: MonadGen m
  => m (f a)
  -> m (Compose (Between' [WhitespaceChar]) f a)
genBetweenWhitespaceF = fmap Compose . genBetweenWhitespace

genBetween'1
  :: MonadGen m
  => m ws
  -> m a
  -> m (Between' (NonEmpty ws) a)
genBetween'1 ws = genBetween' (Gen.nonEmpty (Range.linear 1 10) ws)

genBetweenWhitespace1
  :: MonadGen m
  => m a
  -> m (Between' (NonEmpty WhitespaceChar) a)
genBetweenWhitespace1 = genBetween'1 genWhitespaceChar

genBetweenWhitespace1F
  :: MonadGen m
  => m (f a)
  -> m (Compose (Between' (NonEmpty WhitespaceChar)) f a)
genBetweenWhitespace1F = fmap Compose . genBetweenWhitespace1

genNewlineChar
  :: MonadGen m
  => m NewlineChar
genNewlineChar =
  Gen.element [CR, LF, CRLF]

genIndentationChar
  :: MonadGen m
  => m IndentationChar
genIndentationChar =
  Gen.recursive
    Gen.choice
    [ pure IndentSpace, pure IndentTab ]
    [ IndentContinued <$>
      genNewlineChar <*>
      Gen.list (Range.linear 0 10) genIndentationChar
    ]

genWhitespaceChar
  :: MonadGen m
  => m WhitespaceChar
genWhitespaceChar =
  Gen.choice
    [ pure Space
    , pure Tab
    , Continued <$> genNewlineChar
    ]

genAnyWhitespaceChar
  :: MonadGen m
  => m AnyWhitespaceChar
genAnyWhitespaceChar =
  Gen.choice
    [ Left <$> genWhitespaceChar
    , Right <$> genNewlineChar
    ]
