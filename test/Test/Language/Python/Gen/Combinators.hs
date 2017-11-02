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

genWhitespaceBeforeF
  :: MonadGen m
  => m (f a)
  -> m (Compose (Before [WhitespaceChar]) f a)
genWhitespaceBeforeF = fmap Compose . genWhitespaceBefore

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

genWhitespaceAfterF
  :: MonadGen m
  => m (f a)
  -> m (Compose (After [WhitespaceChar]) f a)
genWhitespaceAfterF = fmap Compose . genWhitespaceAfter

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

genBetweenWhitespace
  :: MonadGen m
  => m a
  -> m (Between' [WhitespaceChar] a)
genBetweenWhitespace = genBetween' genWhitespace

genBetweenWhitespaceF
  :: MonadGen m
  => m (f a)
  -> m (Compose (Between' [WhitespaceChar]) f a)
genBetweenWhitespaceF = fmap Compose . genBetweenWhitespace

genBetweenWhitespace1
  :: MonadGen m
  => m a
  -> m (Between' (NonEmpty WhitespaceChar) a)
genBetweenWhitespace1 = genBetween' genWhitespace1

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
