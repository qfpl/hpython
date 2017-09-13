{-# language ScopedTypeVariables #-}
module Test.Language.Python.AST.TripleString where -- (tripleStringTests) where

import Papa

import Debug.Trace
import Prelude (error)

import Control.Monad
import Data.Maybe
import Hedgehog
import Language.Python.AST.EscapeSeq
import Language.Python.AST.Symbols
import Test.Language.Python.AST.Gen
  (genEscapeSeq, genTripleStringContentSingle, genTripleStringContentDouble)
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Shrink as Shrink
import qualified Hedgehog.Range as Range
import qualified Language.Python.AST.TripleString as TS

ascii :: MonadGen m => m Char
ascii = Gen.element [' '..'~']

prop_snoc_quote_fail :: Property
prop_snoc_quote_fail =
  property $ do
    content1 <-
      forAll $ genTripleStringContentSingle (Range.linear 0 10)
    TS.snoc content1 (Right '\'') === Nothing

    content2 <-
      forAll $ genTripleStringContentDouble (Range.linear 0 10)
    TS.snoc content2 (Right '"') === Nothing

prop_snoc_backslash_fail :: Property
prop_snoc_backslash_fail =
  property $ do
    content1 <-
      forAll $ genTripleStringContentSingle (Range.linear 0 10)
    TS.snoc content1 (Right '\\') === Nothing

    content2 <-
      forAll $ genTripleStringContentDouble (Range.linear 0 10)
    TS.snoc content2 (Right '\\') === Nothing

prop_cons_non_empty_succeeds :: Property
prop_cons_non_empty_succeeds = 
  property $ do
    c <- forAll $ Gen.choice [Left <$> genEscapeSeq, Right <$> ascii]
    content1 <-
      forAll $ genTripleStringContentSingle (Range.linear 1 10)
    assert . isJust $ TS.cons c content1

    content2 <-
      forAll $ genTripleStringContentDouble (Range.linear 1 10)
    assert . isJust $ TS.cons c content2

prop_print_parse_id :: Property
prop_print_parse_id =
  property $ do
    content1 <-
      forAll $ genTripleStringContentSingle (Range.linear 0 10)
    printParse content1 === Just content1

    content2 <-
      forAll $ genTripleStringContentDouble (Range.linear 0 10)
    printParse content2 === Just content2
  where
    printParse
      :: TS.TripleStringInside b
      => TS.TripleStringContent b Char
      -> Maybe (TS.TripleStringContent b Char)
    printParse s =
      (TS._TripleStringContent # s) ^? TS._TripleStringContent

prop_appending_nonempties_succeeds :: Property
prop_appending_nonempties_succeeds =
  property $ do
    content1 :: TS.TripleStringContent SingleQuote Char <-
      forAll $ genTripleStringContentSingle (Range.linear 1 10)
    content1' <-
      forAll $ genTripleStringContentSingle (Range.linear 1 10)
    assert . isJust $ content1 `TS.append` content1'

    content2 :: TS.TripleStringContent DoubleQuote Char <-
      forAll $ genTripleStringContentDouble (Range.linear 1 10)
    content2' <-
      forAll $ genTripleStringContentDouble (Range.linear 1 10)
    assert . isJust $ content2 `TS.append` content2'

prop_escapes_are_mushed :: Property
prop_escapes_are_mushed =
  property $ do
    esc <- review _Escape <$> forAll genEscapeSeq

    content1 <-
      forAll $ genTripleStringContentSingle (Range.linear 0 10)
    assert $ noEscape esc content1

    content1' <-
      forAll $ genTripleStringContentSingle (Range.linear 1 10)
    assert . noEscape esc . fromJust $ content1 `TS.append` content1'

    content2 <-
      forAll $ genTripleStringContentDouble (Range.linear 0 10)
    assert $ noEscape esc content2

    content2' <-
      forAll $ genTripleStringContentDouble (Range.linear 1 10)
    assert . noEscape esc . fromJust $ content2 `TS.append` content2'
  where
    getNonEscapes
      :: TS.TripleStringInside b
      => TS.TripleStringContent b Char
      -> String
    getNonEscapes = toListOf (TS.tripleStringContent . _Right)

    noEscape esc = not . isInfixOf ('\\' : esc) . getNonEscapes

prop_cons_backslash_changes_escape :: Property
prop_cons_backslash_changes_escape =
  property $ do
    esc <- forAll genEscapeSeq
    content1 <-
      forAll $ genTripleStringContentSingle (Range.linear 0 10)
    let
      content1' = TS.cons (Right '\\') =<< TS.cons (Left esc) content1
    case esc of
      Slash_backslash -> success
      Slash_singlequote -> success
      _ ->
        content1' ^? _Just . TS.tripleStringContent ===
          Just (Left Slash_backslash)

    content2 <-
      forAll $ genTripleStringContentDouble (Range.linear 0 10)
    let
      content2' = TS.cons (Right '\\') =<< TS.cons (Left esc) content2
    case esc of
      Slash_backslash -> success
      Slash_doublequote -> success
      _ ->
        content2' ^? _Just . TS.tripleStringContent ===
          Just (Left Slash_backslash)

prop_cons_backslash_fails :: Property
prop_cons_backslash_fails = 
  property $ do
    esc1 <- forAll $ Gen.element [Slash_backslash, Slash_singlequote]
    content1 <-
      forAll $ genTripleStringContentSingle (Range.linear 0 0)
    let
      content1' = TS.cons (Right '\\') =<< TS.cons (Left esc1) content1
    content1' === Nothing

    esc2 <- forAll $ Gen.element [Slash_backslash, Slash_doublequote]
    content2 <-
      forAll $ genTripleStringContentDouble (Range.linear 0 0)
    let
      content2' = TS.cons (Right '\\') =<< TS.cons (Left esc2) content2
    content2' === Nothing

tripleStringTests :: [TestTree]
tripleStringTests =
  [ testProperty
    "snoccing a quote will fail"
    prop_snoc_quote_fail
  , testProperty
    "snoccing a backslash will fail"
    prop_snoc_backslash_fail
  , testProperty
    "printing then parsing a TripleStringContent gives the original input"
    prop_print_parse_id
  , testProperty
    "consing to a non-empty string always succeeds"
    prop_cons_non_empty_succeeds
  , testProperty
    ("TripleStringContent never contains adjacent characters that form " <>
     "a valid escape sequence")
    prop_escapes_are_mushed
  , testProperty
    "Appending non-empty strings never fails"
    prop_appending_nonempties_succeeds
  , testProperty
    ("consing a '\\' onto a string starting with an escape sequence " <>
     "escapes the backslash implicitly contained in the escape sequence")
    prop_cons_backslash_changes_escape
  , testProperty
    ("consing a '\\' onto a string containing \"\\\\\", \"\\\"\", " <>
     "or \"\\\'\" fails")
    prop_cons_backslash_fails
  ]
