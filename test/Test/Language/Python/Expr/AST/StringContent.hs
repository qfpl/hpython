{-# language ScopedTypeVariables #-}
module Test.Language.Python.Expr.AST.StringContent (stringContentTests) where

import Papa

import Control.Monad
import Data.Maybe
import Hedgehog
import Language.Python.AST.Symbols
import Language.Python.Expr.AST.EscapeSeq
import Test.Language.Python.Expr.Gen
  (genEscapeSeq, genStringContentSingle, genStringContentDouble)
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Shrink as Shrink
import qualified Hedgehog.Range as Range
import qualified Language.Python.Expr.AST.StringContent as SC

ascii :: MonadGen m => m Char
ascii = Gen.element [' '..'~']

prop_snoc_quote_fail :: Property
prop_snoc_quote_fail =
  property $ do
    content1 <-
      forAll $ genStringContentSingle (Range.linear 0 10)
    SC.snoc content1 (Right '\'') === Nothing

    content2 <-
      forAll $ genStringContentDouble (Range.linear 0 10)
    SC.snoc content2 (Right '"') === Nothing

prop_snoc_backslash_fail :: Property
prop_snoc_backslash_fail =
  property $ do
    content1 <-
      forAll $ genStringContentSingle (Range.linear 0 10)
    SC.snoc content1 (Right '\\') === Nothing

    content2 <-
      forAll $ genStringContentDouble (Range.linear 0 10)
    SC.snoc content2 (Right '\\') === Nothing

prop_cons_non_empty_succeeds :: Property
prop_cons_non_empty_succeeds = 
  property $ do
    c <- forAll $ Gen.choice [Left <$> genEscapeSeq, Right <$> ascii]
    content1 <-
      forAll $ genStringContentSingle (Range.linear 1 10)
    assert . isJust $ SC.cons c content1

    content2 <-
      forAll $ genStringContentDouble (Range.linear 1 10)
    assert . isJust $ SC.cons c content2

prop_print_parse_id :: Property
prop_print_parse_id =
  property $ do
    content1 <-
      forAll $ genStringContentSingle (Range.linear 0 10)
    printParse content1 === Just content1

    content2 <-
      forAll $ genStringContentDouble (Range.linear 0 10)
    printParse content2 === Just content2
  where
    printParse
      :: SC.StringInside b
      => SC.StringContent b Char
      -> Maybe (SC.StringContent b Char)
    printParse s =
      (SC._StringContent # s) ^? SC._StringContent

prop_appending_nonempties_succeeds :: Property
prop_appending_nonempties_succeeds =
  property $ do
    content1 :: SC.StringContent SingleQuote Char <-
      forAll $ genStringContentSingle (Range.linear 1 10)
    content1' <-
      forAll $ genStringContentSingle (Range.linear 1 10)
    assert . isJust $ content1 `SC.append` content1'

    content2 :: SC.StringContent DoubleQuote Char <-
      forAll $ genStringContentDouble (Range.linear 1 10)
    content2' <-
      forAll $ genStringContentDouble (Range.linear 1 10)
    assert . isJust $ content2 `SC.append` content2'

prop_escapes_are_mushed :: Property
prop_escapes_are_mushed =
  property $ do
    esc <- review _Escape <$> forAll genEscapeSeq

    content1 <-
      forAll $ genStringContentSingle (Range.linear 0 10)
    assert $ noEscape esc content1

    content1' <-
      forAll $ genStringContentSingle (Range.linear 1 10)
    assert . noEscape esc . fromJust $ content1 `SC.append` content1'

    content2 <-
      forAll $ genStringContentDouble (Range.linear 0 10)
    assert $ noEscape esc content2

    content2' <-
      forAll $ genStringContentDouble (Range.linear 1 10)
    assert . noEscape esc . fromJust $ content2 `SC.append` content2'
  where
    getNonEscapes
      :: SC.StringInside b
      => SC.StringContent b Char
      -> String
    getNonEscapes = toListOf (SC.stringContent . _Right)

    noEscape esc = not . isInfixOf ('\\' : esc) . getNonEscapes

prop_cons_backslash_changes_escape :: Property
prop_cons_backslash_changes_escape =
  property $ do
    esc <- forAll genEscapeSeq
    content1 <-
      forAll $ genStringContentSingle (Range.linear 0 10)
    let
      content1' = SC.cons (Right '\\') =<< SC.cons (Left esc) content1
    case esc of
      Slash_backslash -> success
      Slash_singlequote -> success
      _ ->
        content1' ^? _Just . SC.stringContent ===
          Just (Left Slash_backslash)

    content2 <-
      forAll $ genStringContentDouble (Range.linear 0 10)
    let
      content2' = SC.cons (Right '\\') =<< SC.cons (Left esc) content2
    case esc of
      Slash_backslash -> success
      Slash_doublequote -> success
      _ ->
        content2' ^? _Just . SC.stringContent ===
          Just (Left Slash_backslash)

prop_cons_backslash_fails :: Property
prop_cons_backslash_fails = 
  property $ do
    esc1 <- forAll $ Gen.element [Slash_backslash, Slash_singlequote]
    content1 <-
      forAll $ genStringContentSingle (Range.linear 0 0)
    let
      content1' = SC.cons (Right '\\') =<< SC.cons (Left esc1) content1
    content1' === Nothing

    esc2 <- forAll $ Gen.element [Slash_backslash, Slash_doublequote]
    content2 <-
      forAll $ genStringContentDouble (Range.linear 0 0)
    let
      content2' = SC.cons (Right '\\') =<< SC.cons (Left esc2) content2
    content2' === Nothing

stringContentTests :: [TestTree]
stringContentTests =
  [ testProperty
    "snoccing a quote will fail"
    prop_snoc_quote_fail
  , testProperty
    "snoccing a backslash will fail"
    prop_snoc_backslash_fail
  , testProperty
    "printing then parsing a StringContent gives the original input"
    prop_print_parse_id
  , testProperty
    "consing to a non-empty string always succeeds"
    prop_cons_non_empty_succeeds
  , testProperty
    ("StringContent never contains adjacent characters that form " <>
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
