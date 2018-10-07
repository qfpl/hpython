{-# language OverloadedStrings #-}
module Printer (printerTests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty(..))

import Language.Python.Internal.Render
import Language.Python.Internal.Render.Correction
import Language.Python.Internal.Syntax.Expr
import Language.Python.Internal.Syntax.Strings

import Generators.Common

printerTests :: Group
printerTests =
  Group "Printer tests"
  [ ("Printer test 1", withTests 1 test_1)
  , ("Printer test 2", withTests 1 test_2)
  , ("Printer test 3", withTests 1 test_3)
  , ("Printer test 4", test_4)
  , ("Printer test 5", test_5)
  ]

test_1 :: Property
test_1 =
  property $ do
    let
      e1 =
        String () $
        StringLiteral () Nothing ShortString SingleQuote [] [] :|
        [StringLiteral () Nothing ShortString SingleQuote [] []]

    showExpr e1 === "'' ''"

    let
      e2 =
        String () $
        StringLiteral () Nothing ShortString DoubleQuote [] [] :|
        [StringLiteral () Nothing ShortString DoubleQuote [] []]

    showExpr e2 === "\"\" \"\""

    let
      e3 =
        String () $
        StringLiteral () Nothing ShortString SingleQuote [] [] :|
        [StringLiteral () Nothing ShortString DoubleQuote [] []]

    showExpr e3 === "''\"\""

    let
      e4 =
        String () $
        StringLiteral () Nothing ShortString SingleQuote [] [] :|
        [StringLiteral () (Just Prefix_u) ShortString SingleQuote [] []]

    showExpr e4 === "''u''"

test_2 :: Property
test_2 =
  property $ do
    let
      e1 = [Char_lit '\\']

    correctInitialFinalBackslashes e1 === [Char_esc_bslash]

test_3 :: Property
test_3 =
  property $ do
    let
      e2 = [Char_lit '\\', Char_lit ' ']

    correctInitialFinalBackslashes e2 === [Char_lit '\\', Char_lit ' ']

test_4 :: Property
test_4 =
  property $ do
    ls <- forAll $ Gen.list (Range.constant 0 100) $ genPyChar Gen.unicode
    let
      bs = takeWhile (\x -> x == Char_lit '\\' || isEscape x) $ reverse ls
      bs' =
        takeWhile (\x -> x == Char_lit '\\' || isEscape x) . reverse $
        correctInitialFinalBackslashes ls
    if not (null bs)
      then do
        length bs === length bs'
        traverse_ (/== Char_lit '\\') bs'
      else correctInitialFinalBackslashes ls === ls

test_5 :: Property
test_5 =
  property $ do
    (q, qt, esc) <-
      forAll $
      Gen.element
        [ (Char_lit '\'', SingleQuote, Char_esc_singlequote)
        , (Char_lit '"', DoubleQuote, Char_esc_doublequote)
        ]
    let
      e1 = [q]

    correctInitialFinalQuotes qt e1 === [esc]

    let
      e2 = [q, Char_lit ' ']

    correctInitialFinalQuotes qt e2 === e2

    let
      e3 = [q, q, q, Char_lit ' ']

    correctInitialFinalQuotes qt e3 === [q, q, esc, Char_lit ' ']

    let
      e4 = [q, q, q, q, Char_lit ' ']

    correctInitialFinalQuotes qt e4 === [q, q, esc, q, Char_lit ' ']
