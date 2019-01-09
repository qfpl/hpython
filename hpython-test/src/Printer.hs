{-# language OverloadedStrings, TemplateHaskell #-}
module Printer (printerTests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty(..))
import Language.Python.Render
import Language.Python.Internal.Render.Correction
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Strings

import Generators.Common

printerTests :: Group
printerTests = $$discover

prop_printer_1 :: Property
prop_printer_1 =
  withTests 1 . property $ do
    let
      e1 =
        String (Ann ()) $
        StringLiteral () Nothing ShortString SingleQuote [] [] :|
        [StringLiteral () Nothing ShortString SingleQuote [] []]

    showExpr e1 === "'' ''"

    let
      e2 =
        String (Ann ()) $
        StringLiteral () Nothing ShortString DoubleQuote [] [] :|
        [StringLiteral () Nothing ShortString DoubleQuote [] []]

    showExpr e2 === "\"\" \"\""

    let
      e3 =
        String (Ann ()) $
        StringLiteral () Nothing ShortString SingleQuote [] [] :|
        [StringLiteral () Nothing ShortString DoubleQuote [] []]

    showExpr e3 === "''\"\""

    let
      e4 =
        String (Ann ()) $
        StringLiteral () Nothing ShortString SingleQuote [] [] :|
        [StringLiteral () (Just Prefix_u) ShortString SingleQuote [] []]

    showExpr e4 === "''u''"

prop_printer_2 :: Property
prop_printer_2 =
  withTests 1 . property $ do
    let
      e1 = [Char_lit '\\']

    correctBackslashes e1 === [Char_esc_bslash]

prop_printer_3 :: Property
prop_printer_3 =
  withTests 1 . property $ do
    let
      e2 = [Char_lit '\\', Char_lit ' ']

    correctBackslashes e2 === [Char_lit '\\', Char_lit ' ']

prop_printer_4 :: Property
prop_printer_4 =
  withTests 1 . property $ do
    ls <- forAll $ Gen.list (Range.constant 0 100) $ genPyChar Gen.unicode
    let
      bs = takeWhile (\x -> x == Char_lit '\\' || isEscape x) $ reverse ls
      bs' =
        takeWhile (\x -> x == Char_lit '\\' || isEscape x) . reverse $
        correctBackslashes ls
    if not (null bs)
      then do
        length bs === length bs'
        traverse_ (/== Char_lit '\\') bs'
      else correctBackslashes ls === ls

prop_printer_5 :: Property
prop_printer_5 =
  withTests 1 . property $ do
    (q, qt, esc) <-
      forAll $
      Gen.element
        [ (Char_lit '\'', SingleQuote, Char_esc_singlequote)
        , (Char_lit '"', DoubleQuote, Char_esc_doublequote)
        ]
    let
      e1 = [q]

    correctInitialFinalQuotesLong qt e1 === [esc]

    let
      e2 = [q, Char_lit ' ']

    correctInitialFinalQuotesLong qt e2 === e2

    let
      e3 = [q, q, q, Char_lit ' ']

    correctInitialFinalQuotesLong qt e3 === [q, q, esc, Char_lit ' ']

    let
      e4 = [q, q, q, q, Char_lit ' ']

    correctInitialFinalQuotesLong qt e4 === [q, q, esc, q, Char_lit ' ']

prop_printer_6 :: Property
prop_printer_6 =
  withTests 1 . property $ do
    let
      s = [Char_lit '\\', Char_esc_bslash]
      e =
        String (Ann ()) $
        RawBytesLiteral () Prefix_br ShortString SingleQuote s [] :|
        []

    correctBackslashes s === [Char_esc_bslash, Char_esc_bslash]
    showExpr e === "br'\\\\\\\\'"

prop_printer_7 :: Property
prop_printer_7 =
  withTests 1 . property $ do
    let
      s = [Char_newline, Char_lit '\\', Char_esc_doublequote]
      e =
        String (Ann ()) $
        StringLiteral () Nothing ShortString DoubleQuote s [] :|
        []

    showExpr e === "\"\\newline\\\\\\\"\""
