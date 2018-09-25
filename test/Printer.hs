{-# language OverloadedStrings #-}
module Printer (printerTests) where

import Hedgehog
import Data.List.NonEmpty (NonEmpty(..))

import Language.Python.Internal.Render
import Language.Python.Internal.Syntax.Expr
import Language.Python.Internal.Syntax.Strings

printerTests :: Group
printerTests =
  Group "Printer tests"
  [ ("Printer test 1", withTests 1 test_1)
  ]

test_1 :: Property
test_1 =
  property $ do
    let
      e1 =
        String () $
        StringLiteral () Nothing SingleQuote ShortString [] [] :|
        [StringLiteral () Nothing SingleQuote ShortString [] []]

    showExpr e1 === "'' ''"

    let
      e2 =
        String () $
        StringLiteral () Nothing DoubleQuote ShortString [] [] :|
        [StringLiteral () Nothing DoubleQuote ShortString [] []]

    showExpr e2 === "\"\" \"\""

    let
      e3 =
        String () $
        StringLiteral () Nothing SingleQuote ShortString [] [] :|
        [StringLiteral () Nothing DoubleQuote ShortString [] []]

    showExpr e3 === "''\"\""

    let
      e4 =
        String () $
        StringLiteral () Nothing SingleQuote ShortString [] [] :|
        [StringLiteral () (Just Prefix_u) SingleQuote ShortString [] []]

    showExpr e4 === "''u''"
