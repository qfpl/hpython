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
