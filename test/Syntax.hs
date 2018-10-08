{-# language OverloadedStrings #-}
module Syntax (syntaxTests) where

import Hedgehog

import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.Expr
import Language.Python.Internal.Syntax.Whitespace

import Helpers (shouldBeFailure, syntaxValidateExpr)

syntaxTests :: Group
syntaxTests =
  Group "Syntax tests"
  [ ("Syntax test 1", withTests 1 test_1)
  ]

test_1 :: Property
test_1 =
  property $ do
    let
      e =
        -- lambda *: None
        Lambda ()
          [Space]
          (CommaSepMany (StarParam () [] Nothing Nothing) [] CommaSepNone)
          [Space]
          (None () [])
    res <- syntaxValidateExpr e
    shouldBeFailure res
