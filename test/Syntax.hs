{-# language OverloadedStrings, TemplateHaskell #-}
module Syntax (syntaxTests) where

import Hedgehog

import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.Expr
import Language.Python.Internal.Syntax.Whitespace

import Helpers (shouldBeFailure, syntaxValidateExpr)

syntaxTests :: Group
syntaxTests = $$discover

prop_syntax_1 :: Property
prop_syntax_1 =
  withTests 1 . property $ do
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
