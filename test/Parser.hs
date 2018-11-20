{-# language OverloadedStrings, TemplateHaskell #-}
module Parser (parserTests) where

import Hedgehog

import Language.Python.Internal.Token (PyToken(..))
import Language.Python.Parse (parseStatement)

import Helpers (shouldBeParseError)

parserTests :: Group
parserTests = $$discover

prop_parser_1 :: Property
prop_parser_1 =
  withTests 1 . property $ do
    let
      e = "for x in a, *b: pass"
      res = parseStatement "test" e

    shouldBeParseError 1 13 (TkStar ()) res
