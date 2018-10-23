{-# language OverloadedStrings, OverloadedLists, TemplateHaskell #-}
module LexerParser (lexerParserTests) where

import Hedgehog
import Control.Monad (void)
import Data.Validation (Validation(..), validation)
import qualified Data.Text as Text

import Language.Python.Render
import Language.Python.Parse (parseModule, parseStatement, parseExpr)

import Helpers (shouldBeSuccess)

lexerParserTests :: Group
lexerParserTests = $$discover

prop_fulltrip_1 :: Property
prop_fulltrip_1 =
  withTests 1 . property $ do
    let str = "def a(x, y=2, *z, **w):\n   return 2 + 3"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseStatement "test" str
    annotateShow tree

    showStatement tree === str

prop_fulltrip_2 :: Property
prop_fulltrip_2 =
  withTests 1 . property $ do
    let str = "(   1\n       *\n  3\n    )"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseExpr "test" str
    annotateShow tree

    showExpr tree === str

prop_fulltrip_3 :: Property
prop_fulltrip_3 =
  withTests 1 . property $ do
    let str = "pass;"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseStatement "test" str
    annotateShow tree

    showStatement tree === str

prop_fulltrip_4 :: Property
prop_fulltrip_4 =
  withTests 1 . property $ do
    let str = "def a():\n pass\n #\n pass\n"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseStatement "test" str
    annotateShow tree

    showStatement tree === str

prop_fulltrip_5 :: Property
prop_fulltrip_5 =
  withTests 1 . property $ do
    let str = "if False:\n pass\n pass\nelse:\n pass\n pass\n"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseStatement "test" str
    annotateShow tree

    showStatement tree === str

prop_fulltrip_6 :: Property
prop_fulltrip_6 =
  withTests 1 . property $ do
    let str = "# blah\ndef boo():\n    pass\n       #bing\n    #   bop\n"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow tree

    showModule tree === str

prop_fulltrip_7 :: Property
prop_fulltrip_7 =
  withTests 1 . property $ do
    let str = "if False:\n pass\nelse \\\n      \\\r\n:\n pass\n"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow tree

    showModule tree === str

prop_fulltrip_8 :: Property
prop_fulltrip_8 =
  withTests 1 . property $ do
    let str = "def a():\n \n pass\n pass\n"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow tree

    showModule tree === str

prop_fulltrip_9 :: Property
prop_fulltrip_9 =
  withTests 1 . property $ do
    let
      str =
        "try:\n pass\nexcept False:\n pass\nelse:\n pass\nfinally:\n pass\n def a():\n  pass\n pass\n"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow tree

    showModule tree === str

prop_fulltrip_10 :: Property
prop_fulltrip_10 =
  withTests 1 . property $ do
    let
      str =
        Text.unlines
        [ "from blah import  boo"
        , "import baz   as wop"
        , ""
        , "def thing():"
        , "    pass"
        , ""
        , "def    hello():"
        , "    what; up;"
        , ""
        , "def boo(a, *b, c=1, **d):"
        , "    pass"
        ]

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_11 :: Property
prop_fulltrip_11 =
  withTests 1 . property $ do
    let
      str =
        Text.unlines
        [ "if False:"
        , " pass"
        , " pass"
        , "else:"
        , " \tpass"
        , " \tpass"
        ]

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_12 :: Property
prop_fulltrip_12 =
  withTests 1 . property $ do
    let
      str =
        Text.unlines
        [ "try:"
        , " \tpass"
        , " \tdef a():"
        , " \t pass"
        , " \tpass"
        , "finally:"
        , " pass"
        ]

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_13 :: Property
prop_fulltrip_13 =
  withTests 1 . property $ do
    let
      str =
        Text.unlines
        [ "if []:"
        , " False"
        , " def a():"
        , "  pass"
        , "  pass"
        , ""
        , "else:"
        , " pass"
        , " pass"
        ]

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_14 :: Property
prop_fulltrip_14 =
  withTests 1 . property $ do
    let
      str = "not ((False for a in False) if False else False or False)"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_15 :: Property
prop_fulltrip_15 =
  withTests 1 . property $ do
    let
      str = "01."

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_16 :: Property
prop_fulltrip_16 =
  withTests 1 . property $ do
    let
      str = "def a():\n  return ~i"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_17 :: Property
prop_fulltrip_17 =
  withTests 1 . property $ do
    let str = "r\"\\\"\""

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_18 :: Property
prop_fulltrip_18 =
  withTests 1 . property $ do
    let str = "\"\0\""

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

prop_fulltrip_19 :: Property
prop_fulltrip_19 =
  withTests 1 . property $ do
    let str = " \\\n"

    let res = parseModule "test" str
    case res of
      Failure{} -> success
      Success a -> do
        annotateShow a
        failure

prop_fulltrip_20 :: Property
prop_fulltrip_20 =
  withTests 1 . property $ do
    let str = " pass"

    let res = parseModule "test" str
    case res of
      Failure{} -> success
      Success a -> do
        annotateShow a
        failure

prop_fulltrip_21 :: Property
prop_fulltrip_21 =
  withTests 1 . property $ do
    let str = "if a:\n  \\\n\n  pass"

    let res = parseModule "test" str
    case res of
      Failure{} -> success
      Success a -> do
        annotateShow a
        failure

prop_fulltrip_22 :: Property
prop_fulltrip_22 =
  withTests 1 . property $ do
    let str = "for a in (b, *c): pass"

    void . shouldBeSuccess $ parseModule "test" str

prop_fulltrip_23 :: Property
prop_fulltrip_23 =
  withTests 1 . property $ do
    let str = "None,*None"

    void . shouldBeSuccess $ parseModule "test" str
