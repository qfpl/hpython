{-# language OverloadedStrings, OverloadedLists #-}
module LexerParser (lexerParserTests) where

import Data.Validate (Validate(..), validate)
import qualified Data.Text as Text
import Hedgehog

import Helpers (doTokenize, doTabs)
import Language.Python.Internal.Lexer (initialSrcInfo)

import Language.Python.Internal.Render
import Language.Python.Parse (parseModule, parseStatement, parseExpr)

lexerParserTests :: Group
lexerParserTests =
  Group "Lexer/Parser tests"
  [ ("Test full trip 1", test_fulltrip_1)
  , ("Test full trip 2", test_fulltrip_2)
  , ("Test full trip 3", test_fulltrip_3)
  , ("Test full trip 4", test_fulltrip_4)
  , ("Test full trip 5", test_fulltrip_5)
  , ("Test full trip 6", test_fulltrip_6)
  , ("Test full trip 7", test_fulltrip_7)
  , ("Test full trip 8", test_fulltrip_8)
  , ("Test full trip 9", test_fulltrip_9)
  , ("Test full trip 10", test_fulltrip_10)
  , ("Test full trip 11", test_fulltrip_11)
  , ("Test full trip 12", test_fulltrip_12)
  , ("Test full trip 13", test_fulltrip_13)
  , ("Test full trip 14", test_fulltrip_14)
  , ("Test full trip 15", test_fulltrip_15)
  , ("Test full trip 16", test_fulltrip_16)
  , ("Test full trip 17", test_fulltrip_17)
  , ("Test full trip 18", test_fulltrip_18)
  , ("Test full trip 19", test_fulltrip_19)
  , ("Test full trip 20", test_fulltrip_20)
  , ("Test full trip 21", test_fulltrip_21)
  ]

test_fulltrip_1 :: Property
test_fulltrip_1 =
  withTests 1 . property $ do
    let str = "def a(x, y=2, *z, **w):\n   return 2 + 3"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseStatement "test" str
    annotateShow tree

    showStatement tree === str

test_fulltrip_2 :: Property
test_fulltrip_2 =
  withTests 1 . property $ do
    let str = "(   1\n       *\n  3\n    )"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseExpr "test" str
    annotateShow tree

    showExpr tree === str

test_fulltrip_3 :: Property
test_fulltrip_3 =
  withTests 1 . property $ do
    let str = "pass;"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseStatement "test" str
    annotateShow tree

    showStatement tree === str

test_fulltrip_4 :: Property
test_fulltrip_4 =
  withTests 1 . property $ do
    let str = "def a():\n pass\n #\n pass\n"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseStatement "test" str
    annotateShow tree

    showStatement tree === str

test_fulltrip_5 :: Property
test_fulltrip_5 =
  withTests 1 . property $ do
    let str = "if False:\n pass\n pass\nelse:\n pass\n pass\n"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseStatement "test" str
    annotateShow tree

    showStatement tree === str

test_fulltrip_6 :: Property
test_fulltrip_6 =
  withTests 1 . property $ do
    let str = "# blah\ndef boo():\n    pass\n       #bing\n    #   bop\n"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow tree

    showModule tree === str

test_fulltrip_7 :: Property
test_fulltrip_7 =
  withTests 1 . property $ do
    let str = "if False:\n pass\nelse \\\n      \\\r\n:\n pass\n"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow tree

    showModule tree === str

test_fulltrip_8 :: Property
test_fulltrip_8 =
  withTests 1 . property $ do
    let str = "def a():\n \n pass\n pass\n"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow tree

    showModule tree === str

test_fulltrip_9 :: Property
test_fulltrip_9 =
  withTests 1 . property $ do
    let
      str =
        "try:\n pass\nexcept False:\n pass\nelse:\n pass\nfinally:\n pass\n def a():\n  pass\n pass\n"

    tks <- doTokenize str
    tks' <- doTabs (initialSrcInfo "test") tks

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow tree

    showModule tree === str

test_fulltrip_10 :: Property
test_fulltrip_10 =
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

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_11 :: Property
test_fulltrip_11 =
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

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_12 :: Property
test_fulltrip_12 =
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

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_13 :: Property
test_fulltrip_13 =
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

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_14 :: Property
test_fulltrip_14 =
  withTests 1 . property $ do
    let
      str = "not ((False for a in False) if False else False or False)"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_15 :: Property
test_fulltrip_15 =
  withTests 1 . property $ do
    let
      str = "01."

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_16 :: Property
test_fulltrip_16 =
  withTests 1 . property $ do
    let
      str = "def a():\n  return ~i"

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_17 :: Property
test_fulltrip_17 =
  withTests 1 . property $ do
    let str = "r\"\\\"\""

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_18 :: Property
test_fulltrip_18 =
  withTests 1 . property $ do
    let str = "\"\0\""

    tree <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    annotateShow $! tree

    showModule tree === str

test_fulltrip_19 :: Property
test_fulltrip_19 =
  withTests 1 . property $ do
    let str = " \\\n"

    let res = parseModule "test" str
    case res of
      Failure{} -> success
      Success a -> do
        annotateShow a
        failure

test_fulltrip_20 :: Property
test_fulltrip_20 =
  withTests 1 . property $ do
    let str = " pass"

    let res = parseModule "test" str
    case res of
      Failure{} -> success
      Success a -> do
        annotateShow a
        failure

test_fulltrip_21 :: Property
test_fulltrip_21 =
  withTests 1 . property $ do
    let str = "if a:\n  \\\n\n  pass"

    let res = parseModule "test" str
    case res of
      Failure{} -> success
      Success a -> do
        annotateShow a
        failure
