{-# language OverloadedStrings, OverloadedLists, TemplateHaskell #-}
module LexerParser (lexerParserTests) where

import Hedgehog
import Control.Monad (void)
import qualified Data.Text as Text

import Language.Python.Render
import Language.Python.Parse (parseModule, parseStatement, parseExpr)
import Language.Python.Syntax.Expr (Expr(..))
import Language.Python.Internal.Syntax.Strings
  ( StringLiteral(..), StringType(..), QuoteType(..), PyChar(..)
  , RawBytesPrefix(..), RawStringPrefix(..)
  )

import Helpers (shouldBeParseSuccess, shouldBeParseFailure)

lexerParserTests :: Group
lexerParserTests = $$discover

prop_fulltrip_1 :: Property
prop_fulltrip_1 =
  withTests 1 . property $ do
    let str = "def a(x, y=2, *z, **w):\n   return 2 + 3"

    tree <- shouldBeParseSuccess parseStatement str

    showStatement tree === str

prop_fulltrip_2 :: Property
prop_fulltrip_2 =
  withTests 1 . property $ do
    let str = "(   1\n       *\n  3\n    )"

    tree <- shouldBeParseSuccess parseExpr str

    showExpr tree === str

prop_fulltrip_3 :: Property
prop_fulltrip_3 =
  withTests 1 . property $ do
    let str = "pass;"

    tree <- shouldBeParseSuccess parseStatement str

    showStatement tree === str

prop_fulltrip_4 :: Property
prop_fulltrip_4 =
  withTests 1 . property $ do
    let str = "def a():\n pass\n #\n pass\n"

    tree <- shouldBeParseSuccess parseStatement str

    showStatement tree === str

prop_fulltrip_5 :: Property
prop_fulltrip_5 =
  withTests 1 . property $ do
    let str = "if False:\n pass\n pass\nelse:\n pass\n pass\n"

    tree <- shouldBeParseSuccess parseStatement str

    showStatement tree === str

prop_fulltrip_6 :: Property
prop_fulltrip_6 =
  withTests 1 . property $ do
    let str = "# blah\ndef boo():\n    pass\n       #bing\n    #   bop\n"

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_7 :: Property
prop_fulltrip_7 =
  withTests 1 . property $ do
    let str = "if False:\n pass\nelse \\\n      \\\r\n:\n pass\n"

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_8 :: Property
prop_fulltrip_8 =
  withTests 1 . property $ do
    let str = "def a():\n \n pass\n pass\n"

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_9 :: Property
prop_fulltrip_9 =
  withTests 1 . property $ do
    let
      str =
        "try:\n pass\nexcept False:\n pass\nelse:\n pass\nfinally:\n pass\n def a():\n  pass\n pass\n"

    tree <- shouldBeParseSuccess parseModule str

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

    tree <- shouldBeParseSuccess parseModule str

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

    tree <- shouldBeParseSuccess parseModule str

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

    tree <- shouldBeParseSuccess parseModule str

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

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_14 :: Property
prop_fulltrip_14 =
  withTests 1 . property $ do
    let
      str = "not ((False for a in False) if False else False or False)"

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_15 :: Property
prop_fulltrip_15 =
  withTests 1 . property $ do
    let
      str = "01."

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_16 :: Property
prop_fulltrip_16 =
  withTests 1 . property $ do
    let
      str = "def a():\n  return ~i"

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_17 :: Property
prop_fulltrip_17 =
  withTests 1 . property $ do
    let str = "r\"\\\"\""

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_18 :: Property
prop_fulltrip_18 =
  withTests 1 . property $ do
    let str = "\"\0\""

    tree <- shouldBeParseSuccess parseModule str

    showModule tree === str

prop_fulltrip_19 :: Property
prop_fulltrip_19 =
  withTests 1 . property $ do
    let str = " \\\n"

    shouldBeParseFailure parseModule str

prop_fulltrip_20 :: Property
prop_fulltrip_20 =
  withTests 1 . property $ do
    let str = " pass"

    shouldBeParseFailure parseModule str

prop_fulltrip_21 :: Property
prop_fulltrip_21 =
  withTests 1 . property $ do
    let str = "if a:\n  \\\n\n  pass"

    shouldBeParseFailure parseModule str

prop_fulltrip_22 :: Property
prop_fulltrip_22 =
  withTests 1 . property $ do
    let str = "for a in (b, *c): pass"

    void $ shouldBeParseSuccess parseModule str

prop_fulltrip_23 :: Property
prop_fulltrip_23 =
  withTests 1 . property $ do
    let str = "None,*None"

    void $ shouldBeParseSuccess parseModule str

prop_fulltrip_24 :: Property
prop_fulltrip_24 =
  withTests 1 . property $ do
    let str = "'\1'"

    void $ shouldBeParseSuccess parseModule str

prop_fulltrip_25 :: Property
prop_fulltrip_25 =
  withTests 1 . property $ do
    let str = "'\11'"

    void $ shouldBeParseSuccess parseModule str

prop_fulltrip_26 :: Property
prop_fulltrip_26 =
  withTests 1 . property $ do
    let str =
          showExpr $
          String ()
            (pure $
             RawBytesLiteral ()
               Prefix_br
               LongString
               SingleQuote
               [ Char_esc_bslash ]
               [])
    annotateShow str

    res <- shouldBeParseSuccess parseExpr str
    str === showExpr (() <$ res)

prop_fulltrip_27 :: Property
prop_fulltrip_27 =
  withTests 1 . property $ do
    let str =
          showExpr $
          String ()
            (pure $
             RawStringLiteral ()
               Prefix_r
               LongString
               SingleQuote
               [ Char_lit '\\', Char_lit '\\', Char_lit '\\', Char_lit '\'' ]
               [])
    annotateShow str

    res <- shouldBeParseSuccess parseExpr str
    str === showExpr (() <$ res)

prop_fulltrip_28 :: Property
prop_fulltrip_28 =
  withTests 1 . property $ do
    let str =
          showExpr $
          String ()
            (pure $
             RawStringLiteral ()
               Prefix_r
               ShortString
               DoubleQuote
               [ Char_lit '\\' ]
               [])
    annotateShow str

    res <- shouldBeParseSuccess parseExpr str
    str === showExpr (() <$ res)

prop_fulltrip_29 :: Property
prop_fulltrip_29 =
  withTests 1 . property $ do
    let str =
          showExpr $
          String ()
            (pure $
             RawStringLiteral ()
               Prefix_r
               ShortString
               DoubleQuote
               [ Char_lit '\\', Char_lit '\\' ]
               [])
    annotateShow str

    res <- shouldBeParseSuccess parseExpr str
    str === showExpr (() <$ res)

prop_fulltrip_30 :: Property
prop_fulltrip_30 =
  withTests 1 . property $ do
    let str =
          showExpr $
          String ()
            (pure $
             RawStringLiteral ()
               Prefix_r
               ShortString
               DoubleQuote
               [ Char_lit '\\', Char_lit '\\', Char_lit '\\' ]
               [])
    annotateShow str

    res <- shouldBeParseSuccess parseExpr str
    str === showExpr (() <$ res)

prop_fulltrip_31 :: Property
prop_fulltrip_31 =
  withTests 1 . property $ do
    let str = "del(a)"

    void $ shouldBeParseSuccess parseModule str

prop_fulltrip_32 :: Property
prop_fulltrip_32 =
  withTests 1 . property $ do
    let str =
          showExpr $
          String ()
            (pure $
             RawStringLiteral ()
               Prefix_r
               LongString
               DoubleQuote
               [ Char_lit ' ', Char_lit '"' ]
               [])
    annotateShow str

    res <- shouldBeParseSuccess parseExpr str
    str === showExpr (() <$ res)

prop_fulltrip_33 :: Property
prop_fulltrip_33 =
  withTests 1 . property $ do
    let str =
          showExpr $
          String ()
            (pure $
             RawStringLiteral ()
               Prefix_r
               LongString
               DoubleQuote
               [ Char_lit '"', Char_lit ' ' ]
               [])
    annotateShow str

    res <- shouldBeParseSuccess parseExpr "test"
    str === showExpr (() <$ res)

prop_fulltrip_34 :: Property
prop_fulltrip_34 =
  withTests 1 . property $ do
    let str =
          showExpr $
          String ()
            (pure $
             RawStringLiteral ()
               Prefix_r
               LongString
               DoubleQuote
               [ Char_lit '"' ]
               [])
    annotateShow str

    res <- shouldBeParseSuccess parseExpr str
    str === showExpr (() <$ res)
