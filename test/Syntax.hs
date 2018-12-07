{-# language OverloadedStrings, TemplateHaskell #-}
{-# language DataKinds #-}
module Syntax (syntaxTests) where

import Hedgehog

import Control.Lens.Iso (from)
import Control.Lens.Getter ((^.))
import Control.Monad (void)
import Language.Python.DSL
import Language.Python.Render (showStatement, showExpr)
import Language.Python.Parse (parseModule, parseStatement, parseExpr)
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace

import Helpers
  ( shouldBeParseSuccess, shouldBeFailure, shouldBeSuccess
  , syntaxValidateExpr, syntaxValidateStatement, syntaxValidateModule
  )

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
          (CommaSepMany (UnnamedStarParam () []) (Comma []) CommaSepNone)
          (Colon [Space])
          (None () [])
    res <- syntaxValidateExpr e
    shouldBeFailure res

prop_syntax_2 :: Property
prop_syntax_2 =
  withTests 1 . property $ do
    let
      i = replicate 4 Space ^. from indentWhitespaces
      e :: Statement '[] ()
      e =
        CompoundStatement .
        Fundef () []
          (Indents mempty ())
          Nothing
          (pure Space)
            "test"
            [] CommaSepNone [] Nothing .
          SuiteMany () (Colon []) Nothing LF $
          Block []
            (SmallStatement (Indents [i] ()) $
             MkSmallStatement (Pass () []) [] Nothing Nothing Nothing)
            [Right . SmallStatement (Indents [i] ()) $
             MkSmallStatement (Pass () []) [] Nothing Nothing Nothing]
    res <- shouldBeParseSuccess parseStatement (showStatement e)
    res' <- shouldBeParseSuccess parseStatement (showStatement res)
    void res === void res'

prop_syntax_3 :: Property
prop_syntax_3 =
  withTests 1 . property $ do
    let
      s = "@a\ndef a():\n pass\n @a\n class a: return "
    e <- shouldBeParseSuccess parseModule s
    shouldBeFailure =<< syntaxValidateModule (() <$ e)

prop_syntax_4 :: Property
prop_syntax_4 =
  withTests 1 . property $ do
    let
      e :: Expr '[] ()
      e =
        String () . pure $
        StringLiteral ()
          Nothing
        ShortString SingleQuote
        [Char_lit '\\', Char_lit 'u']
        []
    res <- shouldBeParseSuccess parseExpr (showExpr e)
    res' <- shouldBeParseSuccess parseExpr (showExpr res)
    void res === void res'

prop_syntax_5 :: Property
prop_syntax_5 =
  withTests 1 . property $ do
    let
      e :: Expr '[] ()
      e =
        String () . pure $
        StringLiteral ()
          Nothing
        ShortString SingleQuote
        [Char_lit '\\', Char_lit 'x']
        []
    res <- shouldBeParseSuccess parseExpr (showExpr e)
    res' <- shouldBeParseSuccess parseExpr (showExpr res)
    void res === void res'

prop_syntax_6 :: Property
prop_syntax_6 =
  withTests 1 . property $ do
    let s= "async def a():\n class a(await None):\n  pass"
    e <- shouldBeParseSuccess parseModule s
    void . shouldBeSuccess =<< syntaxValidateModule (() <$ e)

prop_syntax_7 :: Property
prop_syntax_7 =
  withTests 1 . property $ do
    let
      s = "def a(b): global b"
    e <- shouldBeParseSuccess parseModule s
    shouldBeFailure =<< syntaxValidateModule (() <$ e)

prop_syntax_8 :: Property
prop_syntax_8 =
  withTests 1 . property $ do
    let
      s = "def a(*): pass"
    e <- shouldBeParseSuccess parseModule s
    shouldBeFailure =<< syntaxValidateModule (() <$ e)

prop_syntax_9 :: Property
prop_syntax_9 =
  withTests 1 . property $ do
    let
      s = "def a(*, b=None): pass"
    e <- shouldBeParseSuccess parseModule s
    void . shouldBeSuccess =<< syntaxValidateModule (() <$ e)

prop_syntax_10 :: Property
prop_syntax_10 =
  withTests 1 . property $ do
    let e = def_ "a" [s_ "b", s_ "c"] [line_ pass_]
    void . shouldBeFailure =<< syntaxValidateStatement e

prop_syntax_11 :: Property
prop_syntax_11 =
  withTests 1 . property $ do
    let e = lambda_ [s_ "a", s_ "b"] (var_ "a")
    void . shouldBeFailure =<< syntaxValidateExpr e

prop_syntax_12 :: Property
prop_syntax_12 =
  withTests 1 . property $ do
    let e = def_ "a" [star_, s_ "b"] [line_ pass_]
    void . shouldBeFailure =<< syntaxValidateStatement e

prop_syntax_13 :: Property
prop_syntax_13 =
  withTests 1 . property $ do
    let e = lambda_ [star_, s_ "a"] (var_ "b")
    void . shouldBeFailure =<< syntaxValidateExpr e

prop_syntax_14 :: Property
prop_syntax_14 =
  withTests 1 . property $ do
    let e = def_ "a" [star_, k_ "b" none_, s_ "c"] [line_ pass_]
    void . shouldBeFailure =<< syntaxValidateStatement e

prop_syntax_15 :: Property
prop_syntax_15 =
  withTests 1 . property $ do
    let e = lambda_ [star_, k_ "a" none_, s_ "b"] (var_ "c")
    void . shouldBeFailure =<< syntaxValidateExpr e
