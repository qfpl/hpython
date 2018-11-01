{-# language OverloadedStrings, TemplateHaskell #-}
{-# language DataKinds #-}
module Syntax (syntaxTests) where

import Hedgehog

import Control.Lens.Iso (from)
import Control.Lens.Getter ((^.))
import Control.Monad (void)
import Language.Python.Render (showStatement, showExpr)
import Language.Python.Internal.Syntax.Strings
import Language.Python.Parse (parseModule, parseStatement, parseExpr)
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Whitespace

import Helpers
  (shouldBeFailure, shouldBeSuccess, syntaxValidateExpr, syntaxValidateModule)

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
          (CommaSepMany (StarParam () [] Nothing Nothing) (Comma []) CommaSepNone)
          [Space]
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
          SuiteMany () [] Nothing LF $
          Block []
            (SimpleStatement (Indents [i] ()) $
             MkSimpleStatement (Pass () []) [] Nothing Nothing Nothing)
            [Right . SimpleStatement (Indents [i] ()) $
             MkSimpleStatement (Pass () []) [] Nothing Nothing Nothing]
    res <- shouldBeSuccess $ parseStatement "test" (showStatement e)
    res' <- shouldBeSuccess $ parseStatement "test" (showStatement res)
    void res === void res'

prop_syntax_3 :: Property
prop_syntax_3 =
  withTests 1 . property $ do
    let
      s = "@a\ndef a():\n pass\n @a\n class a: return "
    e <- shouldBeSuccess $ parseModule "test" s
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
    res <- shouldBeSuccess $ parseExpr "test" (showExpr e)
    res' <- shouldBeSuccess $ parseExpr "test" (showExpr res)
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
    res <- shouldBeSuccess $ parseExpr "test" (showExpr e)
    res' <- shouldBeSuccess $ parseExpr "test" (showExpr res)
    void res === void res'

prop_syntax_6 :: Property
prop_syntax_6 =
  withTests 1 . property $ do
    let s= "async def a():\n class a(await None):\n  pass"
    e <- shouldBeSuccess $ parseModule "test" s
    void . shouldBeSuccess =<< syntaxValidateModule (() <$ e)