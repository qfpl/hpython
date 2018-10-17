{-# language OverloadedStrings, TemplateHaskell #-}
{-# language DataKinds #-}
module Syntax (syntaxTests) where

import Hedgehog

import Control.Lens.Iso (from)
import Control.Lens.Getter ((^.))
import Control.Monad (void)
import Language.Python.Internal.Render (showStatement)
import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.Expr
import Language.Python.Internal.Syntax.Statement
import Language.Python.Internal.Syntax.Whitespace
import Language.Python.Parse (parseStatement)

import Helpers
  (shouldBeFailure, shouldBeSuccess, syntaxValidateExpr)

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
