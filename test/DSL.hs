{-# language OverloadedStrings, TemplateHaskell #-}
module DSL (dslTests) where

import Hedgehog

import Control.Lens.Fold ((^?))
import Control.Lens.Setter ((.~), over)
import Data.Function ((&))

import Language.Python.DSL
import Language.Python.Optics
import Language.Python.Render (showExpr)
import Language.Python.Syntax.CommaSep (CommaSep(..))
import Language.Python.Syntax.Punctuation (Comma(..))
import Language.Python.Syntax.Whitespace (Whitespace(..), Indents(..))

dslTests :: Group
dslTests = $$discover

prop_subscript_1 :: Property
prop_subscript_1 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (int_ 1)
    showExpr expr  === "a[1]"

prop_subscript_2 :: Property
prop_subscript_2 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ int_ 1, ti_ $ int_ 2])
    showExpr expr  === "a[1, 2]"

prop_subscript_3 :: Property
prop_subscript_3 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ int_ 1, s_ $ var_ "b"])
    showExpr expr  === "a[(1, *b)]"

prop_subscript_4 :: Property
prop_subscript_4 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ int_ 1])
    showExpr expr  === "a[1,]"

prop_subscript_5 :: Property
prop_subscript_5 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [s_ $ var_ "b"])
    showExpr expr  === "a[((*b),)]"

prop_subscript_6 :: Property
prop_subscript_6 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (slice_ Nothing Nothing Nothing)
    showExpr expr  === "a[:]"

prop_subscript_7 :: Property
prop_subscript_7 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ slice_ Nothing Nothing Nothing])
    showExpr expr  === "a[:,]"

prop_subscript_8 :: Property
prop_subscript_8 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ tuple_ [ti_ $ slice_ Nothing Nothing Nothing]])
    showExpr expr  === "a[(slice(None, None, None),),]"

prop_subscript_9 :: Property
prop_subscript_9 =
  withTests 1 . property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_ [ti_ $ slice_ Nothing Nothing Nothing, ti_ $ slice_ Nothing Nothing Nothing]
    showExpr expr  === "a[:, :]"

prop_subscript_10 :: Property
prop_subscript_10 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (slice_ (Just $ int_ 1) Nothing Nothing)
    showExpr expr  === "a[1:]"

prop_subscript_11 :: Property
prop_subscript_11 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (slice_ (Just $ int_ 1) (Just $ int_ 1) Nothing)
    showExpr expr  === "a[1:1]"

prop_subscript_12 :: Property
prop_subscript_12 =
  withTests 1 . property $ do
    let
      expr = subs_ (var_ "a") (slice_ (Just $ int_ 1) (Just $ int_ 1) (Just $ int_ 1))
    showExpr expr  === "a[1:1:1]"

prop_subscript_13 :: Property
prop_subscript_13 =
  withTests 1 . property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_
          [ ti_ $ slice_ (Just $ int_ 1) Nothing Nothing
          , ti_ $ slice_ (Just $ int_ 2) Nothing Nothing
          ]
    showExpr expr  === "a[1:, 2:]"

prop_subscript_14 :: Property
prop_subscript_14 =
  withTests 1 . property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_
          [ ti_ $ slice_ (Just $ int_ 1) (Just $ int_ 1) Nothing
          , ti_ $ slice_ (Just $ int_ 2) Nothing Nothing
          ]
    showExpr expr  === "a[1:1, 2:]"

prop_subscript_15 :: Property
prop_subscript_15 =
  withTests 1 . property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_
          [ ti_ $ slice_ (Just $ int_ 1) (Just $ int_ 1) (Just $ int_ 1)
          , ti_ $ slice_ (Just $ int_ 2) Nothing Nothing
          ]
    showExpr expr  === "a[1:1:1, 2:]"

prop_subscript_16 :: Property
prop_subscript_16 =
  withTests 1 . property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_
        [ ti_ $
          tuple_
            [ ti_ $ slice_ (Just $ int_ 1) Nothing Nothing
            , ti_ $ int_ 2
            ]
        ]
    showExpr expr  === "a[(slice(1, None, None), 2),]"

prop_parameters_1 :: Property
prop_parameters_1 =
  withTests 1 . property $ do
    let
      st = def_ "a" [] [line_ pass_]

      params1 =
        CommaSepMany (p_ "test1") (Comma $ replicate 5 Space) $
        CommaSepMany (p_ "test2") (Comma $ replicate 3 Space) $
        CommaSepNone
      st1 = st & _Fundef.fdParameters .~ params1

      params2 =
        CommaSepMany (p_ "test3") (Comma $ replicate 5 Space) $
        CommaSepMany (p_ "test4") (Comma $ replicate 3 Space) $
        CommaSepNone
      st2 = st & _Fundef.fdParameters .~ params2

    (st1 & _Fundef.parameters_ .~ [p_ "test3", p_ "test4"]) === st2

prop_parameters_2 :: Property
prop_parameters_2 =
  withTests 1 . property $ do
    let
      st = def_ "a" [] [line_ pass_]

      params1 =
        CommaSepMany (p_ "test1") (Comma $ replicate 5 Space) $
        CommaSepMany (p_ "test2") (Comma $ replicate 3 Space) $
        CommaSepNone
      st1 = st & _Fundef.fdParameters .~ params1

      params2 =
        CommaSepMany (p_ "test3") (Comma $ replicate 5 Space) $
        CommaSepMany (p_ "test4") (Comma $ replicate 3 Space) $
        CommaSepOne (p_ "test5")
      st2 = st & _Fundef.fdParameters .~ params2

    (st1 & _Fundef.parameters_ .~ [p_ "test3", p_ "test4", p_ "test5"]) === st2

prop_parameters_3 :: Property
prop_parameters_3 =
  withTests 1 . property $ do
    let
      st = def_ "a" [] [line_ pass_]

      params1 =
        CommaSepMany (p_ "test1") (Comma $ replicate 5 Space) $
        CommaSepMany (p_ "test2") (Comma $ replicate 3 Space) $
        CommaSepNone
      st1 = st & _Fundef.fdParameters .~ params1

      params2 = CommaSepMany (p_ "test3") (Comma $ replicate 5 Space) CommaSepNone
      st2 = st & _Fundef.fdParameters .~ params2

    (st1 & _Fundef.parameters_ .~ [p_ "test3"]) === st2

prop_parameters_4 :: Property
prop_parameters_4 =
  withTests 1 . property $ do
    let
      st = def_ "a" [] [line_ pass_]

      params1 =
        CommaSepMany (p_ "test1") (Comma $ replicate 5 Space) $
        CommaSepOne (p_ "test2")
      st1 = st & _Fundef.fdParameters .~ params1

      params2 =
        CommaSepMany (p_ "test3") (Comma $ replicate 5 Space) $
        CommaSepOne (p_ "test4")
      st2 = st & _Fundef.fdParameters .~ params2

    (st1 & _Fundef.parameters_ .~ [p_ "test3", p_ "test4"]) === st2

prop_parameters_5 :: Property
prop_parameters_5 =
  withTests 1 . property $ do
    let
      st = def_ "a" [] [line_ pass_]

      params1 =
        CommaSepMany (p_ "test1") (Comma $ replicate 5 Space) $
        CommaSepOne (p_ "test2")
      st1 = st & _Fundef.fdParameters .~ params1

      params2 =
        CommaSepMany (p_ "test3") (Comma $ replicate 5 Space) $
        CommaSepMany (p_ "test4") (Comma [Space]) $
        CommaSepOne (p_ "test5")
      st2 = st & _Fundef.fdParameters .~ params2

    (st1 & _Fundef.parameters_ .~ [p_ "test3", p_ "test4", p_ "test5"]) === st2

prop_body_1 :: Property
prop_body_1 =
  withTests 1 . property $ do
    let
      st = def_ "a" [] [line_ pass_]

    st ^? _Fundef.fdIndents === Just (Indents [] ())
    over (_Fundef.body_) id st === st

prop_body_2 :: Property
prop_body_2 =
  withTests 1 . property $ do
    let
      st = def_ "a" [] [line_ pass_]

    st ^? _Fundef.body_ === Just [line_ pass_]

prop_body_3 :: Property
prop_body_3 =
  withTests 1 . property $ do
    let
      stInner = def_ "b" [] [line_ pass_]
      stOuter = def_ "a" [] [line_ pass_, line_ stInner]

      newIndent = replicate 10 Space

    (stOuter & _Indent .~ newIndent) ^? _Fundef.body_ ===
      Just
      [ line_ $ pass_ & _Indent .~ newIndent
      , line_ $ stInner & _Indent .~ newIndent
      ]

prop_body_4 :: Property
prop_body_4 =
  withTests 1 . property $ do
    let
      newIndent = replicate 10 Space

      stInner = def_ "b" [] [line_ pass_]

      outerBody =
        [ line_ pass_
        , line_ stInner
        ]

      outerBody' =
        [ line_ $ pass_ & _Indent .~ newIndent
        , line_ $ stInner & _Indent .~ newIndent
        ]

      stOuter = def_ "a" [] outerBody & _Indent .~ newIndent

      finalBody =
        [ line_ pass_
        , line_ $ stInner & _Indent .~ newIndent
        , line_ pass_
        ]

      stFinal' = def_ "a" [] finalBody & _Indent .~ newIndent

    stOuter ^? _Fundef.body_ === Just outerBody'
    (stOuter & _Fundef.body_ .~ finalBody) === stFinal'
