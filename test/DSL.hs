{-# language OverloadedStrings, TemplateHaskell #-}
module DSL (dslTests) where

import Hedgehog

import Language.Python.Render (showExpr)
import Language.Python.Syntax

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
