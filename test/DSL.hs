{-# language OverloadedStrings #-}
module DSL (dslTests) where

import Language.Python.Internal.Render (showExpr)
import Language.Python.Syntax

import Hedgehog ((===), Group(..), Property, property)

dslTests :: [Group]
dslTests =
  [ subscriptDslTests
  ]

subscriptDslTests :: Group
subscriptDslTests =
  Group "Subscript DSL Tests"
  [ ("Subscript test 1", subscript_test_1)
  , ("Subscript test 2", subscript_test_2)
  , ("Subscript test 3", subscript_test_3)
  , ("Subscript test 4", subscript_test_4)
  , ("Subscript test 5", subscript_test_5)
  , ("Subscript test 6", subscript_test_6)
  , ("Subscript test 7", subscript_test_7)
  , ("Subscript test 8", subscript_test_8)
  , ("Subscript test 9", subscript_test_9)
  , ("Subscript test 10", subscript_test_10)
  , ("Subscript test 11", subscript_test_11)
  , ("Subscript test 12", subscript_test_12)
  , ("Subscript test 13", subscript_test_13)
  , ("Subscript test 14", subscript_test_14)
  , ("Subscript test 15", subscript_test_15)
  , ("Subscript test 16", subscript_test_16)
  ]

subscript_test_1 :: Property
subscript_test_1 =
  property $ do
    let
      expr = subs_ (var_ "a") (int_ 1)
    showExpr expr  === "a[1]"

subscript_test_2 :: Property
subscript_test_2 =
  property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ int_ 1, ti_ $ int_ 2])
    showExpr expr  === "a[1, 2]"

subscript_test_3 :: Property
subscript_test_3 =
  property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ int_ 1, s_ $ var_ "b"])
    showExpr expr  === "a[(1, *b)]"

subscript_test_4 :: Property
subscript_test_4 =
  property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ int_ 1])
    showExpr expr  === "a[1,]"

subscript_test_5 :: Property
subscript_test_5 =
  property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [s_ $ var_ "b"])
    showExpr expr  === "a[((*b),)]"

subscript_test_6 :: Property
subscript_test_6 =
  property $ do
    let
      expr = subs_ (var_ "a") (slice_ Nothing Nothing Nothing)
    showExpr expr  === "a[:]"

subscript_test_7 :: Property
subscript_test_7 =
  property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ slice_ Nothing Nothing Nothing])
    showExpr expr  === "a[:,]"

subscript_test_8 :: Property
subscript_test_8 =
  property $ do
    let
      expr = subs_ (var_ "a") (tuple_ [ti_ $ tuple_ [ti_ $ slice_ Nothing Nothing Nothing]])
    showExpr expr  === "a[(slice(None, None, None),),]"

subscript_test_9 :: Property
subscript_test_9 =
  property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_ [ti_ $ slice_ Nothing Nothing Nothing, ti_ $ slice_ Nothing Nothing Nothing]
    showExpr expr  === "a[:, :]"

subscript_test_10 :: Property
subscript_test_10 =
  property $ do
    let
      expr = subs_ (var_ "a") (slice_ (Just $ int_ 1) Nothing Nothing)
    showExpr expr  === "a[1:]"

subscript_test_11 :: Property
subscript_test_11 =
  property $ do
    let
      expr = subs_ (var_ "a") (slice_ (Just $ int_ 1) (Just $ int_ 1) Nothing)
    showExpr expr  === "a[1:1]"

subscript_test_12 :: Property
subscript_test_12 =
  property $ do
    let
      expr = subs_ (var_ "a") (slice_ (Just $ int_ 1) (Just $ int_ 1) (Just $ int_ 1))
    showExpr expr  === "a[1:1:1]"

subscript_test_13 :: Property
subscript_test_13 =
  property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_
          [ ti_ $ slice_ (Just $ int_ 1) Nothing Nothing
          , ti_ $ slice_ (Just $ int_ 2) Nothing Nothing
          ]
    showExpr expr  === "a[1:, 2:]"

subscript_test_14 :: Property
subscript_test_14 =
  property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_
          [ ti_ $ slice_ (Just $ int_ 1) (Just $ int_ 1) Nothing
          , ti_ $ slice_ (Just $ int_ 2) Nothing Nothing
          ]
    showExpr expr  === "a[1:1, 2:]"

subscript_test_15 :: Property
subscript_test_15 =
  property $ do
    let
      expr =
        subs_ (var_ "a") $
        tuple_
          [ ti_ $ slice_ (Just $ int_ 1) (Just $ int_ 1) (Just $ int_ 1)
          , ti_ $ slice_ (Just $ int_ 2) Nothing Nothing
          ]
    showExpr expr  === "a[1:1:1, 2:]"

subscript_test_16 :: Property
subscript_test_16 =
  property $ do
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
