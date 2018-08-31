{-# language OverloadedStrings, OverloadedLists, DataKinds #-}
module Scope (scopeTests) where

import Control.Lens (has)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Validate (Validate(..), _Success)

import Language.Python.Validate.Syntax
import Language.Python.Validate.Indentation
import Language.Python.Validate.Scope
import Language.Python.Internal.Syntax
import Language.Python.Syntax

import Hedgehog

scopeTests :: Group
scopeTests =
  Group "Scope tests"
  [ ("Scope test 1", withTests 1 test_1)
  , ("Scope test 2", withTests 1 test_2)
  , ("Scope test 3", withTests 1 test_3)
  , ("Scope test 4", withTests 1 test_4)
  , ("Scope test 5", withTests 1 test_5)
  , ("Scope test 6", withTests 1 test_6)
  , ("Scope test 7", withTests 1 test_7)
  , ("Scope test 8", withTests 1 test_8)
  , ("Scope test 9", withTests 1 test_9)
  , ("Scope test 10", withTests 1 test_10)
  , ("Scope test 11", withTests 1 test_11)
  ]

fullyValidate
  :: Statement '[] ()
  -> PropertyT IO
       (Validate
          [ScopeError '[Syntax, Indentation] ()]
          (Statement '[Scope, Syntax, Indentation] ()))
fullyValidate x =
  case runValidateIndentation $ validateStatementIndentation x of
    Failure errs -> do
      annotateShow (errs :: [IndentationError '[] ()])
      failure
    Success a ->
      case runValidateSyntax initialSyntaxContext [] (validateStatementSyntax a) of
        Failure errs -> do
          annotateShow (errs :: [SyntaxError '[Indentation] ()])
          failure
        Success a' -> pure $ runValidateScope initialScopeContext (validateStatementScope a')

test_1 :: Property
test_1 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ line_ $ if_ true_ [ line_ (var_ "c" .= 2) ]
          , line_ . return_ $ var_ "a" .+ var_ "b" .+ var_ "c"
          ]
    res <- fullyValidate expr
    res === Failure [FoundDynamic () (MkIdent () "c" [])]

test_2 :: Property
test_2 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ line_ (var_ "c" .= 0)
          , line_ $ if_ true_ [ line_ (var_ "c" .= 2) ]
          , line_ . return_ $ var_ "a" .+ var_ "b" .+ var_ "c"
          ]
    res <- fullyValidate expr
    annotateShow res
    assert $ has _Success res

test_3 :: Property
test_3 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ line_ . return_ $ var_ "a" .+ var_ "b" .+ var_ "c" ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure [NotInScope (MkIdent () "c" [])]

test_4 :: Property
test_4 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ line_ $ def_ "f" [] [ line_ $ def_ "g" [] [ line_ pass_ ] ]
          , line_ $ call_ (var_ "g") []
          ]
    res <- fullyValidate expr
    res === Failure [NotInScope (MkIdent () "g" [])]

test_5 :: Property
test_5 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a"]
          [ line_ $ def_ "f" [k_ "b" (var_ "c")] [ line_ pass_ ]
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure [NotInScope (MkIdent () "c" [])]

test_6 :: Property
test_6 =
  property $ do
    let
      expr =
        def_ "test" []
          [ line_ $
              if_ true_ [ line_ (var_ "x" .= 2) ] &
              else_ [ line_ pass_ ]
          , line_ $ var_ "x"
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure [FoundDynamic () (MkIdent () "x" [])]

test_7 :: Property
test_7 =
  property $ do
    let
      expr =
        def_ "test" []
          [ line_ $
              if_ true_ [ line_ pass_ ] &
              else_ [ line_ (var_ "x" .= 3) ]
          , line_ $ var_ "x"
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure [FoundDynamic () (MkIdent () "x" [])]

test_8 :: Property
test_8 =
  property $ do
    let
      expr =
        def_ "test" []
          [ line_ $
              if_ true_ [ line_ pass_ ] &
              else_ [ line_ (var_ "x" .= 3) ]
          , line_ (var_ "x" .= 1)
          , line_ $ var_ "x"
          ]
    res <- fullyValidate expr
    annotateShow res
    (res $> ()) === Success ()

test_9 :: Property
test_9 =
  property $ do
    let
      expr =
        def_ "test" []
          [ line_ $ for_ "x" (list_ [1]) [ line_ pass_ ]
          , line_ $ var_ "x"
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure [FoundDynamic () (MkIdent () "x" [])]

test_10 :: Property
test_10 =
  property $ do
    let
      expr =
        def_ "test" []
          [ line_ $ for_ "x" (list_ [1]) [ line_ $ var_ "x" ]
          ]
    res <- fullyValidate expr
    annotateShow res
    (res $> ()) === Success ()

test_11 :: Property
test_11 =
  property $ do
    let
      expr =
        def_ "test" []
          [ line_ ("x" .= 2)
          , line_ $ for_ "x" (list_ [1]) [ line_ pass_ ]
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure [BadShadowing (MkIdent () "x" [Space])]
