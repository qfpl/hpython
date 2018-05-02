{-# language OverloadedStrings, OverloadedLists, DataKinds #-}
module Scope (scopeTests) where

import Control.Lens (has)
import Data.Validate
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Scope
import Language.Python.Validate.Scope.Error
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
  ]

validate
  :: Statement '[] ()
  -> PropertyT IO
       (Validate
          [ScopeError '[Syntax, Indentation] ()]
          (Statement '[Scope, Syntax, Indentation] ()))
validate x =
  case validateStatementIndentation x of
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
          [ if_ true_ [ var_ "c" .= 2]
          , return_ $ var_ "a" .+ var_ "b" .+ var_ "c"
          ]
    res <- validate expr
    res === Failure [FoundDynamic () (MkIdent () "c" [])]

test_2 :: Property
test_2 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ var_ "c" .= 0
          , if_ true_ [ var_ "c" .= 2]
          , return_ $ var_ "a" .+ var_ "b" .+ var_ "c"
          ]
    res <- validate expr
    annotateShow res
    assert $ has _Success res

test_3 :: Property
test_3 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ return_ $ var_ "a" .+ var_ "b" .+ var_ "c" ]
    res <- validate expr
    annotateShow res
    res === Failure [NotInScope (MkIdent () "c" [])]

test_4 :: Property
test_4 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ def_ "f" [] [ def_ "g" [] [pass_] ]
          , expr_ $ call_ (var_ "g") []
          ]
    res <- validate expr
    res === Failure [NotInScope (MkIdent () "g" [])]

test_5 :: Property
test_5 =
  property $ do
    let
      expr =
        def_ "test" [p_ "a"]
          [ def_ "f" [k_ "b" (var_ "c")] [ pass_ ]
          ]
    res <- validate expr
    annotateShow res
    res === Failure [NotInScope (MkIdent () "c" [])]

test_6 :: Property
test_6 =
  property $ do
    let
      expr =
        def_ "test" []
          [ ifElse_ true_ [ var_ "x" .= 2 ] [ var_ "x" .= 3 ]
          , call_ (var_ "print") (var_ "x")
          ]
    res <- validate expr
    annotateShow res
    res === Failure [NotInScope (MkIdent () "x" [])]
