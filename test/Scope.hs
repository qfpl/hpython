{-# language OverloadedStrings, DataKinds, TemplateHaskell #-}
module Scope (scopeTests) where

import Hedgehog

import Control.Lens (has)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Validation (Validation(..), _Success)

import Language.Python.Validate
import Language.Python.Internal.Syntax
import Language.Python.Syntax
import Language.Python.Syntax.Whitespace

scopeTests :: Group
scopeTests = $$discover

fullyValidate
  :: Statement '[] ()
  -> PropertyT IO
       (Validation
          (NonEmpty (ScopeError '[Syntax, Indentation] ()))
          (Statement '[Scope, Syntax, Indentation] ()))
fullyValidate x =
  case runValidateIndentation $ validateStatementIndentation x of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError '[] ()))
      failure
    Success a ->
      case runValidateSyntax initialSyntaxContext [] (validateStatementSyntax a) of
        Failure errs -> do
          annotateShow (errs :: NonEmpty (SyntaxError '[Indentation] ()))
          failure
        Success a' -> pure $ runValidateScope initialScopeContext (validateStatementScope a')

prop_scope_1 :: Property
prop_scope_1 =
  withTests 1 . property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ line_ $ if_ true_ [ line_ (var_ "c" .= 2) ]
          , line_ . return_ $ var_ "a" .+ var_ "b" .+ var_ "c"
          ]
    res <- fullyValidate expr
    res === Failure (FoundDynamic () (MkIdent () "c" []) :| [])

prop_scope_2 :: Property
prop_scope_2 =
  withTests 1 . property $ do
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

prop_scope_3 :: Property
prop_scope_3 =
  withTests 1 . property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ line_ . return_ $ var_ "a" .+ var_ "b" .+ var_ "c" ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (NotInScope (MkIdent () "c" []) :| [])

prop_scope_4 :: Property
prop_scope_4 =
  withTests 1 . property $ do
    let
      expr =
        def_ "test" [p_ "a", p_ "b"]
          [ line_ $ def_ "f" [] [ line_ $ def_ "g" [] [ line_ pass_ ] ]
          , line_ $ call_ (var_ "g") []
          ]
    res <- fullyValidate expr
    res === Failure (NotInScope (MkIdent () "g" []) :| [])

prop_scope_5 :: Property
prop_scope_5 =
  withTests 1 . property $ do
    let
      expr =
        def_ "test" [p_ "a"]
          [ line_ $ def_ "f" [k_ "b" (var_ "c")] [ line_ pass_ ]
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (NotInScope (MkIdent () "c" []) :| [])

prop_scope_6 :: Property
prop_scope_6 =
  withTests 1 . property $ do
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
    res === Failure (FoundDynamic () (MkIdent () "x" []) :| [])

prop_scope_7 :: Property
prop_scope_7 =
  withTests 1 . property $ do
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
    res === Failure (FoundDynamic () (MkIdent () "x" []) :| [])

prop_scope_8 :: Property
prop_scope_8 =
  withTests 1 . property $ do
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

prop_scope_9 :: Property
prop_scope_9 =
  withTests 1 . property $ do
    let
      expr =
        def_ "test" []
          [ line_ $ for_ ("x" `in_` [ list_ [li_ $ int_ 1] ]) [ line_ pass_ ]
          , line_ $ var_ "x"
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (FoundDynamic () (MkIdent () "x" []) :| [])

prop_scope_10 :: Property
prop_scope_10 =
  withTests 1 . property $ do
    let
      expr =
        def_ "test" []
          [ line_ $ for_ ("x" `in_` [ list_ [li_ $ int_ 1] ]) [ line_ $ var_ "x" ]
          ]
    res <- fullyValidate expr
    annotateShow res
    (res $> ()) === Success ()

prop_scope_11 :: Property
prop_scope_11 =
  withTests 1 . property $ do
    let
      expr =
        def_ "test" []
          [ line_ ("x" .= 2)
          , line_ $ for_ ("x" `in_` [ list_ [li_ $ int_ 1] ]) [ line_ pass_ ]
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (BadShadowing (MkIdent () "x" [Space]) :| [])
