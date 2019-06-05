{-# language OverloadedStrings, TemplateHaskell #-}
module Scope (scopeTests) where

import Hedgehog

import Control.Lens ((#), has)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Validation (Validation(..), _Success)

import Language.Python.Validate
import Language.Python.DSL
import Language.Python.Optics
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Whitespace

scopeTests :: Group
scopeTests = $$discover

fullyValidate
  :: Statement ()
  -> PropertyT IO
       (Validation
          (NonEmpty (ScopeError ()))
          (Statement ()))
fullyValidate x =
  case runValidateIndentation $ validateStatementIndentation x of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError ()))
      failure
    Success a ->
      case runValidateSyntax (validateStatementSyntax a) of
        Failure errs -> do
          annotateShow (errs :: NonEmpty (SyntaxError ()))
          failure
        Success a' -> pure $ runValidateScope (validateStatementScope a')

prop_scope_1 :: Property
prop_scope_1 =
  withTests 1 . property $ do
    let
      expr =
        _Fundef #
        def_ "test" [p_ "a", p_ "b"]
          [ line_ $ if_ true_ [ line_ (var_ "c" .= 2) ]
          , line_ . return_ $ var_ "a" .+ var_ "b" .+ var_ "c"
          ]
    res <- fullyValidate expr
    res === Failure (FoundDynamic () (MkIdent (Ann ()) "c" []) :| [])

prop_scope_2 :: Property
prop_scope_2 =
  withTests 1 . property $ do
    let
      expr =
        _Fundef #
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
        _Fundef #
        def_ "test" [p_ "a", p_ "b"]
          [ line_ . return_ $ var_ "a" .+ var_ "b" .+ var_ "c" ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (NotInScope (MkIdent (Ann ()) "c" []) :| [])

prop_scope_4 :: Property
prop_scope_4 =
  withTests 1 . property $ do
    let
      expr =
        _Fundef #
        def_ "test" [p_ "a", p_ "b"]
          [ line_ $ def_ "f" [] [ line_ $ def_ "g" [] [ line_ pass_ ] ]
          , line_ $ call_ (var_ "g") []
          ]
    res <- fullyValidate expr
    res === Failure (NotInScope (MkIdent (Ann ()) "g" []) :| [])

prop_scope_5 :: Property
prop_scope_5 =
  withTests 1 . property $ do
    let
      expr =
        _Fundef #
        def_ "test" [p_ "a"]
          [ line_ $ def_ "f" [k_ "b" (var_ "c")] [ line_ pass_ ]
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (NotInScope (MkIdent (Ann ()) "c" []) :| [])

prop_scope_6 :: Property
prop_scope_6 =
  withTests 1 . property $ do
    let
      expr =
        _Fundef #
        def_ "test" []
          [ line_ $
              if_ true_ [ line_ (var_ "x" .= 2) ] &
              else_ [ line_ pass_ ]
          , line_ $ var_ "x"
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (FoundDynamic () (MkIdent (Ann ()) "x" []) :| [])

prop_scope_7 :: Property
prop_scope_7 =
  withTests 1 . property $ do
    let
      expr =
        _Fundef #
        def_ "test" []
          [ line_ $
              if_ true_ [ line_ pass_ ] &
              else_ [ line_ (var_ "x" .= 3) ]
          , line_ $ var_ "x"
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (FoundDynamic () (MkIdent (Ann ()) "x" []) :| [])

prop_scope_8 :: Property
prop_scope_8 =
  withTests 1 . property $ do
    let
      expr =
        _Fundef #
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
        _Fundef #
        def_ "test" []
          [ line_ $ for_ ("x" `in_` [ list_ [li_ $ int_ 1] ]) [ line_ pass_ ]
          , line_ $ var_ "x"
          ]
    res <- fullyValidate expr
    annotateShow res
    res === Failure (FoundDynamic () (MkIdent (Ann ()) "x" []) :| [])

prop_scope_10 :: Property
prop_scope_10 =
  withTests 1 . property $ do
    let
      expr =
        _Fundef #
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
      st =
        _Fundef #
        def_ "test" []
          [ line_ ("x" .= 2)
          , line_ $ for_ ("x" `in_` [ list_ [li_ $ int_ 1] ]) [ line_ pass_ ]
          ]
    res <- fullyValidate st
    annotateShow res
    res === Failure (BadShadowing (MkIdent (Ann ()) "x" [Space]) :| [])

prop_scope_12 :: Property
prop_scope_12 =
  withTests 1 . property $ do
    let
      st =
        _If #
        (if_ none_ [line_ (var_ "x" .= 2)] &
         else_ [line_ (var_ "y" .= var_ "x")])
    res <- fullyValidate st
    annotateShow res
    res === Failure (NotInScope (MkIdent (Ann ()) "x" []) :| [])

prop_scope_13 :: Property
prop_scope_13 =
  withTests 1 . property $ do
    let
      st =
        _Fundef #
        def_ "test" []
        [ line_ $
          if_ none_ [line_ (var_ "x" .= 1)] &
          else_ [line_ (var_ "y" .= 2)]
        , line_ $ var_ "x"
        ]
    res <- fullyValidate st
    annotateShow res
    res === Failure (FoundDynamic () (MkIdent (Ann ()) "x" []) :| [])

prop_scope_14 :: Property
prop_scope_14 =
  withTests 1 . property $ do
    let
      st =
        _Fundef #
        def_ "test" []
        [ line_ $
          if_ none_ [line_ (var_ "x" .= 1)] &
          else_ [line_ (var_ "y" .= 2)]
        , line_ $ var_ "y"
        ]
    res <- fullyValidate st
    annotateShow res
    res === Failure (FoundDynamic () (MkIdent (Ann ()) "y" []) :| [])
