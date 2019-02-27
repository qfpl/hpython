{-# language OverloadedStrings, DataKinds, TemplateHaskell #-}
module Scope (scopeTests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Lens ((#), has)
import Control.Monad (void)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Sequence (ViewR(..))
import Data.Validation (Validation(..), _Success)
import GHC.Exts (fromList)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Language.Python.Validate
import Language.Python.DSL
import Language.Python.Optics
import Language.Python.Syntax

scopeTests :: Group
scopeTests = $$discover

fullyValidate
  :: Statement '[] ()
  -> PropertyT IO
       (Validation
          (NonEmpty (ScopeError ()))
          (Statement '[Scope, Syntax, Indentation] ()))
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
        Success a' -> pure $ runValidateScope mempty (validateStatementScope a')

fullyValidateModule
  :: Module '[] ()
  -> PropertyT IO
       (Validation
          (NonEmpty (ScopeError ()))
          (Module '[Scope, Syntax, Indentation] ()))
fullyValidateModule x =
  case runValidateIndentation $ validateModuleIndentation x of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError ()))
      failure
    Success a ->
      case runValidateSyntax (validateModuleSyntax a) of
        Failure errs -> do
          annotateShow (errs :: NonEmpty (SyntaxError ()))
          failure
        Success a' -> pure $ runValidateScope mempty (validateModuleScope a')

prop_setEntry_1 :: Property
prop_setEntry_1 =
  property $ do
    keys <-
      fmap fromList . forAll $
      Gen.list (Range.constant 1 10) (Gen.bytes (Range.constant 0 10))
    let
      entry :: Entry ()
      entry = GlobalEntry mempty
    case Seq.viewr keys of
      EmptyR -> discard
      ks :> k ->
        foldr (\a b -> Map.singleton a $ GlobalEntry b) (Map.singleton k entry) ks ===
        setEntry keys entry mempty

prop_setEntry_2 :: Property
prop_setEntry_2 =
  property $ do
    keys <-
      fmap fromList . forAll $
      Gen.list (Range.constant 1 10) (Gen.bytes (Range.constant 0 10))
    case Seq.viewr keys of
      EmptyR -> discard
      ks :> k -> do
        someKey <- forAll $ Gen.bytes (Range.constant 0 10)
        let
          entry1 :: Entry ()
          entry1 = GlobalEntry mempty

          entry2 :: Entry ()
          entry2 = GlobalEntry $ Map.singleton someKey (GlobalEntry mempty)
        annotateShow entry2

        let map1 = setEntry keys entry1 mempty
        annotateShow map1

        foldr (\a b -> Map.singleton a $ GlobalEntry b) (Map.singleton k entry2) ks ===
          setEntry keys entry2 map1

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

prop_scope_15 :: Property
prop_scope_15 =
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

prop_scope_16 :: Property
prop_scope_16 =
  withTests 1 . property $ do
    let
      code =
        module_
        [ line_ $ class_ "a" [] [line_ pass_]
        , line_ ((var_ "a" /> "b") .= 1)
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b"]
        ]
    res <- fullyValidateModule code
    annotateShow res
    void res === Success ()

prop_scope_17 :: Property
prop_scope_17 =
  withTests 1 . property $ do
    let
      code =
        module_
        [ line_ $ def_ "a" [] [line_ pass_]
        , line_ ((var_ "a" /> "b") .= 1)
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b"]
        ]
    res <- fullyValidateModule code
    annotateShow res
    void res === Success ()

prop_scope_18 :: Property
prop_scope_18 =
  withTests 1 . property $ do
    let
      code =
        module_
        [ line_ (var_ "a" .= 1)
        , line_ ((var_ "a" /> "b") .= 1)
        ]
    res <- fullyValidateModule code
    annotateShow res
    void res ===
      Failure
      (MissingAttribute
         (Ident (Ann ()) $ MkIdent (Ann ()) "a" [])
         (MkIdent (Ann ()) "b" [Space]) :| [])

prop_scope_19 :: Property
prop_scope_19 =
  withTests 1 . property $ do
    let
      code =
        module_
        [ line_ $ class_ "a" [] [line_ (var_ "b" .= 1)]
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b"]
        ]
    res <- fullyValidateModule code
    annotateShow res
    void res === Success ()

prop_scope_20 :: Property
prop_scope_20 =
  withTests 1 . property $ do
    let
      code =
        module_
        [ line_ $
          class_ "a" []
          [line_ $ chainEq (var_ "b") [var_ "c", 0]
          ]
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b"]
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "c"]
        ]
    res <- fullyValidateModule code
    annotateShow res
    void res === Success ()

prop_scope_21 :: Property
prop_scope_21 =
  withTests 1 . property $ do
    let
      code =
        module_
        [ line_ $
          class_ "a" []
          [ line_ $
            decorated_ [var_ "classmethod"] $
            def_ "b" [] [line_ pass_]
          ]
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b"]
        ]
    res <- fullyValidateModule code
    annotateShow res
    void res === Success ()

prop_scope_22 :: Property
prop_scope_22 =
  withTests 1 . property $ do
    let
      code =
        module_
        [ line_ $
          class_ "a" []
          [ line_ $
            decorated_ [var_ "staticmethod"] $
            def_ "b" [] [line_ pass_]
          ]
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b"]
        ]
    res <- fullyValidateModule code
    annotateShow res
    void res === Success ()

prop_scope_23 :: Property
prop_scope_23 =
  withTests 1 . property $ do
    let
      code =
        module_
        [ line_ $
          class_ "a" []
          [ line_ $
            def_ "b" [] [line_ pass_]
          ]
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b"]
        ]
    res <- fullyValidateModule code
    annotateShow res
    void res ===
      Failure (MissingAttribute (var_ "a") "b" :| [])
