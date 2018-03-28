{-# language DataKinds #-}
module Main where

import Control.Lens

import Example
import Data.Validate
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Scope
import Language.Python.Validate.Scope.Error
import Language.Python.Internal.Render

runExampleSyntax x =
  case validateStatementIndentation x of
    Failure errs -> print (errs :: [IndentationError '[] ()])
    Success a ->
      case runValidateSyntax initialSyntaxContext [] (validateStatementSyntax a) of
        Failure errs -> print (errs :: [SyntaxError '[Indentation] ()])
        Success a' -> putStrLn . renderLines $ renderStatement a'

runExampleScope x =
  case validateStatementIndentation x of
    Failure errs -> print (errs :: [IndentationError '[] ()])
    Success a ->
      case runValidateSyntax initialSyntaxContext [] (validateStatementSyntax a) of
        Failure errs -> print (errs :: [SyntaxError '[Indentation] ()])
        Success a' ->
          case runValidateScope initialScopeContext (validateStatementScope a') of
            Failure errs -> print (errs :: [ScopeError '[Syntax, Indentation] ()])
            Success a'' ->
              putStrLn . renderLines $ renderStatement a''

main = do
  runExampleSyntax (append_to ())
  runExampleSyntax (rewrite fixMDA append_to')
  runExampleSyntax (append_to'' ())
  runExampleSyntax bracketing
  runExampleSyntax (indentSpaces 2 append_to')
  runExampleSyntax (indentTabs append_to')
  runExampleScope (rewrite optimize_tr fact_tr)
  runExampleSyntax (rewrite optimize_tr spin)
  runExampleSyntax (rewrite optimize_tr yes)
  runExampleSyntax badly_scoped
  runExampleScope badly_scoped
