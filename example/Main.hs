{-# language DataKinds #-}
module Main where

import Control.Lens

import Example
import Data.Validate
import Language.Python.Validate.Syntax
import Language.Python.Validate.Syntax.Error
import Language.Python.Validate.Indentation
import Language.Python.Validate.Indentation.Error
import Language.Python.Internal.Render

runExample x =
  case validateStatementIndentation x of
    Failure errs -> print (errs :: [IndentationError '[] ()])
    Success a ->
      case validateStatementSyntax (SyntaxContext {_inLoop = False, _inFunction = False}) a of
        Failure errs -> print (errs :: [SyntaxError '[Indentation] ()])
        Success a' -> putStrLn . renderLines $ renderStatement a'

main = do
  runExample (append_to ())
  runExample (rewrite fixMDA append_to')
  runExample (append_to'' ())
  runExample bracketing
  runExample (indentSpaces 2 append_to')
  runExample (indentTabs append_to')
  runExample (rewrite optimize_tr fact_tr)
  runExample (rewrite optimize_tr spin)
  runExample (rewrite optimize_tr yes)
