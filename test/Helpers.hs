{-# language DataKinds #-}
module Helpers where

import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import Data.Validation (Validation(..))
import Hedgehog

import Language.Python.Internal.Lexer
  (SrcInfo, insertTabs, tokenize
  )
import Language.Python.Internal.Token (PyToken)
import Language.Python.Internal.Parse (Parser, runParser)
import Language.Python.Internal.Syntax (Statement, Expr)
import Language.Python.Validate.Syntax
import Language.Python.Validate.Indentation

doTokenize :: Monad m => Text -> PropertyT m [PyToken SrcInfo]
doTokenize input =
  case tokenize "test" input of
    Left err -> annotateShow err *> failure
    Right a -> pure a

doTabs
  :: (Semigroup ann, Show ann, Monad m)
  => ann
  -> [PyToken ann]
  -> PropertyT m [PyToken ann]
doTabs ann input =
  case insertTabs ann input of
    Left err -> annotateShow err *> failure
    Right a -> pure a

doParse :: Monad m => Parser a -> [PyToken SrcInfo] -> PropertyT m a
doParse pa input = do
  let res = runParser "test" pa input
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

syntaxValidateStatement
  :: Statement '[] ()
  -> PropertyT IO
       (Validation
          (NonEmpty (SyntaxError '[Indentation] ()))
          (Statement '[Syntax, Indentation] ()))
syntaxValidateStatement x =
  case runValidateIndentation $ validateStatementIndentation x of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError '[] ()))
      failure
    Success a ->
      pure $ runValidateSyntax initialSyntaxContext [] (validateStatementSyntax a)

syntaxValidateExpr
  :: Expr '[] ()
  -> PropertyT IO
       (Validation
          (NonEmpty (SyntaxError '[Indentation] ()))
          (Expr '[Syntax, Indentation] ()))
syntaxValidateExpr x =
  case runValidateIndentation $ validateExprIndentation x of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError '[] ()))
      failure
    Success a ->
      pure $ runValidateSyntax initialSyntaxContext [] (validateExprSyntax a)
