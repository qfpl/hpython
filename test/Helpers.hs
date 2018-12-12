{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Helpers where

import Hedgehog

import Control.Lens.Fold ((^?), folded)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import Data.Validation (Validation(..), _Failure)
import Text.Megaparsec.Pos (SourcePos(..), mkPos)

import Language.Python.Internal.Lexer
  (SrcInfo, insertTabs, tokenize
  )
import Language.Python.Internal.Token (PyToken)
import Language.Python.Parse (Parser)
import Language.Python.Parse.Error (ParseError, ErrorItem(..), _ParseError)
import Language.Python.Internal.Parse (runParser)
import Language.Python.Syntax.Expr (Expr)
import Language.Python.Syntax.Module (Module)
import Language.Python.Syntax.Statement (Statement)
import Language.Python.Validate

doTokenize :: Monad m => Text -> PropertyT m [PyToken SrcInfo]
doTokenize input =
  case tokenize "test" input of
    Left err -> annotateShow (err :: ParseError SrcInfo) *> failure
    Right a -> pure a

doTabs
  :: forall ann m
   . (Semigroup ann, Show ann, Monad m)
  => ann
  -> [PyToken ann]
  -> PropertyT m [PyToken ann]
doTabs ann input =
  case insertTabs ann input of
    Left err -> annotateShow (err :: ParseError ann) *> failure
    Right a -> pure a

doParse :: Monad m => Parser a -> [PyToken SrcInfo] -> PropertyT m a
doParse pa input = do
  let res = runParser "test" pa input
  case res of
    Left err -> do
      annotateShow (err :: ParseError SrcInfo)
      failure
    Right a -> pure a

syntaxValidateModule
  :: Module '[] ()
  -> PropertyT IO
       (Validation
          (NonEmpty (SyntaxError ()))
          (Module '[Syntax, Indentation] ()))
syntaxValidateModule x =
  case runValidateIndentation $ validateModuleIndentation x of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError ()))
      failure
    Success a ->
      pure $ runValidateSyntax (validateModuleSyntax a)

syntaxValidateStatement
  :: Statement '[] ()
  -> PropertyT IO
       (Validation
          (NonEmpty (SyntaxError ()))
          (Statement '[Syntax, Indentation] ()))
syntaxValidateStatement x =
  case runValidateIndentation $ validateStatementIndentation x of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError ()))
      failure
    Success a ->
      pure $ runValidateSyntax (validateStatementSyntax a)

syntaxValidateExpr
  :: Expr '[] ()
  -> PropertyT IO
       (Validation
          (NonEmpty (SyntaxError ()))
          (Expr '[Syntax, Indentation] ()))
syntaxValidateExpr x =
  case runValidateIndentation $ validateExprIndentation x of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError ()))
      failure
    Success a ->
      pure $ runValidateSyntax (validateExprSyntax a)

shouldBeFailure :: MonadTest m => Validation e a -> m ()
shouldBeFailure res =
  case res of
    Success{} -> failure
    Failure{} -> success

shouldBeSuccess :: (MonadTest m, Show e) => Validation e a -> m a
shouldBeSuccess res =
  case res of
    Success a -> pure a
    Failure err -> do
      annotateShow err
      failure

shouldBeParseSuccess
  :: MonadTest m
  => (FilePath -> Text -> Validation (NonEmpty (ParseError SrcInfo)) a)
  -> Text -> m a
shouldBeParseSuccess p = shouldBeSuccess . p "test"

shouldBeParseFailure
  :: MonadTest m
  => (FilePath -> Text -> Validation (NonEmpty (ParseError SrcInfo)) a)
  -> Text -> m ()
shouldBeParseFailure p = shouldBeFailure . p "test"

shouldBeParseError
  :: (MonadTest m, Show e, Show a)
  => Int
  -> Int
  -> PyToken ()
  -> Validation (NonEmpty (ParseError e)) a
  -> m ()
shouldBeParseError line col tk res =
  case res ^? _Failure.folded._ParseError of
    Just (srcPos :| _, Just (Tokens (errorItem :| [])), _) -> do
      sourceLine srcPos === mkPos line
      sourceColumn srcPos === mkPos col

      void errorItem === tk
    _ -> do
      annotateShow res
      failure

shouldBeSyntaxError
  :: (MonadTest m, Show a)
  => SyntaxError ()
  -> Validation (NonEmpty (SyntaxError ())) a
  -> m ()
shouldBeSyntaxError err res =
  case res ^? _Failure.folded of
    Just err' -> err === err'
    _ -> do
      annotateShow res
      failure
