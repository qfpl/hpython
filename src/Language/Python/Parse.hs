{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}

{-|
Module      : Language.Python.Parse
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Parse
  ( Parser
  , parseModule
  , parseStatement
  , parseExpr
  , parseExprList
    -- * Source Information
  , SrcInfo(..), initialSrcInfo
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Validation (Validation, bindValidation, fromEither)
import Data.Void (Void)
import Text.Megaparsec (eof)

import Language.Python.Internal.Lexer
  ( AsLexicalError(..), AsTabError(..)
  , SrcInfo(..), initialSrcInfo, withSrcInfo
  , tokenize, insertTabs
  )
import Language.Python.Internal.Token (PyToken)
import Language.Python.Internal.Parse
  (AsParseError, Parser, runParser, level, module_, statement, exprList, expr, space)
import Language.Python.Syntax.Expr (Expr)
import Language.Python.Internal.Syntax.IR (AsIRError)
import Language.Python.Syntax.Module (Module)
import Language.Python.Syntax.Statement (Statement)
import Language.Python.Syntax.Whitespace (Indents (..))

import qualified Language.Python.Internal.Syntax.IR as IR

parseModule
  :: ( AsLexicalError e Char Void
     , AsTabError e SrcInfo
     , AsParseError e (PyToken SrcInfo) Void
     , AsIRError e SrcInfo
     )
  => FilePath
  -> Text
  -> Validation (NonEmpty e) (Module '[] SrcInfo)
parseModule fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- tokenize fp input
      tabbed <- insertTabs si tokens
      runParser fp module_ tabbed
  in
    fromEither (first pure ir) `bindValidation` IR.fromIR

parseStatement
  :: ( AsLexicalError e Char Void
     , AsTabError e SrcInfo
     , AsParseError e (PyToken SrcInfo) Void
     , AsIRError e SrcInfo
     )
  => FilePath
  -> Text
  -> Validation (NonEmpty e) (Statement '[] SrcInfo)
parseStatement fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- tokenize fp input
      tabbed <- insertTabs si tokens
      runParser fp ((statement tlIndent =<< tlIndent) <* eof) tabbed
  in
    fromEither (first pure ir) `bindValidation` IR.fromIR_statement
  where
    tlIndent = level <|> withSrcInfo (pure $ Indents [])

parseExprList
  :: ( AsLexicalError e Char Void
     , AsTabError e SrcInfo
     , AsParseError e (PyToken SrcInfo) Void
     , AsIRError e SrcInfo
     )
  => FilePath
  -> Text
  -> Validation (NonEmpty e) (Expr '[] SrcInfo)
parseExprList fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- tokenize fp input
      tabbed <- insertTabs si tokens
      runParser fp (exprList space <* eof) tabbed
  in
    fromEither (first pure ir) `bindValidation` IR.fromIR_expr

parseExpr
  :: ( AsLexicalError e Char Void
     , AsTabError e SrcInfo
     , AsParseError e (PyToken SrcInfo) Void
     , AsIRError e SrcInfo
     )
  => FilePath
  -> Text
  -> Validation (NonEmpty e) (Expr '[] SrcInfo)
parseExpr fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- tokenize fp input
      tabbed <- insertTabs si tokens
      runParser fp (expr space <* eof) tabbed
  in
    fromEither (first pure ir) `bindValidation` IR.fromIR_expr
