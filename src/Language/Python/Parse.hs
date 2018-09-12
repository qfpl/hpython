{-# language ApplicativeDo, DataKinds #-}
module Language.Python.Parse
  ( SrcInfo(..)
  , initialSrcInfo
  , ParseError(..)
  , ErrorItem(..)
  , Parse.Parser
  , parseModule
  , parseStatement
  , parseExpr
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)
import Data.Validate (Validate, bindValidate, fromEither)
import Data.Void (Void)
import Text.Megaparsec.Error (ErrorItem(..))
import Text.Megaparsec.Pos (SourcePos(..))

import Language.Python.Internal.Lexer
  (SrcInfo(..), initialSrcInfo, tokenize, insertTabs)
import Language.Python.Internal.Token (PyToken)
import Language.Python.Internal.Syntax (Module, Statement, Expr, Indents(..))

import qualified Text.Megaparsec.Error as Megaparsec
import qualified Language.Python.Internal.Lexer as Lexer
import qualified Language.Python.Internal.Parse as Parse
import qualified Language.Python.Internal.Syntax.IR as IR

data ParseError a
  = LexicalError (NonEmpty SourcePos) (Maybe (ErrorItem Char)) (Set (ErrorItem Char))
  | ParseError (NonEmpty SourcePos) (Maybe (ErrorItem (PyToken a))) (Set (ErrorItem (PyToken a)))
  | TabError a
  | IncorrectDedent a
  | ExpectedDedent a
  | InvalidUnpacking a
  deriving (Eq, Show)

fromLexicalError :: Megaparsec.ParseError Char Void -> ParseError SrcInfo
fromLexicalError Megaparsec.FancyError{} = error "there are none of these"
fromLexicalError (Megaparsec.TrivialError pos a b) = LexicalError pos a b

fromTabError :: Lexer.TabError a -> ParseError a
fromTabError e =
  case e of
    Lexer.TabError a -> TabError a
    Lexer.IncorrectDedent a -> IncorrectDedent a

fromParseError :: Megaparsec.ParseError (PyToken SrcInfo) Void -> ParseError SrcInfo
fromParseError Megaparsec.FancyError{} = error "there are none of these"
fromParseError (Megaparsec.TrivialError pos a b) = ParseError pos a b

fromIRError :: IR.IRError a -> ParseError a
fromIRError e =
  case e of
    IR.InvalidUnpacking a -> InvalidUnpacking a

parseModule :: FilePath -> Text -> Validate [ParseError SrcInfo] (Module '[] SrcInfo)
parseModule fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- first fromLexicalError $ tokenize fp input
      tabbed <- first fromTabError $ insertTabs si tokens
      first fromParseError $ Parse.runParser fp Parse.module_ tabbed
  in
    fromEither (first pure ir) `bindValidate` (first (fmap fromIRError) . IR.fromIR)

parseStatement :: FilePath -> Text -> Validate [ParseError SrcInfo] (Statement '[] SrcInfo)
parseStatement fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- first fromLexicalError $ tokenize fp input
      tabbed <- first fromTabError $ insertTabs si tokens
      first fromParseError $ Parse.runParser fp Parse.statement tabbed
  in
    fromEither (first pure ir) `bindValidate`
    (first (fmap fromIRError) . IR.fromIR_statement . ($ Indents [] si))

parseExpr :: FilePath -> Text -> Validate [ParseError SrcInfo] (Expr '[] SrcInfo)
parseExpr fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- first fromLexicalError $ tokenize fp input
      tabbed <- first fromTabError $ insertTabs si tokens
      first fromParseError $ Parse.runParser fp (Parse.exprList Parse.space) tabbed
  in
    fromEither (first pure ir) `bindValidate` (first (fmap fromIRError) . IR.fromIR_expr)
