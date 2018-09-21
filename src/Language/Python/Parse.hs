{-# language ApplicativeDo, DataKinds #-}
module Language.Python.Parse
  ( SrcInfo(..)
  , initialSrcInfo
  , ParseError(..)
  , ErrorItem(..)
  , parseModule
  , parseStatement
  , parseExpr
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)
import Data.Validation (Validation, bindValidation, fromEither)
import Data.Void (Void)
import Text.Megaparsec.Error (ErrorItem(..))
import Text.Megaparsec.Pos (SourcePos(..), unPos)

import Language.Python.Internal.Lexer
  (SrcInfo(..), initialSrcInfo, Line(..), indentation, logicalLines, nested, tokenize)
import Language.Python.Internal.Token (PyToken)
import Language.Python.Internal.Syntax (Module, Statement, Expr)

import qualified Text.Megaparsec.Error as Megaparsec
import qualified Language.Python.Internal.Lexer as Lexer
import qualified Language.Python.Internal.Parse as Parse
import qualified Language.Python.Internal.Syntax.IR as IR

data ParseError a
  = LexicalError (NonEmpty a) (Maybe (ErrorItem Char)) (Set (ErrorItem Char))
  | TabError a
  | IncorrectDedent a
  | UnexpectedDedent a
  | UnexpectedEndOfInput a
  | UnexpectedEndOfLine a
  | UnexpectedEndOfBlock a
  | UnexpectedIndent a
  | ExpectedDedent a
  | ExpectedIndent a
  | ExpectedEndOfBlock { peGotCtxt :: Line a }
  | ExpectedIdentifier { peGot :: PyToken a }
  | ExpectedContinued { peGot :: PyToken a }
  | ExpectedNewline { peGot :: PyToken a }
  | ExpectedStringOrBytes { peGot :: PyToken a }
  | ExpectedInteger { peGot :: PyToken a }
  | ExpectedFloat { peGot :: PyToken a }
  | ExpectedImag { peGot :: PyToken a }
  | ExpectedComment { peGot :: PyToken a }
  | ExpectedToken { peExpected :: PyToken (), peGot :: PyToken a }
  | ExpectedEndOfLine { peGotTokens :: [PyToken a] }
  | ExpectedEndOfInput { peGotCtxt :: Line a }
  | InvalidUnpacking a
  deriving (Eq, Show)

fromLexicalError :: Megaparsec.ParseError Char Void -> ParseError SrcInfo
fromLexicalError Megaparsec.FancyError{} = error "there are none of these"
fromLexicalError (Megaparsec.TrivialError pos a b) =
  LexicalError ((\(SourcePos x y z) -> SrcInfo x (unPos y) (unPos z) Nothing) <$> pos) a b

fromTabError :: Lexer.TabError a -> ParseError a
fromTabError e =
  case e of
    Lexer.TabError a -> TabError a
    Lexer.IncorrectDedent a -> IncorrectDedent a

fromIndentationError :: Lexer.IndentationError a -> ParseError a
fromIndentationError e =
  case e of
    Lexer.UnexpectedDedent a -> UnexpectedDedent a
    Lexer.ExpectedDedent a -> ExpectedDedent a

fromParseError :: Parse.ParseError a -> ParseError a
fromParseError e =
  case e of
    Parse.UnexpectedEndOfInput ann -> UnexpectedEndOfInput ann
    Parse.UnexpectedEndOfLine ann -> UnexpectedEndOfLine ann
    Parse.UnexpectedEndOfBlock ann -> UnexpectedEndOfBlock ann
    Parse.UnexpectedIndent ann -> UnexpectedIndent ann
    Parse.ExpectedIndent ann -> ExpectedIndent ann
    Parse.ExpectedEndOfBlock a -> ExpectedEndOfBlock a
    Parse.ExpectedIdentifier a -> ExpectedIdentifier a
    Parse.ExpectedContinued a -> ExpectedContinued a
    Parse.ExpectedNewline a -> ExpectedNewline a
    Parse.ExpectedStringOrBytes a -> ExpectedStringOrBytes a
    Parse.ExpectedInteger a -> ExpectedInteger a
    Parse.ExpectedFloat a -> ExpectedFloat a
    Parse.ExpectedImag a -> ExpectedImag a
    Parse.ExpectedComment a -> ExpectedComment a
    Parse.ExpectedToken a b -> ExpectedToken a b
    Parse.ExpectedEndOfLine a -> ExpectedEndOfLine a
    Parse.ExpectedEndOfInput a -> ExpectedEndOfInput a

fromIRError :: IR.IRError a -> ParseError a
fromIRError e =
  case e of
    IR.InvalidUnpacking a -> InvalidUnpacking a

parseModule :: FilePath -> Text -> Validation (NonEmpty (ParseError SrcInfo)) (Module '[] SrcInfo)
parseModule fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- first fromLexicalError $ tokenize fp input
      let ls = logicalLines tokens
      ils <- first fromTabError $ indentation si ls
      nst <- first fromIndentationError $ nested ils
      first fromParseError $ Parse.runParser si Parse.module_ nst
  in
    fromEither (first pure ir) `bindValidation` (first (fmap fromIRError) . IR.fromIR)

parseStatement :: FilePath -> Text -> Validation (NonEmpty (ParseError SrcInfo)) (Statement '[] SrcInfo)
parseStatement fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- first fromLexicalError $ tokenize fp input
      let ls = logicalLines tokens
      ils <- first fromTabError $ indentation si ls
      nst <- first fromIndentationError $ nested ils
      first fromParseError $ Parse.runParser si Parse.statement nst
  in
    fromEither (first pure ir) `bindValidation` (first (fmap fromIRError) . IR.fromIR_statement)

parseExpr :: FilePath -> Text -> Validation (NonEmpty (ParseError SrcInfo)) (Expr '[] SrcInfo)
parseExpr fp input =
  let
    si = initialSrcInfo fp
    ir = do
      tokens <- first fromLexicalError $ tokenize fp input
      let ls = logicalLines tokens
      ils <- first fromTabError $ indentation si ls
      nst <- first fromIndentationError $ nested ils
      first fromParseError $ Parse.runParser si (Parse.exprList Parse.space) nst
  in
    fromEither (first pure ir) `bindValidation` (first (fmap fromIRError) . IR.fromIR_expr)
