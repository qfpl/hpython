{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
module Language.Python.Internal.Parse where

import Control.Lens.Getter ((^.))
import Control.Monad (unless)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.State
  (StateT(..), get, put, evalStateT, runStateT)
import Control.Monad.Writer.Strict (Writer, runWriter, writer, tell)
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Functor.Alt (Alt((<!>)), many, optional)
import Data.Functor.Classes (liftEq)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Lexer
import Language.Python.Internal.Syntax

data ParseError ann
  = UnexpectedEndOfInput
  | UnexpectedEndOfLine
  | UnexpectedEndOfBlock
  | UnexpectedIndent
  | ExpectedIndent
  | ExpectedEndOfBlock
  | ExpectedIdentifier { peGot :: PyToken ann }
  | ExpectedString { peGot :: PyToken ann }
  | ExpectedInteger { peGot :: PyToken ann }
  | ExpectedComment { peGot :: PyToken ann }
  | ExpectedToken { peExpected :: PyToken (), peGot :: PyToken ann }
  | ExpectedEndOfLine { peGotTokens :: [PyToken ann] }
  | ExpectedEndOfInput { peGotCtxt :: [Either (Nested ann) (LogicalLine ann)] }
  deriving (Eq, Show)

newtype Consumed = Consumed { unConsumed :: Bool }
  deriving (Eq, Show)

instance Monoid Consumed where
  mempty = Consumed False
  Consumed a `mappend` Consumed b = Consumed $! a || b

newtype Parser ann a
  = Parser
  { unParser
      :: StateT
           [[Either (Nested ann) (LogicalLine ann)]]
           (ExceptT (ParseError ann) (Writer Consumed))
           a
  } deriving (Functor, Applicative, Monad)

instance Alt (Parser ann) where
  Parser pa <!> Parser pb =
    Parser $ do
      st <- get
      let (res, Consumed b) = runWriter . runExceptT $ runStateT pa st
      if b
        then StateT $ \_ -> ExceptT (writer (res, Consumed b))
        else case res of
          Left{} -> pb
          Right (a, st') -> put st' $> a

runParser :: Parser ann a -> Nested ann -> Either (ParseError ann) a
runParser (Parser p) =
  fst . runWriter .
  runExceptT .
  evalStateT p .
  pure . toList . unNested

currentToken :: Parser ann (PyToken ann)
currentToken = Parser $ do
  ctxt <- get
  case ctxt of
    [] -> throwError UnexpectedEndOfInput
    current : rest ->
      case current of
        [] -> throwError UnexpectedEndOfBlock
        Right ll@(LogicalLine _ _ _ tks nl) : rest' ->
          case tks of
            [] -> throwError UnexpectedEndOfLine
            [tk] | Nothing <- nl -> do
              put $ case rest' of
                [] -> rest
                _ -> rest' : rest
              pure tk
            tk : rest'' ->
              put ((Right (ll { llLine = rest'' }) : rest') : rest) $> tk
        Left _ : _ -> throwError UnexpectedIndent

eol :: Parser ann Newline
eol = Parser $ do
  ctxt <- get
  case ctxt of
    [] -> throwError UnexpectedEndOfInput
    current : rest ->
      case current of
        [] -> throwError UnexpectedEndOfBlock
        Left _ : _ -> throwError UnexpectedIndent
        Right ll@(LogicalLine _ _ _ tks nl) : rest' ->
          case tks of
            _ : _ -> throwError $ ExpectedEndOfLine tks
            [] ->
              case nl of
                Nothing -> throwError $ ExpectedEndOfLine tks
                Just (_, nl') -> do
                  put (rest' : rest)
                  tell (Consumed True) $> nl'

eof :: Parser ann ()
eof = Parser $ do
  ctxt <- get
  case ctxt of
    [] -> tell $ Consumed True
    current : _ -> throwError $ ExpectedEndOfInput current

indent :: Parser ann ()
indent = Parser $ do
  ctxt <- get
  case ctxt of
    [] -> throwError UnexpectedEndOfInput
    current : rest ->
      case current of
        [] -> throwError UnexpectedEndOfBlock
        Right _ : _ -> throwError ExpectedIndent
        Left inner : rest' -> do
          put $ toList (unNested inner) : (case rest' of; [] -> rest; _ -> rest' : rest)
          tell $ Consumed True

dedent :: Parser ann ()
dedent = Parser $ do
  ctxt <- get
  case ctxt of
    [] -> pure ()
    current : rest ->
      case current of
        [] -> put rest *> tell (Consumed True)
        _ -> throwError ExpectedEndOfBlock

space :: Parser ann Whitespace
space =
  Space <$ token (TkSpace ()) <!>
  Tab <$ token (TkTab ())

parseError :: ParseError ann -> Parser ann a
parseError pe = Parser $ throwError pe

token :: PyToken b -> Parser ann (PyToken ann, [Whitespace])
token tk = do
  curTk <- currentToken
  unless (liftEq (\_ _ -> True) tk curTk) .
    parseError $ ExpectedToken (() <$ tk) curTk
  Parser (tell $ Consumed True)
  (,) curTk <$> many space

identifier :: Parser ann (Ident '[] ann)
identifier = do
  curTk <- currentToken
  case curTk of
    TkIdent n ann -> do
      Parser (tell $ Consumed True)
      MkIdent ann n <$> many space
    _ -> Parser . throwError $ ExpectedIdentifier curTk

integer :: Parser ann (Expr '[] ann)
integer = do
  curTk <- currentToken
  case curTk of
    TkInt n ann -> do
      Parser (tell $ Consumed True)
      Int ann n <$> many space
    _ -> Parser . throwError $ ExpectedInteger curTk

string :: Parser ann (Expr '[] ann)
string = do
  curTk <- currentToken
  (case curTk of
    TkShortString sp qt val ann ->
      Parser (tell $ Consumed True) $>
      String ann sp
      (case qt of
         SingleQuote -> ShortSingle
         DoubleQuote -> ShortDouble)
      val
    TkLongString sp qt val ann ->
      Parser (tell $ Consumed True) $>
      String ann sp
      (case qt of
         SingleQuote -> LongSingle
         DoubleQuote -> LongDouble)
      val
    _ -> Parser . throwError $ ExpectedString curTk) <*>
    many space

between :: Parser ann left -> Parser ann right -> Parser ann a -> Parser ann a
between left right pa = left *> pa <* right

indents :: Parser ann ([Whitespace], ann)
indents = Parser $ do
  ctxt <- get
  case ctxt of
    [] -> throwError UnexpectedEndOfInput
    current : rest ->
      case current of
        [] -> throwError UnexpectedEndOfBlock
        Right ll@(LogicalLine a _ is _ _) : rest' -> pure (is, a)
        Left _ : _ -> throwError UnexpectedIndent

expr :: Parser ann (Expr '[] ann)
expr =
  binOp plusOp
  where
    plusOp :: Parser ann (BinOp ann)
    plusOp = (\(tk, ws) -> Plus (pyTokenAnn tk) ws) <$> token (TkPlus ())

    binOp :: Parser ann (BinOp ann) -> Parser ann (Expr '[] ann)
    binOp op =
      (\t ts ->
          case ts of
            [] -> t
            _ -> foldl (\tm (o, val) -> BinOp (tm ^. exprAnnotation) tm o val) t ts) <$>
     term <*>
     many ((,) <$> op <*> term)

    term = factor

    factor = power

    power = atomExpr

    atomExpr = atom

    atom =
      integer <!>
      string <!>
      (\a -> Ident (_identAnnotation a) a) <$> identifier

smallStatement :: Parser ann (SmallStatement '[] ann)
smallStatement =
  returnSt
  where
    returnSt =
      (\(tkReturn, retSpaces) -> Return (pyTokenAnn tkReturn) retSpaces) <$>
      token (TkReturn ()) <*>
      expr

statement :: Parser ann (Statement '[] ann)
statement =
  SmallStatements <$>
  smallStatement <*>
  many ((,) <$> fmap snd (token $ TkSemicolon ()) <*> smallStatement) <*>
  optional (snd <$> token (TkSemicolon ())) <*>
  (Just <$> eol <!> Nothing <$ eof)

  <!>

  CompoundStatement <$> compoundStatement

comment :: Parser ann (Comment, Newline)
comment = do
  curTk <- currentToken
  case curTk of
    TkComment str nl _ -> Parser (tell $ Consumed True) $> (Comment str, nl)
    _ -> Parser . throwError $ ExpectedComment curTk

block :: Parser ann (Block '[] ann)
block = fmap Block $ (:|) <$> line <*> many line
  where
    commentOrEmpty = do
      cmt <- optional comment
      case cmt of
        Nothing -> (,) Nothing <$> eol
        Just (cmt', nl) -> pure (Just cmt', nl)

    line = do
      (ws, a) <- indents
      fmap ((,,) a ws) $
        Left <$> commentOrEmpty <!>
        Right <$> statement

comma :: Parser ann (PyToken ann, [Whitespace])
comma = token $ TkComma ()

commaSep :: Parser ann a -> Parser ann (CommaSep a)
commaSep pa =
  (\a -> maybe (CommaSepOne a) (uncurry $ CommaSepMany a)) <$>
  pa <*>
  optional ((,) <$> (snd <$> comma) <*> commaSep pa)

  <!>

  pure CommaSepNone

param :: Parser ann (Param '[] ann)
param =
  (\a ->
     maybe
       (PositionalParam (_identAnnotation a) a)
       (uncurry $ KeywordParam (_identAnnotation a) a)) <$>
  identifier <*>
  optional ((,) <$> (snd <$> token (TkEq ())) <*> expr)

  <!>

  (\(a, b) -> StarParam (pyTokenAnn a) b) <$> token (TkStar ()) <*> identifier

  <!>

  (\(a, b) -> DoubleStarParam (pyTokenAnn a) b) <$> token (TkDoubleStar ()) <*> identifier

compoundStatement :: Parser ann (CompoundStatement '[] ann)
compoundStatement = fundef
  where
    fundef =
      (\(tkDef, defSpaces) -> Fundef (pyTokenAnn tkDef) (NonEmpty.fromList defSpaces)) <$>
      token (TkDef ()) <*>
      identifier <*>
      fmap snd (token $ TkLeftParen ()) <*>
      commaSep param <*>
      fmap snd (token $ TkRightParen ()) <*>
      fmap snd (token $ TkColon ()) <*>
      eol <*
      indent <*>
      block <*
      dedent

module_ :: Parser ann (Module '[] ann)
module_ =
  Module <$>
  many (Left <$> maybeComment <!> Right <$> statement)
  where
    maybeComment =
      (\ws (cmt, nl) -> (ws, cmt, nl)) <$>
      fmap fst indents <*>
      (maybe (Nothing, Nothing) (\(a, b) -> (Just a, Just b)) <$>
       optional comment

       <!>

       fmap ((,) Nothing) (Just <$> eol <!> Nothing <$ eof))
