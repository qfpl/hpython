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
  | ExpectedContinued { peGot :: PyToken ann }
  | ExpectedNewline { peGot :: PyToken ann }
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

continued :: Parser ann Whitespace
continued = do
  curTk <- currentToken
  case curTk of
    TkContinued nl _ -> do
      Parser (tell $ Consumed True)
      Continued nl <$> many space
    _ -> Parser . throwError $ ExpectedContinued curTk

newline :: Parser ann Newline
newline = do
  curTk <- currentToken
  case curTk of
    TkNewline nl _ -> do
      Parser (tell $ Consumed True)
      pure nl
    _ -> Parser . throwError $ ExpectedNewline curTk

anySpace :: Parser ann Whitespace
anySpace =
  Space <$ tokenEq (TkSpace ()) <!>
  Tab <$ tokenEq (TkTab ()) <!>
  continued <!>
  Newline <$> newline

space :: Parser ann Whitespace
space =
  Space <$ tokenEq (TkSpace ()) <!>
  Tab <$ tokenEq (TkTab ()) <!>
  continued

parseError :: ParseError ann -> Parser ann a
parseError pe = Parser $ throwError pe

tokenEq :: PyToken b -> Parser ann (PyToken ann)
tokenEq tk = do
  curTk <- currentToken
  unless (liftEq (\_ _ -> True) tk curTk) .
    parseError $ ExpectedToken (() <$ tk) curTk
  Parser (tell $ Consumed True)
  pure curTk

token :: Parser ann Whitespace -> PyToken b -> Parser ann (PyToken ann, [Whitespace])
token ws tk = do
  curTk <- tokenEq tk
  (,) curTk <$> many ws

identifier :: Parser ann Whitespace -> Parser ann (Ident '[] ann)
identifier ws = do
  curTk <- currentToken
  case curTk of
    TkIdent n ann -> do
      Parser (tell $ Consumed True)
      MkIdent ann n <$> many ws
    _ -> Parser . throwError $ ExpectedIdentifier curTk

bool :: Parser ann Whitespace -> Parser ann (Expr '[] ann)
bool ws =
  (\(tk, s) ->
     Bool
       (pyTokenAnn tk)
       (case tk of
          TkTrue{} -> True
          TkFalse{} -> False
          _ -> error "impossible")
       s) <$>
  (token ws (TkTrue ()) <!> token ws (TkFalse ()))

integer :: Parser ann Whitespace -> Parser ann (Expr '[] ann)
integer ws = do
  curTk <- currentToken
  case curTk of
    TkInt n ann -> do
      Parser (tell $ Consumed True)
      Int ann n <$> many ws
    _ -> Parser . throwError $ ExpectedInteger curTk

string :: Parser ann Whitespace -> Parser ann (Expr '[] ann)
string ws = do
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
    many ws

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

expr :: Parser ann Whitespace -> Parser ann (Expr '[] ann)
expr ws = arithExpr
  where
    arithOp = (\(tk, ws) -> Plus (pyTokenAnn tk) ws) <$> token ws (TkPlus ())

    termOp =
      (\(tk, ws) -> Multiply (pyTokenAnn tk) ws) <$> token ws (TkStar ()) <!>
      (\(tk, ws) -> Divide (pyTokenAnn tk) ws) <$> token ws (TkDoubleSlash ())

    binOp op tm =
      (\t ts ->
          case ts of
            [] -> t
            _ -> foldl (\tm (o, val) -> BinOp (tm ^. exprAnnotation) tm o val) t ts) <$>
     tm <*>
     many ((,) <$> op <*> tm)

    arithExpr = binOp arithOp term

    term = binOp termOp factor

    factor = power

    power = atomExpr

    atomExpr = atom

    parens =
      (\(a, b) c d -> Parens (pyTokenAnn a) b c d) <$>
      token anySpace (TkLeftParen ()) <*>
      expr anySpace <*>
      fmap snd (token space $ TkRightParen ())

    atom =
      (\(tk, s) -> List (pyTokenAnn tk) s) <$>
      token ws (TkLeftBracket ()) <*>
      optional (commaSep1' ws $ expr ws) <*>
      (snd <$> token ws (TkRightBracket()))

      <!>

      bool ws <!>
      integer ws <!>
      string ws <!>
      (\a -> Ident (_identAnnotation a) a) <$> identifier ws <!>
      parens

smallStatement :: Parser ann (SmallStatement '[] ann)
smallStatement =
  returnSt <!>
  passSt <!>
  breakSt <!>
  continueSt <!>
  exprSt
  where
    returnSt =
      (\(tkReturn, retSpaces) -> Return (pyTokenAnn tkReturn) retSpaces) <$>
      token space (TkReturn ()) <*>
      expr space

    passSt = Pass . pyTokenAnn <$> tokenEq (TkPass ())
    breakSt = Break . pyTokenAnn <$> tokenEq (TkBreak ())
    continueSt = Continue . pyTokenAnn <$> tokenEq (TkContinue ())

    exprSt = (\a -> Expr (_exprAnnotation a) a) <$> expr space

sepBy1' :: Parser ann a -> Parser ann sep -> Parser ann (a, [(sep, a)], Maybe sep)
sepBy1' val sep = go
  where
    go =
      (\a b ->
         case b of
           Nothing -> (a, [], Nothing)
           Just (sc, b') ->
             case b' of
               Nothing -> (a, [], Just sc)
               Just (a', ls, sc') -> (a, (sc, a') : ls, sc')) <$>
      val <*>
      optional ((,) <$> sep <*> optional go)

statement :: Parser ann (Statement '[] ann)
statement =
  (\(a, b, c) -> SmallStatements a b c) <$>
  sepBy1' smallStatement (snd <$> semicolon space) <*>
  (Just <$> eol <!> Nothing <$ eof)

  <!>

  CompoundStatement <$> compoundStatement
  where
    smallst1 =
      (\a b ->
         case b of
           Nothing -> (a, [], Nothing)
           Just (sc, b') ->
             case b' of
               Nothing -> (a, [], Just sc)
               Just (a', ls, sc') -> (a, (sc, a') : ls, sc')) <$>
      smallStatement <*>
      smallst2

    smallst2 =
      optional ((,) <$> (snd <$> semicolon space) <*> optional smallst1)

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

comma :: Parser ann Whitespace -> Parser ann (PyToken ann, [Whitespace])
comma ws = token ws $ TkComma ()

colon :: Parser ann Whitespace -> Parser ann (PyToken ann, [Whitespace])
colon ws = token ws $ TkColon ()

semicolon :: Parser ann Whitespace -> Parser ann (PyToken ann, [Whitespace])
semicolon ws = token ws $ TkSemicolon ()

commaSep :: Parser ann Whitespace -> Parser ann a -> Parser ann (CommaSep a)
commaSep ws pa =
  (\a -> maybe (CommaSepOne a) (uncurry $ CommaSepMany a)) <$>
  pa <*>
  optional ((,) <$> (snd <$> comma ws) <*> commaSep ws pa)

  <!>

  pure CommaSepNone

commaSep1' :: Parser ann Whitespace -> Parser ann a -> Parser ann (CommaSep1' a)
commaSep1' ws pa =
  (\(a, b, c) -> from a b c) <$> sepBy1' pa (snd <$> comma ws)
  where
    from a [] b = CommaSepOne1' a b
    from a ((b, c) : bs) d = CommaSepMany1' a b $ from c bs d

param :: Parser ann (Param '[] ann)
param =
  (\a ->
     maybe
       (PositionalParam (_identAnnotation a) a)
       (uncurry $ KeywordParam (_identAnnotation a) a)) <$>
  identifier anySpace <*>
  optional ((,) <$> (snd <$> token anySpace (TkEq ())) <*> expr anySpace)

  <!>

  (\(a, b) -> StarParam (pyTokenAnn a) b) <$> token anySpace (TkStar ()) <*> identifier anySpace

  <!>

  (\(a, b) -> DoubleStarParam (pyTokenAnn a) b) <$>
  token anySpace (TkDoubleStar ()) <*>
  identifier anySpace

compoundStatement :: Parser ann (CompoundStatement '[] ann)
compoundStatement = fundef
  where
    fundef =
      (\(tkDef, defSpaces) -> Fundef (pyTokenAnn tkDef) (NonEmpty.fromList defSpaces)) <$>
      token space (TkDef ()) <*>
      identifier space <*>
      fmap snd (token anySpace $ TkLeftParen ()) <*>
      commaSep anySpace param <*>
      fmap snd (token space $ TkRightParen ()) <*>
      fmap snd (colon space) <*>
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
