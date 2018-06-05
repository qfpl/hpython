{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
module Language.Python.Internal.Parse where

import Control.Applicative ((<|>), liftA2, many, some, optional)
import Control.Lens.Getter ((^.))
import Control.Monad (replicateM, unless)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.State
  (MonadState, StateT(..), get, put, modify, evalStateT, runStateT)
import Control.Monad.Writer.Strict (Writer, runWriter, writer, tell)
import Data.Char (chr, isAscii)
import Data.Foldable
import Data.Function ((&))
import Data.Functor
import Data.Functor.Alt (Alt((<!>)))
import qualified Data.Functor.Alt as Alt (many, optional)
import Data.Functor.Classes (liftEq)
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Semigroup hiding (Arg)
import Text.Parser.Token (Unspaced(..), TokenParsing, ident)
import Text.Trifecta
  ( (<?>), CharParsing, DeltaParsing, Span, Spanned(..), eof, try, noneOf, char
  , string, manyTill, spanned, notFollowedBy, chainl1, digit, unexpected, oneOf
  , satisfy
  )

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Lexer
import Language.Python.Internal.Syntax

type Untagged s a = a -> s '[] a

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

newtype Parser' ann a
  = Parser'
  { unParser
      :: StateT
           [[Either (Nested ann) (LogicalLine ann)]]
           (ExceptT (ParseError ann) (Writer Consumed))
           a
  } deriving (Functor, Applicative, Monad)

instance Alt (Parser' ann) where
  Parser' pa <!> Parser' pb =
    Parser' $ do
      st <- get
      let (res, Consumed b) = runWriter . runExceptT $ runStateT pa st
      if b
        then StateT $ \_ -> ExceptT (writer (res, Consumed b))
        else case res of
          Left{} -> pb
          Right (a, st') -> put st' $> a

runParser :: Parser' ann a -> Nested ann -> Either (ParseError ann) a
runParser (Parser' p) =
  fst . runWriter .
  runExceptT .
  evalStateT p .
  pure . toList . unNested

currentToken :: Parser' ann (PyToken ann)
currentToken = Parser' $ do
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

eol :: Parser' ann Newline
eol = Parser' $ do
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

eof' :: Parser' ann ()
eof' = Parser' $ do
  ctxt <- get
  case ctxt of
    [] -> tell $ Consumed True
    current : _ -> throwError $ ExpectedEndOfInput current

indent' :: Parser' ann ()
indent' = Parser' $ do
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

dedent' :: Parser' ann ()
dedent' = Parser' $ do
  ctxt <- get
  case ctxt of
    [] -> pure ()
    current : rest ->
      case current of
        [] -> put rest *> tell (Consumed True)
        _ -> throwError ExpectedEndOfBlock

space :: Parser' ann Whitespace
space =
  Space <$ token (TkSpace ()) <!>
  Tab <$ token (TkTab ())

parseError :: ParseError ann -> Parser' ann a
parseError pe = Parser' $ throwError pe

token :: PyToken b -> Parser' ann (PyToken ann, [Whitespace])
token tk = do
  curTk <- currentToken
  unless (liftEq (\_ _ -> True) tk curTk) .
    parseError $ ExpectedToken (() <$ tk) curTk
  Parser' (tell $ Consumed True)
  (,) curTk <$> Alt.many space

identifier' :: Parser' ann (Ident '[] ann)
identifier' = do
  curTk <- currentToken
  case curTk of
    TkIdent n ann -> do
      Parser' (tell $ Consumed True)
      MkIdent ann n <$> Alt.many space
    _ -> Parser' . throwError $ ExpectedIdentifier curTk

integer' :: Parser' ann (Expr '[] ann)
integer' = do
  curTk <- currentToken
  case curTk of
    TkInt n ann -> do
      Parser' (tell $ Consumed True)
      Int ann n <$> Alt.many space
    _ -> Parser' . throwError $ ExpectedInteger curTk

string' :: Parser' ann (Expr '[] ann)
string' = do
  curTk <- currentToken
  (case curTk of
    TkShortString qt val ann ->
      Parser' (tell $ Consumed True) $>
      String ann Nothing
      (case qt of
         SingleQuote -> ShortSingle
         DoubleQuote -> ShortDouble)
      val
    TkLongString qt val ann ->
      Parser' (tell $ Consumed True) $>
      String ann Nothing
      (case qt of
         SingleQuote -> LongSingle
         DoubleQuote -> LongDouble)
      val
    _ -> Parser' . throwError $ ExpectedString curTk) <*>
    Alt.many space

between' :: Parser' ann left -> Parser' ann right -> Parser' ann a -> Parser' ann a
between' left right pa = left *> pa <* right

indents :: Parser' ann ([Whitespace], ann)
indents = Parser' $ do
  ctxt <- get
  case ctxt of
    [] -> throwError UnexpectedEndOfInput
    current : rest ->
      case current of
        [] -> throwError UnexpectedEndOfBlock
        Right ll@(LogicalLine a _ is _ _) : rest' -> pure (is, a)
        Left _ : _ -> throwError UnexpectedIndent

expr' :: Parser' ann (Expr '[] ann)
expr' =
  binOp plusOp
  where
    plusOp :: Parser' ann (BinOp ann)
    plusOp = do
      (tk, ws) <- token (TkPlus ())
      pure $ Plus (pyTokenAnn tk) ws

    binOp :: Parser' ann (BinOp ann) -> Parser' ann (Expr '[] ann)
    binOp op = do
      (t, ts) <- (,) <$> term <*> Alt.many ((,) <$> op <*> term)
      pure $ case ts of
        [] -> t
        _ -> foldl (\tm (o, val) -> BinOp (tm ^. exprAnnotation) tm o val) t ts

    term = factor

    factor = power

    power = atomExpr

    atomExpr = atom

    atom =
      integer' <!>
      string' <!>
      (\a -> Ident (_identAnnotation a) a) <$> identifier'

smallStatement' :: Parser' ann (SmallStatement '[] ann)
smallStatement' =
  returnSt
  where
    returnSt = do
      (tkReturn, retSpaces) <- token $ TkReturn ()
      Return (pyTokenAnn tkReturn) retSpaces <$> expr'

statement' :: Parser' ann (Statement '[] ann)
statement' =
  SmallStatements <$>
  smallStatement' <*>
  Alt.many ((,) <$> fmap snd (token $ TkSemicolon ()) <*> smallStatement') <*>
  Alt.optional (snd <$> token (TkSemicolon ())) <*>
  (Just <$> eol <!> Nothing <$ eof')

  <!>

  CompoundStatement <$> compoundStatement'

comment' :: Parser' ann (Comment, Newline)
comment' = do
  curTk <- currentToken
  case curTk of
    TkComment str nl _ -> Parser' (tell $ Consumed True) $> (Comment str, nl)
    _ -> Parser' . throwError $ ExpectedComment curTk

block' :: Parser' ann (Block '[] ann)
block' = fmap Block $ (:|) <$> line <*> Alt.many line
  where
    commentOrEmpty = do
      cmt <- Alt.optional comment'
      case cmt of
        Nothing -> (,) Nothing <$> eol
        Just (cmt', nl) -> pure (Just cmt', nl)

    line = do
      (ws, a) <- indents
      (,,) a ws <$>
        (Left <$> commentOrEmpty <!>
         Right <$> statement')

compoundStatement' :: Parser' ann (CompoundStatement '[] ann)
compoundStatement' = fundef
  where
    fundef = do
      (tkDef, defSpaces) <- token $ TkDef ()
      fnName <- identifier'
      (_, lpSpaces) <- token $ TkLeftParen ()
      pure () -- stuff here
      (_, rpSpaces) <- token $ TkRightParen ()
      (_, colonSpaces) <- token $ TkColon ()
      nl <- eol
      indent'
      bl <- block'
      dedent'
      pure $
        Fundef (pyTokenAnn tkDef)
          (NonEmpty.fromList defSpaces) fnName
          lpSpaces CommaSepNone rpSpaces
          colonSpaces
          nl
          bl

stringChar :: (CharParsing m, Monad m) => m Char
stringChar = (char '\\' *> (escapeChar <|> hexChar)) <|> other
  where
    other = satisfy isAscii
    escapeChar =
      asum
      [ char '\\'
      , char '\''
      , char '"'
      , char 'a' $> '\a'
      , char 'b' $> '\b'
      , char 'f' $> '\f'
      , char 'n' $> '\n'
      , char 'r' $> '\r'
      , char 't' $> '\t'
      , char 'v' $> '\v'
      ]

    hexChar =
      char 'U' *>
      (hexToInt <$> replicateM 8 (oneOf "0123456789ABCDEF") >>=
       \a -> if a <= 0x10FFFF then pure (chr a) else unexpected "value outside unicode range")

    hexDigitInt c =
      case c of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'A' -> 10
        'B' -> 11
        'C' -> 12
        'D' -> 13
        'E' -> 14
        'F' -> 15
        _ -> error "impossible"

    hexToInt str =
      let
        size = length str
      in
        snd $! foldr (\a (sz, val) -> (sz-1, hexDigitInt a * 16 ^ sz + val)) (size, 0) str

annotated :: DeltaParsing m => m (Span -> b) -> m b
annotated m = (\(f :~ sp) -> f sp) <$> spanned m

whitespace :: CharParsing m => m Whitespace
whitespace =
  (char ' ' $> Space) <|>
  (char '\t' $> Tab) <|>
  (Continued <$ char '\\' <*> newline <*> many whitespace)

anyWhitespace :: CharParsing m => m Whitespace
anyWhitespace = whitespace <|> Newline <$> newline

identifier :: (TokenParsing m, DeltaParsing m) => m Whitespace -> m (Ident '[] Span)
identifier ws =
  annotated $
  (\a b c -> MkIdent c a b) <$>
  runUnspaced (ident idStyle) <*>
  many ws

commaSep :: (CharParsing m, Monad m) => m a -> m (CommaSep a)
commaSep e = someCommaSep <|> pure CommaSepNone
  where
    someCommaSep =
      (\val -> maybe (CommaSepOne val) ($ val)) <$>
      e <*>
      optional
        ((\a b c -> CommaSepMany c a b) <$>
          (char ',' *> many whitespace) <*>
          commaSep e)

commaSep1 :: (CharParsing m, Monad m) => m a -> m (CommaSep1 a)
commaSep1 e =
  (\val -> maybe (CommaSepOne1 val) ($ val)) <$>
  e <*>
  optional
    ((\a b c -> CommaSepMany1 c a b) <$>
     (char ',' *> many whitespace) <*>
     commaSep1 e)

commaSep1' :: (CharParsing m, Monad m) => m Whitespace -> m a -> m (CommaSep1' a)
commaSep1' ws e = do
  e' <- e
  ws' <- optional (char ',' *> many ws)
  case ws' of
    Nothing -> pure $ CommaSepOne1' e' Nothing
    Just ws'' ->
      maybe (CommaSepOne1' e' $ Just ws'') (CommaSepMany1' e' ws'') <$>
      optional (commaSep1' ws e)

parameter :: DeltaParsing m => m (Untagged Param Span)
parameter =
  (\a b c -> maybe (PositionalParam c a) (uncurry $ KeywordParam c a) b) <$>
  identifier anyWhitespace <*>
  optional
    ((,) <$>
     (char '=' *> many anyWhitespace) <*>
     exprNoList anyWhitespace) <|>
  char '*' *>
  ((\a b c -> DoubleStarParam c a b) <$
   char '*' <*> many anyWhitespace <*>
   identifier anyWhitespace <|>

   (\a b c -> StarParam c a b) <$>
   many anyWhitespace <*>
   identifier anyWhitespace)

argument :: DeltaParsing m => m (Untagged Arg Span)
argument = stars <|> nonStars
  where
    stars =
      char '*' *>
      ((\a b c -> DoubleStarArg c a b) <$
       char '*' <*>
       many anyWhitespace <*>
       exprNoList anyWhitespace
       <|>
       (\a b c -> StarArg c a b) <$>
       many anyWhitespace <*>
       exprNoList anyWhitespace)

    nonStars = do
      e <- exprNoList anyWhitespace
      case e of
        Ident _ f ->
          (\a b -> maybe (PositionalArg b e) (uncurry $ KeywordArg b f) a) <$>
          optional
            ((,) <$>
            (char '=' *> many anyWhitespace) <*>
            exprNoList anyWhitespace)
        _ -> pure $ flip PositionalArg e

stringPrefix :: CharParsing m => m StringPrefix
stringPrefix =
  (char 'r' *> (char 'b' $> Prefix_rb <|> char 'B' $> Prefix_rB <|> pure Prefix_r)) <|>
  (char 'R' *> (char 'b' $> Prefix_Rb <|> char 'B' $> Prefix_RB <|> pure Prefix_R)) <|>
  (char 'b' *> (char 'r' $> Prefix_br <|> char 'R' $> Prefix_bR <|> pure Prefix_b)) <|>
  (char 'B' *> (char 'r' $> Prefix_Br <|> char 'R' $> Prefix_BR <|> pure Prefix_B)) <|>
  (char 'u' $> Prefix_u) <|>
  (char 'U' $> Prefix_U)

expr :: DeltaParsing m => m Whitespace -> m (Expr '[] Span)
expr ws = tuple_list
  where
    tuple_list =
      annotated $
      (\a b c -> either (const a) (uncurry (Tuple c a)) b) <$>
      exprNoList ws <*>
      (fmap Left (notFollowedBy $ char ',') <|>
       fmap Right
         ((,) <$> (char ',' *> many ws) <*> optional (commaSep1' ws (exprNoList ws))))

exprNoList :: DeltaParsing m => m Whitespace -> m (Expr '[] Span)
exprNoList ws = orExpr ws
  where
    atom =
      bool <|>
      none <|>
      strLit <|>
      int <|>
      ident' <|>
      list <|>
      parenthesis <|>
      not

    not =
      annotated $
      (\a b c -> Not c a b) <$>
      (reserved "not" *> many ws) <*>
      exprNoList ws

    ident' =
      annotated $
      flip Ident <$> identifier ws

    list =
      annotated $
      (\a b c d -> List d a b c) <$
      char '[' <*>
      many anyWhitespace <*>
      commaSep (orExpr anyWhitespace) <*>
      (char ']' *> many ws)

    bool =
      annotated $
      (\a b c -> Bool c a b) <$>
      (reserved "True" $> True <|> reserved "False" $> False) <*>
      many ws

    none =
      annotated $
      flip None <$
      reserved "None" <*>
      many ws

    tripleSingle = try (string "''") *> char '\'' <?> "'''"
    tripleDouble = try (string "\"\"") *> char '"' <?> "\"\"\""

    strLit =
      annotated $
      ((\a b c d -> String d a LongSingle b c) <$>
         try (optional stringPrefix <* tripleSingle) <*>
         manyTill stringChar (string "'''") <|>
       (\a b c d -> String d a LongDouble b c) <$>
         try (optional stringPrefix <* tripleDouble) <*>
         manyTill stringChar (string "\"\"\"") <|>
       (\a b c d -> String d a ShortSingle b c) <$>
         try (optional stringPrefix <* char '\'') <*>
         manyTill stringChar (char '\'') <|>
       (\a b c d -> String d a ShortDouble b c) <$>
         try (optional stringPrefix <* char '\"') <*>
         manyTill stringChar (char '\"')) <*>
      many ws

    int =
      annotated $
      (\a b c -> Int c (read a) b) <$>
      some digit <*>
      many ws

    parenthesis =
      annotated $
      (\a b c d -> Parens d a b c) <$>
      (char '(' *> many anyWhitespace) <*>
      expr anyWhitespace <*> (char ')' *> many ws)

    binOpL inner p = chainl1 inner $ do
      (op, s) <- do
        op :~ s <- spanned p
        pure (op, s)
      pure $ \a b -> BinOp (a ^. exprAnnotation <> b ^. exprAnnotation) a (op $> s) b

    orExpr ws' = binOpL (andExpr ws') (BoolOr () <$> (reserved "or" *> many ws'))

    andExpr ws' = binOpL (notExpr ws') (BoolAnd () <$> (reserved "and" *> many ws'))

    notExpr = comparison

    comparison ws' = binOpL (bitOr ws') $
      Is () <$> (reserved "is" *> many ws') <|>
      Equals () <$> (string "==" *> many ws')

    bitOr = bitXor

    bitXor = bitAnd

    bitAnd = bitShift

    bitShift = arith

    arith ws' = binOpL (term ws') $
      Plus () <$> (char '+' *> many ws') <|>
      Minus () <$> (char '-' *> many ws')

    term ws' = binOpL (factor ws' ) $
      Multiply () <$> (char '*' *> many ws') <|>
      Divide () <$> (char '/' *> many ws')

    factor ws' =
      annotated ((\a b c -> Negate c a b) <$ char '-' <*> many ws' <*> factor ws') <|>
      power ws'

    power ws' = do
      a <- atomExpr ws'
      v <-
        optional
          (try ((,,) <$> spanned (string "**")) <*>
           many ws' <*>
           factor ws')
      case v of
        Nothing -> pure a
        Just (_ :~ s, ws2, b) ->
          pure $ BinOp (a ^. exprAnnotation <> b ^. exprAnnotation) a (Exp s ws2) b

    atomExpr ws' =
      (\a afters -> case afters of; [] -> a; _ -> foldl' (&) a afters) <$>
      atom <*>
      many (deref <|> call)
      where
        deref =
          (\ws1 (str :~ s) a -> Deref (a ^. exprAnnotation <> s) a ws1 str) <$>
          (char '.' *> many ws') <*>
          spanned (identifier ws')
        call =
          (\ws1 (csep :~ s) ws2 a -> Call (a ^. exprAnnotation <> s) a ws1 csep ws2) <$>
          (char '(' *> many anyWhitespace) <*>
          spanned (commaSep (annotated argument)) <*>
          (char ')' *> many ws')

indent :: (CharParsing m, MonadState [[Whitespace]] m) => m ()
indent = do
  level
  ws <- some whitespace <?> "indent"
  modify (ws :)

level :: (CharParsing m, MonadState [[Whitespace]] m) => m ()
level = (get >>= foldl (\b a -> b <* traverse parseWs a) (pure ())) <?> "level indentation"
  where
    parseWs Space = char ' '$> ()
    parseWs Tab = char '\t' $> ()
    parseWs (Continued nl ws) = pure ()
    parseWs Newline{} = error "newline in indentation state"

dedent :: MonadState [[Whitespace]] m => m ()
dedent = modify $ \case; [] -> error "cannot dedent further"; _:xs -> xs

block :: (DeltaParsing m, MonadState [[Whitespace]] m) => m (Block '[] Span)
block = fmap Block ((\(a :| b) c -> a :| (b ++ c)) <$> firsts <*> go) <* dedent
  where
    firsts =
      liftA2 NonEmpty.cons
        (annotated $
         (\a b d -> (d, a, b)) <$>
         try (many whitespace <* notFollowedBy (noneOf "#\r\n")) <*>
         fmap Left ((,) <$> optional comment <*> newline))
        firsts
      <|>
      fmap pure
        (annotated $
         (\a b d -> (d, a, b)) <$>
         (indent *> fmap head get) <*>
         (Right <$> statement))

    go =
      many $
      (\(f :~ a) -> f a) <$>
      spanned
        (((\a b d -> (d, a, b)) <$>
         try (many whitespace <* notFollowedBy (noneOf "#\r\n")) <*>
         fmap Left ((,) <$> optional comment <*> newline)) <|>

        ((\a b d -> (d, a, b)) <$>
         (try level *> fmap head get) <*>
         (Right <$> statement)))

exceptAs :: DeltaParsing m => m (ExceptAs '[] Span)
exceptAs =
  annotated $
  (\a b c -> ExceptAs c a b) <$>
  expr whitespace <*>
  optional
    ((,) <$ string "as" <*> many whitespace <*> identifier whitespace)

compoundStatement
  :: (DeltaParsing m, MonadState [[Whitespace]] m) => m (CompoundStatement '[] Span)
compoundStatement =
  annotated $
  (fundef <?> "function definition") <|>
  (ifSt <?> "if statement") <|>
  (while <?> "while statement") <|>
  (trySt <?> "try statement") <|>
  (for <?> "for statement") <|>
  (classSt <?> "class definition")
  where
    trySt =
      (\b c d e f g ->
         either
           (\(h, m, n) -> TryExcept g b c d e h m n)
           (\(h, i, j, k) -> TryFinally g b c d e h i j k)
           f) <$
      reserved "try" <*>
      many whitespace <*
      char ':' <*>
      many whitespace <*>
      newline <*>
      block <*>
      (fmap Left
       ((,,) <$>
        some1
          ((,,,,) <$
           reserved "except" <*>
           many whitespace <*>
           exceptAs <*
           char ':' <*>
           many whitespace <*>
           newline <*>
           block) <*>
        optional
          ((,,,) <$ string "else" <*>
           many whitespace <* char ':' <*> many whitespace <*> newline <*>
           block) <*>
        optional
          ((,,,) <$ string "finally" <*>
           many whitespace <* char ':' <*> many whitespace <*> newline <*>
           block)) <|>

       fmap Right
       ((,,,) <$
        reserved "finally" <*>
        many whitespace <*
        char ':' <*>
        many whitespace <*>
        newline <*>
        block))

    fundef =
      (\a b c d e f g h i -> Fundef i a b c d e f g h) <$
      reserved "def" <*> some1 whitespace <*> identifier whitespace <*
      char '(' <*> many whitespace <*> commaSep (annotated parameter) <*
      char ')' <*> many whitespace <* char ':' <*> many whitespace <*> newline <*>
      block

    ifSt =
      (\a b c d e f h -> If h a b c d e f) <$
      reserved "if" <*> many whitespace <*>
      expr whitespace <* char ':' <*>
      many whitespace <*> newline <*> block <*>
      optional
        ((,,,) <$> (reserved "else" *> many whitespace) <*
         char ':' <*> many whitespace <*> newline <*> block)

    while =
      (\a b c d e g -> While g a b c d e) <$
      reserved "while" <*> many whitespace <*>
      expr whitespace <* char ':' <*>
      many whitespace <*> newline <*> block

    for =
      (\a b c d e f g h i -> For i a b c d e f g h) <$
      reserved "for" <*> many whitespace <*> expr whitespace <*
      reserved "in" <*> many whitespace <*> expr whitespace <*
      char ':' <*> many whitespace <*> newline <*>
      block <*>
      optional
        ((,,,) <$ reserved "else" <*>
         many whitespace <* char ':' <*>
         many whitespace <*> newline <*>
         block)

    classSt =
      (\a b c d e f g -> ClassDef g a b c d e f) <$
      reserved "class" <*> some1 whitespace <*>
      identifier whitespace <*>
      optional
        ((,,) <$>
         (char '(' *> many anyWhitespace) <*>
         optional (commaSep1 (annotated argument)) <*>
         (char ')' *> many whitespace)) <*>
      (char ':' *> many whitespace) <*>
      newline <*>
      block

smallStatement :: (DeltaParsing m, MonadState [[Whitespace]] m) => m (SmallStatement '[] Span)
smallStatement =
  annotated $
  (returnSt <?> "return statement") <|>
  assignOrExpr <|>
  (pass <?> "pass statement") <|>
  (from <?> "import statement") <|>
  (import_ <?> "import statement") <|>
  (break <?> "break statement") <|>
  (continue <?> "continue statement") <|>
  (raise <?> "raise statement")
  where
    break = reserved "break" $> Break
    continue = reserved "continue" $> Continue
    pass = reserved "pass" $> Pass

    raise =
      (\a b c -> Raise c a b) <$
      reserved "raise" <*>
      many whitespace <*>
      optional
        ((,) <$>
         exprNoList whitespace <*>
         optional ((,) <$ reserved "as" <*> many whitespace <*> exprNoList whitespace))

    assignOrExpr = do
      e <- expr whitespace <?> "expression"
      mws <- optional (many whitespace <* char '=')
      case mws of
        Nothing -> pure (`Expr` e)
        Just ws ->
          (\a b c -> Assign c e ws a b) <$>
          many whitespace <*>
          expr whitespace

    returnSt =
      (\a b c -> Return c a b) <$ reserved "return" <*> many whitespace <*> expr whitespace

    dot = Dot <$> (char '.' *> many whitespace)

    importTargets =
      annotated $
      (char '*' $> flip ImportAll <*> many whitespace) <|>
      ((\a b c d -> ImportSomeParens d a b c) <$>
       (char '(' *> many anyWhitespace) <*>
       commaSep1' anyWhitespace (importAs anyWhitespace identifier) <*>
       manyTill anyWhitespace (char ')')) <|>
      flip ImportSome <$> commaSep1 (importAs whitespace identifier)

    importAs ws p =
      annotated $
      (\a b c -> ImportAs c a b) <$>
      p ws <*>
      optional (string "as" $> (,) <*> some1 ws <*> identifier ws)

    moduleName ws =
      annotated $
      (\a c d -> maybe (ModuleNameOne d a) (\(x, y) -> ModuleNameMany d a x y) c) <$>
      identifier ws <*>
      optional
        ((,) <$> (char '.' *> many ws) <*> moduleName ws)

    relativeModuleName =
      (\ds -> either (RelativeWithName ds) (Relative $ NonEmpty.fromList ds)) <$>
      some dot <*>
      (Left <$> moduleName whitespace <|> Right <$> some whitespace)
      <|>
      RelativeWithName [] <$> moduleName whitespace

    from =
      (\a b c d e f -> From f a b d e) <$>
      (reserved "from" *> some whitespace) <*>
      relativeModuleName <*>
      reserved "import" <*>
      many whitespace <*>
      importTargets

    import_ =
      (\a b c -> Import c a b) <$>
      (reserved "import" *> some1 whitespace) <*>
      commaSep1 (importAs whitespace moduleName)

statement :: (DeltaParsing m, MonadState [[Whitespace]] m) => m (Statement '[] Span)
statement =
  CompoundStatement <$> compoundStatement <|>
  smallStatements
  where
    smallStatements =
      SmallStatements <$>
      smallStatement <*>
      many
        (try $
         (,) <$
         char ';' <*>
         many whitespace <*>
         smallStatement) <*>
      optional (char ';' *> many whitespace) <*>
      (Just <$> newline <|> Nothing <$ eof)

comment :: DeltaParsing m => m Comment
comment = (Comment <$ char '#' <*> many (noneOf "\n\r")) <?> "comment"

module_ :: DeltaParsing m => m (Module '[] Span)
module_ =
  Module <$>
  many
    (try (fmap Left $ (,,) <$> many whitespace <*> optional comment <*> newline) <|>
     Right <$> evalStateT statement [])
  <* eof
