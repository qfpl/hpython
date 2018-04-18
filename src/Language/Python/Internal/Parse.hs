{-# language DataKinds #-}
{-# language FlexibleContexts #-}
module Language.Python.Internal.Parse where

import Control.Applicative ((<|>), liftA2)
import Control.Lens hiding (List, argument)
import Control.Monad.State
import Data.Char (chr, isAscii)
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Semigroup hiding (Arg)
import Text.Parser.Token hiding (commaSep, commaSep1, dot)
import Text.Trifecta hiding (newline, commaSep, commaSep1, dot)

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax

type Untagged s a = a -> s '[] a

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

newline :: CharParsing m => m Newline
newline =
  char '\n' $> LF <|>
  char '\r' *> (char '\n' $> CRLF <|> pure CR)

annotated :: DeltaParsing m => m (Untagged b Span) -> m (b '[] Span)
annotated m = (\(f :~ sp) -> f sp) <$> spanned m

whitespace :: CharParsing m => m Whitespace
whitespace =
  (char ' ' $> Space) <|>
  (char '\t' $> Tab) <|>
  (Continued <$ char '\\' <*> newline <*> many whitespace)

anyWhitespace :: CharParsing m => m (Either Newline Whitespace)
anyWhitespace = Left <$> newline <|> Right <$> whitespace

identifier :: (TokenParsing m, Monad m) => m (Untagged Ident Span)
identifier = fmap (flip MkIdent) . runUnspaced $ ident idStyle

commaSep :: (CharParsing m, Monad m) => m a -> m (CommaSep a)
commaSep e = someCommaSep <|> pure CommaSepNone
  where
    someCommaSep =
      (\val -> maybe (CommaSepOne val) ($ val)) <$>
      e <*>
      optional
        ((\a b c d -> CommaSepMany d a b c) <$>
          many whitespace <* char ',' <*>
          many whitespace <*> commaSep e)

commaSep1 :: (CharParsing m, Monad m) => m a -> m (CommaSep1 a)
commaSep1 e =
  (\val -> maybe (CommaSepOne1 val) ($ val)) <$>
  e <*>
  optional
    ((\a b c d -> CommaSepMany1 d a b c) <$>
      many whitespace <* char ',' <*>
      many whitespace <*> commaSep1 e)

commaSep1' :: (CharParsing m, Monad m) => m ws -> m a -> m (CommaSep1' ws a)
commaSep1' ws e =
  (\val rest c -> maybe (CommaSepOne1' val c) ($ val) rest) <$>
  e <*>
  optional
    ((\a b d -> CommaSepMany1' d a b) <$>
      (char ',' *> many ws) <*>
      commaSep1' ws e) <*>
  optional (char ',' *> many ws)

commaSep1'_try :: (CharParsing m, Monad m) => m ws -> m a -> m (CommaSep1' ws a)
commaSep1'_try ws e =
  (\val rest c -> maybe (CommaSepOne1' val c) ($ val) rest) <$>
  e <*>
  optional
    (try $
     (\a b d -> CommaSepMany1' d a b) <$>
     (char ',' *> many ws) <*>
     commaSep1'_try ws e) <*>
  optional (char ',' *> many ws)

parameter :: DeltaParsing m => m (Untagged Param Span)
parameter = kwparam <|> posparam
  where
    kwparam =
      try
        ((\a b c d e -> KeywordParam e a b c d) <$>
         annotated identifier <*>
         many whitespace <*
         char '=') <*>
      many whitespace <*> expr
    posparam = flip PositionalParam <$> annotated identifier

argument :: DeltaParsing m => m (Untagged Arg Span)
argument = kwarg <|> posarg
  where
    kwarg =
      try
        ((\a b c d e -> KeywordArg e a b c d) <$>
         annotated identifier <*>
         many whitespace <*
         char '=') <*>
      many whitespace <*> expr
    posarg = flip PositionalArg <$> expr

expr :: DeltaParsing m => m (Expr '[] Span)
expr = tuple_list
  where
    atom =
      bool <|>
      none <|>
      strLit <|>
      int <|>
      ident' <|>
      list <|>
      parenthesis

    ident' =
      annotated $
      (\a b c -> Ident c a b) <$> annotated identifier <*> many whitespace

    tuple_list =
      annotated $
      (\a b c -> either (const a) (uncurry (Tuple c a)) b) <$>
      orExpr <*>
      (fmap Left (notFollowedBy $ char ',') <|>
       fmap Right
         ((,) <$> (char ',' *> many whitespace) <*> optional (commaSep1'_try whitespace orExpr)))

    list =
      annotated $
      (\a b c d -> List d a b c) <$
      char '[' <*> many whitespace <*> commaSep orExpr <*> (char ']' *> many whitespace) 

    bool =
      annotated $
      (\a b c -> Bool c a b) <$>
      (reserved "True" $> True <|> reserved "False" $> False) <*>
      many whitespace

    none =
      annotated $
      flip None <$
      reserved "None" <*>
      many whitespace

    tripleSingle = try (string "''") *> char '\'' <?> "'''"
    tripleDouble = try (string "\"\"") *> char '"' <?> "\"\"\""

    strLit =
      annotated $
      ((\a b c -> String c LongSingle a b) <$>
         (tripleSingle *> manyTill stringChar (string "'''")) <|>
       (\a b c -> String c LongDouble a b) <$>
         (tripleDouble *> manyTill stringChar (string "\"\"\"")) <|>
       (\a b c -> String c ShortSingle a b) <$>
         (char '\'' *> manyTill stringChar (char '\'')) <|>
       (\a b c -> String c ShortDouble a b) <$>
         (char '\"' *> manyTill stringChar (char '\"'))) <*>
      many whitespace

    int =
      annotated $
      (\a b c -> Int c (read a) b) <$>
      some digit <*>
      many whitespace

    parenthesis =
      annotated $
      (\a b c d -> Parens d a b c) <$>
      (char '(' *> many whitespace) <*>
      expr <*> (char ')' *> many whitespace)

    binOpL inner p = chainl1 inner $ do
      (op, s) <- do
        op :~ s <- spanned p
        pure (op, s)
      ws2 <- many whitespace
      pure $ \a b -> BinOp (a ^. exprAnnotation <> b ^. exprAnnotation) a (op $> s) ws2 b

    orExpr = binOpL andExpr (reserved "or" $> BoolOr ())

    andExpr = binOpL notExpr (reserved "and" $> BoolAnd ())

    notExpr = comparison

    comparison = binOpL bitOr $
      reserved "is" $> Is () <|>
      string "==" $> Equals ()

    bitOr = bitXor

    bitXor = bitAnd

    bitAnd = bitShift

    bitShift = arith

    arith = binOpL term $
      char '+' $> Plus () <|>
      char '-' $> Minus ()

    term = binOpL factor $
      char '*' $> Multiply () <|>
      char '/' $> Divide ()

    factor =
      annotated ((\a b c -> Negate c a b) <$ char '-' <*> many whitespace <*> factor) <|>
      power

    power = do
      a <- atomExpr
      v <-
        optional
          (try ((,,) <$> spanned (string "**")) <*>
           many whitespace <*>
           factor)
      case v of
        Nothing -> pure a
        Just (_ :~ s, ws2, b) ->
          pure $ BinOp (a ^. exprAnnotation <> b ^. exprAnnotation) a (Exp s) ws2 b

    atomExpr =
      (\a afters -> case afters of; [] -> a; _ -> foldl' (\b f -> f b) a afters) <$>
      atom <*>
      many (deref <|> call)
      where
        deref =
          (\ws1 (str :~ s) ws2 a -> Deref (a ^. exprAnnotation <> s) a ws1 str ws2) <$>
          (char '.' *> many whitespace) <*>
          spanned (annotated identifier) <*>
          many whitespace
        call =
          (\ws1 (csep :~ s) ws2 a -> Call (a ^. exprAnnotation <> s) a ws1 csep ws2) <$>
          (char '(' *> many whitespace) <*>
          spanned (commaSep (annotated argument)) <*>
          (char ')' *> many whitespace)

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

dedent :: MonadState [[Whitespace]] m => m ()
dedent = modify tail

block :: (DeltaParsing m, MonadState [[Whitespace]] m) => m (Block '[] Span)
block = fmap Block (liftA2 (:|) first go) <* dedent
  where
    first =
      (\(f :~ a) -> f a) <$>
      spanned
        ((\a b d -> (d, a, b)) <$>
         (indent *> fmap head get) <*>
         statement)
    go =
      many $
      (\(f :~ a) -> f a) <$>
      spanned
        ((\a b d -> (d, a, b)) <$>
         (try level *> fmap head get) <*>
         statement)

compoundStatement
  :: (DeltaParsing m, MonadState [[Whitespace]] m) => m (CompoundStatement '[] Span)
compoundStatement =
  annotated $
  fundef <|>
  ifSt <|>
  while
  where
    fundef =
      (\a b c d e f g h i -> Fundef i a b c d e f g h) <$
      reserved "def" <*> some1 whitespace <*> annotated identifier <*>
      many whitespace <*> between (char '(') (char ')') (commaSep $ annotated parameter) <*>
      many whitespace <* char ':' <*> many whitespace <*> newline <*> block
    ifSt =
      (\a b c d e f g h -> If h a b c d e f g) <$>
      (reserved "if" *> many whitespace) <*> expr <*> many whitespace <* char ':' <*>
      many whitespace <*> newline <*> block <*>
      optional
        ((,,,) <$> (reserved "else" *> many whitespace) <*
         char ':' <*> many whitespace <*> newline <*> block)
    while =
      (\a b c d e f g -> While g a b c d e f) <$>
      (reserved "while" *> many whitespace) <*> expr <*> many whitespace <* char ':' <*>
      many whitespace <*> newline <*> block

smallStatement :: (DeltaParsing m, MonadState [[Whitespace]] m) => m (SmallStatement '[] Span)
smallStatement =
  annotated $
  returnSt <|>
  assignOrExpr <|>
  pass <|>
  from <|>
  import_ <|>
  break
  where
    break = reserved "break" $> Break
    pass = reserved "pass" $> Pass
    assignOrExpr = do
      e <- expr
      mws <- optional (many whitespace <* char '=')
      case mws of
        Nothing -> pure (`Expr` e)
        Just ws -> 
          (\a b c -> Assign c e ws a b) <$>
          many whitespace <*>
          expr
    returnSt = (\a b c -> Return c a b) <$ reserved "return" <*> many whitespace <*> expr
    dot = Dot <$> (char '.' *> many whitespace)
    importTargets =
      (char '*' $> ImportAll) <|>
      (ImportSomeParens <$
       char '(' <*> many anyWhitespace <*>
       commaSep1'_try
         anyWhitespace
         ((,) <$> annotated identifier <*> optional (as1 $ annotated identifier)) <*>
       manyTill anyWhitespace (char ')')) <|>
      ImportSome <$>
      commaSep1
        ((,) <$> annotated identifier <*> optional (as1 $ annotated identifier))
    as1 m = As1 <$> some1 whitespace <* string "as" <*> some1 whitespace <*> m
    moduleName =
      (\a b c -> maybe (ModuleNameOne c a) (\(x, y, z) -> ModuleNameMany c a x y z) b) <$>
      annotated identifier <*>
      optional
        ((,,) <$> try (many whitespace <* char '.') <*> many whitespace <*> annotated moduleName)
    relativeModuleName =
      (\ds -> maybe (Relative $ NonEmpty.fromList ds) (RelativeWithName ds)) <$>
      some dot <*>
      optional (annotated moduleName)
      <|>
      RelativeWithName [] <$> annotated moduleName
    from =
      (\a b c d e f -> From f a b c d e) <$>
      (reserved "from" *> some whitespace) <*>
      relativeModuleName <*>
      some1 whitespace <*
      reserved "import" <*>
      many whitespace <*>
      importTargets
    import_ =
      (\a b c -> Import c a b) <$>
      (reserved "import" *> some1 whitespace) <*>
      commaSep1 ((,) <$> annotated moduleName <*> optional (as1 $ annotated identifier))

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
         (,,) <$>
         many whitespace <* char ';' <*>
         many whitespace <*>
         smallStatement) <*>
      optional ((,) <$> many whitespace <* char ';' <*> many whitespace) <*>
      newline

module_ :: DeltaParsing m => m (Module '[] Span)
module_ =
  Module <$>
  many
    (try (Left <$> liftA2 (,) (many whitespace) newline) <|>
     Right <$> evalStateT statement [])
  <* eof
