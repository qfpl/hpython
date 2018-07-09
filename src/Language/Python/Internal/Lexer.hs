{-# language BangPatterns #-}
{-# language OverloadedLists #-}
{-# language TypeApplications #-}
{-# language MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
module Language.Python.Internal.Lexer where

import Control.Applicative ((<|>), some, many, optional)
import Control.Lens.Iso (from)
import Control.Lens.Getter ((^.))
import Control.Monad (when, replicateM)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Data.Bifunctor (first)
import Data.Char (chr, isAscii)
import Data.FingerTree (FingerTree, Measured(..))
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Sum(..))
import Data.Semigroup ((<>))
import Data.Sequence ((!?), (|>), Seq)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Trifecta
  ( CharParsing, DeltaParsing, Caret, Careted(..), char, careted, noneOf
  , digit, string, manyTill, parseString, unexpected, oneOf, satisfy, try
  , notFollowedBy
  )

import qualified Data.FingerTree as FingerTree
import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Trifecta as Trifecta

import Language.Python.Internal.Syntax
import Language.Python.Internal.Token (PyToken(..), pyTokenAnn)

parseNewline :: CharParsing m => m Newline
parseNewline =
  LF <$ char '\n' <|>
  char '\r' *> (CRLF <$ char '\n' <|> pure CR)

stringOrBytesPrefix :: CharParsing m => m (Either StringPrefix BytesPrefix)
stringOrBytesPrefix =
  (char 'r' *>
   (Right Prefix_rb <$ char 'b' <|>
    Right Prefix_rB <$ char 'B' <|>
    pure (Left Prefix_r))) <|>
  (char 'R' *>
   (Right Prefix_Rb <$ char 'b' <|>
    Right Prefix_RB <$ char 'B' <|>
    pure (Left Prefix_R))) <|>
  (char 'b' *>
   (Right Prefix_br <$ char 'r' <|>
    Right Prefix_bR <$ char 'R' <|>
    pure (Right Prefix_b))) <|>
  (char 'B' *>
   (Right Prefix_Br <$ char 'r' <|>
    Right Prefix_BR <$ char 'R' <|>
    pure (Right Prefix_B))) <|>
  (Left Prefix_u <$ char 'u') <|>
  (Left Prefix_U <$ char 'U')

hexDigitInt :: Char -> Int
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

hexToInt :: String -> Int
hexToInt =
  (snd $!) .
  foldr (\a (sz, val) -> (sz+1, hexDigitInt a * 16 ^ sz + val)) (0, 0)

stringChar :: (CharParsing m, Monad m) => m Char
stringChar = (char '\\' *> (escapeChar <|> hexChar)) <|> other
  where
    other = satisfy isAscii
    escapeChar =
      asum @[]
      [ char '\\'
      , char '\''
      , char '"'
      , '\a' <$ char 'a'
      , '\b' <$ char 'b'
      , '\f' <$ char 'f'
      , '\n' <$ char 'n'
      , '\r' <$ char 'r'
      , '\t' <$ char 't'
      , '\v' <$ char 'v'
      ]

    hexChar =
      char 'U' *>
      (hexToInt <$> replicateM 8 (oneOf "0123456789ABCDEF") >>=
       \a ->
         if a <= 0x10FFFF
         then pure (chr a)
         else unexpected $ "value: " <> show a <> " outside unicode range")

parseToken :: (DeltaParsing m, LookAheadParsing m) => m (PyToken Caret)
parseToken =
  fmap (\(f :^ sp) -> f sp) . careted $
  asum @[] $
    fmap
    (\p -> try $ p <* notFollowedBy (satisfy isIdentifierStart))
    [ TkIf <$ string "if"
    , TkElse <$ string "else"
    , TkElif <$ string "elif"
    , TkWhile <$ string "while"
    , TkDef <$ string "def"
    , TkReturn <$ string "return"
    , TkPass <$ string "pass"
    , TkBreak <$ string "break"
    , TkContinue <$ string "continue"
    , TkTrue <$ string "True"
    , TkFalse <$ string "False"
    , TkOr <$ string "or"
    , TkAnd <$ string "and"
    , TkIs <$ string "is"
    , TkNot <$ string "not"
    , TkGlobal <$ string "global"
    , TkDel <$ string "del"
    , TkImport <$ string "import"
    , TkFrom <$ string "from"
    , TkAs <$ string "as"
    , TkRaise <$ string "raise"
    , TkTry <$ string "try"
    , TkExcept <$ string "except"
    , TkFinally <$ string "finally"
    , TkClass <$ string "class"
    , TkFor <$ string "for"
    , TkIn <$ string "in"
    , TkYield <$ string "yield"
    ] <>
    [ (\a b -> maybe (TkInt a) (TkFloat a) b) <$>
        fmap read (some digit) <*>
        optional (char '.' *> optional (read <$> some digit))
    , TkSpace <$ char ' '
    , TkTab <$ char '\t'
    , TkNewline <$> parseNewline
    , TkLeftBracket <$ char '['
    , TkRightBracket <$ char ']'
    , TkLeftParen <$ char '('
    , TkRightParen <$ char ')'
    , TkLeftBrace <$ char '{'
    , TkRightBrace <$ char '}'
    , char '<' *>
      (TkLte <$ char '=' <|>
       char '<' *> (TkShiftLeftEq <$ char '=' <|> pure TkShiftLeft) <|>
       pure TkLt)
    , char '=' *> (TkDoubleEq <$ char '=' <|> pure TkEq)
    , char '>' *>
      (TkGte <$ char '=' <|>
       char '>' *> (TkShiftRightEq <$ char '=' <|> pure TkShiftRight) <|>
       pure TkGt)
    , char '*' *>
      (char '*' *> (TkDoubleStarEq <$ char '=' <|> pure TkDoubleStar) <|>
       TkStarEq <$ char '=' <|>
       pure TkStar)
    , char '/' *>
      (char '/' *> (TkDoubleSlashEq <$ char '=' <|> pure TkDoubleSlash) <|>
       TkSlashEq <$ char '=' <|>
       pure TkSlash)
    , TkBangEq <$ string "!="
    , TkCaretEq <$ string "^="
    , TkPipeEq <$ string "|="
    , TkAtEq <$ string "@="
    , TkAmphersandEq <$ string "&="
    , char '+' *> (TkPlusEq <$ char '=' <|> pure TkPlus)
    , char '-' *> (TkMinusEq <$ char '=' <|> pure TkMinus)
    , char '%' *> (TkPercentEq <$ char '=' <|> pure TkPercent)
    , TkContinued <$ char '\\' <*> parseNewline
    , TkColon <$ char ':'
    , TkSemicolon <$ char ';'
    , do
        sp <- optional . try $ stringOrBytesPrefix <* lookAhead (char '"')
        char '"'
        case sp of
          Nothing ->
            TkString Nothing DoubleQuote LongString <$
            string "\"\"" <*>
            manyTill stringChar (string "\"\"\"")
            <|>
            TkString Nothing DoubleQuote ShortString <$> manyTill stringChar (char '"')
          Just (Left prefix) ->
            TkString (Just prefix) DoubleQuote LongString <$
            string "\"\"" <*>
            manyTill stringChar (string "\"\"\"")
            <|>
            TkString (Just prefix) DoubleQuote ShortString <$> manyTill stringChar (char '"')
          Just (Right prefix) ->
            TkBytes prefix DoubleQuote LongString <$
            string "\"\"" <*>
            manyTill stringChar (string "\"\"\"")
            <|>
            TkBytes prefix DoubleQuote ShortString <$> manyTill stringChar (char '"')
    , do
        sp <- optional . try $ stringOrBytesPrefix <* lookAhead (char '\'')
        char '\''
        case sp of
          Nothing ->
            TkString Nothing SingleQuote LongString <$
            string "''" <*>
            manyTill stringChar (string "'''")
            <|>
            TkString Nothing SingleQuote ShortString <$> manyTill stringChar (char '\'')
          Just (Left prefix) ->
            TkString (Just prefix) SingleQuote LongString <$
            string "''" <*>
            manyTill stringChar (string "'''")
            <|>
            TkString (Just prefix) SingleQuote ShortString <$> manyTill stringChar (char '\'')
          Just (Right prefix) ->
            TkBytes prefix SingleQuote LongString <$
            string "''" <*>
            manyTill stringChar (string "'''")
            <|>
            TkBytes prefix SingleQuote ShortString <$> manyTill stringChar (char '\'')
    , TkComment <$
      char '#' <*>
      many (noneOf "\r\n")
    , TkComma <$ char ','
    , TkDot <$ char '.'
    , fmap TkIdent $
      (:) <$>
      satisfy isIdentifierStart <*>
      many (satisfy isIdentifierChar)
    ]

tokenize :: String -> Trifecta.Result [PyToken Caret]
tokenize = parseString (many parseToken) mempty

data LogicalLine a
  = LogicalLine
  { llAnn :: a
  , llSpaces :: Indent
  , llLine :: [PyToken a]
  , llEnd :: Maybe (PyToken a, Newline)
  } deriving (Eq, Show)

spaceToken :: PyToken a -> Maybe Whitespace
spaceToken TkSpace{} = Just Space
spaceToken TkTab{} = Just Tab
spaceToken (TkContinued nl _) = Just $ Continued nl []
spaceToken _ = Nothing

collapseContinue :: [(PyToken a, Whitespace)] -> [([PyToken a], Whitespace)]
collapseContinue [] = []
collapseContinue ((tk@TkSpace{}, Space) : xs) =
  ([tk], Space) : collapseContinue xs
collapseContinue ((tk@TkTab{}, Tab) : xs) =
  ([tk], Tab) : collapseContinue xs
collapseContinue ((tk@TkNewline{}, Newline nl) : xs) =
  ([tk], Newline nl) : collapseContinue xs
collapseContinue ((tk@TkContinued{}, Continued nl ws) : xs) =
  let
    xs' = collapseContinue xs
  in
    [(tk : (xs' >>= fst), Continued nl $ ws <> fmap snd xs')]
collapseContinue _ = error "invalid token/whitespace pair in collapseContinue"

newlineToken :: PyToken a -> Maybe Newline
newlineToken (TkNewline nl _) = Just nl
newlineToken _ = Nothing

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f as =
  case as of
    [] -> ([], [])
    x : xs ->
      case f x of
        Nothing -> ([], as)
        Just b -> first (b :) $ spanMaybe f xs

-- | Acts like break, but encodes the "insignificant whitespace" rule for parens, braces
-- and brackets
breakOnNewline :: [PyToken a] -> ([PyToken a], Maybe ((PyToken a, Newline), [PyToken a]))
breakOnNewline = go 0
  where
    go _ [] = ([], Nothing)
    go !careWhen0 (tk : tks) =
      case tk of
        TkLeftParen{} -> first (tk :) $ go (careWhen0 + 1) tks
        TkLeftBracket{} -> first (tk :) $ go (careWhen0 + 1) tks
        TkLeftBrace{} -> first (tk :) $ go (careWhen0 + 1) tks
        TkRightParen{} -> first (tk :) $ go (max 0 $ careWhen0 - 1) tks
        TkRightBracket{} -> first (tk :) $ go (max 0 $ careWhen0 - 1) tks
        TkRightBrace{} -> first (tk :) $ go (max 0 $ careWhen0 - 1) tks
        TkNewline nl _
          | careWhen0 == 0 -> ([], Just ((tk, nl), tks))
          | otherwise -> first (tk :) $ go careWhen0 tks
        _ -> first (tk :) $ go careWhen0 tks

logicalLines :: [PyToken a] -> [LogicalLine a]
logicalLines [] = []
logicalLines tks =
  let
    (spaces, rest) = spanMaybe (\a -> (,) a <$> spaceToken a) tks
    (line, rest') = breakOnNewline rest
  in
    LogicalLine
      (case tks of
         [] -> error "couldn't generate annotation for logical line"
         tk : _ -> pyTokenAnn tk)
      (fmap snd (collapseContinue spaces) ^. from indentWhitespaces)
      line
      (fst <$> rest')
      :
    logicalLines (maybe [] snd rest') 

data IndentedLine a
  = Indent Int a
  | Dedent
  | IndentedLine (LogicalLine a)
  deriving (Eq, Show)

isBlankToken :: PyToken a -> Bool
isBlankToken TkSpace{} = True
isBlankToken TkTab{} = True
isBlankToken TkComment{} = True
isBlankToken TkNewline{} = True
isBlankToken _ = False

data TabError a
  = TabError
  | IncorrectDedent a
  deriving (Eq, Show)

indentation :: [LogicalLine a] -> Either (TabError a) [IndentedLine a]
indentation lls =
  flip evalStateT (pure mempty) $
  (<>) <$> (concat <$> traverse go lls) <*> finalDedents
  where
    finalDedents :: StateT (NonEmpty Indent) (Either (TabError a)) [IndentedLine a]
    finalDedents = do
      i :| is <- get
      case is of
        [] -> pure []
        i' : is' -> do
          put $ i' :| is'
          (Dedent :) <$> finalDedents

    dedents :: a -> Int -> StateT (NonEmpty Indent) (Either (TabError a)) [IndentedLine a]
    dedents ann n = do
      is <- get
      let (popped, remainder) = NonEmpty.span ((> n) . indentLevel) is
      when (n `notElem` fmap indentLevel (NonEmpty.toList is)) .
        throwError $ IncorrectDedent ann
      put $ case remainder of
        [] -> error "I don't know whether this can happen"
        x : xs -> x :| xs
      pure $ replicate (length popped) Dedent

    go :: LogicalLine a -> StateT (NonEmpty Indent) (Either (TabError a)) [IndentedLine a]
    go ll@(LogicalLine ann spcs line nl)
      | all isBlankToken line = pure [IndentedLine ll]
      | otherwise = do
          i :| is <- get
          let
            et8 = absoluteIndentLevel 8 spcs
            et1 = absoluteIndentLevel 1 spcs
            et8i = absoluteIndentLevel 8 i
            et1i = absoluteIndentLevel 1 i
          when
            (not (et8 < et8i && et1 < et1i) &&
             not (et8 > et8i && et1 > et1i) &&
             not (et8 == et8i && et1 == et1i))
            (throwError TabError)
          let
            ilSpcs = indentLevel spcs
            ili = indentLevel i
          case compare ilSpcs ili of
            LT -> (<> [IndentedLine ll]) <$> dedents ann ilSpcs
            EQ -> pure [IndentedLine ll]
            GT -> do
              modify $ NonEmpty.cons spcs
              pure [Indent (ilSpcs - ili) ann, IndentedLine ll]

data Line a
  = Line
  { lineAnn :: a
  , lineSpaces :: [Indent]
  , lineLine :: [PyToken a]
  , lineEnd :: Maybe Newline
  } deriving (Eq, Show)

logicalToLine :: FingerTree (Sum Int) (Summed Int) -> LogicalLine a -> Line a
logicalToLine leaps (LogicalLine a b c d) =
  Line a (if all isBlankToken c then [b] else splitIndents leaps b) c (snd <$> d)

newtype Nested a
  = Nested
  { unNested :: Seq (Either (Nested a) (Line a))
  } deriving (Eq, Show)

nestedAnnotation :: Nested a -> Maybe a
nestedAnnotation (Nested s) = s !? 0 >>= either nestedAnnotation (pure . lineAnn)

data IndentationError
  = UnexpectedDedent
  | ExpectedDedent
  deriving (Eq, Show)

newtype Summed a
  = Summed
  { getSummed :: a }
  deriving (Eq, Show, Ord, Num)

instance Num a => Measured (Sum a) (Summed a) where
  measure (Summed a) = Sum a

-- | Given a list of indentation jumps (first to last) and some whitespace,
-- divide the whitespace up into "blocks" which correspond to each jump
splitIndents :: FingerTree (Sum Int) (Summed Int) -> Indent -> [Indent]
splitIndents ns ws = go ns ws []
  where
    go :: FingerTree (Sum Int) (Summed Int) -> Indent -> [Indent] -> [Indent]
    go ns ws =
      case FingerTree.viewr ns of
        FingerTree.EmptyR -> (ws :)
        ns' FingerTree.:> n
          | FingerTree.null ns' -> (ws :)
          | otherwise ->
              let
                (befores, afters) =
                  FingerTree.split ((> getSum (measure ns')) . getIndentLevel) $ unIndent ws
              in
                if FingerTree.null afters
                then error $ "could not carve out " <> show n <> " from " <> show ws
                else go ns' (MkIndent befores) .  (MkIndent afters :)

nested :: [IndentedLine a] -> Either IndentationError (Nested a)
nested = fmap Nested . go FingerTree.empty []
  where
    go
      :: FingerTree (Sum Int) (Summed Int)
      -> [Seq (Either (Nested a) (Line a))]
      -> [IndentedLine a]
      -> Either
           IndentationError
           (Seq (Either (Nested a) (Line a)))
    go leaps [] [] = pure []
    go leaps (a : as) [] = foldr (\_ _ -> Left ExpectedDedent) (pure a) as
    go leaps ctxt (Indent n a : is) = go (leaps FingerTree.|> Summed n) ([] : ctxt) is
    go leaps [] (Dedent : is) = Left UnexpectedDedent
    go leaps (a : as) (Dedent : is) =
      case FingerTree.viewr leaps of
        FingerTree.EmptyR -> error "impossible"
        leaps' FingerTree.:> _ ->
          case as of
            x : xs -> go leaps' ((x |> Left (Nested a)) : xs) is
            [] -> go leaps' [[Left (Nested a)]] is
    go leaps [] (IndentedLine ll : is) = go leaps [[Right $ logicalToLine leaps ll]] is
    go leaps (a : as) (IndentedLine ll : is) = go leaps ((a |> Right (logicalToLine leaps ll)) : as) is
