{-# language DeriveFunctor #-}
{-# language BangPatterns #-}
{-# language OverloadedLists #-}
{-# language TypeApplications #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Lexer where

import Control.Applicative ((<|>), some, many, optional)
import Control.Monad (when, replicateM)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Data.Bifunctor (first)
import Data.Char (chr, isAscii)
import Data.Deriving (deriveEq1)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Data.Sequence ((|>), Seq)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Trifecta
  ( CharParsing, DeltaParsing, Caret, Careted(..), char, careted, letter, noneOf
  , digit, string, manyTill, parseString, unexpected, oneOf, satisfy, try
  )

import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Trifecta as Trifecta

import Language.Python.Internal.Syntax (StringPrefix(..))
import Language.Python.Internal.Syntax.Whitespace
  (Newline(..), Whitespace(..))

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Show)

data PyToken a
  = TkIf a
  | TkElse a
  | TkWhile a
  | TkDef a
  | TkReturn a
  | TkPass a
  | TkBreak a
  | TkContinue a
  | TkTrue a
  | TkFalse a
  | TkOr a
  | TkAnd a
  | TkIs a
  | TkNot a
  | TkGlobal a
  | TkDel a
  | TkImport a
  | TkFrom a
  | TkAs a
  | TkRaise a
  | TkInt Integer a
  | TkFloat Integer (Maybe Integer) a
  | TkIdent String a
  | TkShortString (Maybe StringPrefix) QuoteType String a
  | TkLongString (Maybe StringPrefix) QuoteType String a
  | TkSpace a
  | TkTab a
  | TkNewline Newline a
  | TkLeftBracket a
  | TkRightBracket a
  | TkLeftParen a
  | TkRightParen a
  | TkLeftBrace a
  | TkRightBrace a
  | TkLt a
  | TkLte a
  | TkEq a
  | TkDoubleEq a
  | TkGt a
  | TkGte a
  | TkContinued Newline a
  | TkColon a
  | TkSemicolon a
  | TkComma a
  | TkDot a
  | TkPlus a
  | TkMinus a
  | TkComment String a
  | TkStar a
  | TkDoubleStar a
  | TkSlash a
  | TkDoubleSlash a
  | TkPercent a
  | TkShiftLeft a
  | TkShiftRight a
  deriving (Eq, Show, Functor)
deriveEq1 ''PyToken

pyTokenAnn :: PyToken a -> a
pyTokenAnn tk =
  case tk of
    TkDef a -> a
    TkReturn a -> a
    TkPass a -> a
    TkBreak a -> a
    TkContinue a -> a
    TkTrue a -> a
    TkFalse a -> a
    TkOr a -> a
    TkAnd a -> a
    TkIs a -> a
    TkNot a -> a
    TkGlobal a -> a
    TkDel a -> a
    TkImport a -> a
    TkFrom a -> a
    TkAs a -> a
    TkRaise a -> a
    TkPlus a -> a
    TkMinus a -> a
    TkIf a -> a
    TkElse a -> a
    TkWhile a -> a
    TkInt _ a -> a
    TkFloat _ _ a -> a
    TkIdent _ a -> a
    TkShortString _ _ _ a -> a
    TkLongString _ _ _ a -> a
    TkSpace a -> a
    TkTab a -> a
    TkNewline _ a -> a
    TkLeftBracket a -> a
    TkRightBracket a -> a
    TkLeftParen a -> a
    TkRightParen a -> a
    TkLeftBrace a -> a
    TkRightBrace a -> a
    TkLt a -> a
    TkLte a -> a
    TkEq a -> a
    TkDoubleEq a -> a
    TkGt a -> a
    TkGte a -> a
    TkContinued _ a -> a
    TkColon a -> a
    TkSemicolon a -> a
    TkComma a -> a
    TkDot a -> a
    TkComment _ a -> a
    TkStar a -> a
    TkDoubleStar a -> a
    TkSlash a -> a
    TkDoubleSlash a -> a
    TkPercent a -> a
    TkShiftLeft a -> a
    TkShiftRight a -> a

parseNewline :: CharParsing m => m Newline
parseNewline =
  char '\n' $> LF <|>
  char '\r' *> (char '\n' $> CRLF <|> pure CR)

stringPrefix :: CharParsing m => m StringPrefix
stringPrefix =
  (char 'r' *> (char 'b' $> Prefix_rb <|> char 'B' $> Prefix_rB <|> pure Prefix_r)) <|>
  (char 'R' *> (char 'b' $> Prefix_Rb <|> char 'B' $> Prefix_RB <|> pure Prefix_R)) <|>
  (char 'b' *> (char 'r' $> Prefix_br <|> char 'R' $> Prefix_bR <|> pure Prefix_b)) <|>
  (char 'B' *> (char 'r' $> Prefix_Br <|> char 'R' $> Prefix_BR <|> pure Prefix_B)) <|>
  (char 'u' $> Prefix_u) <|>
  (char 'U' $> Prefix_U)

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
       \a ->
         if a <= 0x10FFFF
         then pure (chr a)
         else unexpected $ "value: " <> show a <> " outside unicode range")

parseToken :: (DeltaParsing m, LookAheadParsing m) => m (PyToken Caret)
parseToken =
  fmap (\(f :^ sp) -> f sp) . careted $
  asum @[]
    [ string "if" $> TkIf
    , string "else" $> TkElse
    , string "while" $> TkWhile
    , string "def" $> TkDef
    , string "return" $> TkReturn
    , string "pass" $> TkPass
    , string "break" $> TkBreak
    , string "continue" $> TkContinue
    , string "True" $> TkTrue
    , string "False" $> TkFalse
    , string "or" $> TkOr
    , string "and" $> TkAnd
    , string "is" $> TkIs
    , string "not" $> TkNot
    , string "global" $> TkGlobal
    , string "del" $> TkDel
    , string "import" $> TkImport
    , string "from" $> TkFrom
    , string "as" $> TkAs
    , string "raise" $> TkRaise
    , (\a b -> maybe (TkInt a) (TkFloat a) b) <$>
        fmap read (some digit) <*>
        optional (char '.' *> optional (read <$> some digit))
    , char ' ' $> TkSpace
    , char '\t' $> TkTab
    , TkNewline <$> parseNewline
    , char '[' $> TkLeftBracket
    , char ']' $> TkRightBracket
    , char '(' $> TkLeftParen
    , char ')' $> TkRightParen
    , char '{' $> TkLeftBrace
    , char '}' $> TkRightBrace
    , char '<' *> (char '=' $> TkLte <|> char '<' $> TkShiftLeft <|> pure TkLt)
    , char '=' *> (char '=' $> TkDoubleEq <|> pure TkEq)
    , char '>' *> (char '=' $> TkGte <|> char '>' $> TkShiftRight <|> pure TkGt)
    , char '*' *> (char '*' $> TkDoubleStar <|> pure TkStar)
    , char '/' *> (char '/' $> TkDoubleSlash <|> pure TkSlash)
    , char '+' $> TkPlus
    , char '-' $> TkMinus
    , char '%' $> TkPercent
    , char '\\' $> TkContinued <*> parseNewline
    , char ':' $> TkColon
    , char ';' $> TkSemicolon
    , do
        sp <- optional . try $ stringPrefix <* lookAhead (char '"')
        char '"' *>
          (string "\"\"" $>
           TkLongString sp DoubleQuote <*>
           manyTill stringChar (string "\"\"\"")
           <|>
           TkShortString sp DoubleQuote <$> manyTill stringChar (char '"'))
    , do
        sp <- optional . try $ stringPrefix <* lookAhead (char '\'')
        char '\'' *>
          (string "''" $>
           TkLongString sp SingleQuote <*>
           manyTill stringChar (string "'''")
           <|>
           TkShortString sp SingleQuote <$> manyTill stringChar (char '\''))
    , TkComment <$
      char '#' <*>
      many (noneOf "\r\n")
    , char ',' $> TkComma
    , char '.' $> TkDot
    , fmap TkIdent $
      (:) <$>
      (letter <|> char '_') <*>
      many (letter <|> digit <|> char '_')
    ]

tokenize :: String -> Trifecta.Result [PyToken Caret]
tokenize = parseString (many parseToken) mempty

data LogicalLine a
  = LogicalLine
  { llAnn :: a
  , llSpacesTokens :: [PyToken a]
  , llSpaces :: [Whitespace]
  , llLine :: [PyToken a]
  , llEnd :: Maybe (PyToken a, Newline)
  } deriving (Eq, Show)

spaceToken :: PyToken a -> Maybe Whitespace
spaceToken TkSpace{} = Just Space
spaceToken TkTab{} = Just Tab
spaceToken (TkContinued nl _) = Just $ Continued nl []
spaceToken _ = Nothing

collapseContinue :: [Whitespace] -> [Whitespace]
collapseContinue [] = []
collapseContinue (Space : xs) = Space : collapseContinue xs
collapseContinue (Tab : xs) = Tab : collapseContinue xs
collapseContinue (Newline nl : xs) = Newline nl : collapseContinue xs
collapseContinue (Continued nl ws : xs) = [Continued nl $ ws <> xs]

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
      (fst <$> spaces)
      (collapseContinue $ snd <$> spaces)
      line
      (fst <$> rest')
      :
    logicalLines (maybe [] snd rest') 

data IndentedLine a
  = Indent a
  | Dedent
  | IndentedLine (LogicalLine a)
  deriving (Eq, Show)

isBlankToken :: PyToken a -> Bool
isBlankToken TkSpace{} = True
isBlankToken TkTab{} = True
isBlankToken TkComment{} = True
isBlankToken TkNewline{} = True
isBlankToken _ = False

expandTabs :: Int -> [Whitespace] -> [Whitespace]
expandTabs size = go 0
  where
    go !n (Tab : xs) =
      let
        (q, r) = quotRem n 8
        count = size - r
      in
        Space :
        if r == 0
        then replicate (size-1) Space <> go (n + size) xs
        else replicate (count-1) Space <> go (n + count) xs
    go !n (Space : xs) = Space : go (n + 1) xs
    go !n (Newline{} : _) = error "newline in expandTabs"
    go !n xs = xs

countSpaces :: [Whitespace] -> Int
countSpaces (Space : xs) = 1 + countSpaces xs
countSpaces (Tab : _) = error "tab in countSpaces"
countSpaces (Newline{} : _) = error "newline in countSpaces"
countSpaces _ = 0

data TabError a
  = TabError
  | IncorrectDedent a
  deriving (Eq, Show)

indentation :: [LogicalLine a] -> Either (TabError a) [IndentedLine a]
indentation lls =
  flip evalStateT (pure []) $
  (<>) <$> (concat <$> traverse go lls) <*> finalDedents
  where
    finalDedents :: StateT (NonEmpty [Whitespace]) (Either (TabError a)) [IndentedLine a]
    finalDedents = do
      i :| is <- get
      case is of
        [] -> pure []
        i' : is' -> do
          put $ i' :| is'
          (Dedent :) <$> finalDedents

    dedents :: a -> Int -> StateT (NonEmpty [Whitespace]) (Either (TabError a)) [IndentedLine a]
    dedents ann n = do
      is <- get
      let (popped, remainder) = NonEmpty.span ((> n) . countSpaces . expandTabs 8) is
      when (n `notElem` fmap (countSpaces . expandTabs 8) (NonEmpty.toList is)) .
        throwError $ IncorrectDedent ann
      put $ case remainder of
        [] -> error "I don't know whether this can happen"
        x : xs -> x :| xs
      pure $ replicate (length popped) Dedent

    go :: LogicalLine a -> StateT (NonEmpty [Whitespace]) (Either (TabError a)) [IndentedLine a]
    go ll@(LogicalLine ann _ spaces line nl)
      | all isBlankToken line = pure [IndentedLine ll]
      | otherwise = do
          i :| is <- get
          let
            et8 = countSpaces $ expandTabs 8 spaces
            et1 = countSpaces $ expandTabs 1 spaces
            et8i = countSpaces $ expandTabs 8 i
            et1i = countSpaces $ expandTabs 1 i
          when
            (not (et8 < et8i && et1 < et1i) &&
            not (et8 > et8i && et1 > et1i) &&
            not (et8 == et8i && et1 == et1i))
            (throwError TabError)
          case compare et8 et8i of
            LT -> (<> [IndentedLine ll]) <$> dedents ann et8
            EQ -> pure [IndentedLine ll]
            GT -> do
              modify $ NonEmpty.cons spaces
              pure [Indent ann, IndentedLine ll]

newtype Nested a
  = Nested
  { unNested :: Seq (Either (Nested a) (LogicalLine a))
  } deriving (Eq, Show)

data IndentationError
  = UnexpectedDedent
  | ExpectedDedent
  deriving (Eq, Show)

nested :: [IndentedLine a] -> Either IndentationError (Nested a)
nested = fmap Nested . go []
  where
    go
      :: [Seq (Either (Nested a) (LogicalLine a))]
      -> [IndentedLine a]
      -> Either
           IndentationError
           (Seq (Either (Nested a) (LogicalLine a)))
    go [] [] = pure []
    go (a : as) [] = foldr (\_ _ -> Left ExpectedDedent) (pure a) as
    go ctxt (Indent a : is) = go ([] : ctxt) is
    go [] (Dedent : is) = Left UnexpectedDedent
    go (a : as) (Dedent : is) =
      case as of
        x : xs -> go ((x |> Left (Nested a)) : xs) is
        [] -> go [[Left (Nested a)]] is
    go [] (IndentedLine ll : is) = go [[Right ll]] is
    go (a : as) (IndentedLine ll : is) = go ((a |> Right ll) : as) is
