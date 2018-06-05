{-# language DeriveFunctor #-}
{-# language BangPatterns #-}
{-# language OverloadedLists #-}
{-# language TypeApplications #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Lexer where

import Control.Applicative ((<|>), some, many, optional)
import Control.Monad (when, replicateM)
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Char (chr, isAscii)
import Data.Deriving (deriveEq1)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Data.Sequence ((|>), Seq)
import Text.Trifecta
  ( CharParsing, DeltaParsing, Caret, Careted(..), char, careted, letter, noneOf
  , digit, string, manyTill, parseString, unexpected, oneOf, satisfy
  )

import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Trifecta as Trifecta

import Language.Python.Internal.Syntax (StringPrefix(..))
import Language.Python.Internal.Syntax.Whitespace
  (Newline(..), Whitespace(..), newline)

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Show)

data PyToken a
  = TkIf a
  | TkDef a
  | TkReturn a
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
  | TkContinue Newline a
  | TkColon a
  | TkSemicolon a
  | TkComma a
  | TkPlus a
  | TkMinus a
  | TkComment String Newline a
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
    TkPlus a -> a
    TkMinus a -> a
    TkIf a -> a
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
    TkContinue _ a -> a
    TkColon a -> a
    TkSemicolon a -> a
    TkComma a -> a
    TkComment _ _ a -> a
    TkStar a -> a
    TkDoubleStar a -> a
    TkSlash a -> a
    TkDoubleSlash a -> a
    TkPercent a -> a
    TkShiftLeft a -> a
    TkShiftRight a -> a

stringPrefix :: CharParsing m => m StringPrefix
stringPrefix =
  (char 'r' *> (char 'b' $> Prefix_rb <|> char 'B' $> Prefix_rB <|> pure Prefix_r)) <|>
  (char 'R' *> (char 'b' $> Prefix_Rb <|> char 'B' $> Prefix_RB <|> pure Prefix_R)) <|>
  (char 'b' *> (char 'r' $> Prefix_br <|> char 'R' $> Prefix_bR <|> pure Prefix_b)) <|>
  (char 'B' *> (char 'r' $> Prefix_Br <|> char 'R' $> Prefix_BR <|> pure Prefix_B)) <|>
  (char 'u' $> Prefix_u) <|>
  (char 'U' $> Prefix_U)

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

parseToken :: DeltaParsing m => m (PyToken Caret)
parseToken =
  fmap (\(f :^ sp) -> f sp) . careted $
  asum @[]
    [ string "if" $> TkIf
    , string "def" $> TkDef
    , string "return" $> TkReturn
    , (\a b -> maybe (TkInt a) (TkFloat a) b) <$>
        fmap read (some digit) <*>
        optional (char '.' *> optional (read <$> some digit))
    , char ' ' $> TkSpace
    , char '\t' $> TkTab
    , char '\n' $> TkNewline LF
    , char '\r' *> (char '\n' $> TkNewline CRLF <|> pure (TkNewline CR))
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
    , char '\\' $> TkContinue <*> newline
    , char ':' $> TkColon
    , char ';' $> TkSemicolon
    , do
        sp <- optional stringPrefix
        char '"' *>
          (string "\"\"" $>
           TkLongString sp DoubleQuote <*>
           manyTill stringChar (string "\"\"\"")
           <|>
           TkShortString sp DoubleQuote <$> manyTill stringChar (char '"'))
    , do
        sp <- optional stringPrefix
        char '\'' *>
          (string "''" $>
           TkLongString sp SingleQuote <*>
           manyTill stringChar (string "'''")
           <|>
           TkShortString sp SingleQuote <$> manyTill stringChar (char '\''))
    , TkComment <$ char '#' <*> many (noneOf "\r\n") <*> newline
    , char ',' $> TkComma
    , TkIdent <$> some letter
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
spaceToken (TkContinue nl _) = Just $ Continued nl []
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

breakMaybe :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
breakMaybe f as =
  case as of
    [] -> ([], Nothing)
    x : xs ->
      case f x of
        Just b -> ([], Just (b, xs))
        Nothing -> first (x :) $ breakMaybe f xs

logicalLines :: [PyToken a] -> [LogicalLine a]
logicalLines [] = []
logicalLines tks =
  let
    (spaces, rest) = spanMaybe (\a -> (,) a <$> spaceToken a) tks
    (line, rest') = breakMaybe (\a -> (,) a <$> newlineToken a) rest
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

data TabError = TabError deriving (Eq, Show)

indentation :: [LogicalLine a] -> Either TabError [IndentedLine a]
indentation lls =
  flip evalStateT (pure []) $
  (<>) <$> (concat <$> traverse go lls) <*> finalDedents
  where
    finalDedents :: StateT (NonEmpty [Whitespace]) (Either TabError) [IndentedLine a]
    finalDedents = do
      i :| is <- get
      case is of
        [] -> pure []
        i' : is' -> do
          put $ i' :| is'
          (Dedent :) <$> finalDedents

    dedents :: Int -> StateT (NonEmpty [Whitespace]) (Either TabError) [IndentedLine a]
    dedents n = do
      i :| is <- get
      if countSpaces (expandTabs 8 i) < n
        then (Dedent :) <$> dedents n
        else pure []

    go :: LogicalLine a -> StateT (NonEmpty [Whitespace]) (Either TabError) [IndentedLine a]
    go ll@(LogicalLine ann _ spaces line nl) = do
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
        (lift $ Left TabError)
      case compare et8 et8i of
        LT -> (<> [IndentedLine ll]) <$> dedents et8
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
