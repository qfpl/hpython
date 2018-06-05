{-# language OverloadedStrings #-}
module LexerParser (lexerParserTests) where

import Data.Functor.Alt ((<!>))
import qualified Data.Functor.Alt as Alt (many)
import qualified Text.Trifecta as Trifecta
import Hedgehog

import Language.Python.Internal.Lexer
import Language.Python.Internal.Parse
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax.Whitespace

lexerParserTests :: Group
lexerParserTests =
  Group "Lexer/Parser tests"
  [ ("Test parse 1", test_parse_1)
  , ("Test parse 2", test_parse_2)
  , ("Test full trip 1", test_fulltrip_1)
  ]

doTokenize :: Monad m => String -> PropertyT m [PyToken Trifecta.Caret]
doTokenize str = do
  let res = tokenize str
  case res of
    Trifecta.Failure err -> do
      annotateShow err
      failure
    Trifecta.Success a -> pure a

doIndentation :: Monad m => [LogicalLine a] -> PropertyT m [IndentedLine a]
doIndentation lls = do
  let res = indentation lls
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doNested :: Monad m => [IndentedLine a] -> PropertyT m (Nested a)
doNested ils = do
  let res = nested ils
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doParse :: (Show ann, Monad m) => Parser' ann a -> Nested ann -> PropertyT m a
doParse pa input = do
  let res = runParser pa input
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

test_fulltrip_1 :: Property
test_fulltrip_1 =
  withTests 1 . property $ do
    let str = "def a():\n   return 2 + 3"
    tks <- doTokenize str
    length tks === 17

    let ll = logicalLines tks
    length ll === 2

    indents <- doIndentation ll
    length indents === 4

    nested <- doNested indents
    annotateShow nested

    a <- doParse statement' nested
    renderLines (renderStatement a) === str

parseTab :: Parser' ann Whitespace
parseTab = do
  curTk <- currentToken
  case curTk of
    TkTab{} -> pure Tab
    _ -> parseError $ ExpectedToken (TkTab ()) curTk

parseSpace :: Parser' ann Whitespace
parseSpace = do
  curTk <- currentToken
  case curTk of
    TkSpace{} -> pure Space
    _ -> parseError $ ExpectedToken (TkSpace ()) curTk

test_parse_1 :: Property
test_parse_1 =
  withTests 1 . property $ do
    let
      line =
        [ IndentedLine
            LogicalLine
            { llAnn = ()
            , llSpacesTokens = []
            , llSpaces = []
            , llLine = [ TkTab () ]
            , llEnd = Nothing
            }
        ]

    nested <- doNested line

    res <- doParse (parseSpace <!> parseTab) nested
    case res of
      Tab -> success
      _ -> annotateShow res *> failure

test_parse_2 :: Property
test_parse_2 =
  withTests 1 . property $ do
    let
      line =
        [ IndentedLine
            LogicalLine
            { llAnn = ()
            , llSpacesTokens = []
            , llSpaces = []
            , llLine = [ TkSpace (), TkSpace (), TkSpace (), TkSpace () ]
            , llEnd = Nothing
            }
        ]

    nested <- doNested line

    () <$ doParse (Alt.many space) nested
