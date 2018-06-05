{-# language OverloadedStrings #-}
module LexerParser (lexerParserTests) where

import Data.Functor.Alt ((<!>))
import qualified Data.Functor.Alt as Alt (many)
import Hedgehog

import Language.Python.Internal.Lexer
import Language.Python.Internal.Parse
import Language.Python.Internal.Render
import Language.Python.Internal.Syntax.Whitespace

import Helpers (doToPython, doParse, doNested)

lexerParserTests :: Group
lexerParserTests =
  Group "Lexer/Parser tests"
  [ ("Test parse 1", test_parse_1)
  , ("Test parse 2", test_parse_2)
  , ("Test full trip 1", test_fulltrip_1)
  ]

test_fulltrip_1 :: Property
test_fulltrip_1 =
  withTests 1 . property $ do
    let str = "def a(x, y=2, *z, **w):\n   return 2 + 3"
    a <- doToPython statement str
    renderLines (renderStatement a) === str

parseTab :: Parser ann Whitespace
parseTab = do
  curTk <- currentToken
  case curTk of
    TkTab{} -> pure Tab
    _ -> parseError $ ExpectedToken (TkTab ()) curTk

parseSpace :: Parser ann Whitespace
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
