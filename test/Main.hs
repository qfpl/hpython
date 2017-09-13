module Main where

import Papa
import Test.Tasty

import Test.Language.Python.ParserPrinter (makeParserPrinterTests)
import Test.Language.Python.AST.TripleString (tripleStringTests)

main :: IO ()
main = do
  parserPrinterTests <- makeParserPrinterTests
  defaultMain $
    testGroup "hpython-tests"
    [ testGroup "parser+printer" parserPrinterTests
    , testGroup "TripleString" tripleStringTests
    ]
