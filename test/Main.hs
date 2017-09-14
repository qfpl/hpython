module Main where

import Papa
import Test.Tasty

import Test.Language.Python.ParserPrinter (makeParserPrinterTests)
import Test.Language.Python.AST.EscapeSeq (escapeSeqTests)
import Test.Language.Python.AST.StringContent (stringContentTests)

main :: IO ()
main = do
  parserPrinterTests <- makeParserPrinterTests
  defaultMain $
    testGroup "hpython-tests"
    [ testGroup "parser+printer" parserPrinterTests
    , testGroup "StringContent" stringContentTests
    , testGroup "EscapeSeq" escapeSeqTests
    ]
