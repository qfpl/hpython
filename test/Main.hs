module Main where

import Papa
import Test.Tasty

import Test.Language.Python.Expr.AST.EscapeSeq (escapeSeqTests)
import Test.Language.Python.Expr.AST.StringContent (stringContentTests)
import Test.Language.Python.ParserPrinter (makeParserPrinterTests)

main :: IO ()
main = do
  parserPrinterTests <- makeParserPrinterTests
  defaultMain $
    testGroup "hpython-tests"
    [ testGroup "parser+printer" parserPrinterTests
    , testGroup "StringContent" stringContentTests
    , testGroup "EscapeSeq" escapeSeqTests
    ]
