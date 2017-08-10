module Main where

import Papa
import Test.Tasty

import Test.Language.Python.ParserPrinter (makeParserPrinterTests)

main :: IO ()
main = do
  parserPrinterTests <- makeParserPrinterTests
  
  defaultMain $
    testGroup "hpython-tests"
    [ testGroup "parser+printer" parserPrinterTests
    ]
