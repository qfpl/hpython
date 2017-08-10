{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Python.ParserPrinter (makeParserPrinterTests) where


import Papa
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Hspec
import Text.Trifecta hiding (render)

import qualified Text.PrettyPrint.ANSI.Leijen as WL
import qualified Text.PrettyPrint as HPJ

import qualified Language.Python.Parser as Parse
import qualified Language.Python.Printer as Print

examplesDir :: FilePath
examplesDir = "test" </> "examples" </> "expressions" </> "valid"

parse_print_expr_id :: String -> Expectation
parse_print_expr_id input =
  case parseString (Parse.test <* eof) mempty input of
    Success ast -> HPJ.render (Print.test ast) `shouldBe` input
    Failure (ErrInfo info _) ->
      expectationFailure $ WL.displayS (WL.renderPretty 1.0 80 info) ""

makeParserPrinterTests :: IO [TestTree]
makeParserPrinterTests = do
  files <- over (mapped.mapped) (examplesDir </>) $ listDirectory examplesDir
  contents <- traverse readFile files
  let filesExpectations =
        zip files (parse_print_expr_id <$> contents)
  let spec = traverse_ (uncurry it) filesExpectations
  testSpecs spec
