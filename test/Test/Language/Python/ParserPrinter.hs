{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Python.ParserPrinter (makeParserPrinterTests) where

import Papa as Papa
import Prelude (error)
import Control.Monad.IO.Class
import Hedgehog
import System.Directory
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Hedgehog
import Text.Trifecta hiding (render)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Text.PrettyPrint.ANSI.Leijen as WL
import qualified Text.PrettyPrint as HPJ

import qualified Language.Python.Parser as Parse
import qualified Language.Python.Printer as Print
import qualified Test.Language.Python.AST.Gen as GenAST

examplesDir :: FilePath
examplesDir = "test" </> "examples" </> "expressions" </> "valid"

parse_print_expr_id :: String -> Expectation
parse_print_expr_id input =
  case parseString (Parse.test <* eof) mempty input of
    Success ast -> HPJ.render (Print.test ast) `shouldBe` input
    Failure (ErrInfo info _) ->
      expectationFailure $ WL.displayS (WL.renderPretty 1.0 80 info) ""

data SyntaxCheckResult
  = SyntaxCorrect
  | SyntaxError String
  deriving (Eq, Show)

checkSyntax :: HasCallStack => String -> IO SyntaxCheckResult
checkSyntax input = do
  pythonExe <- findExecutable "python3"
  case pythonExe of
    Nothing ->
      error $
        unwords
          [ "python3 is required to run the tests,"
          , "but could not be found on this system"
          ]
    Just _ -> pure ()
    
  (_, _, err) <-
    readProcessWithExitCode
      "python3"
      [ "-c"
      , input
      , "-m"
      , "py_compile" 
      ]
      ""
  case last (lines err) of
    Nothing -> pure SyntaxCorrect
    Just l -> 
      case parseString (parseErr err) mempty l of
        Success s -> pure s
        Failure (ErrInfo msg _) ->
          error $
            WL.displayS (WL.renderPretty 1.0 80 $
              WL.text "Parsing of Python stderr failed." WL.<$>
              WL.line <>
              WL.text "Parser error: " WL.<$> WL.line <> msg WL.<$> WL.line <>
              WL.text "Input string: " WL.<$> WL.line <> WL.text err) ""
  where
    parseErr :: (Monad m, DeltaParsing m) => String -> m SyntaxCheckResult
    parseErr errorMsg = do
      err <- optional (manyTill anyChar (try $ string "Error: "))
      msg <- manyTill anyChar eof
      pure $ case err of
        Just "Syntax" -> SyntaxError errorMsg
        Just "Indentation" -> SyntaxError errorMsg
        _ -> SyntaxCorrect

prop_ast_is_valid_python :: Property
prop_ast_is_valid_python =
  property $ do
    expr <- forAll GenAST.genTest
    let program = HPJ.render $ Print.test expr
    res <- liftIO $ checkSyntax program
    case res of
      SyntaxError pythonError -> do
        footnote $
          unlines
          [ "Input string caused a syntax error."
          , ""
          , "Input string:"
          , show program
          , ""
          , "Error message:"
          , ""
          , pythonError
          ]
        failure
      SyntaxCorrect -> success

makeParserPrinterTests :: IO [TestTree]
makeParserPrinterTests = do
  files <- over (mapped.mapped) (examplesDir </>) $ listDirectory examplesDir
  contents <- traverse readFile files
  let filesExpectations =
        zip files (parse_print_expr_id <$> contents)
  let spec = traverse_ (uncurry it) filesExpectations
  (testProperty "AST is valid python" prop_ast_is_valid_python :) <$> testSpecs spec
