{-# LANGUAGE DataKinds #-}
module Test.Language.Python.ParserPrinter (makeParserPrinterTests) where

import Papa
import Prelude (error)
import Control.Monad.IO.Class
import Hedgehog
import System.Directory
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Hedgehog
import Text.Trifecta hiding (render, runUnspaced)

import qualified Text.PrettyPrint.ANSI.Leijen as WL
import qualified Text.PrettyPrint as HPJ

import Language.Python.IR.SyntaxConfig

import qualified Language.Python.Expr.Parser as Parse
import qualified Language.Python.Expr.IR.Checker as Check
import qualified Language.Python.Expr.Printer as Print
import qualified Language.Python.Expr.AST as AST
import qualified Test.Language.Python.Expr.Gen as GenAST

import Text.Parser.Unspaced

examplesDir :: FilePath
examplesDir = "test" </> "examples" </> "expressions" </> "valid"

parse_print_expr_id :: String -> Expectation
parse_print_expr_id input =
  case parseString (runUnspaced $ Parse.test <* eof) mempty input of
    Success unchecked ->
      let
        checkResult =
          (fmap Print.test . Check.runChecker $
            Check.checkTest
              (SyntaxConfig SNotAssignable STopLevel)
              unchecked) <!>
          (fmap Print.test . Check.runChecker $
            Check.checkTest
              (SyntaxConfig SAssignable STopLevel)
              unchecked)
      in
        case checkResult of
          Left es ->
            expectationFailure $
            WL.displayS (WL.renderPretty 1.0 80 . WL.text $ show es) ""
          Right ast ->
            HPJ.render ast `shouldBe` input
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

  (_, _, errString) <-
    readProcessWithExitCode
      "python3"
      [ "-c"
      , input
      , "-m"
      , "py_compile" 
      ]
      ""
  case last (lines errString) of
    Nothing -> pure SyntaxCorrect
    Just l -> 
      case parseString (parseErr errString) mempty l of
        Success s -> pure s
        Failure (ErrInfo msg _) ->
          error $
            WL.displayS (WL.renderPretty 1.0 80 $
              WL.text "Parsing of Python stderr failed." WL.<$>
              WL.line <>
              WL.text "Parser error: " WL.<$> WL.line <> msg WL.<$> WL.line <>
              WL.text "Input string: " WL.<$> WL.line <> WL.text errString) ""
  where
    parseErr :: (Monad m, DeltaParsing m) => String -> m SyntaxCheckResult
    parseErr errorMsg = do
      errString <- optional (manyTill anyChar (try $ string "Error: "))
      _ <- manyTill anyChar eof
      pure $ case errString of
        Just "Syntax" -> SyntaxError errorMsg
        Just "Indentation" -> SyntaxError errorMsg
        _ -> SyntaxCorrect

prop_ast_is_valid_python :: SAtomType atomType -> Property
prop_ast_is_valid_python assignability =
  property $ do
    expr <- forAll (GenAST.genTest $ SyntaxConfig assignability STopLevel)
    let program = HPJ.render $ Print.test expr
    res <- liftIO $ checkSyntax program
    case res of
      SyntaxError pythonError -> do
        footnote $
          unlines
          [ "Input string caused a syntax error."
          , ""
          , "Input string:"
          , program
          , "( " ++ show program ++ " )"
          , ""
          , "Error message:"
          , ""
          , pythonError
          ]
        failure
      SyntaxCorrect -> success

makeParserPrinterTests :: IO [TestTree]
makeParserPrinterTests = do
  files <-
    fmap sort .
    over (mapped.mapped) (examplesDir </>) $ listDirectory examplesDir
  contents <- traverse readFile files
  let
    filesExpectations =
      zip files (parse_print_expr_id <$> contents)

    spec = traverse_ (uncurry it) filesExpectations
    properties =
      [ testProperty
          "AST is valid python - assignable" $
          prop_ast_is_valid_python SAssignable
      , testProperty
          "AST is valid python - not assignable" $
          prop_ast_is_valid_python SNotAssignable
      ]
  (properties ++) <$> testSpecs spec
