module Test.Language.Python.Parser.Indentation (makeIndentationTests) where

import Papa
import GHC.Stack
import Test.Tasty
import Test.Tasty.Hspec
import Text.Trifecta

import Language.Python.Parser.Indentation

resultShouldBe :: (HasCallStack, Show a, Eq a) => Result a -> a -> Expectation
resultShouldBe supplied exemplar =
  case supplied of
    Success a -> a `shouldBe` exemplar
    Failure e ->
      expectationFailure $ unlines
      [ "Expected successful parse of: " <> show exemplar <> ","
      , "but got an error:"
      , ""
      , show $ _errDoc e
      ]

indentationParse :: IndentationParserT Parser a -> String -> Result a
indentationParse m =
  parseString (runIndentationParserT m $ 0 :| []) mempty

funcDefParser :: IndentationParsing m => m ()
funcDefParser = do
  string "def funcName():\n"
  indented $ do
    absolute $ string "foo\n"
    absolute $ string "bar\n"
    absolute $ string "baz"
  pure ()

indentationSpec :: Spec
indentationSpec =
  describe "funcDefParser" $ do
    it "succeeds" $ do
      indentationParse funcDefParser (unlines
        [ "def funcName():"
        , " foo"
        , " bar"
        , " baz"
        ]) `resultShouldBe` ()
      indentationParse funcDefParser (unlines
        [ "def funcName():"
        , "   foo"
        , "   bar"
        , "   baz"
        ]) `resultShouldBe` ()
      indentationParse funcDefParser (unlines
        [ "def funcName():"
        , "\tfoo"
        , "\tbar"
        , "\tbaz"
        ]) `resultShouldBe` ()

makeIndentationTests :: IO [TestTree]
makeIndentationTests = testSpecs indentationSpec
