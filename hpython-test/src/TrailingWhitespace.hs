{-# language TemplateHaskell #-}
module TrailingWhitespace (trailingWhitespaceTests) where

import Hedgehog

import Control.Lens.Setter ((.~))
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Language.Python.Internal.Render
  (showRenderOutput, renderWhitespace, renderParam)
import Language.Python.Syntax.Whitespace (HasTrailingWhitespace(..))

import qualified Data.Text as Text

import Generators.Common (genWhitespaces)
import Generators.General (genExpr, genParam)

trailingWhitespaceTests :: Group
trailingWhitespaceTests = $$discover

testTrailingWhitespace
  :: (HasTrailingWhitespace s, Show s)
  => Gen s
  -> (s -> Text)
  -> PropertyT IO ()
testTrailingWhitespace g showG = do
  a <- forAll g
  let withoutTrailing = Text.dropWhileEnd ((||) <$> isSpace <*> (== '\\')) $ showG a

  withoutTrailing === showG (a & trailingWhitespace .~ [])

  ws <- forAll genWhitespaces
  (withoutTrailing <> showRenderOutput (traverse_ renderWhitespace ws)) ===
    showG (a & trailingWhitespace .~ ws)

prop_trailingWhitespace_param :: Property
prop_trailingWhitespace_param =
  property $
  testTrailingWhitespace
    (genParam genExpr)
    (showRenderOutput . renderParam)
