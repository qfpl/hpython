module Test.Language.Python.Gen.Comment where

import Papa
import Data.Text (pack)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.AST.Comment
import Language.Python.AST.Symbols

import Test.Language.Python.Gen.Combinators
import Test.Language.Python.Gen.Identifier

genComment :: MonadGen m => m (Comment ())
genComment =
  Comment <$>
  (pack <$> Gen.list (Range.linear 0 100) Gen.ascii) <*>
  pure ()
