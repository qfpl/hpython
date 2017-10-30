module Test.Language.Python.Gen.Identifier where

import Papa

import Data.Text (pack)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.AST.Identifier

genIdentifier :: MonadGen m => m (Identifier ())
genIdentifier =
  Identifier <$>
  (pack <$> Gen.list
    (Range.linear 1 10)
    (Gen.frequency [(1, Gen.upper), (1, Gen.lower), (26, pure '_')])) <*>
  pure ()
