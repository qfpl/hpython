module Test.Language.Python.Gen.DottedName where

import Papa
import Hedgehog
import qualified Hedgehog.Gen as Gen

import Language.Python.AST.DottedName
import Language.Python.AST.Symbols

import Test.Language.Python.Gen.Combinators
import Test.Language.Python.Gen.Identifier

genDottedName :: MonadGen m => m (DottedName ())
genDottedName =
  DottedName <$>
  genIdentifier <*>
  genListF
    (genBeforeF (genBetweenWhitespace $ pure Dot) genIdentifier) <*>
  pure ()
