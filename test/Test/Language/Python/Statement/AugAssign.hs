module Test.Language.Python.Statement.AugAssign where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import Language.Python.Statement.AST.AugAssign

genAugAssign :: MonadGen m => m AugAssign
genAugAssign =
  Gen.element
    [ PlusEquals
    , MinusEquals
    , StarEquals
    , AtEquals
    , SlashEquals
    , PercentEquals
    , AmphersandEquals
    , PipeEquals
    , CaretEquals
    , ShiftLeftEquals
    , ShiftRightEquals
    , DoubleStarEquals
    , DoubleSlashEquals
    ]
