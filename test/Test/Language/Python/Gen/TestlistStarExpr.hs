module Test.Language.Python.Gen.TestlistStarExpr where

import Papa
import Data.Functor.Sum
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.AST.Symbols
import Language.Python.AST.TestlistStarExpr
import Language.Python.IR.ExprConfig

import Test.Language.Python.Gen.Combinators

genTestlistStarExpr
  :: MonadGen m
  => (ExprConfig as dc -> m ws -> m (test ws as dc ()))
  -> (ExprConfig as dc -> m ws -> m (starExpr ws as dc ()))
  -> ExprConfig as dc
  -> m ws
  -> m (TestlistStarExpr ws test starExpr as dc ())
genTestlistStarExpr genTest genStarExpr ecfg ws =
  Gen.choice
    [ TestlistStarExprSingle <$> genTest ecfg ws <*> pure ()
    , TestlistStarExprSingleComma <$>
      testOrStar ws <*>
      genBetween' (Gen.list (Range.linear 0 10) ws) (pure Comma) <*>
      pure ()
    , Gen.just .
      fmap (review _TestlistStarExprMany) $
      (,,,) <$>
      testOrStar ws <*>
      genNonEmptyF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma)
          (testOrStar ws)) <*>
      Gen.maybe (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma) <*>
      pure ()
    ]
  where
    testOrStar ws' =
      Gen.choice
        [ InL <$> genTest ecfg ws'
        , InR <$> genStarExpr ecfg ws'
        ]
