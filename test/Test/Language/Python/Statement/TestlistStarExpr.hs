module Test.Language.Python.Statement.TestlistStarExpr where

import Papa
import Data.Functor.Sum
import Hedgehog
import qualified Hedgehog.Gen as Gen

import Language.Python.AST.Symbols
import Language.Python.IR.ExprConfig
import Language.Python.Statement.AST.TestlistStarExpr

import Test.Language.Python.Expr.Gen
import Test.Language.Python.Gen.Combinators

genTestlistStarExpr
  :: MonadGen m
  => ExprConfig as dc
  -> m (TestlistStarExpr as dc ())
genTestlistStarExpr ecfg =
  Gen.choice
    [ TestlistStarExprSingle <$> genTest ecfg <*> pure ()
    , TestlistStarExprSingleComma <$>
      testOrStar <*>
      genBetweenWhitespace (pure Comma) <*>
      pure ()
    , Gen.just .
      fmap (review _TestlistStarExprMany) $
      (,,,) <$>
      testOrStar <*>
      genNonEmptyF
        (genBeforeF
          (genBetweenWhitespace $ pure Comma)
          testOrStar) <*>
      Gen.maybe (genBetweenWhitespace $ pure Comma) <*>
      pure ()
    ]
  where
    testOrStar =
      Gen.choice
        [ InL <$> genTest ecfg
        , InR <$> genStarExpr ecfg
        ]
