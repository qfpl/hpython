module Language.Python.IR.Checker.TestlistStarExpr where

import Papa hiding (Sum)
import Data.Functor.Compose
import Data.Functor.Sum
import Language.Python.IR.ExprConfig
import Language.Python.IR.SyntaxChecker

import qualified Language.Python.AST.TestlistStarExpr as Safe
import qualified Language.Python.IR.TestlistStarExpr as IR

{-
checkTestlistStarExpr
  :: ExprConfig assignable ctxt
  -> _
  -> _
  -> IR.TestlistStarExpr ann
  -> SyntaxChecker ann (Safe.TestlistStarExpr test starExpr assignable ctxt ann)
-}
checkTestlistStarExpr checkTest checkStarExpr ecfg (IR.TestlistStarExpr h t c ann) =
  case (h, t, c) of
    (InL h', Compose [], Nothing) ->
      Safe.TestlistStarExprSingle <$>
      checkTest ecfg h' <*>
      pure ann
    (_, Compose [], Nothing) -> syntaxError $ TopLevelUnpacking ann
    (_, Compose [], Just c') ->
      Safe.TestlistStarExprSingleComma <$>
      testOrStar h <*>
      pure c' <*>
      pure ann
    (_, Compose (t':ts'), _) ->
      liftError MultipleUnpackingsInLHS $
      fmap (maybe (Left ann) Right . review Safe._TestlistStarExprMany) $
      (,,,) <$>
      testOrStar h <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        testOrStar
        (Compose $ t' :| ts') <*>
      pure c <*>
      pure ann
  where
    testOrStar (InL a) = InL <$> checkTest ecfg a
    testOrStar (InR a) = InR <$> checkStarExpr ecfg a
