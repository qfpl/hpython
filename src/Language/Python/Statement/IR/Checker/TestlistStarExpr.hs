module Language.Python.Statement.IR.Checker.TestlistStarExpr where

import Papa hiding (Sum)
import Data.Functor.Compose
import Data.Functor.Sum
import Language.Python.IR.ExprConfig
import Language.Python.IR.SyntaxChecker
import Language.Python.Expr.IR.Checker

import qualified Language.Python.Statement.AST.TestlistStarExpr as Safe
import qualified Language.Python.Statement.IR.TestlistStarExpr as IR

checkTestlistStarExpr
  :: ExprConfig assignable ctxt
  -> IR.TestlistStarExpr ann
  -> SyntaxChecker ann (Safe.TestlistStarExpr assignable ctxt ann)
checkTestlistStarExpr ecfg (IR.TestlistStarExpr h t c ann) =
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
