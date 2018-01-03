module Language.Python.Printer.TestlistStarExpr where

import Papa
import Text.PrettyPrint hiding ((<>), comma)

import Language.Python.AST.TestlistStarExpr
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols

testlistStarExpr
  :: (ws -> Doc)
  -> ((ws -> Doc) -> test ws as ectxt a -> Doc)
  -> ((ws -> Doc) -> starExpr ws as ectxt a -> Doc)
  -> TestlistStarExpr ws test starExpr as ectxt a -> Doc
testlistStarExpr ws test starExpr s =
  case s of
    TestlistStarExprSingle v _ -> test ws v
    TestlistStarExprSingleComma v c _ ->
      sumElim (test ws) (starExpr ws) v <>
      between' (foldMap ws) comma c
    _
      | Just (h, t, c, _) <- Just s ^? _TestlistStarExprMany ->
          sumElim (test ws) (starExpr ws) h <>
          foldMapOf
            (_Wrapped.folded)
            (beforeF (between' (foldMap ws) comma) (sumElim (test ws) (starExpr ws)))
            t <>
          foldMap (between' (foldMap ws) comma) c
