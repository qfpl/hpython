{-# language RankNTypes #-}
module Language.Python.Printer.TestlistStarExpr where

import Papa
import Text.PrettyPrint hiding ((<>), comma)

import Language.Python.AST.TestlistStarExpr
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols

testlistStarExpr
  :: (forall x. test as ectxt x -> Doc)
  -> (forall x. starExpr as ectxt x -> Doc)
  -> TestlistStarExpr test starExpr as ectxt a -> Doc
testlistStarExpr test starExpr s =
  case s of
    TestlistStarExprSingle v _ -> test v
    TestlistStarExprSingleComma v c _ ->
      sumElim test starExpr v <>
      betweenWhitespace' comma c
    _
      | Just (h, t, c, _) <- Just s ^? _TestlistStarExprMany ->
          sumElim test starExpr h <>
          foldMapOf
            (_Wrapped.folded)
            (beforeF (betweenWhitespace' comma) (sumElim test starExpr))
            t <>
          foldMap (betweenWhitespace' comma) c
