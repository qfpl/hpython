module Language.Python.Syntax.Expr.Precedence where

import Language.Python.Syntax.Expr
import Language.Python.Syntax.Expr.Optics

-- |
-- @shouldGroupLeft op left@ returns true if @left@ needs to be parenthesised
-- when it is the left argument of @op@
shouldGroupLeft :: AsExpr expr expr => BinOp a -> expr v a -> Bool
shouldGroupLeft op left =
  let
    entry = lookupOpEntry op operatorTable

    lEntry =
      case vout left of
        Binary _ _ lOp _ -> Just $ lookupOpEntry lOp operatorTable
        _ -> Nothing

    leftf =
      case entry ^. opAssoc of
        R | Just (OpEntry _ prec R) <- lEntry -> prec <= entry ^. opPrec
        _ -> False

    leftf' =
      case (vout left, op) of
        (Unary{}, Exp{}) -> True
        (Tuple{}, _) -> True
        (Not{}, BoolAnd{}) -> False
        (Not{}, BoolOr{}) -> False
        (Not{}, _) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (lEntry ^? _Just.opPrec)
  in
    leftf || leftf'

-- |
-- @shouldGroupRight op right@ returns true if @right@ needs to be parenthesised
-- when it is the right argument of @op@
shouldGroupRight :: AsExpr expr expr => BinOp a -> expr v a -> Bool
shouldGroupRight op right =
  let
    entry = lookupOpEntry op operatorTable

    rEntry =
      case vout right of
        Binary _ _ rOp _ -> Just $ lookupOpEntry rOp operatorTable
        _ -> Nothing

    rightf =
      case entry ^. opAssoc of
        L | Just (OpEntry _ prec L) <- rEntry -> prec <= entry ^. opPrec
        _ -> False

    rightf' =
      case (op, vout right) of
        (_, Tuple{}) -> True
        (BoolAnd{}, Not{}) -> False
        (BoolOr{}, Not{}) -> False
        (_, Not{}) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (rEntry ^? _Just.opPrec)
  in
    rightf || rightf'