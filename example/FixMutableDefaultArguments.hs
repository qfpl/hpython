{-# language DataKinds #-}
module FixMutableDefaultArguments where

import Control.Lens.Cons (_head)
import Control.Lens.Fold ((^..), (^?), filtered, folded, foldMapOf, has)
import Control.Lens.Getter ((^.))
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.~), (%~))
import Control.Monad (guard)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Monoid (Any(..))

import Data.VConst
import Data.VFix
import Data.VFoldable
import Language.Python.DSL
import Language.Python.Optics
import Language.Python.Syntax

{-

I want to write a function that fixes the 'mutable default argument' pattern.

It takes a function like this:

def a(b=[]):
  b.push(1)

to this:

def a(b=None):
  if b is None:
    b = []
  b.push(1)

if the parameter is a 'mutable' thing.

-}
fixMutableDefaultArguments :: Raw Statement -> Maybe (Raw Statement)
fixMutableDefaultArguments input = do
  {-

  Firstly, this transformation only applies to function definitions. So we use the '_Fundef'
  prism to pull out the function definition from a statement.

  -}
  function <- input ^? _Fundef

  let

    {-

    I want the parameters of the function as a list. I can use the 'parameters' lens to get
    at them.

    -}
    paramsList = function ^.. parameters.folded

    {-

    I also want to know which parameters meet the 'mutable default argument' pattern.

    The '_KeywordParam' prism only matches keyword parameters, and '_kpExpr' is the field
    accessor for the right hand side of an '=' in a keyword parameter.

    This expression gets 'all the keyword parameters that have mutable value on their RHS'

    -}
    targetParams = paramsList ^.. folded._KeywordParam.filtered (isMutable._kpExpr)

  {-

  If the list of targetParams is empty, then we don't need to do the transformation

  -}
  guard $ has _head targetParams

  let
    {-

    Let's generate some 'if' statements

    -}
    conditionalAssignments =
      {-

      for each 'param' in our list of keyword parameters

      -}
      (\param ->
         let
           {-

           let <pName> be the left hand side of the '='

           -}
           pName = var_ (param ^. kpName.identValue)

           {-

           let <pValue> be the right hand side of the '='

           -}
           pValue = param ^. kpExpr

         in
           {-

           output a new line, which says...

           -}
           line_ $
           {-

           if <pName> is None:
               <pName> = <pValue>

           -}
           if_ (pName `is_` none_) [ line_ (pName .= pValue) ]) <$>
      targetParams

    {-

    For each parameter in the original parameter list, set the right hand sides of
    the target parameters to 'None', but leave all of the others as they were.

    -}
    newparams =
      paramsList & traverse._KeywordParam.kpExpr.filtered isMutable .~ none_

  pure $
    {-

    Return a new function defition statement

    -}
    _Fundef #
      {-

      that consists of the original function

      -}
      (function &
       {-

       with its problem parameters set to 'None'

       -}
       parameters_ .~ newparams &

       {-

       and for each problem parameter, there is a corresponding if statement at the
       start of the function definition

       -}
       body_ %~ (conditionalAssignments <>))

  where
    {-

    This function decides whether or not an expression is mutable

    -}
    isMutable :: Raw Expr -> Bool
    isMutable e = unVConst $ vcata alg e
      where
        alg expr =
          VConst $
          case expr of
            Unit{} -> False
            None{} -> False
            Ellipsis{} -> False
            Lambda{} -> False
            Float{} -> False
            Imag{} -> False
            Int{} -> False
            Bool{} -> False
            String{} -> False

            List{} -> True
            ListComp{} -> True
            Deref{} -> True
            Call{} -> True
            Binary{} -> True
            Unary{} -> True
            Not{} -> True
            DictComp{} -> True
            Dict{} -> True
            Ident{} -> True
            Yield{} -> True
            Await{} -> True
            YieldFrom{} -> True
            SetComp{} -> True
            Set{} -> True
            Subscript{} -> True
            Generator{} -> True

            Ternary _ _ _ a _ b -> unVConst a || unVConst b
            Parens _ _ a _ -> unVConst a
            Tuple _ a _ as ->
              {-

              Tuples contain many expressions, and are mutable if any of the sub-expressions
              are mutable. The '_Exprs' traversal can get at all these sub-expressions

              -}
              getAny $
              vfoldMap (Any . unVConst) a <>
              foldMapOf (folded.folded) (vfoldMap (Any . unVConst)) as
