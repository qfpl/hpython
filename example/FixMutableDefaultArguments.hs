{-# language DataKinds #-}
module FixMutableDefaultArguments where

import Control.Lens.Cons (_head)
import Control.Lens.Fold ((^..), (^?), filtered, folded, anyOf, has)
import Control.Lens.Getter ((^.))
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.~), (%~))
import Control.Monad (guard)
import Data.Function ((&))
import Data.Semigroup ((<>))

import Language.Python.DSL
import Language.Python.Optics
import Language.Python.Syntax.Expr (Expr(..), _Exprs)

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
    isMutable Unit{} = False
    isMutable None{} = False
    isMutable Ellipsis{} = False
    isMutable Lambda{} = False
    isMutable Float{} = False
    isMutable Imag{} = False
    isMutable Int{} = False
    isMutable Bool{} = False
    isMutable String{} = False

    isMutable List{} = True
    isMutable ListComp{} = True
    isMutable Deref{} = True
    isMutable Call{} = True
    isMutable BinOp{} = True
    isMutable UnOp{} = True
    isMutable Not{} = True
    isMutable DictComp{} = True
    isMutable Dict{} = True
    isMutable Ident{} = True
    isMutable Yield{} = True
    isMutable Await{} = True
    isMutable YieldFrom{} = True
    isMutable SetComp{} = True
    isMutable Set{} = True
    isMutable Subscript{} = True
    isMutable Generator{} = True

    isMutable (Ternary _ _ _ a _ b) = isMutable a || isMutable b
    isMutable (Parens _ _ a _) = isMutable a
    isMutable (Tuple _ a _ as) =
      {-

      Tuples contain many expressions, and are mutable if any of the sub-expressions
      are mutable. The '_Exprs' traversal can get at all these sub-expressions

      -}
      anyOf _Exprs isMutable a ||
      anyOf (folded.folded._Exprs) isMutable as
