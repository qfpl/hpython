{-# language OverloadedLists #-}
{-# language DataKinds #-}
module FixMutableDefaultArguments where

import Control.Lens.Fold ((^..), (^?), filtered, folded, anyOf)
import Control.Lens.Getter (getting)
import Control.Lens.Setter ((.~))
import Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax
import Language.Python.Syntax

fixMutableDefaultArguments :: Raw Statement -> Maybe (Raw Statement)
fixMutableDefaultArguments input = do
  function <- input ^? _Fundef

  let paramsList = function ^.. parameters.folded
  _ <- paramsList ^? folded._KeywordParam.filtered (isMutable._kpExpr)

  let
    targetParams = paramsList ^.. folded._KeywordParam.filtered (isMutable._kpExpr)

    conditionalAssignments =
      (\(pname, value) ->
         st_ $ if_ (var_ pname `is_` none_) [ st_ $ var_ pname .= value ]) <$>
      zip
        (targetParams ^.. folded.kpName.identValue)
        (paramsList ^.. folded._KeywordParam.kpExpr.filtered isMutable)

    newparams =
      paramsList & traverse._KeywordParam.filtered (isMutable._kpExpr).kpExpr .~ none_

  pure $
    fundef
      (function &
       setParameters newparams &
       modifyBody (flip (foldr NonEmpty.cons) conditionalAssignments))
  where
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
      anyOf (getting _Exprs) isMutable a || anyOf (folded.folded.getting _Exprs) isMutable as
