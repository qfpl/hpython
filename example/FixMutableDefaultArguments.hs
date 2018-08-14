{-# language OverloadedLists #-}
{-# language DataKinds #-}
module FixMutableDefaultArguments where

import Control.Lens.Fold ((^..), (^?), filtered, folded, anyOf)
import Control.Lens.Getter ((^.), getting)
import Control.Lens.Setter ((.~), over)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax
import Language.Python.Syntax

fixMutableDefaultArguments :: Statement '[] () -> Maybe (Statement '[] ())
fixMutableDefaultArguments input = do
  (_, decos, idnts, _, name, _, params, _, suite) <- input ^? _Fundef

  let paramsList = toList params
  _ <- paramsList ^? folded._KeywordParam.filtered (isMutable._kpExpr)

  let
    targetParams = paramsList ^.. folded._KeywordParam.filtered (isMutable._kpExpr)

    conditionalAssignments =
      (\(pname, value) -> if_ (var_ pname `is_` none_) [ var_ pname .= value ]) <$>
      zip
        (targetParams ^.. folded.kpName.identValue)
        (paramsList ^.. folded._KeywordParam.kpExpr.filtered isMutable)

    newparams =
      paramsList & traverse._KeywordParam.filtered (isMutable._kpExpr).kpExpr .~ none_

  pure $
    over (_Indents.indentsValue) (idnts ^. indentsValue <>) $
    def_ name newparams
    (NonEmpty.fromList $ conditionalAssignments <> (suite ^.. _Statements.noIndents))
  where
    isMutable :: Expr v a -> Bool
    isMutable Unit{} = False
    isMutable None{} = False
    isMutable Lambda{} = False
    isMutable Float{} = False
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
    isMutable Dict{} = True
    isMutable Ident{} = True
    isMutable Yield{} = True
    isMutable YieldFrom{} = True
    isMutable Set{} = True
    isMutable Subscript{} = True
    isMutable Generator{} = True
    isMutable (Ternary _ _ _ a _ b) = isMutable a && isMutable b
    isMutable (Parens _ _ a _) = isMutable a
    isMutable (Tuple _ a _ as) = anyOf (getting _Exprs) isMutable a || anyOf (folded.folded.getting _Exprs) isMutable as
