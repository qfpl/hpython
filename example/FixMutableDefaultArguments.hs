{-# language OverloadedLists #-}
{-# language DataKinds #-}
module FixMutableDefaultArguments where

import Control.Lens.Fold ((^..), (^?), filtered, folded)
import Control.Lens.Getter ((^.))
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
  (idnts, _, _, name, _, params, _, _, _, body) <- input ^? _Fundef

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
    (NonEmpty.fromList $ conditionalAssignments <> (body ^.. _Statements.noIndents))
  where
    isMutable :: Expr v a -> Bool
    isMutable None{} = False
    isMutable Int{} = False
    isMutable Bool{} = False
    isMutable String{} = False
    isMutable List{} = True
    isMutable Deref{} = True
    isMutable Call{} = True
    isMutable BinOp{} = True
    isMutable Negate{} = True
    isMutable Not{} = True
    isMutable Ident{} = True
    isMutable (Parens _ _ a _) = isMutable a
    isMutable (Tuple _ a _ as) = isMutable a || any (any isMutable) as
