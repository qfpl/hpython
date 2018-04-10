{-# language OverloadedLists #-}
{-# language DataKinds #-}
module FixMutableDefaultArguments where

import Control.Lens.Fold ((^..), (^?), filtered, folded)
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax
import Language.Python.Syntax

fixMutableDefaultArguments :: Statement '[] () -> Maybe (Statement '[] ())
fixMutableDefaultArguments input = do
  (_, _, name, _, params, _, _, _, body) <- input ^? _Fundef

  let paramsList = toList params
  targetParam <- paramsList ^? folded._KeywordParam.filtered (isMutable._kpExpr)

  let
    pname = targetParam ^. kpName.identValue

    newparams =
      paramsList & traverse._KeywordParam.filtered (isMutable._kpExpr).kpExpr .~ none_

    fixed =
      fmap (\value -> if_ (var_ pname `is_` none_) [ var_ pname .= value ]) $
      paramsList ^.. folded._KeywordParam.kpExpr.filtered isMutable

  pure $ def_ name newparams (NonEmpty.fromList $ fixed <> (body ^.. _Statements))
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
    isMutable Ident{} = True
    isMutable (Parens _ _ a _) = isMutable a
