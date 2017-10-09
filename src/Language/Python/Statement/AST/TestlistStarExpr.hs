{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Language.Python.Statement.AST.TestlistStarExpr
  ( TestlistStarExpr
    ( TestlistStarExprSingle
    , TestlistStarExprSingleComma
    , _testlistStarExprSingle_value
    , _testlistStarExprSingle_ann
    , _testlistStarExprSingleComma_value
    , _testlistStarExprSingleComma_comma
    , _testlistStarExprSingleComma_ann
    , _testlistStarExpr_head
    , _testlistStarExpr_tail
    , _testlistStarExpr_comma
    , _testlistStarExpr_ann
    )
  , _TestlistStarExprMany
  ) where

import Papa hiding (Sum)
import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Symbols
import Language.Python.Expr.AST
import Language.Python.IR.ExprConfig

data TestlistStarExpr (assignable :: AtomType) (ctxt :: DefinitionContext) a
  = TestlistStarExprSingle
  { _testlistStarExprSingle_value :: Test assignable ctxt a
  , _testlistStarExprSingle_ann :: a
  }
  | TestlistStarExprSingleComma
  { _testlistStarExprSingleComma_value
    :: Sum (Test assignable ctxt) (StarExpr assignable ctxt) a
  , _testlistStarExprSingleComma_comma :: Between' [WhitespaceChar] Comma
  , _testlistStarExprSingleComma_ann :: a
  }
  | TestlistStarExprMany
  { _testlistStarExpr_head
    :: Sum (Test assignable ctxt) (StarExpr assignable ctxt) a
  , _testlistStarExpr_tail
    :: Compose
         NonEmpty
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum (Test assignable ctxt) (StarExpr assignable ctxt)))
         a
  , _testlistStarExpr_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _testlistStarExpr_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

_TestlistStarExprMany
  :: Prism'
       (Maybe (TestlistStarExpr as ctxt a))
       ( Sum (Test as ctxt) (StarExpr as ctxt) a
       , Compose
           NonEmpty
           (Compose
             (Before (Between' [WhitespaceChar] Comma))
             (Sum (Test as ctxt) (StarExpr as ctxt)))
           a
       , Maybe (Between' [WhitespaceChar] Comma)
       , a
       )
_TestlistStarExprMany =
  prism'
    (\(a, b, c, d) ->
       case toListOf (_Wrapped.folded._Wrapped.before._2.filtered isStar) b of
         [] -> Just $ TestlistStarExprMany a b c d
         [_] -> Just $ TestlistStarExprMany a b c d
         _ -> Nothing)
    (\case
        Just (TestlistStarExprMany a b c d) -> Just (a, b, c, d)
        _ -> Nothing)
  where
    isStar (InL _) = False
    isStar (InR _) = True

deriveEq1 ''TestlistStarExpr
deriveShow1 ''TestlistStarExpr
