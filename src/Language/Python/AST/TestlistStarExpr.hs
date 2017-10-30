{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Language.Python.AST.TestlistStarExpr
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
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Functor.Sum.Lens
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.Symbols
import Language.Python.IR.ExprConfig

data TestlistStarExpr test starExpr (as :: AtomType) (ctxt :: DefinitionContext) a
  = TestlistStarExprSingle
  { _testlistStarExprSingle_value :: test as ctxt a
  , _testlistStarExprSingle_ann :: a
  }
  | TestlistStarExprSingleComma
  { _testlistStarExprSingleComma_value
    :: Sum (test as ctxt) (starExpr as ctxt) a
  , _testlistStarExprSingleComma_comma :: Between' [WhitespaceChar] Comma
  , _testlistStarExprSingleComma_ann :: a
  }
  | TestlistStarExprMany
  { _testlistStarExpr_head
    :: Sum (test as ctxt) (starExpr as ctxt) a
  , _testlistStarExpr_tail
    :: Compose
         NonEmpty
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Sum (test as ctxt) (starExpr as ctxt)))
         a
  , _testlistStarExpr_comma :: Maybe (Between' [WhitespaceChar] Comma)
  , _testlistStarExpr_ann :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)
$(return [])

_TestlistStarExprMany
  :: Prism'
       (Maybe (TestlistStarExpr test starExpr as ctxt a))
       ( Sum (test as ctxt) (starExpr as ctxt) a
       , Compose
           NonEmpty
           (Compose
             (Before (Between' [WhitespaceChar] Comma))
             (Sum (test as ctxt) (starExpr as ctxt)))
           a
       , Maybe (Between' [WhitespaceChar] Comma)
       , a
       )
_TestlistStarExprMany =
  prism'
    (\(a, b, c, d) ->
       case toListOf (_Wrapped.folded._Wrapped.before._2.filtered (isn't _InL)) b of
         [] -> Just $ TestlistStarExprMany a b c d
         [_]
           | InR _ <- a -> Nothing
           | otherwise -> Just $ TestlistStarExprMany a b c d
         _ -> Nothing)
    (\case
        Just (TestlistStarExprMany a b c d) -> Just (a, b, c, d)
        _ -> Nothing)

instance (Eq1 (test as ctxt), Eq1 (starExpr as ctxt)) =>
  Eq1 (TestlistStarExpr test starExpr as ctxt) where
  liftEq = $(makeLiftEq ''TestlistStarExpr)

instance (Show1 (test as ctxt), Show1 (starExpr as ctxt)) =>
  Show1 (TestlistStarExpr test starExpr as ctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''TestlistStarExpr)

instance (Ord1 (test as ctxt), Ord1 (starExpr as ctxt)) =>
  Ord1 (TestlistStarExpr test starExpr as ctxt) where
  liftCompare = $(makeLiftCompare ''TestlistStarExpr)
