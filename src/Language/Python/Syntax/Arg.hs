{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
module Language.Python.Syntax.Arg where

import Control.Lens.Getter ((^.), to)
import Control.Lens.Lens (Lens, lens)
import Data.Coerce (coerce)
import Data.String (IsString(..))
import Unsafe.Coerce (unsafeCoerce)

import Data.VTraversable
import Language.Python.Optics.Expr
import Language.Python.Optics.Validated
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Raw
import Language.Python.Syntax.Whitespace

-- | Actual parameters for functions
--
-- In the following examples, @x@ is an actual parameter.
--
-- @
-- y = foo(x)
-- y = bar(quux=x)
-- y = baz(*x)
-- y = flux(**x)
-- @
data Arg expr (v :: [*]) a
  = PositionalArg
  { _argAnn :: a
  , _argExpr :: expr v a
  }
  | KeywordArg
  { _argAnn :: a
  , _unsafeKeywordArgName :: Ident v a
  , _unsafeKeywordArgWhitespaceRight :: [Whitespace]
  , _argExpr :: expr v a
  }
  | StarArg
  { _argAnn :: a
  , _unsafeStarArgWhitespace :: [Whitespace]
  , _argExpr :: expr v a
  }
  | DoubleStarArg
  { _argAnn :: a
  , _unsafeDoubleStarArgWhitespace :: [Whitespace]
  , _argExpr :: expr v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Validated e => Validated (Arg e) where; unvalidated = to unsafeCoerce

instance IsString (Raw e) => IsString (Raw (Arg e)) where; fromString = PositionalArg () . fromString

-- | Lens on the Python expression which is passed as the argument
argExpr :: Validated expr => Lens (Arg expr v a) (Arg expr '[] a) (expr v a) (expr '[] a)
argExpr = lens _argExpr (\s a -> (s ^. unvalidated) { _argExpr = a })

instance VTraversable Arg where
  vtraverse f (KeywordArg a b c d) = KeywordArg a b c <$> f d
  vtraverse f (PositionalArg a b) = PositionalArg a <$> f b
  vtraverse f (StarArg a b c) = StarArg a b <$> f c
  vtraverse f (DoubleStarArg a b c) = DoubleStarArg a b <$> f c

instance HasExprs expr expr => HasExprs (Arg expr) expr where
  _Exprs f (KeywordArg a name ws2 expr) = KeywordArg a (coerce name) ws2 <$> f expr
  _Exprs f (PositionalArg a expr) = PositionalArg a <$> f expr
  _Exprs f (StarArg a ws expr) = StarArg a ws <$> f expr
  _Exprs f (DoubleStarArg a ws expr) = StarArg a ws <$> f expr