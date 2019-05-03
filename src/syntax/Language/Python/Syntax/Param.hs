{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
module Language.Python.Syntax.Param where

import Control.Lens.Getter ((^.), view, to)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Prism (_Just)
import Control.Lens.Setter ((.~), over, mapped)
import Control.Lens.Traversal (Traversal, traverseOf)
import Control.Lens.Tuple (_2)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Generics.Product.Typed (typed)
import Data.Maybe (isNothing)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Data.VFoldable
import Data.VFunctor
import Data.VTraversable
import Language.Python.Optics.Idents (HasIdents)
import Language.Python.Optics.Exprs
import Language.Python.Optics.Validated
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Raw
import Language.Python.Syntax.Whitespace

-- | Formal parameters for functions
--
-- See <https://docs.python.org/3.5/reference/compound_stmts.html#function-definitions>
data Param expr (v :: [*]) a
  -- | @def foo(a):@
  = PositionalParam
  { _paramAnn :: Ann a
  , _paramName :: Ident v a
  , _paramType :: Maybe (Colon, expr v a)
  }
  -- | @def foo(bar=None):@
  | KeywordParam
  { _paramAnn :: Ann a
  , _paramName :: Ident v a
  -- ':' spaces <expr>
  , _paramType :: Maybe (Colon, expr v a)
  -- = spaces
  , _unsafeKeywordParamWhitespaceRight :: [Whitespace]
  , _unsafeKeywordParamExpr :: expr v a
  }
  -- | @def foo(*xs):@
  | StarParam
  { _paramAnn :: Ann a
  -- '*' spaces
  , _unsafeStarParamWhitespace :: [Whitespace]
  , _unsafeStarParamName :: Ident v a
  -- ':' spaces <expr>
  , _paramType :: Maybe (Colon, expr v a)
  }
  -- | @def foo(*):@
  | UnnamedStarParam
  { _paramAnn :: Ann a
  -- '*' spaces
  , _unsafeUnnamedStarParamWhitespace :: [Whitespace]
  }
  -- | @def foo(**dict):@
  | DoubleStarParam
  { _paramAnn :: Ann a
  -- '**' spaces
  , _unsafeDoubleStarParamWhitespace :: [Whitespace]
  , _paramName :: Ident v a
  -- ':' spaces <expr>
  , _paramType :: Maybe (Colon, expr v a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasIdents expr => HasIdents (Param expr)

instance HasAnn (Param expr v) where
  annot :: forall a. Lens' (Param expr v a) (Ann a)
  annot = typed @(Ann a)

instance Validated e => Validated (Param e) where; unvalidated = to unsafeCoerce

instance VFunctor Param where; vfmap = vfmapDefault
instance VFoldable Param where; vfoldMap = vfoldMapDefault
instance VTraversable Param where
  vtraverse f p =
    case p of
      PositionalParam a b c ->
        PositionalParam a b <$> traverseOf (traverse._2) f c
      KeywordParam a b c d e ->
        (\c' -> KeywordParam a b c' d) <$>
        traverseOf (traverse._2) f c <*>
        f e
      UnnamedStarParam a b -> pure $ UnnamedStarParam a b
      StarParam a b c d ->
        StarParam a b c <$>
        traverseOf (traverse._2) f d
      DoubleStarParam a b c d ->
        StarParam a b c <$>
        traverseOf (traverse._2) f d

instance IsString (Raw (Param expr)) where
  fromString a = PositionalParam (Ann ()) (fromString a) Nothing

instance HasTrailingWhitespace (expr v a) => HasTrailingWhitespace (Param expr v a) where
  trailingWhitespace =
    lens
      (\case
          PositionalParam _ a b ->
            maybe (a ^. trailingWhitespace) (^. _2.trailingWhitespace) b
          KeywordParam _ _ _ _ a -> a ^. trailingWhitespace
          UnnamedStarParam _ a -> a
          StarParam _ _ b c ->
            maybe
              (b ^. trailingWhitespace)
              (^. _2.trailingWhitespace)
              c
          DoubleStarParam _ _ a b ->
            maybe
              (a ^. trailingWhitespace)
              (^. _2.trailingWhitespace)
              b)
      (\p ws -> case p of
          PositionalParam a b c ->
            PositionalParam a
              (if isNothing c then b & trailingWhitespace .~ ws else b)
              (c & _Just._2.trailingWhitespace .~ ws)
          KeywordParam a b c d e ->
            KeywordParam a b c d $ e & trailingWhitespace .~ ws
          UnnamedStarParam a _ -> UnnamedStarParam a ws
          StarParam a b c d ->
            StarParam a
              b
              (if isNothing d then c & trailingWhitespace .~ ws else c)
              (d & _Just._2.trailingWhitespace .~ ws)
          DoubleStarParam a b c d ->
            DoubleStarParam a b
              (if isNothing d then c & trailingWhitespace .~ ws else c)
              (d & _Just._2.trailingWhitespace .~ ws))

-- | Lens on the syntrax tree annotation on a parameter
paramAnn :: Lens' (Param expr v a) a
paramAnn = annot_

-- | A faux-lens on the optional Python type annotation which may follow a parameter
--
-- This is not a lawful 'Lens' because setting an 'UnnamedStarParam''s type won't
-- have any effect.
--
-- This optic, like many others in hpython, loses validation information
-- (the @v@ type parameter)
--
-- The following is an example, where @int@ is the paramtype:
--
-- @
-- def foo(x: int):
-- @
paramType_
  :: (Functor f, Validated expr)
  => (Maybe (Colon, expr v a) -> f (Maybe (Colon, expr '[] a)))
  -> Param expr v a -> f (Param expr '[] a)
paramType_ =
  lens
    (\case
        UnnamedStarParam{} -> Nothing
        a -> _paramType a)
    (\s ty -> case s ^. unvalidated of
       PositionalParam a b _ -> PositionalParam a b ty
       KeywordParam a b _ c d -> KeywordParam a b ty c d
       StarParam a b c _ -> StarParam a b c ty
       UnnamedStarParam a b -> UnnamedStarParam a b
       DoubleStarParam a b c _ -> DoubleStarParam a b c ty)

-- | 'Traversal' targeting the Python type annotations which may follow a parameter
paramType :: Validated expr => Traversal (Param expr v a) (Param expr '[] a) (Colon, expr v a) (Colon, expr '[] a)
paramType = paramType_._Just

-- | (affine) 'Control.Lens.Traversal.Traversal' on the name of a parameter
--
-- The name is @x@ in the following examples:
--
-- @
-- def foo(x):
-- def foo(x=None):
-- def foo(*x):
-- def foo(**x):
-- @
--
-- But the following example does not have a 'paramName':
--
-- @
-- def foo(*):
-- @
paramName :: Validated expr => Traversal (Param expr v a) (Param expr '[] a) (Ident v a) (Ident '[] a)
paramName f (PositionalParam a b c) =
  PositionalParam a <$> f b <*> pure (over (mapped._2) (view unvalidated) c)
paramName f (KeywordParam a b c d e) =
  (\b' -> KeywordParam a b' (over (mapped._2) (view unvalidated) c) d (e ^. unvalidated)) <$>
  f b
paramName f (StarParam a b c d) =
  (\c' -> StarParam a b c' (over (mapped._2) (view unvalidated) d)) <$>
  f c
paramName _ (UnnamedStarParam a b) = pure $ UnnamedStarParam a b
paramName f (DoubleStarParam a b c d) =
  (\c' -> DoubleStarParam a b c' (over (mapped._2) (view unvalidated) d)) <$>
  f c

instance HasExprs expr expr => HasExprs (Param expr) expr where
  _Exprs f (KeywordParam a name ty ws2 expr) =
    KeywordParam a (coerce name) <$>
    traverseOf (traverse._2) f ty <*>
    pure ws2 <*>
    f expr
  _Exprs f (PositionalParam a b c) =
    PositionalParam a (coerce b) <$> traverseOf (traverse._2) f c
  _Exprs f (StarParam a b c d) =
    StarParam a b (coerce c) <$> traverseOf (traverse._2) f d
  _Exprs _ (UnnamedStarParam a b) = pure $ UnnamedStarParam a b
  _Exprs f (DoubleStarParam a b c d) =
    DoubleStarParam a b (coerce c) <$> traverseOf (traverse._2) f d
