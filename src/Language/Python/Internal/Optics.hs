{-# language DataKinds, PolyKinds, LambdaCase, ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language DefaultSignatures, FlexibleContexts #-}
module Language.Python.Internal.Optics where

import Control.Lens.Getter (Getter, to)
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (Traversal', failing)
import Control.Lens.Tuple (_2)
import Control.Lens.Prism (Prism, _Right, _Left, prism)
import Control.Lens.Wrapped (_Wrapped)
import Data.Coerce
import Data.List.NonEmpty
import Language.Python.Internal.Syntax

class Validated (s :: [*] -> * -> *) where
  unvalidated :: Getter (s v a) (s '[] a)
  default unvalidated :: Coercible (s v a) (s '[] a) => Getter (s v a) (s '[] a)
  unvalidated = to coerce

instance Validated Expr where
instance Validated Statement where
instance Validated Block where
instance Validated Ident where
instance Validated Param where

data KeywordParam v a
  = MkKeywordParam
  { _kpAnn :: a
  , _kpName :: Ident v a
  , _kpWhitespaceRight :: [Whitespace]
  , _kpExpr :: Expr v a
  } deriving (Eq, Show)
makeLenses ''KeywordParam

_KeywordParam
  :: Prism
       (Param v a)
       (Param '[] a)
       (KeywordParam v a)
       (KeywordParam '[] a)
_KeywordParam =
  prism
    (\(MkKeywordParam a b d e) -> KeywordParam a b d e)
    (\case
        (coerce -> KeywordParam a b d e) -> Right (MkKeywordParam a b d e)
        (coerce -> a) -> Left a)

_Fundef
  :: Prism
       (Statement v a)
       (Statement '[] a)
       ( a
       , NonEmpty Whitespace, Ident v a
       , [Whitespace], CommaSep (Param v a)
       , [Whitespace], [Whitespace], Newline
       , Block v a
       )
       ( a
       , NonEmpty Whitespace, Ident '[] a
       , [Whitespace], CommaSep (Param '[] a)
       , [Whitespace], [Whitespace], Newline
       , Block '[] a
       )
_Fundef =
  prism
    (\(a, b, c, d, e, f, g, h, i) -> CompoundStatement (Fundef a b c d e f g h i))
    (\case
        (coerce -> CompoundStatement (Fundef a b c d e f g h i)) ->
          Right (a, b, c, d, e, f, g, h, i)
        (coerce -> a) -> Left a)

_Call
  :: Prism
       (Expr v a)
       (Expr '[] a)
       (a, Expr v a, [Whitespace], CommaSep (Arg v a), [Whitespace])
       (a, Expr '[] a, [Whitespace], CommaSep (Arg '[] a), [Whitespace])
_Call =
  prism
    (\(a, b, c, d, e) -> Call a b c d e)
    (\case; (coerce -> Call a b c d e) -> Right (a, b, c, d, e); (coerce -> a) -> Left a)

_Ident
  :: Prism
       (Expr v a)
       (Expr '[] a)
       (a, Ident v a)
       (a, Ident '[] a)
_Ident =
  prism
    (\(a, b) -> Ident a b)
    (\case; (coerce -> Ident a b) -> Right (a, b); (coerce -> a) -> Left a)

_Indents
  :: Traversal'
       (Statement v a)
       [Whitespace]
_Indents f = fmap coerce . (_Blocks._Wrapped) ((traverse._2) f . coerce)

class HasNewlines s where
  _Newlines :: Traversal' (s v a) Newline

instance HasNewlines Block where
  _Newlines f (Block b) =
    Block <$>
    traverse (\(a, b, c) -> (,,) a b <$> _Newlines f c) b

instance HasNewlines CompoundStatement where
  _Newlines f s =
    case s of
      Fundef ann ws1 name ws2 params ws3 ws4 nl block ->
        Fundef ann ws1 name ws2 params ws3 ws4 <$> f nl <*> _Newlines f block
      If ann ws1 cond ws2 ws3 nl block els ->
        If ann ws1 cond ws2 ws3 <$>
        f nl <*>
        _Newlines f block <*>
        traverse
          (\(a, b, c, d) -> (,,,) a b <$> f nl <*> _Newlines f block)
          els
      While ann ws1 cond ws2 ws3 nl block -> While ann ws1 cond ws2 ws3 <$> f nl <*> _Newlines f block

instance HasNewlines Statement where
  _Newlines f (CompoundStatement c) = CompoundStatement <$> _Newlines f c
  _Newlines f (SmallStatements s ss sc nl) = SmallStatements s ss sc <$> f nl

instance HasNewlines Module where
  _Newlines = _Wrapped.traverse.failing (_Left._2) (_Right._Newlines)
