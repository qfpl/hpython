{-# language DataKinds, PolyKinds, LambdaCase, ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language DefaultSignatures, FlexibleContexts #-}
module Language.Python.Internal.Optics where

import Control.Lens
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
  , _kpWhitespaceLeft :: [Whitespace]
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
    (\(MkKeywordParam a b c d e) -> KeywordParam a b c d e)
    (\case
        (coerce -> KeywordParam a b c d e) -> Right (MkKeywordParam a b c d e)
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
    (\(a, b, c, d, e, f, g, h, i) -> Fundef a b c d e f g h i)
    (\case; (coerce -> Fundef a b c d e f g h i) -> Right (a, b, c, d, e, f, g, h, i); (coerce -> a) -> Left a)

_Call
  :: Prism
       (Expr v a)
       (Expr '[] a)
       (a, Expr v a, [Whitespace], CommaSep (Arg v a))
       (a, Expr '[] a, [Whitespace], CommaSep (Arg '[] a))
_Call =
  prism
    (\(a, b, c, d) -> Call a b c d)
    (\case; (coerce -> Call a b c d) -> Right (a, b, c, d); (coerce -> a) -> Left a)

_Ident
  :: Prism
       (Expr v a)
       (Expr '[] a)
       (a, Ident v a)
       (a, Ident '[] a)
_Ident =
  prism
    (uncurry Ident)
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
    traverse (\(a, b, c, d) -> (,,,) a b <$> _Newlines f c <*> traverse f d) b

instance HasNewlines Statement where
  _Newlines f s =
    case s of
      Fundef ann ws1 name ws2 params ws3 ws4 nl block ->
        Fundef ann ws1 name ws2 params ws3 ws4 <$> f nl <*> _Newlines f block
      Return{} -> pure s
      Expr{} -> pure s
      If ann ws1 cond ws2 ws3 nl block els ->
        If ann ws1 cond ws2 ws3 <$>
        f nl <*>
        _Newlines f block <*>
        traverse
          (\(a, b, c, d) -> (,,,) a b <$> f nl <*> _Newlines f block)
          els
      While ann ws1 cond ws2 ws3 nl block -> While ann ws1 cond ws2 ws3 <$> f nl <*> _Newlines f block
      Assign{} -> pure s
      Pass{} -> pure s
      Break{} -> pure s
      Global{} -> pure s
      Nonlocal{} -> pure s
      Del{} -> pure s
