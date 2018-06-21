{-# language DataKinds, PolyKinds, LambdaCase, ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language DefaultSignatures, FlexibleContexts #-}
module Language.Python.Internal.Optics where

import Control.Lens.Fold (Fold)
import Control.Lens.Getter (Getter, to)
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (Traversal, Traversal', failing)
import Control.Lens.Tuple (_3, _5)
import Control.Lens.Prism (Prism, _Right, _Left, prism)
import Control.Lens.Wrapped (_Wrapped)
import Data.Coerce (Coercible, coerce)
import Data.Function ((&))
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
       ( Indents a
       , a
       , NonEmpty Whitespace, Ident v a
       , [Whitespace], CommaSep (Param v a)
       , [Whitespace], [Whitespace], Newline
       , Block v a
       )
       ( Indents a
       , a
       , NonEmpty Whitespace, Ident '[] a
       , [Whitespace], CommaSep (Param '[] a)
       , [Whitespace], [Whitespace], Newline
       , Block '[] a
       )
_Fundef =
  prism
    (\(idnt, a, b, c, d, e, f, g, h, i) -> CompoundStatement (Fundef idnt a b c d e f g h i))
    (\case
        (coerce -> CompoundStatement (Fundef idnt a b c d e f g h i)) ->
          Right (idnt, a, b, c, d, e, f, g, h, i)
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

_Indent :: HasIndents s => Traversal' (s '[] a) [Whitespace]
_Indent = _Indents.indentsValue.traverse.indentWhitespaces

noIndents :: HasIndents s => Fold (s '[] a) (s '[] a)
noIndents f s = f $ s & _Indents.indentsValue .~ []

class HasIndents s where
  _Indents :: Traversal' (s '[] a) (Indents a)

instance HasIndents Statement where
  _Indents f (SmallStatements idnt a b c d) =
    (\idnt' -> SmallStatements idnt' a b c d) <$> f idnt
  _Indents f (CompoundStatement c) = CompoundStatement <$> _Indents f c

instance HasIndents Block where
  _Indents = _Statements._Indents

instance HasIndents CompoundStatement where
  _Indents fun s =
    case s of
      Fundef idnt a b c d e f g h i ->
        (\idnt' i' -> Fundef idnt' a b c d e f g h i') <$>
        fun idnt <*>
        _Indents fun i
      If idnt a b c d e f elifs g ->
        (\idnt' -> If idnt' a b c d e) <$>
        fun idnt <*>
        _Indents fun f <*>
        traverse
          (\(idnt, a, b, c, d, e) ->
             (\idnt' e' -> (idnt', a, b, c, d, e')) <$>
             fun idnt <*>
             _Indents fun e)
          elifs <*>
        traverse
          (\(idnt, a, b, c, d) ->
             (\idnt' d' -> (idnt', a, b, c, d')) <$>
             fun idnt <*>
             _Indents fun d)
          g
      While idnt a b c d e f ->
        (\idnt' f' -> While idnt' a b c d e f') <$>
        fun idnt <*>
        _Indents fun f
      TryExcept idnt a b c d e f g h ->
        (\idnt' e' f' g' h' -> TryExcept idnt' a b c d e' f' g' h') <$>
        fun idnt <*>
        _Indents fun e <*>
        traverse
          (\(idnt, a, b, c, d, e) ->
             (\idnt' e' -> (idnt', a, b, c, d, e')) <$>
             fun idnt <*>
             _Indents fun e)
          f <*>
        traverse
          (\(idnt, a, b, c, d) ->
             (\idnt' d' -> (idnt', a, b, c, d')) <$>
             fun idnt <*>
             _Indents fun d)
          g <*>
        traverse
          (\(idnt, a, b, c, d) ->
             (\idnt' d' -> (idnt', a, b, c, d')) <$>
             fun idnt <*>
             _Indents fun d)
          h
      TryFinally idnt a b c d e idnt2 f g h i ->
        (\idnt' e' idnt2' i' -> TryFinally idnt' a b c d e' idnt2' f g h i') <$>
        fun idnt <*>
        _Indents fun e <*>
        fun idnt2 <*>
        _Indents fun i
      For idnt a b c d e f g h i ->
        (\idnt' h' i' -> For idnt' a b c d e f g h' i') <$>
        fun idnt <*>
        _Indents fun h <*>
        traverse
          (\(idnt, a, b, c, d) ->
             (\idnt' d' -> (idnt', a, b, c, d')) <$>
             fun idnt <*>
             _Indents fun d)
          i
      ClassDef idnt a b c d e f g ->
        (\idnt' g' -> ClassDef idnt' a b c d e f g') <$>
        fun idnt <*>
        _Indents fun g

class HasNewlines s where
  _Newlines :: Traversal' (s v a) Newline

instance HasNewlines Block where
  _Newlines f (Block b) =
    Block <$>
    (traverse._Right._Newlines) f b

instance HasNewlines CompoundStatement where
  _Newlines fun s =
    case s of
      Fundef idnt ann ws1 name ws2 params ws3 ws4 nl block ->
        Fundef idnt ann ws1 name ws2 params ws3 ws4 <$> fun nl <*> _Newlines fun block
      If idnt ann ws1 cond ws3 nl block elifs els ->
        If idnt ann ws1 cond ws3 <$>
        fun nl <*>
        _Newlines fun block <*>
        traverse
          (\(idnt, a, b, c, d, e) -> (,,,,,) idnt a b c <$> fun d <*> _Newlines fun e)
          elifs <*>
        traverse
          (\(idnt, a, b, c, d) -> (,,,,) idnt a b <$> fun c <*> _Newlines fun d)
          els
      While idnt ann ws1 cond ws3 nl block ->
        While idnt ann ws1 cond ws3 <$> fun nl <*> _Newlines fun block
      TryExcept idnt a b c d e f k l ->
        TryExcept idnt a b c <$> fun d <*> _Newlines fun e <*>
        traverse (\(idnt, x, y, z, w, w') -> (,,,,,) idnt x y z <$> fun w <*> _Newlines fun w') f <*>
        traverse (\(idnt, x, y, z, w) -> (,,,,) idnt x y <$> fun z <*> _Newlines fun w) k <*>
        traverse (\(idnt, x, y, z, w) -> (,,,,) idnt x y <$> fun z <*> _Newlines fun w) l
      TryFinally idnt a b c d e idnt2 f g h i ->
        TryFinally idnt a b c <$> fun d <*> _Newlines fun e <*> pure idnt2 <*>
        pure f <*> pure g <*> fun h <*> _Newlines fun i
      For idnt a b c d e f g h i ->
        For idnt a b c d e f <$> fun g <*> _Newlines fun h <*> (traverse._5._Newlines) fun i
      ClassDef idnt a b c d e f g ->
        ClassDef idnt a b (coerce c) (coerce d) e <$> fun f <*> _Newlines fun g

instance HasNewlines Statement where
  _Newlines f (CompoundStatement c) =
    CompoundStatement <$> _Newlines f c
  _Newlines f (SmallStatements idnts s ss sc nl) =
    SmallStatements idnts s ss sc <$> traverse f nl

instance HasNewlines Module where
  _Newlines = _Wrapped.traverse.failing (_Left._3.traverse) (_Right._Newlines)

assignTargets :: Traversal (Expr v a) (Expr '[] a) (Ident v a) (Ident '[] a)
assignTargets f e =
  case e of
    List a b c d -> (\c' -> List a b c' d) <$> (traverse.traverse.assignTargets) f c
    Parens a b c d -> (\c' -> Parens a b c' d) <$> assignTargets f c
    Ident a b -> Ident a <$> f b
    Tuple a b c d ->
      (\b' d' -> Tuple a b' c d') <$>
      assignTargets f b <*>
      (traverse.traverse.assignTargets) f d
    ListComp{} -> pure $ coerce e
    Deref{} -> pure $ coerce e
    Subscript{} -> pure $ coerce e
    Call{} -> pure $ coerce e
    None{} -> pure $ coerce e
    BinOp{} -> pure $ coerce e
    Negate{} -> pure $ coerce e
    Int{} -> pure $ coerce e
    Bool{} -> pure $ coerce e
    String{} -> pure $ coerce e
    Not{} -> pure $ coerce e
    Dict{} -> pure $ coerce e
    Set{} -> pure $ coerce e
