{-# language DataKinds, PolyKinds, LambdaCase, ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language DefaultSignatures, FlexibleContexts #-}
module Language.Python.Internal.Optics where

import Control.Lens.Fold (Fold)
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (Traversal, Traversal', traverseOf, failing)
import Control.Lens.Tuple (_3, _4)
import Control.Lens.Prism (Prism, _Right, _Left, prism)
import Control.Lens.Wrapped (_Wrapped)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List.NonEmpty

import Language.Python.Internal.Optics.Validated (unvalidated)
import Language.Python.Internal.Syntax

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
        KeywordParam a b d e -> Right (MkKeywordParam a b d e)
        a -> Left $ a ^. unvalidated)

_Fundef
  :: Prism
       (Statement v a)
       (Statement '[] a)
       ( a
       , [Decorator v a]
       , Indents a
       , NonEmpty Whitespace, Ident v a
       , [Whitespace], CommaSep (Param v a)
       , [Whitespace], Suite v a
       )
       ( a
       , [Decorator '[] a]
       , Indents a
       , NonEmpty Whitespace, Ident '[] a
       , [Whitespace], CommaSep (Param '[] a)
       , [Whitespace], Suite '[] a
       )
_Fundef =
  prism
    (\(idnt, a, b, c, d, e, f, g, h) -> CompoundStatement (Fundef idnt a b c d e f g h))
    (\case
        CompoundStatement (Fundef idnt a b c d e f g h) ->
          Right (idnt, a, b, c, d, e, f, g, h)
        a -> Left $ a ^. unvalidated)

_Call
  :: Prism
       (Expr v a)
       (Expr '[] a)
       (a, Expr v a, [Whitespace], Maybe (CommaSep1' (Arg v a)), [Whitespace])
       (a, Expr '[] a, [Whitespace], Maybe (CommaSep1' (Arg '[] a)), [Whitespace])
_Call =
  prism
    (\(a, b, c, d, e) -> Call a b c d e)
    (\case
        Call a b c d e -> Right (a, b, c, d, e)
        a -> Left $ a ^. unvalidated)

_Ident :: Prism (Expr v a) (Expr '[] a) (a, Ident v a) (a, Ident '[] a)
_Ident =
  prism
    (\(a, b) -> Ident a b)
    (\case
        Ident a b -> Right (a, b)
        a -> Left $ a ^. unvalidated)

_Indent :: HasIndents s => Traversal' (s '[] a) [Whitespace]
_Indent = _Indents.indentsValue.traverse.indentWhitespaces

noIndents :: HasIndents s => Fold (s '[] a) (s '[] a)
noIndents f s = f $ s & _Indents.indentsValue .~ []

class HasIndents s where
  _Indents :: Traversal' (s '[] a) (Indents a)

instance HasIndents Statement where
  _Indents f (SmallStatements idnt a b c e) =
    (\idnt' -> SmallStatements idnt' a b c e) <$> f idnt
  _Indents f (CompoundStatement c) = CompoundStatement <$> _Indents f c

instance HasIndents Block where
  _Indents = _Statements._Indents

instance HasIndents Suite where
  _Indents f (SuiteOne a b c d) = pure $ SuiteOne a b c d
  _Indents f (SuiteMany a b c e) = SuiteMany a b c <$> _Indents f e

instance HasIndents Decorator where
  _Indents fun (Decorator a b c d e) =
    (\b' -> Decorator a b' c d e) <$>
    fun b

instance HasIndents CompoundStatement where
  _Indents fun s =
    case s of
      Fundef a decos idnt b c d e f g ->
        (\decos' idnt' -> Fundef a decos' idnt' b c d e f) <$>
        traverse (_Indents fun) decos <*>
        fun idnt <*>
        _Indents fun g
      If idnt a b c d elifs e ->
        (\idnt' -> If idnt' a b c) <$>
        fun idnt <*>
        _Indents fun d <*>
        traverse
          (\(idnt, a, b, c) ->
             (\idnt'  -> (,,,) idnt' a b) <$>
             fun idnt <*>
             _Indents fun c)
          elifs <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          e
      While idnt a b c d ->
        (\idnt' -> While idnt' a b c) <$>
        fun idnt <*>
        _Indents fun d
      TryExcept idnt a b c d e f ->
        (\idnt' -> TryExcept idnt' a b) <$>
        fun idnt <*>
        _Indents fun c <*>
        traverse
          (\(idnt, a, b, c) ->
             (\idnt' -> (,,,) idnt' a b) <$>
             fun idnt <*>
             _Indents fun c)
          d <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          e <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          f
      TryFinally idnt a b c idnt2 d e ->
        (\idnt' c' idnt2' -> TryFinally idnt' a b c' idnt2' d) <$>
        fun idnt <*>
        _Indents fun c <*>
        fun idnt2 <*>
        _Indents fun e
      For idnt a b c d e f g ->
        (\idnt' -> For idnt' a b c d e) <$>
        fun idnt <*>
        _Indents fun f <*>
        traverse
          (\(idnt, a, b) ->
             (\idnt' -> (,,) idnt' a) <$>
             fun idnt <*>
             _Indents fun b)
          g
      ClassDef a decos idnt b c d e ->
        (\decos' idnt' -> ClassDef a decos' idnt' b c d) <$>
        traverse (_Indents fun) decos <*>
        fun idnt <*>
        _Indents fun e
      With a b c d e ->
        (\a' -> With a' b c d) <$>
        fun a <*>
        _Indents fun e

class HasNewlines s where
  _Newlines :: Traversal' (s v a) Newline

instance HasNewlines Block where
  _Newlines f (Block b) =
    Block <$>
    (traverse._Right._Newlines) f b

instance HasNewlines Suite where
  _Newlines f (SuiteOne a b d e) = pure $ SuiteOne a b d e
  _Newlines f (SuiteMany a b d e) = SuiteMany a b <$> f d <*> _Newlines f e

instance HasNewlines Decorator where
  _Newlines fun (Decorator a b c d e) =
    Decorator a b c d <$> fun e

instance HasNewlines CompoundStatement where
  _Newlines fun s =
    case s of
      Fundef ann decos idnt ws1 name ws2 params ws3 s ->
        (\decos' -> Fundef ann decos' idnt ws1 name ws2 params ws3) <$>
        traverse (_Newlines fun) decos <*>
        _Newlines fun s
      If idnt ann ws1 cond s elifs els ->
        If idnt ann ws1 cond <$>
        _Newlines fun s <*>
        traverseOf (traverse._4._Newlines) fun elifs <*>
        traverseOf (traverse._3._Newlines) fun els
      While idnt ann ws1 cond s ->
        While idnt ann ws1 cond <$> _Newlines fun s
      TryExcept idnt a b c f k l ->
        TryExcept idnt a b <$> _Newlines fun c <*>
        traverseOf (traverse._4._Newlines) fun f <*>
        traverseOf (traverse._3._Newlines) fun k <*>
        traverseOf (traverse._3._Newlines) fun l
      TryFinally idnt a b c idnt2 f g ->
        TryFinally idnt a b <$> _Newlines fun c <*> pure idnt2 <*>
        pure f <*> _Newlines fun g
      For idnt a b c d e f g ->
        For idnt a b c d e <$> _Newlines fun f <*> (traverse._3._Newlines) fun g
      ClassDef a decos idnt b c d e ->
        (\decos' -> ClassDef a decos' idnt b (coerce c) (coerce d)) <$>
        traverse (_Newlines fun) decos <*>
        _Newlines fun e
      With a b c d e -> With a b c (coerce d) <$> _Newlines fun e

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
    List a b c d -> (\c' -> List a b c' d) <$> (traverse.traverse._Exprs.assignTargets) f c
    Parens a b c d -> (\c' -> Parens a b c' d) <$> assignTargets f c
    Ident a b -> Ident a <$> f b
    Tuple a b c d ->
      (\b' d' -> Tuple a b' c d') <$>
      (_Exprs.assignTargets) f b <*>
      (traverse.traverse._Exprs.assignTargets) f d
    Unit{} -> pure $ e ^. unvalidated
    Lambda{} -> pure $ e ^. unvalidated
    Yield{} -> pure $ e ^. unvalidated
    YieldFrom{} -> pure $ e ^. unvalidated
    Ternary{} -> pure $ e ^. unvalidated
    ListComp{} -> pure $ e ^. unvalidated
    Deref{} -> pure $ e ^. unvalidated
    Subscript{} -> pure $ e ^. unvalidated
    Call{} -> pure $ e ^. unvalidated
    None{} -> pure $ e ^. unvalidated
    Ellipsis{} -> pure $ e ^. unvalidated
    BinOp{} -> pure $ e ^. unvalidated
    UnOp{} -> pure $ e ^. unvalidated
    Int{} -> pure $ e ^. unvalidated
    Float{} -> pure $ e ^. unvalidated
    Imag{} -> pure $ e ^. unvalidated
    Bool{} -> pure $ e ^. unvalidated
    String{} -> pure $ e ^. unvalidated
    Not{} -> pure $ e ^. unvalidated
    DictComp{} -> pure $ e ^. unvalidated
    Dict{} -> pure $ e ^. unvalidated
    Set{} -> pure $ e ^. unvalidated
    Generator{} -> pure $ e ^. unvalidated
