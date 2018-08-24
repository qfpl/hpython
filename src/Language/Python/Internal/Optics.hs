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

import Language.Python.Internal.Optics.Validated (unvalidated)
import Language.Python.Internal.Syntax hiding (Fundef)
import Language.Python.Syntax.Types
import qualified Language.Python.Internal.Syntax as AST (CompoundStatement(Fundef))

data KeywordParam v a
  = MkKeywordParam
  { _kpAnn :: a
  , _kpName :: Ident v a
  , _kpType :: Maybe ([Whitespace], Expr v a)
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
        KeywordParam a b c d e -> Right (MkKeywordParam a b c d e)
        a -> Left $ a ^. unvalidated)

_Fundef
  :: Prism
       (Statement v a)
       (Statement '[] a)
       (Fundef v a)
       (Fundef '[] a)
_Fundef =
  prism
    (\(Fundef idnt a b c d e f g h i j) ->
       CompoundStatement (AST.Fundef idnt a b c d e f g h i j))
    (\case
        CompoundStatement (AST.Fundef idnt a b c d e f g h i j) ->
          Right $ Fundef idnt a b c d e f g h i j
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
      AST.Fundef a decos idnt asyncWs b c d e f g h ->
        (\decos' idnt' -> AST.Fundef a decos' idnt' asyncWs b c d e f g) <$>
        traverse (_Indents fun) decos <*>
        fun idnt <*>
        _Indents fun h
      If a idnt b c d elifs e ->
        (\idnt' -> If a idnt' b c) <$>
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
      While a idnt b c d ->
        (\idnt' -> While a idnt' b c) <$>
        fun idnt <*>
        _Indents fun d
      TryExcept a idnt b c d e f ->
        (\idnt' -> TryExcept a idnt' b) <$>
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
      TryFinally a idnt b c idnt2 d e ->
        (\idnt' c' idnt2' -> TryFinally a idnt' b c' idnt2' d) <$>
        fun idnt <*>
        _Indents fun c <*>
        fun idnt2 <*>
        _Indents fun e
      For a idnt asyncWs b c d e f g ->
        (\idnt' -> For a idnt' asyncWs b c d e) <$>
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
      With a b asyncWs c d e ->
        (\b' -> With a b' asyncWs c d) <$>
        fun b <*>
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
      AST.Fundef ann decos idnt asyncWs ws1 name ws2 params ws3 mty s ->
        (\decos' -> AST.Fundef ann decos' idnt asyncWs ws1 name ws2 params ws3 mty) <$>
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
      For idnt a asyncWs b c d e f g ->
        For idnt a asyncWs b c d e <$> _Newlines fun f <*> (traverse._3._Newlines) fun g
      ClassDef a decos idnt b c d e ->
        (\decos' -> ClassDef a decos' idnt b (coerce c) (coerce d)) <$>
        traverse (_Newlines fun) decos <*>
        _Newlines fun e
      With a b asyncWs c d e -> With a b asyncWs c (coerce d) <$> _Newlines fun e

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
    SetComp{} -> pure $ e ^. unvalidated
    Set{} -> pure $ e ^. unvalidated
    Generator{} -> pure $ e ^. unvalidated
    Await{} -> pure $ e ^. unvalidated
