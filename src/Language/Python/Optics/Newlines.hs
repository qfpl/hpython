{-|
Module      : Language.Python.Optics.Newlines
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}
module Language.Python.Optics.Newlines where

import Control.Lens.Traversal (Traversal')
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.FingerTree as FingerTree

import Language.Python.Syntax

{-

I can't derive this with generic-lens :( it wants generic instances for certain
things that can't have those instance (and don't contain newlines anyways)

-}
class HasNewlines s where
  -- | 'Traversal'' targeting all of thie 'Newline's in a structure
  --
  -- This only targets places that contain the 'Newline' datatype; it doesn't target
  -- newline characters in string literals, for example.
  _Newlines :: Traversal' s Newline

instance (HasNewlines a, HasNewlines b) => HasNewlines (a, b) where
  _Newlines f (a, b) = (,) <$> _Newlines f a <*> _Newlines f b

instance (HasNewlines a, HasNewlines b, HasNewlines c) => HasNewlines (a, b, c) where
  _Newlines f (a, b, c) =
    (,,) <$>
    _Newlines f a <*>
    _Newlines f b <*>
    _Newlines f c

instance (HasNewlines a, HasNewlines b, HasNewlines c, HasNewlines d) => HasNewlines (a, b, c, d) where
  _Newlines f (a, b, c, d) =
    (,,,) <$>
    _Newlines f a <*>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d

instance HasNewlines (e a) => HasNewlines (ImportAs e v a) where
  _Newlines f (ImportAs a b c) =
    ImportAs a <$>
    _Newlines f b <*>
    _Newlines f c

instance HasNewlines (RelativeModuleName v a) where
  _Newlines f (RelativeWithName a b) =
    RelativeWithName <$>
    _Newlines f a <*>
    _Newlines f b
  _Newlines f (Relative a) = Relative <$> _Newlines f a

instance (HasNewlines a, HasNewlines b) => HasNewlines (Either a b) where
  _Newlines f (Left a) = Left <$> _Newlines f a
  _Newlines f (Right a) = Right <$> _Newlines f a

instance HasNewlines Newline where
  _Newlines = id

instance HasNewlines a => HasNewlines [a] where
  _Newlines = traverse._Newlines

instance HasNewlines Whitespace where
  _Newlines _ Space = pure Space
  _Newlines _ Tab = pure Tab
  _Newlines f (Continued nl ws) = Continued <$> f nl <*> _Newlines f ws
  _Newlines _ (Comment c) = pure $ Comment c
  _Newlines f (Newline nl) = Newline <$> f nl

instance HasNewlines (Blank a) where
  _Newlines f (Blank a b c) = (\b' -> Blank a b' c) <$> _Newlines f b

instance HasNewlines (Block v a) where
  _Newlines f (Block a b c) =
    Block <$>
    _Newlines f a <*>
    _Newlines f b <*>
    _Newlines f c

instance HasNewlines Colon where
  _Newlines f (MkColon a) = MkColon <$> _Newlines f a

instance HasNewlines Dot where
  _Newlines f (MkDot a) = MkDot <$> _Newlines f a

instance HasNewlines Comma where
  _Newlines f (MkComma a) = MkComma <$> _Newlines f a

instance HasNewlines At where
  _Newlines f (MkAt a) = MkAt <$> _Newlines f a

instance HasNewlines (Semicolon a) where
  _Newlines f (MkSemicolon a b) = MkSemicolon a <$> _Newlines f b

instance HasNewlines Equals where
  _Newlines f (MkEquals a) = MkEquals <$> _Newlines f a

instance HasNewlines (Suite v a) where
  _Newlines f (SuiteOne a b c) = SuiteOne a b <$> _Newlines f c
  _Newlines f (SuiteMany a b c d e) =
    (\b' d' e' -> SuiteMany a b' c d' e') <$>
    _Newlines f b <*>
    f d <*>
    _Newlines f e

instance HasNewlines Indent where
  _Newlines f (MkIndent a) = MkIndent <$> (FingerTree.traverse'._Newlines) f a

instance HasNewlines (Indents a) where
  _Newlines f (Indents a b) = (\a' -> Indents a' b) <$> _Newlines f a

instance HasNewlines (UnOp a) where
  _Newlines f (Negate a b) = Negate a <$> _Newlines f b
  _Newlines f (Positive a b) = Positive a <$> _Newlines f b
  _Newlines f (Complement a b) = Complement a <$> _Newlines f b

instance HasNewlines (BinOp a) where
  _Newlines f x =
    case x of
      Is a b -> Is a <$> _Newlines f b
      IsNot a b c -> IsNot a <$> _Newlines f b <*> _Newlines f c
      In a b -> In a <$> _Newlines f b
      NotIn a b c -> NotIn a <$> _Newlines f b <*> _Newlines f c
      Minus a b -> Minus a <$> _Newlines f b
      Exp a b -> Exp a <$> _Newlines f b
      BoolAnd a b -> BoolAnd a <$> _Newlines f b
      BoolOr a b -> BoolOr a <$> _Newlines f b
      Eq a b -> Eq a <$> _Newlines f b
      Lt a b -> Lt a <$> _Newlines f b
      LtEq a b -> LtEq a <$> _Newlines f b
      Gt a b -> Gt a <$> _Newlines f b
      GtEq a b -> GtEq a <$> _Newlines f b
      NotEq a b -> NotEq a <$> _Newlines f b
      Multiply a b -> Multiply a <$> _Newlines f b
      Divide a b -> Divide a <$> _Newlines f b
      FloorDivide a b -> FloorDivide a <$> _Newlines f b
      Percent a b -> Percent a <$> _Newlines f b
      Plus a b -> Plus a <$> _Newlines f b
      BitOr a b -> BitOr a <$> _Newlines f b
      BitXor a b -> BitXor a <$> _Newlines f b
      BitAnd a b -> BitAnd a <$> _Newlines f b
      ShiftLeft a b -> ShiftLeft a <$> _Newlines f b
      ShiftRight a b -> ShiftRight a <$> _Newlines f b
      At a b -> At a <$> _Newlines f b

instance HasNewlines a => HasNewlines (CommaSep a) where
  _Newlines f = go
    where
      go CommaSepNone = pure CommaSepNone
      go (CommaSepOne a) = CommaSepOne <$> _Newlines f a
      go (CommaSepMany a b c) =
        CommaSepMany <$>
        _Newlines f a <*>
        _Newlines f b <*>
        go c

instance HasNewlines a => HasNewlines (CommaSep1 a) where
  _Newlines f = go
    where
      go (CommaSepOne1 a) = CommaSepOne1 <$> _Newlines f a
      go (CommaSepMany1 a b c) =
        CommaSepMany1 <$>
        _Newlines f a <*>
        _Newlines f b <*>
        go c

instance HasNewlines a => HasNewlines (CommaSep1' a) where
  _Newlines f = go
    where
      go (CommaSepOne1' a b) = CommaSepOne1' <$> _Newlines f a <*> _Newlines f b
      go (CommaSepMany1' a b c) =
        CommaSepMany1' <$>
        _Newlines f a <*>
        _Newlines f b <*>
        go c

instance HasNewlines (Ident v a) where
  _Newlines f (MkIdent a b c) = MkIdent a b <$> _Newlines f c

instance HasNewlines a => HasNewlines (Maybe a) where
  _Newlines = traverse._Newlines

instance HasNewlines (Param v a) where
  _Newlines f (PositionalParam a b c) =
    PositionalParam a <$>
    _Newlines f b <*>
    _Newlines f c
  _Newlines f (KeywordParam a b c d e) =
    KeywordParam a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d <*>
    _Newlines f e
  _Newlines f (StarParam a b c d) =
    StarParam a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d
  _Newlines f (UnnamedStarParam a b) =
    UnnamedStarParam a <$>
    _Newlines f b
  _Newlines f (DoubleStarParam a b c d) =
    DoubleStarParam a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d

instance HasNewlines (Arg v a) where
  _Newlines f (PositionalArg a b) =
    PositionalArg a <$>
    _Newlines f b
  _Newlines f (KeywordArg a b c d) =
    KeywordArg a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d
  _Newlines f (StarArg a b c) =
    StarArg a <$>
    _Newlines f b <*>
    _Newlines f c
  _Newlines f (DoubleStarArg a b c) =
    DoubleStarArg a <$>
    _Newlines f b <*>
    _Newlines f c

instance HasNewlines (CompFor v a) where
  _Newlines f (CompFor a b c d e) =
    CompFor a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d <*>
    _Newlines f e

instance HasNewlines (CompIf v a) where
  _Newlines f (CompIf a b c) =
    CompIf a <$>
    _Newlines f b <*>
    _Newlines f c

instance HasNewlines (e v a) => HasNewlines (Comprehension e v a) where
  _Newlines f (Comprehension a b c d) =
    Comprehension a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d

instance HasNewlines (TupleItem v a) where
  _Newlines f (TupleItem a b) = TupleItem a <$> _Newlines f b
  _Newlines f (TupleUnpack a b c d) =
    TupleUnpack a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d

instance HasNewlines (ListItem v a) where
  _Newlines f (ListItem a b) = ListItem a <$> _Newlines f b
  _Newlines f (ListUnpack a b c d) =
    ListUnpack a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d

instance HasNewlines (SetItem v a) where
  _Newlines f (SetItem a b) = SetItem a <$> _Newlines f b
  _Newlines f (SetUnpack a b c d) =
    SetUnpack a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d

instance HasNewlines (DictItem v a) where
  _Newlines f (DictItem a b c d) =
    DictItem a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d
  _Newlines f (DictUnpack a b c) =
    DictUnpack a <$>
    _Newlines f b <*>
    _Newlines f c

instance HasNewlines (Subscript v a) where
  _Newlines f (SubscriptExpr a) = SubscriptExpr <$> _Newlines f a
  _Newlines f (SubscriptSlice a b c d) =
    SubscriptSlice <$>
    _Newlines f a <*>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d

instance HasNewlines a => HasNewlines (NonEmpty a) where
  _Newlines f (a :| as) = (:|) <$> _Newlines f a <*> _Newlines f as

instance HasNewlines (StringLiteral a) where
  _Newlines = stringLiteralWhitespace.traverse._Newlines

instance HasNewlines (Expr v a) where
  _Newlines fun = go
    where
      go e =
        case e of
          Unit a b c -> Unit a <$> _Newlines fun b <*> _Newlines fun c
          Lambda a b c d e ->
            Lambda a <$>
            _Newlines fun b <*>
            _Newlines fun c <*>
            _Newlines fun d <*>
            go e
          Yield a b c ->
            Yield a <$> _Newlines fun b <*> _Newlines fun c
          YieldFrom a b c d ->
            YieldFrom a <$> _Newlines fun b <*> _Newlines fun c <*> go d
          Ternary a b c d e f ->
            Ternary a <$>
            go b <*>
            _Newlines fun c <*>
            go d <*>
            _Newlines fun e <*>
            go f
          ListComp a b c d ->
            ListComp a <$>
            _Newlines fun b <*>
            _Newlines fun c <*>
            _Newlines fun d
          List a b c d ->
            List a <$>
            _Newlines fun b <*>
            _Newlines fun c <*>
            _Newlines fun d
          DictComp a b c d ->
            DictComp a <$>
            _Newlines fun b <*>
            _Newlines fun c <*>
            _Newlines fun d
          Dict a b c d ->
            Dict a <$>
            _Newlines fun b <*>
            _Newlines fun c <*>
            _Newlines fun d
          SetComp a b c d ->
            SetComp a <$>
            _Newlines fun b <*>
            _Newlines fun c <*>
            _Newlines fun d
          Set a b c d ->
            Set a <$>
            _Newlines fun b <*>
            _Newlines fun c <*>
            _Newlines fun d
          Deref a b c d ->
            Deref a <$>
            go b <*>
            _Newlines fun c <*>
            _Newlines fun d
          Subscript a b c d e ->
            Subscript a <$>
            go b <*>
            _Newlines fun c <*>
            _Newlines fun d <*>
            _Newlines fun e
          Call a b c d e ->
            Call a <$>
            go b <*>
            _Newlines fun c <*>
            _Newlines fun d <*>
            _Newlines fun e
          None a b -> None a <$> _Newlines fun b
          Ellipsis a b -> Ellipsis a <$> _Newlines fun b
          BinOp a b c d ->
            BinOp a <$>
            go b <*>
            _Newlines fun c <*>
            go d
          UnOp a b c ->
            UnOp a <$>
            _Newlines fun b <*>
            go c
          Parens a b c d ->
            Parens a <$>
            _Newlines fun b <*>
            go c <*>
            _Newlines fun d
          Ident a -> Ident <$> _Newlines fun a
          Int a b c -> Int a b <$> _Newlines fun c
          Float a b c -> Float a b <$> _Newlines fun c
          Imag a b c -> Imag a b <$> _Newlines fun c
          Bool a b c -> Bool a b <$> _Newlines fun c
          String a b -> String a <$> _Newlines fun b
          Tuple a b c d ->
            Tuple a <$>
            _Newlines fun b <*>
            _Newlines fun c <*>
            _Newlines fun d
          Not a b c -> Not a <$> _Newlines fun b <*> go c
          Generator a b -> Generator a <$> _Newlines fun b
          Await a b c -> Await a <$> _Newlines fun b <*> _Newlines fun c

instance HasNewlines (Decorator v a) where
  _Newlines fun (Decorator a b c d e f g) =
    Decorator a <$>
    _Newlines fun b <*>
    _Newlines fun c <*>
    _Newlines fun d <*>
    pure e <*>
    fun f <*>
    _Newlines fun g

instance HasNewlines (ExceptAs v a) where
  _Newlines f (ExceptAs a b c) = ExceptAs a <$> _Newlines f b <*> _Newlines f c

instance HasNewlines (WithItem v a) where
  _Newlines f (WithItem a b c) = WithItem a <$> _Newlines f b <*> _Newlines f c

instance HasNewlines (CompoundStatement v a) where
  _Newlines fun s =
    case s of
      Fundef ann decos idnt asyncWs ws1 name ws2 params ws3 mty s ->
        Fundef ann <$>
        _Newlines fun decos <*>
        _Newlines fun idnt <*>
        _Newlines fun asyncWs <*>
        _Newlines fun ws1 <*>
        _Newlines fun name <*>
        _Newlines fun ws2 <*>
        _Newlines fun params <*>
        _Newlines fun ws3 <*>
        _Newlines fun mty <*>
        _Newlines fun s
      If ann idnt ws1 cond s elifs els ->
        If ann <$>
        _Newlines fun idnt <*>
        _Newlines fun ws1 <*>
        _Newlines fun cond <*>
        _Newlines fun s <*>
        _Newlines fun elifs <*>
        _Newlines fun els
      While ann idnt ws1 cond s els ->
        While ann <$>
        _Newlines fun idnt <*>
        _Newlines fun ws1 <*>
        _Newlines fun cond <*>
        _Newlines fun s <*>
        _Newlines fun els
      TryExcept ann idnt b c f k l ->
        TryExcept ann <$>
        _Newlines fun idnt <*>
        _Newlines fun b <*>
        _Newlines fun c <*>
        _Newlines fun f <*>
        _Newlines fun k <*>
        _Newlines fun l
      TryFinally ann idnt b c idnt2 f g ->
        TryFinally ann <$>
        _Newlines fun idnt <*>
        _Newlines fun b <*>
        _Newlines fun c <*>
        _Newlines fun idnt2 <*>
        _Newlines fun f <*>
        _Newlines fun g
      For ann idnt asyncWs b c d e f g ->
        For ann <$>
        _Newlines fun idnt <*>
        _Newlines fun asyncWs <*>
        _Newlines fun b <*>
        _Newlines fun c <*>
        _Newlines fun d <*>
        _Newlines fun e <*>
        _Newlines fun f <*>
        _Newlines fun g
      ClassDef a decos idnt b c d e ->
        ClassDef a <$>
        _Newlines fun decos <*>
        _Newlines fun idnt <*>
        _Newlines fun b <*>
        _Newlines fun c <*>
        _Newlines fun d <*>
        _Newlines fun e
      With a b asyncWs c d e ->
        With a <$>
        _Newlines fun b <*>
        _Newlines fun asyncWs <*>
        _Newlines fun c <*>
        _Newlines fun d <*>
        _Newlines fun e

instance HasNewlines (ModuleName v a) where
  _Newlines f = go
    where
      go (ModuleNameOne a b) =
        ModuleNameOne a <$> _Newlines f b
      go (ModuleNameMany a b c d) =
        ModuleNameMany a <$> _Newlines f b <*> _Newlines f c <*> go d

instance HasNewlines (ImportTargets v a) where
  _Newlines f (ImportAll a b) =
    ImportAll a <$> _Newlines f b
  _Newlines f (ImportSome a b) =
    ImportSome a <$> _Newlines f b
  _Newlines f (ImportSomeParens a b c d) =
    ImportSomeParens a <$>
    _Newlines f b <*>
    _Newlines f c <*>
    _Newlines f d

instance HasNewlines (SimpleStatement v a) where
  _Newlines fun s =
    case s of
      Return a b c -> Return a <$> _Newlines fun b <*> _Newlines fun c
      Expr a b -> Expr a <$> _Newlines fun b
      Assign a b c -> Assign a <$> _Newlines fun b <*> _Newlines fun c
      AugAssign a b c d ->
        AugAssign a <$>
        _Newlines fun b <*>
        pure c <*>
        _Newlines fun d
      Pass a b -> Pass a <$> _Newlines fun b
      Break a b -> Break a <$> _Newlines fun b
      Continue a b -> Continue a <$> _Newlines fun b
      Global a b c -> Global a <$> _Newlines fun b <*> _Newlines fun c
      Nonlocal a b c -> Nonlocal a <$> _Newlines fun b <*> _Newlines fun c
      Del a b c -> Del a <$> _Newlines fun b <*> _Newlines fun c
      Import a b c ->
        Import a <$>
        _Newlines fun b <*>
        _Newlines fun c
      From a b c d e ->
        From a <$>
        _Newlines fun b <*>
        _Newlines fun c <*>
        _Newlines fun d <*>
        _Newlines fun e
      Raise a b c ->
        Raise a <$>
        _Newlines fun b <*>
        _Newlines fun c
      Assert a b c d ->
        Assert a <$>
        _Newlines fun b <*>
        _Newlines fun c <*>
        _Newlines fun d

instance HasNewlines (SmallStatement v a) where
  _Newlines f (MkSmallStatement s ss sc cmt nl) =
    MkSmallStatement <$>
    _Newlines f s <*>
    _Newlines f ss <*>
    _Newlines f sc <*>
    pure cmt <*>
    _Newlines f nl

instance HasNewlines (Statement v a) where
  _Newlines f (CompoundStatement c) =
    CompoundStatement <$> _Newlines f c
  _Newlines f (SmallStatement i a) =
    SmallStatement <$>
    _Newlines f i <*>
    _Newlines f a

instance HasNewlines (Module v a) where
  _Newlines f = go
    where
      go ModuleEmpty = pure ModuleEmpty
      go (ModuleBlankFinal a) = pure $ ModuleBlankFinal a
      go (ModuleBlank a b c) =
        ModuleBlank a <$> f b <*> go c
      go (ModuleStatement a b) =
        ModuleStatement <$> _Newlines f a <*> go b
