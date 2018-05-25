{-# language TemplateHaskell #-}
{-# language DataKinds, KindSignatures #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language TypeFamilies #-}
{-# language LambdaCase #-}
{-# language UndecidableInstances #-}
module Language.Python.Internal.Syntax.Statement where

import Control.Lens.Getter ((^.), getting)
import Control.Lens.Lens (Lens, Lens', lens)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Prism (_Just, _Right)
import Control.Lens.Setter ((.~), over, mapped)
import Control.Lens.TH (makeLenses, makeWrapped)
import Control.Lens.Traversal (Traversal, traverseOf)
import Control.Lens.Tuple (_2, _3, _4, _5)
import Control.Lens.Wrapped (_Wrapped)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.Expr
import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.ModuleNames
import Language.Python.Internal.Syntax.Token
import Language.Python.Internal.Syntax.Whitespace

-- | 'Traversal' over all the statements in a term
class HasStatements s where
  _Statements :: Traversal (s v a) (s '[] a) (Statement v a) (Statement '[] a)

data Param (v :: [*]) a
  = PositionalParam
  { _paramAnn :: a
  , _paramName :: Ident v a
  }
  | KeywordParam
  { _paramAnn :: a
  , _paramName :: Ident v a
  -- = spaces
  , _unsafeKeywordParamWhitespaceRight :: [Whitespace]
  , _unsafeKeywordParamExpr :: Expr v a
  }
  | StarParam
  { _paramAnn :: a
  -- '*' spaces
  , _unsafeStarParamWhitespace :: [Whitespace]
  , _paramName :: Ident v a
  }
  | DoubleStarParam
  { _paramAnn :: a
  -- '**' spaces
  , _unsafeDoubleStarParamWhitespace :: [Whitespace]
  , _paramName :: Ident v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

paramAnn :: Lens' (Param v a) a
paramAnn = lens _paramAnn (\s a -> s { _paramAnn = a})

paramName :: Lens (Param v a) (Param '[] a) (Ident v a) (Ident v a)
paramName = lens _paramName (\s a -> coerce $ s { _paramName = a})

instance HasExprs Param where
  _Exprs f (KeywordParam a name ws2 expr) =
    KeywordParam a (coerce name) <$> pure ws2 <*> f expr
  _Exprs _ p@PositionalParam{} = pure $ coerce p
  _Exprs _ p@StarParam{} = pure $ coerce p
  _Exprs _ p@DoubleStarParam{} = pure $ coerce p

newtype Block v a
  = Block
  { unBlock
    :: NonEmpty
         ( a
         , [Whitespace]
         , Either
             (Maybe Comment, Newline)
             (Statement v a)
         )
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

class HasBlocks s where
  _Blocks :: Traversal (s v a) (s '[] a) (Block v a) (Block '[] a)

instance HasBlocks CompoundStatement where
  _Blocks f (Fundef a ws1 name ws2 params ws3 ws4 nl b) =
    Fundef a ws1 (coerce name) ws2 (coerce params) ws3 ws4 nl <$> coerce (f b)
  _Blocks f (If a ws1 e1 ws3 nl b b') =
    If a ws1 (coerce e1) ws3 nl <$>
    coerce (f b) <*>
    traverseOf (traverse._4) (coerce . f) b'
  _Blocks f (While a ws1 e1 ws3 nl b) =
    While a ws1 (coerce e1) ws3 nl <$> coerce (f b)
  _Blocks fun (TryExcept a b c d e f g h) =
    TryExcept a (coerce b) (coerce c) (coerce d) <$>
    fun e <*>
    -- (coerce f) downcasts the ExceptAs
    (traverse._5) fun (coerce f) <*>
    (traverse._4) fun g <*>
    (traverse._4) fun h
  _Blocks fun (TryFinally a b c d e f g h i) =
    TryFinally a (coerce b) (coerce c) (coerce d) <$> fun e <*>
    pure (coerce f) <*> pure (coerce g) <*> pure (coerce h) <*> fun i
  _Blocks fun (For a b c d e f g h i) =
    For a b (coerce c) d (coerce e) f g <$>
    fun h <*>
    (traverse._4) fun i
  _Blocks fun (ClassDef a b c d e f g) =
    ClassDef a b (coerce c) (coerce d) e f <$> fun g

instance HasStatements Block where
  _Statements = _Wrapped.traverse._3._Right

data Statement (v :: [*]) a
  = SmallStatements
      (SmallStatement v a)
      [([Whitespace], SmallStatement v a)]
      (Maybe [Whitespace])
      Newline
  | CompoundStatement (CompoundStatement v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasBlocks Statement where
  _Blocks f (CompoundStatement c) = CompoundStatement <$> _Blocks f c
  _Blocks _ (SmallStatements a b c d) =
    pure $ SmallStatements (coerce a) (over (mapped._2) coerce b) c d

instance Plated (Statement '[] a) where
  plate _ s@SmallStatements{} = pure s
  plate fun (CompoundStatement s) =
    CompoundStatement <$>
    case s of
      Fundef a ws1 b ws2 c ws3 ws4 nl sts ->
        Fundef a ws1 b ws2 c ws3 ws4 nl <$> (_Wrapped.traverse._3._Right) fun sts
      If a ws1 b ws3 nl sts sts' ->
        If a ws1 b ws3 nl <$>
        (_Wrapped.traverse._3._Right) fun sts <*>
        (traverse._4._Wrapped.traverse._3._Right) fun sts'
      While a ws1 b ws3 nl sts ->
        While a ws1 b ws3 nl <$> (_Wrapped.traverse._3._Right) fun sts
      TryExcept a b c d e f g h ->
        TryExcept a b c d <$> (_Wrapped.traverse._3._Right) fun e <*>
        (traverse._5._Wrapped.traverse._3._Right) fun f <*>
        (traverse._4._Wrapped.traverse._3._Right) fun g <*>
        (traverse._4._Wrapped.traverse._3._Right) fun h
      TryFinally a b c d e f g h i ->
        TryFinally a b c d <$> (_Wrapped.traverse._3._Right) fun e <*>
        pure f <*> pure g <*> pure h <*> (_Wrapped.traverse._3._Right) fun i
      For a b c d e f g h i ->
        For a b c d e f g <$>
        (_Wrapped.traverse._3._Right) fun h <*>
        (traverse._4._Wrapped.traverse._3._Right) fun i
      ClassDef a b c d e f g ->
        ClassDef a b c d e f <$> (_Wrapped.traverse._3._Right) fun g

instance HasExprs Statement where
  _Exprs f (SmallStatements s ss a b) =
    SmallStatements <$>
    _Exprs f s <*>
    (traverse._2._Exprs) f ss <*>
    pure a <*>
    pure b
  _Exprs f (CompoundStatement c) = CompoundStatement <$> _Exprs f c

data ImportAs e v a
  = ImportAs a (e a) (Maybe (NonEmpty Whitespace, Ident v a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Token (e a) (e' a) => Token (ImportAs e v a) (ImportAs e' '[] a) where
  unvalidate (ImportAs x a b) = ImportAs x (unvalidate a) (over (mapped._2) unvalidate b)

  whitespaceAfter =
    lens
      (\(ImportAs _ a b) ->
         maybe (a ^. getting whitespaceAfter) (^. _2.getting whitespaceAfter) b)
      (\(ImportAs x a b) ws ->
         ImportAs
           x
           (maybe (a & whitespaceAfter .~ ws) (const $ unvalidate a) b)
           (b & _Just._2.whitespaceAfter .~ ws))

  startChar (ImportAs _ a _) = startChar a

  endChar (ImportAs _ a Nothing) = endChar a
  endChar (ImportAs _ _ (Just (_, b))) = endChar b

data ImportTargets v a
  = ImportAll a [Whitespace]
  | ImportSome a (CommaSep1 (ImportAs (Ident v) v a))
  | ImportSomeParens
      a
      -- ( spaces
      [Whitespace]
      -- imports as
      (CommaSep1' (ImportAs (Ident v) v a))
      -- ) spaces
      [Whitespace]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Token (ImportTargets v a) (ImportTargets '[] a) where
  unvalidate (ImportAll a b) = ImportAll a b
  unvalidate (ImportSome a cs) = ImportSome a $ unvalidate <$> cs
  unvalidate (ImportSomeParens x a b c) = ImportSomeParens x a (unvalidate <$> b) c

  whitespaceAfter =
    lens
      (\case
          ImportAll _ ws -> ws
          ImportSome _ cs -> cs ^. getting whitespaceAfter
          ImportSomeParens _ _ _ ws -> ws)
      (\ts ws ->
         case ts of
           ImportAll a _ -> ImportAll a ws
           ImportSome a cs -> ImportSome a (cs & whitespaceAfter .~ ws)
           ImportSomeParens x a b _ -> ImportSomeParens x a (unvalidate b) ws)

  startChar ImportAll{} = '*'
  startChar (ImportSome _ ts) = startChar ts
  startChar ImportSomeParens{} = '('

  endChar ImportAll{} = '*'
  endChar (ImportSome _ ts) = endChar ts
  endChar ImportSomeParens{} = ')'

data SmallStatement (v :: [*]) a
  = Return a [Whitespace] (Expr v a)
  | Expr a (Expr v a)
  | Assign a (Expr v a) [Whitespace] [Whitespace] (Expr v a)
  | Pass a
  | Break a
  | Continue a
  | Global a (NonEmpty Whitespace) (CommaSep1 (Ident v a))
  | Nonlocal a (NonEmpty Whitespace) (CommaSep1 (Ident v a))
  | Del a (NonEmpty Whitespace) (CommaSep1 (Ident v a))
  | Import
      a
      (NonEmpty Whitespace)
      (CommaSep1 (ImportAs (ModuleName v) v a))
  | From
      a
      [Whitespace]
      (RelativeModuleName v a)
      [Whitespace]
      (ImportTargets v a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance Plated (SmallStatement '[] a) where; plate = gplate

instance HasExprs SmallStatement where
  _Exprs f (Return a ws e) = Return a ws <$> f e
  _Exprs f (Expr a e) = Expr a <$> f e
  _Exprs f (Assign a e1 ws1 ws2 e2) = Assign a <$> f e1 <*> pure ws1 <*> pure ws2 <*> f e2
  _Exprs _ p@Pass{} = pure $ coerce p
  _Exprs _ p@Break{} = pure $ coerce p
  _Exprs _ p@Continue{} = pure $ coerce p
  _Exprs _ p@Global{} = pure $ coerce p
  _Exprs _ p@Nonlocal{} = pure $ coerce p
  _Exprs _ p@Del{} = pure $ coerce p
  _Exprs _ p@Import{} = pure $ coerce p
  _Exprs _ p@From{} = pure $ coerce p

data ExceptAs v a
  = ExceptAs
  { _exceptAsAnn :: a
  , _exceptAsExpr :: Expr v a
  , _exceptAsName :: Maybe ([Whitespace], Ident v a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CompoundStatement (v :: [*]) a
  -- ^ 'def' <spaces> <ident> '(' <spaces> stuff ')' <spaces> ':' <spaces> <newline>
  --   <block>
  = Fundef a
      (NonEmpty Whitespace) (Ident v a)
      [Whitespace] (CommaSep (Param v a))
      [Whitespace] [Whitespace] Newline
      (Block v a)
  -- ^ 'if' <spaces> <expr> ':' <spaces> <newline>
  --   <block>
  --   [ 'else' <spaces> ':' <spaces> <newline>
  --     <block>
  --   ]
  | If a
      [Whitespace] (Expr v a) [Whitespace] Newline
      (Block v a)
      (Maybe ([Whitespace], [Whitespace], Newline, Block v a))
  -- ^ 'if' <spaces> <expr> ':' <spaces> <newline>
  --   <block>
  | While a
      [Whitespace] (Expr v a) [Whitespace] Newline
      (Block v a)
  -- ^ 'try' <spaces> ':' <spaces> <newline> <block>
  --   ( 'except' <spaces> exceptAs ':' <spaces> <newline> <block> )+
  --   [ 'else' <spaces> ':' <spaces> <newline> <block> ]
  --   [ 'finally' <spaces> ':' <spaces> <newline> <block> ]
  | TryExcept a
      [Whitespace] [Whitespace] Newline (Block v a)
      (NonEmpty ([Whitespace], ExceptAs v a, [Whitespace], Newline, Block v a))
      (Maybe ([Whitespace], [Whitespace], Newline, Block v a))
      (Maybe ([Whitespace], [Whitespace], Newline, Block v a))
  -- ^ 'try' <spaces> ':' <spaces> <newline> <block>
  --   'finally' <spaces> ':' <spaces> <newline> <block>
  | TryFinally a
      [Whitespace] [Whitespace] Newline (Block v a)
      [Whitespace] [Whitespace] Newline (Block v a)
  -- ^ 'for' <spaces> expr 'in' <spaces> expr ':' <spaces> <newline> <block>
  --   [ 'else' <spaces> ':' <spaces> <newline> <block> ]
  | For a
      [Whitespace] (Expr v a) [Whitespace] (Expr v a) [Whitespace] Newline
      (Block v a)
      (Maybe ([Whitespace], [Whitespace], Newline, Block v a))
  -- ^ 'class' <spaces> ident [ '(' <spaces> [ args ] ')' <spaces>] ':' <spaces> <newline>
  --   <block>
  | ClassDef a
      (NonEmpty Whitespace) (Ident v a)
      (Maybe ([Whitespace], Maybe (CommaSep1 (Arg v a)), [Whitespace])) [Whitespace] Newline
      (Block v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasExprs ExceptAs where
  _Exprs f (ExceptAs ann e a) = ExceptAs ann <$> f e <*> pure (coerce a)

instance HasExprs Block where
  _Exprs = _Wrapped.traverse._3._Right._Exprs

instance HasExprs CompoundStatement where
  _Exprs f (Fundef a ws1 name ws2 params ws3 ws4 nl sts) =
    Fundef a ws1 (coerce name) ws2 <$>
    (traverse._Exprs) f params <*>
    pure ws3 <*>
    pure ws4 <*>
    pure nl <*>
    (_Wrapped.traverse._3._Right._Exprs) f sts
  _Exprs f (If a ws1 e ws3 nl sts sts') =
    If a ws1 <$>
    f e <*>
    pure ws3 <*>
    pure nl <*>
    _Exprs f sts <*>
    (traverse._4._Exprs) f sts'
  _Exprs f (While a ws1 e ws3 nl sts) =
    While a ws1 <$>
    f e <*>
    pure ws3 <*>
    pure nl <*>
    _Exprs f sts
  _Exprs fun (TryExcept a b c d e f g h) =
    TryExcept a b c d <$> _Exprs fun e <*>
    -- (coerce f) downcasts the ExceptAs
    (traverse._5._Exprs) fun (coerce f) <*>
    (traverse._4._Exprs) fun g <*>
    (traverse._4._Exprs) fun h
  _Exprs fun (TryFinally a b c d e f g h i) =
    TryFinally a b c d <$> _Exprs fun e <*>
    pure f <*> pure g <*> pure h <*> _Exprs fun i
  _Exprs fun (For a b c d e f g h i) =
    For a b <$> fun c <*> pure d <*> fun e <*>
    pure f <*> pure g <*> _Exprs fun h <*>
    (traverse._4._Exprs) fun i
  _Exprs fun (ClassDef a b c d e f g) =
    ClassDef a b (coerce c) <$>
    (traverse._2.traverse.traverse._Exprs) fun d <*> pure e <*> pure f <*>
    _Exprs fun g

makeWrapped ''Block
makeLenses ''ExceptAs
