{-# language DataKinds #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Syntax.IR where

import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Tuple (_2, _3)
import Control.Lens.Prism (_Right)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid ((<>))
import Data.Validate (Validate(..))

import Language.Python.Internal.Syntax.AugAssign
import Language.Python.Internal.Syntax.BinOp
import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Import
import Language.Python.Internal.Syntax.ModuleNames
import Language.Python.Internal.Syntax.Numbers
import Language.Python.Internal.Syntax.Strings
import Language.Python.Internal.Syntax.UnOp
import Language.Python.Internal.Syntax.Whitespace

import qualified Language.Python.Internal.Syntax as Syntax

data IRError a = InvalidUnpacking a
  deriving (Eq, Show)

data Statement a
  = SmallStatements
      (Indents a)
      (SmallStatement a)
      [([Whitespace], SmallStatement a)]
      (Maybe [Whitespace])
      (Either (Maybe Comment) Newline)
  | CompoundStatement
      (CompoundStatement a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CompoundStatement a
  = Fundef a
      [Decorator a]
      (Indents a)
      (Maybe (NonEmpty Whitespace)) -- ^ ['async' <spaces>]
      (NonEmpty Whitespace) -- ^ 'def' <spaces>
      (Ident '[] a) -- ^ <ident>
      [Whitespace] -- ^ '(' <spaces>
      (CommaSep (Param a)) -- ^ <parameters>
      [Whitespace] -- ^ ')' <spaces>
      (Maybe ([Whitespace], Expr a)) -- ^ ['->' <spaces> <expr>]
      (Suite a) -- ^ <suite>
  | If a
      (Indents a)
      [Whitespace] -- ^ 'if' <spaces>
      (Expr a) -- ^ <expr>
      (Suite a) -- ^ <suite>
      [(Indents a, [Whitespace], Expr a, Suite a)] -- ^ ('elif' <spaces> <expr> <suite>)*
      (Maybe (Indents a, [Whitespace], Suite a)) -- ^ ['else' <spaces> <suite>]
  | While a
      (Indents a)
      [Whitespace] -- ^ 'while' <spaces>
      (Expr a) -- ^ <expr>
      (Suite a) -- ^ <suite>
  | TryExcept a
      (Indents a)
      [Whitespace] -- ^ 'try' <spaces>
      (Suite a) -- ^ <suite>
      (NonEmpty (Indents a, [Whitespace], Maybe (ExceptAs a), Suite a)) -- ^ ('except' <spaces> <except_as> <suite>)+
      (Maybe (Indents a, [Whitespace], Suite a)) -- ^ ['else' <spaces> <suite>]
      (Maybe (Indents a, [Whitespace], Suite a)) -- ^ ['finally' <spaces> <suite>]
  | TryFinally a
      (Indents a)
      [Whitespace] -- ^ 'try' <spaces>
      (Suite a) -- ^ <suite>
      (Indents a)
      [Whitespace] -- ^ 'finally' <spaces>
      (Suite a) -- ^ <suite>
  | For a
      (Indents a)
      (Maybe (NonEmpty Whitespace)) -- ^ ['async' <spaces>]
      [Whitespace] -- ^ 'for' <spaces>
      (Expr a) -- ^ <expr>
      [Whitespace] -- ^ 'in' <spaces>
      (Expr a) -- ^ <expr>
      (Suite a) -- ^ <suite>
      (Maybe (Indents a, [Whitespace], Suite a)) -- ^ ['else' <spaces> <suite>]
  | ClassDef a
      [Decorator a]
      (Indents a)
      (NonEmpty Whitespace) -- ^ 'class' <spaces>
      (Ident '[] a) -- ^ <ident>
      (Maybe ([Whitespace], Maybe (CommaSep1' (Arg a)), [Whitespace])) -- ^ ['(' <spaces> [<args>] ')' <spaces>]
      (Suite a) -- ^ <suite>
  | With a
      (Indents a)
      (Maybe (NonEmpty Whitespace)) -- ^ ['async' <spaces>]
      [Whitespace] -- ^ 'with' <spaces>
      (CommaSep1 (WithItem a)) -- ^ <with_items>
      (Suite a) -- ^ <suite>
  deriving (Eq, Show, Functor, Foldable, Traversable)

data SmallStatement a
  = Return a [Whitespace] (Maybe (Expr a))
  | Expr a (Expr a)
  | Assign a (Expr a) (NonEmpty ([Whitespace], Expr a))
  | AugAssign a (Expr a) (AugAssign a) (Expr a)
  | Pass a [Whitespace]
  | Break a [Whitespace]
  | Continue a [Whitespace]
  | Global a (NonEmpty Whitespace) (CommaSep1 (Ident '[] a))
  | Nonlocal a (NonEmpty Whitespace) (CommaSep1 (Ident '[] a))
  | Del a (NonEmpty Whitespace) (CommaSep1' (Expr a))
  | Import
      a
      (NonEmpty Whitespace)
      (CommaSep1 (ImportAs (ModuleName '[]) '[] a))
  | From
      a
      [Whitespace]
      (RelativeModuleName '[] a)
      [Whitespace]
      (ImportTargets '[] a)
  | Raise a
      [Whitespace]
      (Maybe (Expr a, Maybe ([Whitespace], Expr a)))
  | Assert a
      [Whitespace]
      (Expr a)
      (Maybe ([Whitespace], Expr a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Param a
  = PositionalParam
  { _paramAnn :: a
  , _paramName :: Ident '[] a
  , _paramType :: Maybe ([Whitespace], Expr a)
  }
  | KeywordParam
  { _paramAnn :: a
  , _paramName :: Ident '[] a
  -- ':' spaces <expr>
  , _paramType :: Maybe ([Whitespace], Expr a)
  -- = spaces
  , _unsafeKeywordParamWhitespaceRight :: [Whitespace]
  , _unsafeKeywordParamExpr :: Expr a
  }
  | StarParam
  { _paramAnn :: a
  -- '*' spaces
  , _unsafeStarParamWhitespace :: [Whitespace]
  , _unsafeStarParamName :: Maybe (Ident '[] a)
  , _paramType :: Maybe ([Whitespace], Expr a)
  }
  | DoubleStarParam
  { _paramAnn :: a
  -- '**' spaces
  , _unsafeDoubleStarParamWhitespace :: [Whitespace]
  , _paramName :: Ident '[] a
  , _paramType :: Maybe ([Whitespace], Expr a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CompIf a
  -- ^ 'if' <any_spaces> <expr>
  = CompIf a [Whitespace] (Expr a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CompFor a
  -- ^ 'for' <any_spaces> <targets> 'in' <any_spaces> <expr>
  = CompFor a [Whitespace] (Expr a) [Whitespace] (Expr a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Comprehension e a
  -- ^ <expr> <comp_for> (comp_for | comp_if)*
  = Comprehension a (e a) (CompFor a) [Either (CompFor a) (CompIf a)]
  deriving (Eq, Show)

instance Functor e => Functor (Comprehension e) where
  fmap f (Comprehension a b c d) =
    Comprehension (f a) (fmap f b) (fmap f c) (fmap (bimap (fmap f) (fmap f)) d)

instance Foldable e => Foldable (Comprehension e) where
  foldMap f (Comprehension a b c d) =
    f a <> foldMap f b <> foldMap f c <> foldMap (bifoldMap (foldMap f) (foldMap f)) d

instance Traversable e => Traversable (Comprehension e) where
  traverse f (Comprehension a b c d) =
    Comprehension <$>
    f a <*>
    traverse f b <*>
    traverse f c <*>
    traverse (bitraverse (traverse f) (traverse f)) d

data Subscript a
  = SubscriptExpr (Expr a)
  | SubscriptSlice
      -- [expr]
      (Maybe (Expr a))
      -- ':' <spaces>
      [Whitespace]
      -- [expr]
      (Maybe (Expr a))
      -- [':' [expr]]
      (Maybe ([Whitespace], Maybe (Expr a)))
  deriving (Eq, Show, Functor, Foldable, Traversable)

data DictItem a
  = DictItem
  { _dictItemAnn :: a
  , _unsafeDictItemKey :: Expr a
  , _unsafeDictItemWhitespace :: [Whitespace]
  , _unsafeDictItemvalue :: Expr a
  }
  | DictUnpack
  { _dictItemAnn :: a
  , _unsafeDictItemUnpackWhitespace :: [Whitespace]
  , _unsafeDictItemUnpackValue :: Expr a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Arg a
  = PositionalArg
  { _argAnn :: a
  , _argExpr :: Expr a
  }
  | KeywordArg
  { _argAnn :: a
  , _unsafeKeywordArgName :: Ident '[] a
  , _unsafeKeywordArgWhitespaceRight :: [Whitespace]
  , _argExpr :: Expr a
  }
  | StarArg
  { _argAnn :: a
  , _unsafeStarArgWhitespace :: [Whitespace]
  , _argExpr :: Expr a
  }
  | DoubleStarArg
  { _argAnn :: a
  , _unsafeDoubleStarArgWhitespace :: [Whitespace]
  , _argExpr :: Expr a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Expr a
  = StarExpr
  { _exprAnnotation :: a
  , _unsafeStarExprWhitespace :: [Whitespace]
  , _unsafeStarExprValue :: Expr a
  }
  | Unit
  { _exprAnnotation :: a
  , _unsafeUnitWhitespaceInner :: [Whitespace]
  , _unsafeUnitWhitespaceRight :: [Whitespace]
  }
  | Lambda
  { _exprAnnotation :: a
  , _unsafeLambdaWhitespace :: [Whitespace]
  , _unsafeLambdaArgs :: CommaSep (Param a)
  , _unsafeLambdaColon :: [Whitespace]
  , _unsafeLambdaBody :: Expr a
  }
  | Yield
  { _exprAnnotation :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeYieldValue :: Maybe (Expr a)
  }
  | YieldFrom
  { _exprAnnotation :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeFromWhitespace :: [Whitespace]
  , _unsafeYieldFromValue :: Expr a
  }
  | Ternary
  { _exprAnnotation :: a
  -- expr
  , _unsafeTernaryValue :: Expr a
  -- 'if' spaces
  , _unsafeTernaryWhitespaceIf :: [Whitespace]
  -- expr
  , _unsafeTernaryCond :: Expr a
  -- 'else' spaces
  , _unsafeTernaryWhitespaceElse :: [Whitespace]
  -- expr
  , _unsafeTernaryElse :: Expr a
  }
  | ListComp
  { _exprAnnotation :: a
  -- [ spaces
  , _unsafeListCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeListCompValue :: Comprehension Expr a
  -- ] spaces
  , _unsafeListCompWhitespaceRight :: [Whitespace]
  }
  | List
  { _exprAnnotation :: a
  -- [ spaces
  , _unsafeListWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeListValues :: Maybe (CommaSep1' (Expr a))
  -- ] spaces
  , _unsafeListWhitespaceRight :: [Whitespace]
  }
  | DictComp
  { _exprAnnotation :: a
  -- { spaces
  , _unsafeDictCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeDictCompValue :: Comprehension DictItem a
  -- } spaces
  , _unsafeDictCompWhitespaceRight :: [Whitespace]
  }
  | Dict
  { _exprAnnotation :: a
  , _unsafeDictWhitespaceLeft :: [Whitespace]
  , _unsafeDictValues :: Maybe (CommaSep1' (DictItem a))
  , _unsafeDictWhitespaceRight :: [Whitespace]
  }
  | SetComp
  { _exprAnnotation :: a
  -- { spaces
  , _unsafeSetCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeSetCompValue :: Comprehension Expr a
  -- } spaces
  , _unsafeSetCompWhitespaceRight :: [Whitespace]
  }
  | Set
  { _exprAnnotation :: a
  , _unsafeSetWhitespaceLeft :: [Whitespace]
  , _unsafeSetValues :: CommaSep1' (Expr a)
  , _unsafeSetWhitespaceRight :: [Whitespace]
  }
  | Deref
  { _exprAnnotation :: a
  -- expr
  , _unsafeDerefValueLeft :: Expr a
  -- . spaces
  , _unsafeDerefWhitespaceLeft :: [Whitespace]
  -- ident
  , _unsafeDerefValueRight :: Ident '[] a
  }
  | Subscript
  { _exprAnnotation :: a
  -- expr
  , _unsafeSubscriptValueLeft :: Expr a
  -- [ spaces
  , _unsafeSubscriptWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeSubscriptValueRight :: CommaSep1' (Subscript a)
  -- ] spaces
  , _unsafeSubscriptWhitespaceRight :: [Whitespace]
  }
  | Call
  { _exprAnnotation :: a
  -- expr
  , _unsafeCallFunction :: Expr a
  -- ( spaces
  , _unsafeCallWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeCallArguments :: Maybe (CommaSep1' (Arg a))
  -- ) spaces
  , _unsafeCallWhitespaceRight :: [Whitespace]
  }
  | None
  { _exprAnnotation :: a
  , _unsafeNoneWhitespace :: [Whitespace]
  }
  | Ellipsis
  { _exprAnnotation :: a
  , _unsafeEllipsisWhitespace :: [Whitespace]
  }
  | BinOp
  { _exprAnnotation :: a
  , _unsafeBinOpExprLeft :: Expr a
  , _unsafeBinOpOp :: BinOp a
  , _unsafeBinOpExprRight :: Expr a
  }
  | UnOp
  { _exprAnnotation :: a
  , _unsafeUnOpOp :: UnOp a
  , _unsafeUnOpValue :: Expr a
  }
  | Parens
  { _exprAnnotation :: a
  -- ( spaces
  , _unsafeParensWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeParensValue :: Expr a
  -- ) spaces
  , _unsafeParensWhitespaceAfter :: [Whitespace]
  }
  | Ident
  { _exprAnnotation :: a
  , _unsafeIdentValue :: Ident '[] a
  }
  | Int
  { _exprAnnotation :: a
  , _unsafeIntValue :: IntLiteral a
  , _unsafeIntWhitespace :: [Whitespace]
  }
  | Float
  { _exprAnnotation :: a
  , _unsafeFloatValue :: FloatLiteral a
  , _unsafeFloatWhitespace :: [Whitespace]
  }
  | Imag
  { _exprAnnotation :: a
  , _unsafeImagValue :: ImagLiteral a
  , _unsafeImagWhitespace :: [Whitespace]
  }
  | Bool
  { _exprAnnotation :: a
  , _unsafeBoolValue :: Bool
  , _unsafeBoolWhitespace :: [Whitespace]
  }
  | String
  { _exprAnnotation :: a
  , _unsafeStringLiteralValue :: NonEmpty (StringLiteral a)
  }
  | Tuple
  { _exprAnnotation :: a
  -- expr
  , _unsafeTupleHead :: Expr a
  -- , spaces
  , _unsafeTupleWhitespace :: [Whitespace]
  -- [exprs]
  , _unsafeTupleTail :: Maybe (CommaSep1' (Expr a))
  }
  | Not
  { _exprAnnotation :: a
  , _unsafeNotWhitespace :: [Whitespace]
  , _unsafeNotValue :: Expr a
  }
  | Generator
  { _exprAnnotation :: a
  , _generatorValue :: Comprehension Expr a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Suite a
  -- ':' <space> smallstatement
  = SuiteOne a [Whitespace] (SmallStatement a) Newline
  | SuiteMany a
      -- ':' <spaces> [comment] <newline>
      [Whitespace] Newline
      -- <block>
      (Block a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Block a
  = Block
  { unBlock
    :: NonEmpty
         (Either
            ([Whitespace], Newline)
            (Statement a))
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data WithItem a
  = WithItem
  { _withItemAnn :: a
  , _withItemValue :: Expr a
  , _withItemBinder :: Maybe ([Whitespace], Expr a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Decorator a
  = Decorator
  { _decoratorAnn :: a
  , _decoratorIndents :: Indents a
  , _decoratorWhitespaceLeft :: [Whitespace]
  , _decoratorExpr :: Expr a
  , _decoratorNewline :: Newline
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ExceptAs a
  = ExceptAs
  { _exceptAsAnn :: a
  , _exceptAsExpr :: Expr a
  , _exceptAsName :: Maybe ([Whitespace], Ident '[] a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Module a
  = Module
  { unModule :: [Either (Indents a, Maybe Comment, Maybe Newline) (Statement a)]
  } deriving (Eq, Show)

data FromIRContext
  = FromIRContext
  { _allowStarred :: Bool
  }

makeLenses ''FromIRContext

fromIR_expr :: Expr a -> Validate [IRError a] (Syntax.Expr '[] a)
fromIR_expr ex =
  case ex of
    StarExpr{} -> Failure [InvalidUnpacking $ _exprAnnotation ex]
    Unit a b c -> pure $ Syntax.Unit a b c
    Lambda a b c d e ->
      (\c' -> Syntax.Lambda a b c' d) <$>
      traverse fromIR_param c <*>
      fromIR_expr e
    Yield a b c -> Syntax.Yield a b <$> traverse fromIR_expr c
    YieldFrom a b c d -> Syntax.YieldFrom a b c <$> fromIR_expr d
    Ternary a b c d e f ->
      (\b' d' -> Syntax.Ternary a b' c d' e) <$>
      fromIR_expr b <*>
      fromIR_expr d <*>
      fromIR_expr f
    ListComp a b c d ->
      (\c' -> Syntax.ListComp a b c' d) <$>
      fromIR_comprehension fromIR_expr c
    List a b c d ->
      (\c' -> Syntax.List a b c' d) <$>
      traverseOf (traverse.traverse) fromIR_listItem c
    DictComp a b c d ->
      (\c' -> Syntax.DictComp a b c' d) <$>
      fromIR_comprehension fromIR_dictItem c
    Dict a b c d ->
      (\c' -> Syntax.Dict a b c' d) <$>
      traverseOf (traverse.traverse) fromIR_dictItem c
    SetComp a b c d ->
      (\c' -> Syntax.SetComp a b c' d) <$>
      fromIR_comprehension fromIR_setItem c
    Set a b c d ->
      (\c' -> Syntax.Set a b c' d) <$>
      traverse fromIR_setItem c
    Deref a b c d ->
      (\b' -> Syntax.Deref a b' c d) <$>
      fromIR_expr b
    Subscript a b c d e ->
      (\b' d' -> Syntax.Subscript a b' c d' e) <$>
      fromIR_expr b <*>
      traverse fromIR_subscript d
    Call a b c d e ->
      (\b' d' -> Syntax.Call a b' c d' e) <$>
      fromIR_expr b <*>
      traverseOf (traverse.traverse) fromIR_arg d
    None a b -> pure $ Syntax.None a b
    Ellipsis a b -> pure $ Syntax.Ellipsis a b
    BinOp a b c d ->
      (\b' d' -> Syntax.BinOp a b' c d') <$>
      fromIR_expr b <*>
      fromIR_expr d
    UnOp a b c ->
      Syntax.UnOp a b <$> fromIR_expr c
    Parens a b c d ->
      (\c' -> Syntax.Parens a b c' d) <$>
      fromIR_expr c
    Ident a b -> pure $ Syntax.Ident a b
    Int a b c -> pure $ Syntax.Int a b c
    Float a b c -> pure $ Syntax.Float a b c
    Imag a b c -> pure $ Syntax.Imag a b c
    Bool a b c -> pure $ Syntax.Bool a b c
    String a b -> pure $ Syntax.String a b
    Tuple a b c d ->
      (\b' -> Syntax.Tuple a b' c) <$>
      fromIR_tupleItem b <*>
      traverseOf (traverse.traverse) fromIR_tupleItem d
    Not a b c -> Syntax.Not a b <$> fromIR_expr c
    Generator a b -> Syntax.Generator a <$> fromIR_comprehension fromIR_expr b

fromIR_suite :: Suite a -> Validate [IRError a] (Syntax.Suite '[] a)
fromIR_suite s =
  case s of
    SuiteOne a b c d ->
      (\c' -> Syntax.SuiteOne a b c' d) <$>
      fromIR_smallStatement c
    SuiteMany a b c d ->
      Syntax.SuiteMany a b c <$>
      fromIR_block d

fromIR_param :: Param a -> Validate [IRError a] (Syntax.Param '[] a)
fromIR_param p =
  case p of
    PositionalParam a b c ->
      Syntax.PositionalParam a b <$> traverseOf (traverse._2) fromIR_expr c
    KeywordParam a b c d e ->
      Syntax.KeywordParam a b <$>
      traverseOf (traverse._2) fromIR_expr c <*>
      pure d <*>
      fromIR_expr e
    StarParam a b c d ->
      Syntax.StarParam a b c <$> traverseOf (traverse._2) fromIR_expr d
    DoubleStarParam a b c d ->
      Syntax.DoubleStarParam a b c <$> traverseOf (traverse._2) fromIR_expr d

fromIR_arg :: Arg a -> Validate [IRError a] (Syntax.Arg '[] a)
fromIR_arg a =
  case a of
    PositionalArg a b -> Syntax.PositionalArg a <$> fromIR_expr b
    KeywordArg a b c d -> Syntax.KeywordArg a b c <$> fromIR_expr d
    StarArg a b c -> Syntax.StarArg a b <$> fromIR_expr c
    DoubleStarArg a b c -> Syntax.DoubleStarArg a b <$> fromIR_expr c

fromIR_decorator :: Decorator a -> Validate [IRError a] (Syntax.Decorator '[] a)
fromIR_decorator (Decorator a b c d e) =
  (\d' -> Syntax.Decorator a b c d' e) <$>
  fromIR_expr d

fromIR_exceptAs :: ExceptAs a -> Validate [IRError a] (Syntax.ExceptAs '[] a)
fromIR_exceptAs (ExceptAs a b c) =
  (\b' -> Syntax.ExceptAs a b' c) <$>
  fromIR_expr b

fromIR_withItem :: WithItem a -> Validate [IRError a] (Syntax.WithItem '[] a)
fromIR_withItem (WithItem a b c) =
  Syntax.WithItem a <$>
  fromIR_expr b <*>
  traverseOf (traverse._2) fromIR_expr c

fromIR_comprehension
  :: (e a -> Validate [IRError a] (e' '[] a))
  -> Comprehension e a
  -> Validate [IRError a] (Syntax.Comprehension e' '[] a)
fromIR_comprehension f (Comprehension a b c d) =
  Syntax.Comprehension a <$>
  f b <*>
  fromIR_compFor c <*>
  traverse (bitraverse fromIR_compFor fromIR_compIf) d

fromIR_dictItem :: DictItem a -> Validate [IRError a] (Syntax.DictItem '[] a)
fromIR_dictItem di =
  case di of
    DictItem a b c d ->
      (\b' -> Syntax.DictItem a b' c) <$>
      fromIR_expr b <*>
      fromIR_expr d
    DictUnpack a b c ->
      Syntax.DictUnpack a b <$> fromIR_expr c

fromIR_subscript :: Subscript a -> Validate [IRError a] (Syntax.Subscript '[] a)
fromIR_subscript s =
  case s of
    SubscriptExpr a -> Syntax.SubscriptExpr <$> fromIR_expr a
    SubscriptSlice a b c d ->
      (\a' -> Syntax.SubscriptSlice a' b) <$>
      traverse fromIR_expr a <*>
      traverse fromIR_expr c <*>
      traverseOf (traverse._2.traverse) fromIR_expr d

fromIR_block :: Block a -> Validate [IRError a] (Syntax.Block '[] a)
fromIR_block (Block a) =
  Syntax.Block <$> traverseOf (traverse.traverse) fromIR_statement a

fromIR_compFor :: CompFor a -> Validate [IRError a] (Syntax.CompFor '[] a)
fromIR_compFor (CompFor a b c d e) =
  (\c' -> Syntax.CompFor a b c' d) <$>
  fromIR_expr c <*>
  fromIR_expr e

fromIR_compIf :: CompIf a -> Validate [IRError a] (Syntax.CompIf '[] a)
fromIR_compIf (CompIf a b c) =
  Syntax.CompIf a b <$> fromIR_expr c

fromIR_statement :: Statement a -> Validate [IRError a] (Syntax.Statement '[] a)
fromIR_statement ex =
  case ex of
    SmallStatements a b c d e ->
      (\b' c' -> Syntax.SmallStatements a b' c' d e) <$>
      fromIR_smallStatement b <*>
      traverseOf (traverse._2) fromIR_smallStatement c
    CompoundStatement a ->
      Syntax.CompoundStatement <$> fromIR_compoundStatement a

fromIR_smallStatement :: SmallStatement a -> Validate [IRError a] (Syntax.SmallStatement '[] a)
fromIR_smallStatement ex =
  case ex of
    Assign a b c ->
      Syntax.Assign a <$>
      fromIR_expr b <*>
      traverseOf (traverse._2) fromIR_expr c
    Return a b c -> Syntax.Return a b <$> traverse fromIR_expr c
    Expr a b -> Syntax.Expr a <$> fromIR_expr b
    AugAssign a b c d ->
      (\b' d' -> Syntax.AugAssign a b' c d') <$>
      fromIR_expr b <*>
      fromIR_expr d
    Pass a ws -> pure $ Syntax.Pass a ws
    Break a ws -> pure $ Syntax.Break a ws
    Continue a ws -> pure $ Syntax.Continue a ws
    Global a b c -> pure $ Syntax.Global a b c
    Nonlocal a b c -> pure $ Syntax.Nonlocal a b c
    Del a b c -> Syntax.Del a b <$> traverse fromIR_expr c
    Import a b c -> pure $ Syntax.Import a b c
    From a b c d e -> pure $ Syntax.From a b c d e
    Raise a b c ->
      Syntax.Raise a b <$>
      traverse
        (\(a, b) -> (,) <$>
          fromIR_expr a <*>
          traverseOf (traverse._2) fromIR_expr b)
        c
    Assert a b c d ->
      Syntax.Assert a b <$>
      fromIR_expr c <*>
      traverseOf (traverse._2) fromIR_expr d

fromIR_compoundStatement
  :: CompoundStatement a
  -> Validate [IRError a] (Syntax.CompoundStatement '[] a)
fromIR_compoundStatement st =
  case st of
    Fundef a b asyncWs c d e f g h i j ->
      (\b' g' i' -> Syntax.Fundef a b' asyncWs c d e f g' h i') <$>
      traverse fromIR_decorator b <*>
      traverse fromIR_param g <*>
      traverseOf (traverse._2) fromIR_expr i <*>
      fromIR_suite j
    If a b c d e f g ->
      Syntax.If a b c <$>
      fromIR_expr d <*>
      fromIR_suite e <*>
      traverse (\(a, b, c, d) -> (,,,) a b <$> fromIR_expr c <*> fromIR_suite d) f <*>
      traverseOf (traverse._3) fromIR_suite g
    While a b c d e ->
      Syntax.While a b c <$> fromIR_expr d <*> fromIR_suite e
    TryExcept a b c d e f g ->
      Syntax.TryExcept a b c <$>
      fromIR_suite d <*>
      traverse
        (\(a, b, c, d) -> (,,,) a b <$> traverse fromIR_exceptAs c <*> fromIR_suite d)
        e <*>
      traverseOf (traverse._3) fromIR_suite f <*>
      traverseOf (traverse._3) fromIR_suite g
    TryFinally a b c d e f g ->
      (\d' -> Syntax.TryFinally a b c d' e f) <$> fromIR_suite d <*> fromIR_suite g
    For a b asyncWs c d e f g h ->
      (\d' -> Syntax.For a b asyncWs c d' e) <$>
      fromIR_expr d <*>
      fromIR_expr f <*>
      fromIR_suite g <*>
      traverseOf (traverse._3) fromIR_suite h
    ClassDef a b c d e f g ->
      (\b' -> Syntax.ClassDef a b' c d e) <$>
      traverse fromIR_decorator b <*>
      traverseOf (traverse._2.traverse.traverse) fromIR_arg f <*>
      fromIR_suite g
    With a b asyncWs c d e ->
      Syntax.With a b asyncWs c <$>
      traverse fromIR_withItem d <*>
      fromIR_suite e

fromIR_listItem :: Expr a -> Validate [IRError a] (Syntax.ListItem '[] a)
fromIR_listItem (StarExpr a b c) =
  Syntax.ListUnpack a [] b <$> fromIR_expr c
fromIR_listItem (Parens a b c d) =
  (\case
      Syntax.ListUnpack w x y z -> Syntax.ListUnpack w ((b, d) : x) y z
      Syntax.ListItem x y -> Syntax.ListItem a (Syntax.Parens x b y d)) <$>
  fromIR_listItem c
fromIR_listItem e = (\x -> Syntax.ListItem (Syntax._exprAnnotation x) x) <$> fromIR_expr e

fromIR_tupleItem :: Expr a -> Validate [IRError a] (Syntax.TupleItem '[] a)
fromIR_tupleItem (StarExpr a b c) =
  Syntax.TupleUnpack a [] b <$> fromIR_expr c
fromIR_tupleItem (Parens a b c d) =
  (\case
      Syntax.TupleUnpack w x y z -> Syntax.TupleUnpack w ((b, d) : x) y z
      Syntax.TupleItem x y -> Syntax.TupleItem a (Syntax.Parens x b y d)) <$>
  fromIR_tupleItem c
fromIR_tupleItem e =
  (\x -> Syntax.TupleItem (Syntax._exprAnnotation x) x) <$> fromIR_expr e

fromIR_setItem :: Expr a -> Validate [IRError a] (Syntax.SetItem '[] a)
fromIR_setItem (StarExpr a b c) =
  Syntax.SetUnpack a [] b <$> fromIR_expr c
fromIR_setItem (Parens a b c d) =
  (\case
      Syntax.SetUnpack w x y z -> Syntax.SetUnpack w ((b, d) : x) y z
      Syntax.SetItem x y -> Syntax.SetItem a (Syntax.Parens x b y d)) <$>
  fromIR_setItem c
fromIR_setItem e = (\x -> Syntax.SetItem (Syntax._exprAnnotation x) x) <$> fromIR_expr e

fromIR :: Module a -> Validate [IRError a] (Syntax.Module '[] a)
fromIR (Module ms) =
  Syntax.Module <$> traverseOf (traverse._Right) fromIR_statement ms
