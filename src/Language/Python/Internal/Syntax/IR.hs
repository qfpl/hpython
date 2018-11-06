{-# language DataKinds #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Internal.Syntax.IR
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax.IR where

import Control.Lens.Fold (foldMapOf, folded)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~), over, mapped)
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Tuple (_1, _2, _3)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid ((<>))
import Data.Validation (Validation(..))

import Language.Python.Internal.Syntax.AugAssign
import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.Import
import Language.Python.Internal.Syntax.ModuleNames
import Language.Python.Internal.Syntax.Numbers
import Language.Python.Internal.Syntax.Operator.Binary
import Language.Python.Internal.Syntax.Operator.Unary
import Language.Python.Internal.Syntax.Strings
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Whitespace

import qualified Language.Python.Syntax.Module as Syntax
import qualified Language.Python.Syntax.Expr as Syntax
import qualified Language.Python.Syntax.Statement as Syntax

data IRError a = InvalidUnpacking a
  deriving (Eq, Show)

data SimpleStatement a
  = MkSimpleStatement
      (SmallStatement a)
      [([Whitespace], SmallStatement a)]
      (Maybe [Whitespace])
      (Maybe (Comment a))
      (Maybe Newline)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Statement a
  = SimpleStatement (Indents a) (SimpleStatement a)
  | CompoundStatement (CompoundStatement a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CompoundStatement a
  = Fundef
  { _csAnn :: a
  , _unsafeCsFundefDecorators :: [Decorator a]
  , _csIndents :: Indents a
  , _unsafeCsFundefAsync :: Maybe (NonEmpty Whitespace) -- ^ @[\'async\' \<spaces\>]@
  , _unsafeCsFundefDef :: NonEmpty Whitespace -- ^ @\'def\' \<spaces\>@
  , _unsafeCsFundefName :: Ident '[] a -- ^ @\<ident\>@
  , _unsafeCsFundefLeftParen :: [Whitespace] -- ^ @\'(\' \<spaces\>@
  , _unsafeCsFundefParameters :: CommaSep (Param a) -- ^ @\<parameters\>@
  , _unsafeCsFundefRightParen :: [Whitespace] -- ^ @\')\' \<spaces\>@
  , _unsafeCsFundefReturnType :: Maybe ([Whitespace], Expr a) -- ^ @[\'->\' \<spaces\> \<expr\>]@
  , _unsafeCsFundefBody :: Suite a -- ^ @\<suite\>@
  }
  | If
  { _csAnn :: a
  , _csIndents :: Indents a
  , _unsafeCsIfIf :: [Whitespace] -- ^ @\'if\' \<spaces\>@
  , _unsafeCsIfCond :: Expr a -- ^ @\<expr\>@
  , _unsafeCsIfBody :: Suite a -- ^ @\<suite\>@
  , _unsafeCsIfElifs :: [(Indents a, [Whitespace], Expr a, Suite a)] -- ^ @(\'elif\' \<spaces\> \<expr\> \<suite\>)*@
  , _unsafeCsIfElse :: Maybe (Indents a, [Whitespace], Suite a) -- ^ @[\'else\' \<spaces\> \<suite\>]@
  }
  | While
  { _csAnn :: a
  , _csIndents :: Indents a
  , _unsafeCsWhileWhile :: [Whitespace] -- ^ @\'while\' \<spaces\>@
  , _unsafeCsWhileCond :: Expr a -- ^ @\<expr\>@
  , _unsafeCsWhileBody :: Suite a -- ^ @\<suite\>@
  , _unsafeCsWhileElse
    :: Maybe (Indents a, [Whitespace], Suite a) -- ^ @[\'else\' \<spaces\> \<suite\>]@
  }
  | TryExcept
  { _csAnn :: a
  , _csIndents :: Indents a
  , _unsafeCsTryExceptTry :: [Whitespace] -- ^ @\'try\' \<spaces\>@
  , _unsafeCsTryExceptBody :: Suite a -- ^ @\<suite\>@
  , _unsafeCsTryExceptExcepts :: NonEmpty (Indents a, [Whitespace], Maybe (ExceptAs a), Suite a) -- ^ @(\'except\' \<spaces\> \<except_as\> \<suite\>)+@
  , _unsafeCsTryExceptElse :: Maybe (Indents a, [Whitespace], Suite a) -- ^ @[\'else\' \<spaces\> \<suite\>]@
  , _unsafeCsTryExceptFinally :: Maybe (Indents a, [Whitespace], Suite a) -- ^ @[\'finally\' \<spaces\> \<suite\>]@
  }
  | TryFinally
  { _csAnn :: a
  , _csIndents :: Indents a
  , _unsafeCsTryFinallyTry :: [Whitespace] -- ^ @\'try\' \<spaces\>@
  , _unsafeCsTryFinallyTryBody :: Suite a -- ^ @\<suite\>@
  , _unsafeCsTryFinallyFinallyIndents :: Indents a
  , _unsafeCsTryFinallyFinally :: [Whitespace] -- ^ @\'finally\' \<spaces\>@
  , _unsafeCsTryFinallyFinallyBody :: Suite a -- ^ @\<suite\>@
  }
  | For
  { _csAnn :: a
  , _csIndents :: Indents a
  , _unsafeCsForAsync :: Maybe (NonEmpty Whitespace) -- ^ @[\'async\' \<spaces\>]@
  , _unsafeCsForFor :: [Whitespace] -- ^ @\'for\' \<spaces\>@
  , _unsafeCsForBinder :: Expr a -- ^ @\<expr\>@
  , _unsafeCsForIn :: [Whitespace] -- ^ @\'in\' \<spaces\>@
  , _unsafeCsForCollection :: CommaSep1' (Expr a) -- ^ @\<exprs\>@
  , _unsafeCsForBody :: Suite a -- ^ @\<suite\>@
  , _unsafeCsForElse :: Maybe (Indents a, [Whitespace], Suite a) -- ^ @[\'else\' \<spaces\> \<suite\>]@
  }
  | ClassDef
  { _csAnn :: a
  , _unsafeCsClassDefDecorators :: [Decorator a]
  , _csIndents :: Indents a
  , _unsafeCsClassDefClass :: NonEmpty Whitespace -- ^ @\'class\' \<spaces\>@
  , _unsafeCsClassDefName :: Ident '[] a -- ^ @\<ident\>@
  , _unsafeCsClassDefArguments :: Maybe ([Whitespace], Maybe (CommaSep1' (Arg a)), [Whitespace]) -- ^ @[\'(\' \<spaces\> [\<args\>] \')\' \<spaces\>]@
  , _unsafeCsClassDefBody :: Suite a -- ^ @\<suite\>@
  }
  | With
  { _csAnn :: a
  , _csIndents :: Indents a
  , _unsafeCsWithAsync :: Maybe (NonEmpty Whitespace) -- ^ @[\'async\' \<spaces\>]@
  , _unsafeCsWithWith :: [Whitespace] -- ^ @\'with\' \<spaces\>@
  , _unsafeCsWithItems :: CommaSep1 (WithItem a) -- ^ @\<with_items\>@
  , _unsafeCsWithBody :: Suite a -- ^ @\<suite\>@
  }
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
  | Del a [Whitespace] (CommaSep1' (Expr a))
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
      (Maybe (Comma, Expr a))
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
  = CompIf a [Whitespace] (Expr a) -- ^ 'if' <any_spaces> <expr>
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CompFor a
  = CompFor a [Whitespace] (Expr a) [Whitespace] (Expr a) -- ^ 'for' <any_spaces> <targets> 'in' <any_spaces> <expr>
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Comprehension e a
  = Comprehension a (e a) (CompFor a) [Either (CompFor a) (CompIf a)] -- ^ <expr> <comp_for> (comp_for | comp_if)*
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
  { _unsafeExprAnn :: a
  , _unsafeStarExprWhitespace :: [Whitespace]
  , _unsafeStarExprValue :: Expr a
  }
  | Unit
  { _unsafeExprAnn :: a
  , _unsafeUnitWhitespaceInner :: [Whitespace]
  , _unsafeUnitWhitespaceRight :: [Whitespace]
  }
  | Lambda
  { _unsafeExprAnn :: a
  , _unsafeLambdaWhitespace :: [Whitespace]
  , _unsafeLambdaArgs :: CommaSep (Param a)
  , _unsafeLambdaColon :: [Whitespace]
  , _unsafeLambdaBody :: Expr a
  }
  | Yield
  { _unsafeExprAnn :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeYieldValue :: Maybe (Expr a)
  }
  | YieldFrom
  { _unsafeExprAnn :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeFromWhitespace :: [Whitespace]
  , _unsafeYieldFromValue :: Expr a
  }
  | Ternary
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  -- [ spaces
  , _unsafeListCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeListCompValue :: Comprehension Expr a
  -- ] spaces
  , _unsafeListCompWhitespaceRight :: [Whitespace]
  }
  | List
  { _unsafeExprAnn :: a
  -- [ spaces
  , _unsafeListWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeListValues :: Maybe (CommaSep1' (Expr a))
  -- ] spaces
  , _unsafeListWhitespaceRight :: [Whitespace]
  }
  | DictComp
  { _unsafeExprAnn :: a
  -- { spaces
  , _unsafeDictCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeDictCompValue :: Comprehension DictItem a
  -- } spaces
  , _unsafeDictCompWhitespaceRight :: [Whitespace]
  }
  | Dict
  { _unsafeExprAnn :: a
  , _unsafeDictWhitespaceLeft :: [Whitespace]
  , _unsafeDictValues :: Maybe (CommaSep1' (DictItem a))
  , _unsafeDictWhitespaceRight :: [Whitespace]
  }
  | SetComp
  { _unsafeExprAnn :: a
  -- { spaces
  , _unsafeSetCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeSetCompValue :: Comprehension Expr a
  -- } spaces
  , _unsafeSetCompWhitespaceRight :: [Whitespace]
  }
  | Set
  { _unsafeExprAnn :: a
  , _unsafeSetWhitespaceLeft :: [Whitespace]
  , _unsafeSetValues :: CommaSep1' (Expr a)
  , _unsafeSetWhitespaceRight :: [Whitespace]
  }
  | Deref
  { _unsafeExprAnn :: a
  -- expr
  , _unsafeDerefValueLeft :: Expr a
  -- . spaces
  , _unsafeDerefWhitespaceLeft :: [Whitespace]
  -- ident
  , _unsafeDerefValueRight :: Ident '[] a
  }
  | Subscript
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
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
  { _unsafeExprAnn :: a
  , _unsafeNoneWhitespace :: [Whitespace]
  }
  | Ellipsis
  { _unsafeExprAnn :: a
  , _unsafeEllipsisWhitespace :: [Whitespace]
  }
  | BinOp
  { _unsafeExprAnn :: a
  , _unsafeBinOpExprLeft :: Expr a
  , _unsafeBinOpOp :: BinOp a
  , _unsafeBinOpExprRight :: Expr a
  }
  | UnOp
  { _unsafeExprAnn :: a
  , _unsafeUnOpOp :: UnOp a
  , _unsafeUnOpValue :: Expr a
  }
  | Parens
  { _unsafeExprAnn :: a
  -- ( spaces
  , _unsafeParensWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeParensValue :: Expr a
  -- ) spaces
  , _unsafeParensWhitespaceAfter :: [Whitespace]
  }
  | Ident
  { _unsafeIdentValue :: Ident '[] a
  }
  | Int
  { _unsafeExprAnn :: a
  , _unsafeIntValue :: IntLiteral a
  , _unsafeIntWhitespace :: [Whitespace]
  }
  | Float
  { _unsafeExprAnn :: a
  , _unsafeFloatValue :: FloatLiteral a
  , _unsafeFloatWhitespace :: [Whitespace]
  }
  | Imag
  { _unsafeExprAnn :: a
  , _unsafeImagValue :: ImagLiteral a
  , _unsafeImagWhitespace :: [Whitespace]
  }
  | Bool
  { _unsafeExprAnn :: a
  , _unsafeBoolValue :: Bool
  , _unsafeBoolWhitespace :: [Whitespace]
  }
  | String
  { _unsafeExprAnn :: a
  , _unsafeStringLiteralValue :: NonEmpty (StringLiteral a)
  }
  | Tuple
  { _unsafeExprAnn :: a
  -- expr
  , _unsafeTupleHead :: Expr a
  -- , spaces
  , _unsafeTupleWhitespace :: Comma
  -- [exprs]
  , _unsafeTupleTail :: Maybe (CommaSep1' (Expr a))
  }
  | Not
  { _unsafeExprAnn :: a
  , _unsafeNotWhitespace :: [Whitespace]
  , _unsafeNotValue :: Expr a
  }
  | Generator
  { _unsafeExprAnn :: a
  , _generatorValue :: Comprehension Expr a
  }
  | Await
  { _unsafeExprAnn :: a
  , _unsafeAwaitWhitespace :: [Whitespace]
  , _unsafeAwaitValue :: Expr a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

exprAnn :: Lens' (Expr a) a
exprAnn =
  lens
    (\case
        Unit a _ _ -> a
        StarExpr a _ _ -> a
        Lambda a _ _ _ _ -> a
        Yield a _ _ -> a
        YieldFrom a _ _ _ -> a
        Ternary a _ _ _ _ _ -> a
        None a _ -> a
        Ellipsis a _ -> a
        List a _ _ _ -> a
        ListComp a _ _ _ -> a
        Deref a _ _ _ -> a
        Subscript a _ _ _ _ -> a
        Call a _ _ _ _ -> a
        BinOp a _ _ _ -> a
        UnOp a _ _ -> a
        Parens a _ _ _ -> a
        Ident a -> a ^. identAnn
        Int a _ _ -> a
        Float a _ _ -> a
        Imag a _ _ -> a
        Bool a _ _ -> a
        String a _ -> a
        Not a _ _ -> a
        Tuple a _ _ _ -> a
        DictComp a _ _ _ -> a
        Dict a _ _ _ -> a
        SetComp a _ _ _ -> a
        Set a _ _ _ -> a
        Generator a _ -> a
        Await a _ _ -> a)
    (\e ann ->
      case e of
        Unit _ a b -> Unit ann a b
        StarExpr _ a b -> StarExpr ann a b
        Lambda _ a b c d -> Lambda ann a b c d
        Yield _ a b -> Yield ann a b
        YieldFrom ann a b c -> YieldFrom ann a b c
        Ternary ann a b c d e -> Ternary ann a b c d e
        None _ a -> None ann a
        Ellipsis _ a -> Ellipsis ann a
        List _ a b c -> List ann a b c
        ListComp _ a b c -> ListComp ann a b c
        Deref _ a b c -> Deref ann a b c
        Subscript _ a b c d -> Subscript ann a b c d
        Call _ a b c d -> Call ann a b c d
        BinOp _ a b c -> BinOp ann a b c
        UnOp _ a b -> UnOp ann a b
        Parens _ a b c -> Parens ann a b c
        Ident a -> Ident $ a & identAnn .~ ann
        Int _ a b -> Int ann a b
        Float _ a b -> Float ann a b
        Imag _ a b -> Imag ann a b
        Bool _ a b -> Bool ann a b
        String _ a -> String ann a
        Not _ a b -> Not ann a b
        Tuple _ a b c -> Tuple ann a b c
        DictComp _ a b c -> DictComp ann a b c
        Dict _ a b c -> Dict ann a b c
        SetComp _ a b c -> SetComp ann a b c
        Set _ a b c -> Set ann a b c
        Generator _ a -> Generator ann a
        Await _ a b -> Not ann a b)

data Suite a
  -- ':' <space> simplestatement
  = SuiteOne a [Whitespace] (SimpleStatement a)
  | SuiteMany a
      -- ':' <spaces> [comment] <newline>
      [Whitespace] (Maybe (Comment a)) Newline
      -- <block>
      (Block a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Block a
  = Block
  { _blockBlankLines :: [(Blank a, Newline)]
  , _blockHead :: Statement a
  , _blockTail :: [Either (Blank a, Newline) (Statement a)]
  } deriving (Eq, Show)

instance Functor Block where
  fmap f (Block a b c) =
    Block
      (over (mapped._1.mapped) f a)
      (fmap f b)
      (bimap (over (_1.mapped) f) (fmap f) <$> c)

instance Foldable Block where
  foldMap f (Block a b c) =
    foldMapOf (folded._1.folded) f a <>
    foldMap f b <>
    foldMap (bifoldMap (foldMapOf (_1.folded) f) (foldMap f)) c

instance Traversable Block where
  traverse f (Block a b c) =
    Block <$>
    traverseOf (traverse._1.traverse) f a <*>
    traverse f b <*>
    traverse (bitraverse (traverseOf (_1.traverse) f) (traverse f)) c

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
  , _decoratorComment :: Maybe (Comment a)
  , _decoratorNewline :: Newline
  , _decoratorBlankLines :: [(Blank a, Newline)]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ExceptAs a
  = ExceptAs
  { _exceptAsAnn :: a
  , _exceptAsExpr :: Expr a
  , _exceptAsName :: Maybe ([Whitespace], Ident '[] a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Module a
  = ModuleEmpty
  | ModuleBlankFinal (Blank a)
  | ModuleBlank (Blank a) Newline (Module a)
  | ModuleStatement (Statement a) (Module a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data FromIRContext
  = FromIRContext
  { _allowStarred :: Bool
  }

makeLenses ''FromIRContext

fromIR_expr :: Expr a -> Validation (NonEmpty (IRError a)) (Syntax.Expr '[] a)
fromIR_expr ex =
  case ex of
    StarExpr{} -> Failure (pure (InvalidUnpacking $ ex ^. exprAnn))
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
    Ident a -> pure $ Syntax.Ident a
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
    Await a b c -> Syntax.Await a b <$> fromIR_expr c

fromIR_suite :: Suite a -> Validation (NonEmpty (IRError a)) (Syntax.Suite '[] a)
fromIR_suite s =
  case s of
    SuiteOne a b c ->
      Syntax.SuiteOne a b <$> fromIR_simpleStatement c
    SuiteMany a b c d e ->
      Syntax.SuiteMany a b c d <$> fromIR_block e

fromIR_param :: Param a -> Validation (NonEmpty (IRError a)) (Syntax.Param '[] a)
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

fromIR_arg :: Arg a -> Validation (NonEmpty (IRError a)) (Syntax.Arg '[] a)
fromIR_arg a =
  case a of
    PositionalArg a b -> Syntax.PositionalArg a <$> fromIR_expr b
    KeywordArg a b c d -> Syntax.KeywordArg a b c <$> fromIR_expr d
    StarArg a b c -> Syntax.StarArg a b <$> fromIR_expr c
    DoubleStarArg a b c -> Syntax.DoubleStarArg a b <$> fromIR_expr c

fromIR_decorator
  :: Decorator a
  -> Validation (NonEmpty (IRError a)) (Syntax.Decorator '[] a)
fromIR_decorator (Decorator a b c d e f g) =
  (\d' -> Syntax.Decorator a b c d' e f g) <$>
  fromIR_expr d

fromIR_exceptAs :: ExceptAs a -> Validation (NonEmpty (IRError a)) (Syntax.ExceptAs '[] a)
fromIR_exceptAs (ExceptAs a b c) =
  (\b' -> Syntax.ExceptAs a b' c) <$>
  fromIR_expr b

fromIR_withItem :: WithItem a -> Validation (NonEmpty (IRError a)) (Syntax.WithItem '[] a)
fromIR_withItem (WithItem a b c) =
  Syntax.WithItem a <$>
  fromIR_expr b <*>
  traverseOf (traverse._2) fromIR_expr c

fromIR_comprehension
  :: (e a -> Validation (NonEmpty (IRError a)) (e' '[] a))
  -> Comprehension e a
  -> Validation (NonEmpty (IRError a)) (Syntax.Comprehension e' '[] a)
fromIR_comprehension f (Comprehension a b c d) =
  Syntax.Comprehension a <$>
  f b <*>
  fromIR_compFor c <*>
  traverse (bitraverse fromIR_compFor fromIR_compIf) d

fromIR_dictItem :: DictItem a -> Validation (NonEmpty (IRError a)) (Syntax.DictItem '[] a)
fromIR_dictItem di =
  case di of
    DictItem a b c d ->
      (\b' -> Syntax.DictItem a b' c) <$>
      fromIR_expr b <*>
      fromIR_expr d
    DictUnpack a b c ->
      Syntax.DictUnpack a b <$> fromIR_expr c

fromIR_subscript :: Subscript a -> Validation (NonEmpty (IRError a)) (Syntax.Subscript '[] a)
fromIR_subscript s =
  case s of
    SubscriptExpr a -> Syntax.SubscriptExpr <$> fromIR_expr a
    SubscriptSlice a b c d ->
      (\a' -> Syntax.SubscriptSlice a' b) <$>
      traverse fromIR_expr a <*>
      traverse fromIR_expr c <*>
      traverseOf (traverse._2.traverse) fromIR_expr d

fromIR_block :: Block a -> Validation (NonEmpty (IRError a)) (Syntax.Block '[] a)
fromIR_block (Block a b c) =
  Syntax.Block a <$>
  fromIR_statement b <*>
  traverseOf (traverse.traverse) fromIR_statement c

fromIR_compFor :: CompFor a -> Validation (NonEmpty (IRError a)) (Syntax.CompFor '[] a)
fromIR_compFor (CompFor a b c d e) =
  (\c' -> Syntax.CompFor a b c' d) <$>
  fromIR_expr c <*>
  fromIR_expr e

fromIR_compIf :: CompIf a -> Validation (NonEmpty (IRError a)) (Syntax.CompIf '[] a)
fromIR_compIf (CompIf a b c) =
  Syntax.CompIf a b <$> fromIR_expr c

fromIR_simpleStatement
  :: SimpleStatement a
  -> Validation (NonEmpty (IRError a)) (Syntax.SimpleStatement '[] a)
fromIR_simpleStatement (MkSimpleStatement b c d e f) =
  (\b' c' -> Syntax.MkSimpleStatement b' c' d e f) <$>
  fromIR_smallStatement b <*>
  traverseOf (traverse._2) fromIR_smallStatement c

fromIR_statement :: Statement a -> Validation (NonEmpty (IRError a)) (Syntax.Statement '[] a)
fromIR_statement ex =
  case ex of
    SimpleStatement i a ->
      Syntax.SimpleStatement i <$> fromIR_simpleStatement a
    CompoundStatement a ->
      Syntax.CompoundStatement <$> fromIR_compoundStatement a

fromIR_smallStatement :: SmallStatement a -> Validation (NonEmpty (IRError a)) (Syntax.SmallStatement '[] a)
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
  -> Validation (NonEmpty (IRError a)) (Syntax.CompoundStatement '[] a)
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
    While a b c d e f ->
      Syntax.While a b c <$>
      fromIR_expr d <*>
      fromIR_suite e <*>
      traverseOf (traverse._3) fromIR_suite f
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
      traverse fromIR_expr f <*>
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

fromIR_listItem :: Expr a -> Validation (NonEmpty (IRError a)) (Syntax.ListItem '[] a)
fromIR_listItem (StarExpr a b c) =
  Syntax.ListUnpack a [] b <$> fromIR_expr c
fromIR_listItem (Parens a b c d) =
  (\case
      Syntax.ListUnpack w x y z -> Syntax.ListUnpack w ((b, d) : x) y z
      Syntax.ListItem x y -> Syntax.ListItem a (Syntax.Parens x b y d)) <$>
  fromIR_listItem c
fromIR_listItem e = (\x -> Syntax.ListItem (x ^. Syntax.exprAnn) x) <$> fromIR_expr e

fromIR_tupleItem :: Expr a -> Validation (NonEmpty (IRError a)) (Syntax.TupleItem '[] a)
fromIR_tupleItem (StarExpr a b c) =
  Syntax.TupleUnpack a [] b <$> fromIR_expr c
fromIR_tupleItem (Parens a b c d) =
  (\case
      Syntax.TupleUnpack w x y z -> Syntax.TupleUnpack w ((b, d) : x) y z
      Syntax.TupleItem x y -> Syntax.TupleItem a (Syntax.Parens x b y d)) <$>
  fromIR_tupleItem c
fromIR_tupleItem e =
  (\x -> Syntax.TupleItem (x ^. Syntax.exprAnn) x) <$> fromIR_expr e

fromIR_setItem :: Expr a -> Validation (NonEmpty (IRError a)) (Syntax.SetItem '[] a)
fromIR_setItem (StarExpr a b c) =
  Syntax.SetUnpack a [] b <$> fromIR_expr c
fromIR_setItem (Parens a b c d) =
  (\case
      Syntax.SetUnpack w x y z -> Syntax.SetUnpack w ((b, d) : x) y z
      Syntax.SetItem x y -> Syntax.SetItem a (Syntax.Parens x b y d)) <$>
  fromIR_setItem c
fromIR_setItem e = (\x -> Syntax.SetItem (x ^. Syntax.exprAnn) x) <$> fromIR_expr e

fromIR :: Module a -> Validation (NonEmpty (IRError a)) (Syntax.Module '[] a)
fromIR ModuleEmpty = pure Syntax.ModuleEmpty
fromIR (ModuleBlankFinal a) = pure $ Syntax.ModuleBlankFinal a
fromIR (ModuleBlank a b c) = Syntax.ModuleBlank a b <$> fromIR c
fromIR (ModuleStatement a b) = Syntax.ModuleStatement <$> fromIR_statement a <*> fromIR b
