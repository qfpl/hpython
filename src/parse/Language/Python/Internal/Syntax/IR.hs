{-# language DataKinds #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Internal.Syntax.IR
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

This module only exists as our current best solution to decoupling parts of the
concrete syntax from abstract syntax. You won't need to care about its existence
and hopefully it will be deleted soon.

-}

module Language.Python.Internal.Syntax.IR where

import Control.Lens.Fold (foldMapOf, folded)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Prism (Prism')
import Control.Lens.Review ((#))
import Control.Lens.Setter (over, mapped)
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Tuple (_1, _2, _3)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid ((<>))
import Data.Validation (Validation(..))

import Data.VIdentity
import Language.Python.Optics.Expr
import Language.Python.Syntax.Ann
import Language.Python.Syntax.AugAssign
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comment
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Import
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace

import qualified Language.Python.Syntax.Module as Syntax
import qualified Language.Python.Syntax.Expr as Syntax
import qualified Language.Python.Syntax.Types as Syntax
import qualified Language.Python.Syntax.Statement as Syntax

class AsIRError s a | s -> a where
  _InvalidUnpacking :: Prism' s a

data IRError a
  -- | Unpacking ( @*value@ ) was used in an invalid position
  = InvalidUnpacking a
  deriving (Eq, Show)

fromIRError :: AsIRError s a => IRError a -> s
fromIRError (InvalidUnpacking a) = _InvalidUnpacking # a

data SmallStatement a
  = MkSmallStatement
      (SimpleStatement a)
      [(Semicolon a, SimpleStatement a)]
      (Maybe (Semicolon a))
      (Maybe (Comment a))
      (Maybe Newline)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Statement a
  = SmallStatement (Indents a) (SmallStatement a)
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

data SimpleStatement a
  = Return a [Whitespace] (Maybe (Expr a))
  | Expr a (Expr a)
  | Assign a (Expr a) (NonEmpty (Equals, Expr a))
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
      (CommaSep1 (ImportAs ModuleName '[] a))
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
  , _paramType :: Maybe (Colon, Expr a)
  }
  | KeywordParam
  { _paramAnn :: a
  , _paramName :: Ident '[] a
  -- ':' spaces <expr>
  , _paramType :: Maybe (Colon, Expr a)
  -- = spaces
  , _unsafeKeywordParamWhitespaceRight :: [Whitespace]
  , _unsafeKeywordParamExpr :: Expr a
  }
  | StarParam
  { _paramAnn :: a
  -- '*' spaces
  , _unsafeStarParamWhitespace :: [Whitespace]
  , _unsafeStarParamName :: Ident '[] a
  , _paramType :: Maybe (Colon, Expr a)
  }
  | UnnamedStarParam
  { _paramAnn :: a
  -- '*' spaces
  , _unsafeUnnamedStarParamWhitespace :: [Whitespace]
  }
  | DoubleStarParam
  { _paramAnn :: a
  -- '**' spaces
  , _unsafeDoubleStarParamWhitespace :: [Whitespace]
  , _paramName :: Ident '[] a
  , _paramType :: Maybe (Colon, Expr a)
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

data SubscriptItem a
  = SubscriptExpr (Expr a)
  | SubscriptSlice
      -- [expr]
      (Maybe (Expr a))
      -- ':' <spaces>
      Colon
      -- [expr]
      (Maybe (Expr a))
      -- [':' [expr]]
      (Maybe (Colon, Maybe (Expr a)))
  deriving (Eq, Show, Functor, Foldable, Traversable)

data DictItem a
  = DictItem
  { _dictItemAnn :: a
  , _unsafeDictItemKey :: Expr a
  , _unsafeDictItemColon :: Colon
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
  { _exprAnn :: a
  , _unsafeStarExprWhitespace :: [Whitespace]
  , _unsafeStarExprValue :: Expr a
  }
  | Unit
  { _exprAnn :: a
  , _unsafeUnitWhitespaceInner :: [Whitespace]
  , _unsafeUnitWhitespaceRight :: [Whitespace]
  }
  | Lambda
  { _exprAnn :: a
  , _unsafeLambdaWhitespace :: [Whitespace]
  , _unsafeLambdaArgs :: CommaSep (Param a)
  , _unsafeLambdaColon :: Colon
  , _unsafeLambdaBody :: Expr a
  }
  | Yield
  { _exprAnn :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeYieldValue :: CommaSep (Expr a)
  }
  | YieldFrom
  { _exprAnn :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeFromWhitespace :: [Whitespace]
  , _unsafeYieldFromValue :: Expr a
  }
  | Ternary
  { _exprAnn :: a
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
  { _exprAnn :: a
  -- [ spaces
  , _unsafeListCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeListCompValue :: Comprehension Expr a
  -- ] spaces
  , _unsafeListCompWhitespaceRight :: [Whitespace]
  }
  | List
  { _exprAnn :: a
  -- [ spaces
  , _unsafeListWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeListValues :: Maybe (CommaSep1' (Expr a))
  -- ] spaces
  , _unsafeListWhitespaceRight :: [Whitespace]
  }
  | DictComp
  { _exprAnn :: a
  -- { spaces
  , _unsafeDictCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeDictCompValue :: Comprehension DictItem a
  -- } spaces
  , _unsafeDictCompWhitespaceRight :: [Whitespace]
  }
  | Dict
  { _exprAnn :: a
  , _unsafeDictWhitespaceLeft :: [Whitespace]
  , _unsafeDictValues :: Maybe (CommaSep1' (DictItem a))
  , _unsafeDictWhitespaceRight :: [Whitespace]
  }
  | SetComp
  { _exprAnn :: a
  -- { spaces
  , _unsafeSetCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeSetCompValue :: Comprehension Expr a
  -- } spaces
  , _unsafeSetCompWhitespaceRight :: [Whitespace]
  }
  | Set
  { _exprAnn :: a
  , _unsafeSetWhitespaceLeft :: [Whitespace]
  , _unsafeSetValues :: CommaSep1' (Expr a)
  , _unsafeSetWhitespaceRight :: [Whitespace]
  }
  | Deref
  { _exprAnn :: a
  -- expr
  , _unsafeDerefValueLeft :: Expr a
  -- . spaces
  , _unsafeDerefWhitespaceLeft :: [Whitespace]
  -- ident
  , _unsafeDerefValueRight :: Ident '[] a
  }
  | Subscript
  { _exprAnn :: a
  -- expr
  , _unsafeSubscriptValueLeft :: Expr a
  -- [ spaces
  , _unsafeSubscriptWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeSubscriptValueRight :: CommaSep1' (SubscriptItem a)
  -- ] spaces
  , _unsafeSubscriptWhitespaceRight :: [Whitespace]
  }
  | Call
  { _exprAnn :: a
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
  { _exprAnn :: a
  , _unsafeNoneWhitespace :: [Whitespace]
  }
  | Ellipsis
  { _exprAnn :: a
  , _unsafeEllipsisWhitespace :: [Whitespace]
  }
  | Binary
  { _exprAnn :: a
  , _unsafeBinaryExprLeft :: Expr a
  , _unsafeBinaryOp :: BinOp a
  , _unsafeBinaryExprRight :: Expr a
  }
  | Unary
  { _exprAnn :: a
  , _unsafeUnaryOp :: UnOp a
  , _unsafeUnaryValue :: Expr a
  }
  | Parens
  { _exprAnn :: a
  -- ( spaces
  , _unsafeParensWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeParensValue :: Expr a
  -- ) spaces
  , _unsafeParensWhitespaceAfter :: [Whitespace]
  }
  | Ident
  { _exprAnn :: a
  , _unsafeIdentValue :: Ident '[] a
  }
  | Int
  { _exprAnn :: a
  , _unsafeIntValue :: IntLiteral a
  , _unsafeIntWhitespace :: [Whitespace]
  }
  | Float
  { _exprAnn :: a
  , _unsafeFloatValue :: FloatLiteral a
  , _unsafeFloatWhitespace :: [Whitespace]
  }
  | Imag
  { _exprAnn :: a
  , _unsafeImagValue :: ImagLiteral a
  , _unsafeImagWhitespace :: [Whitespace]
  }
  | Bool
  { _exprAnn :: a
  , _unsafeBoolValue :: Bool
  , _unsafeBoolWhitespace :: [Whitespace]
  }
  | String
  { _exprAnn :: a
  , _unsafeStringLiteralValue :: NonEmpty (StringLiteral a)
  }
  | Tuple
  { _exprAnn :: a
  -- expr
  , _unsafeTupleHead :: Expr a
  -- , spaces
  , _unsafeTupleWhitespace :: Comma
  -- [exprs]
  , _unsafeTupleTail :: Maybe (CommaSep1' (Expr a))
  }
  | Not
  { _exprAnn :: a
  , _unsafeNotWhitespace :: [Whitespace]
  , _unsafeNotValue :: Expr a
  }
  | Generator
  { _exprAnn :: a
  , _generatorValue :: Comprehension Expr a
  }
  | Await
  { _exprAnn :: a
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
        Binary a _ _ _ -> a
        Unary a _ _ -> a
        Parens a _ _ _ -> a
        Ident a _ -> a
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
        YieldFrom _ a b c -> YieldFrom ann a b c
        Ternary _ a b c d e' -> Ternary ann a b c d e'
        None _ a -> None ann a
        Ellipsis _ a -> Ellipsis ann a
        List _ a b c -> List ann a b c
        ListComp _ a b c -> ListComp ann a b c
        Deref _ a b c -> Deref ann a b c
        Subscript _ a b c d -> Subscript ann a b c d
        Call _ a b c d -> Call ann a b c d
        Binary _ a b c -> Binary ann a b c
        Unary _ a b -> Unary ann a b
        Parens _ a b c -> Parens ann a b c
        Ident _ b -> Ident ann b
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
  -- ':' <space> smallStatement
  = SuiteOne a Colon (SmallStatement a)
  | SuiteMany a
      -- ':' <spaces> [comment] <newline>
      Colon (Maybe (Comment a)) Newline
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
  , _decoratorAt :: At
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

fromIR_expr
  :: AsIRError e a
  => Expr a
  -> Validation (NonEmpty e) (Syntax.Expr '[] a)
fromIR_expr ex =
  case ex of
    StarExpr{} -> Failure $ pure (_InvalidUnpacking # (ex ^. exprAnn))
    Unit a b c -> pure $ _Unit # Syntax.MkUnit (Ann a) b c
    Lambda a b c d e ->
      (\c' -> (_Lambda #) . Syntax.MkLambda (Ann a) b c' d) <$>
      traverse fromIR_param c <*>
      fromIR_expr e
    Yield a b c -> (_Yield #) . Syntax.MkYield (Ann a) b <$> traverse fromIR_expr c
    YieldFrom a b c d -> (_YieldFrom #) . Syntax.MkYieldFrom (Ann a) b c <$> fromIR_expr d
    Ternary a b c d e f ->
      (\b' d' -> (_Ternary #) . Syntax.MkTernary (Ann a) b' c d' e) <$>
      fromIR_expr b <*>
      fromIR_expr d <*>
      fromIR_expr f
    ListComp a b c d ->
      (\c' -> _ListComp # Syntax.MkListComp (Ann a) b c' d) <$>
      fromIR_comprehension (fmap VIdentity . fromIR_expr) c
    List a b c d ->
      (\c' -> _List # Syntax.MkList (Ann a) b c' d) <$>
      traverseOf (traverse.traverse) fromIR_listItem c
    DictComp a b c d ->
      (\c' -> _DictComp # Syntax.MkDictComp (Ann a) b c' d) <$>
      fromIR_comprehension fromIR_dictItem c
    Dict a b c d ->
      (\c' -> _Dict # Syntax.MkDict (Ann a) b c' d) <$>
      traverseOf (traverse.traverse) fromIR_dictItem c
    SetComp a b c d ->
      (\c' -> _SetComp # Syntax.MkSetComp (Ann a) b c' d) <$>
      fromIR_comprehension fromIR_setItem c
    Set a b c d ->
      (\c' -> _Set # Syntax.MkSet (Ann a) b c' d) <$>
      traverse fromIR_setItem c
    Deref a b c d ->
      (\b' -> _Deref # Syntax.MkDeref (Ann a) b' c d) <$>
      fromIR_expr b
    Subscript a b c d e ->
      (\b' d' -> _Subscript # Syntax.MkSubscript (Ann a) b' c d' e) <$>
      fromIR_expr b <*>
      traverse fromIR_subscriptItem d
    Call a b c d e ->
      (\b' d' -> _Call # Syntax.MkCall (Ann a) b' c d' e) <$>
      fromIR_expr b <*>
      traverseOf (traverse.traverse) fromIR_arg d
    None a b -> pure $ _None # Syntax.MkNone (Ann a) b
    Ellipsis a b -> pure $ _Ellipsis # Syntax.MkEllipsis (Ann a) b
    Binary a b c d ->
      (\b' d' -> _Binary # Syntax.MkBinary (Ann a) b' c d') <$>
      fromIR_expr b <*>
      fromIR_expr d
    Unary a b c ->
      (_Unary #) . Syntax.MkUnary (Ann a) b <$> fromIR_expr c
    Parens a b c d ->
      (\c' -> _Parens # Syntax.MkParens (Ann a) b c' d) <$>
      fromIR_expr c
    Ident _ b -> pure $ _Ident # b
    Int a b c -> pure $ _Int # Syntax.MkInt (Ann a) b c
    Float a b c -> pure $ _Float # Syntax.MkFloat (Ann a) b c
    Imag a b c -> pure $ _Imag # Syntax.MkImag (Ann a) b c
    Bool a b c -> pure $ _Bool # Syntax.MkBool (Ann a) b c
    String a b -> pure $ _String # Syntax.MkString (Ann a) b
    Tuple a b c d ->
      (\b' -> (_Tuple #) . Syntax.MkTuple (Ann a) b' c) <$>
      fromIR_tupleItem b <*>
      traverseOf (traverse.traverse) fromIR_tupleItem d
    Not a b c -> (_Not #) . Syntax.MkNot (Ann a) b <$> fromIR_expr c
    Generator a b ->
      (_Generator #) . Syntax.MkGenerator (Ann a) <$>
      fromIR_comprehension (fmap VIdentity . fromIR_expr) b
    Await a b c -> (_Await #) . Syntax.MkAwait (Ann a) b <$> fromIR_expr c

fromIR_suite
  :: AsIRError e a
  => Suite a
  -> Validation (NonEmpty e) (Syntax.Suite '[] a)
fromIR_suite s =
  case s of
    SuiteOne a b c ->
      Syntax.SuiteOne (Ann a) b <$> fromIR_smallStatement c
    SuiteMany a b c d e ->
      Syntax.SuiteMany (Ann a) b c d <$> fromIR_block e

fromIR_param
  :: AsIRError e a
  => Param a
  -> Validation (NonEmpty e) (Syntax.Param Syntax.Expr '[] a)
fromIR_param p =
  case p of
    PositionalParam a b c ->
      Syntax.PositionalParam (Ann a) b <$> traverseOf (traverse._2) fromIR_expr c
    KeywordParam a b c d e ->
      Syntax.KeywordParam (Ann a) b <$>
      traverseOf (traverse._2) fromIR_expr c <*>
      pure d <*>
      fromIR_expr e
    StarParam a b c d ->
      Syntax.StarParam (Ann a) b c <$> traverseOf (traverse._2) fromIR_expr d
    UnnamedStarParam a b -> pure $ Syntax.UnnamedStarParam (Ann a) b
    DoubleStarParam a b c d ->
      Syntax.DoubleStarParam (Ann a) b c <$> traverseOf (traverse._2) fromIR_expr d

fromIR_arg
  :: AsIRError e a
  => Arg a
  -> Validation (NonEmpty e) (Syntax.Arg Syntax.Expr '[] a)
fromIR_arg a =
  case a of
    PositionalArg b c -> Syntax.PositionalArg (Ann b) <$> fromIR_expr c
    KeywordArg b c d e -> Syntax.KeywordArg (Ann b) c d <$> fromIR_expr e
    StarArg b c d -> Syntax.StarArg (Ann b) c <$> fromIR_expr d
    DoubleStarArg b c d -> Syntax.DoubleStarArg (Ann b) c <$> fromIR_expr d

fromIR_decorator
  :: AsIRError e a
  => Decorator a
  -> Validation (NonEmpty e) (Syntax.Decorator '[] a)
fromIR_decorator (Decorator a b c d e f g) =
  (\d' -> Syntax.Decorator (Ann a) b c d' e f g) <$>
  fromIR_expr d

fromIR_exceptAs
  :: AsIRError e a
  => ExceptAs a
  -> Validation (NonEmpty e) (Syntax.ExceptAs '[] a)
fromIR_exceptAs (ExceptAs a b c) =
  (\b' -> Syntax.ExceptAs (Ann a) b' c) <$>
  fromIR_expr b

fromIR_withItem
  :: AsIRError e a
  => WithItem a
  -> Validation (NonEmpty e) (Syntax.WithItem '[] a)
fromIR_withItem (WithItem a b c) =
  Syntax.WithItem (Ann a) <$>
  fromIR_expr b <*>
  traverseOf (traverse._2) fromIR_expr c

fromIR_comprehension
  :: AsIRError e a
  => (ex a -> Validation (NonEmpty e) (h Syntax.Expr '[] a))
  -> Comprehension ex a
  -> Validation (NonEmpty e) (Syntax.Comprehension h Syntax.Expr '[] a)
fromIR_comprehension f (Comprehension a b c d) =
  Syntax.Comprehension (Ann a) <$>
  f b <*>
  fromIR_compFor c <*>
  traverse (bitraverse fromIR_compFor fromIR_compIf) d

fromIR_dictItem
  :: AsIRError e a
  => DictItem a
  -> Validation (NonEmpty e) (Syntax.DictItem Syntax.Expr '[] a)
fromIR_dictItem di =
  case di of
    DictItem a b c d ->
      (\b' -> Syntax.DictItem (Ann a) b' c) <$>
      fromIR_expr b <*>
      fromIR_expr d
    DictUnpack a b c ->
      Syntax.DictUnpack (Ann a) b <$> fromIR_expr c

fromIR_subscriptItem
  :: AsIRError e a
  => SubscriptItem a
  -> Validation (NonEmpty e) (Syntax.SubscriptItem Syntax.Expr '[] a)
fromIR_subscriptItem s =
  case s of
    SubscriptExpr a -> Syntax.SubscriptExpr <$> fromIR_expr a
    SubscriptSlice a b c d ->
      (\a' -> Syntax.SubscriptSlice a' b) <$>
      traverse fromIR_expr a <*>
      traverse fromIR_expr c <*>
      traverseOf (traverse._2.traverse) fromIR_expr d

fromIR_block
  :: AsIRError e a
  => Block a
  -> Validation (NonEmpty e) (Syntax.Block '[] a)
fromIR_block (Block a b c) =
  Syntax.Block a <$>
  fromIR_statement b <*>
  traverseOf (traverse.traverse) fromIR_statement c

fromIR_compFor
  :: AsIRError e a
  => CompFor a
  -> Validation (NonEmpty e) (Syntax.CompFor Syntax.Expr '[] a)
fromIR_compFor (CompFor a b c d e) =
  (\c' -> Syntax.CompFor (Ann a) b c' d) <$>
  fromIR_expr c <*>
  fromIR_expr e

fromIR_compIf
  :: AsIRError e a
  => CompIf a
  -> Validation (NonEmpty e) (Syntax.CompIf Syntax.Expr '[] a)
fromIR_compIf (CompIf a b c) =
  Syntax.CompIf (Ann a) b <$> fromIR_expr c

fromIR_smallStatement
  :: AsIRError e a
  => SmallStatement a
  -> Validation (NonEmpty e) (Syntax.SmallStatement '[] a)
fromIR_smallStatement (MkSmallStatement b c d e f) =
  (\b' c' -> Syntax.MkSmallStatement b' c' d e f) <$>
  fromIR_SimpleStatement b <*>
  traverseOf (traverse._2) fromIR_SimpleStatement c

fromIR_statement
  :: AsIRError e a
  => Statement a
  -> Validation (NonEmpty e) (Syntax.Statement '[] a)
fromIR_statement ex =
  case ex of
    SmallStatement i a ->
      Syntax.SmallStatement i <$> fromIR_smallStatement a
    CompoundStatement a ->
      Syntax.CompoundStatement <$> fromIR_compoundStatement a

fromIR_SimpleStatement
  :: AsIRError e a
  => SimpleStatement a
  -> Validation (NonEmpty e) (Syntax.SimpleStatement '[] a)
fromIR_SimpleStatement ex =
  case ex of
    Assign a b c ->
      Syntax.Assign (Ann a) <$>
      fromIR_expr b <*>
      traverseOf (traverse._2) fromIR_expr c
    Return a b c -> Syntax.Return (Ann a) b <$> traverse fromIR_expr c
    Expr a b -> Syntax.Expr (Ann a) <$> fromIR_expr b
    AugAssign a b c d ->
      (\b' d' -> Syntax.AugAssign (Ann a) b' c d') <$>
      fromIR_expr b <*>
      fromIR_expr d
    Pass a ws -> pure $ Syntax.Pass (Ann a) ws
    Break a ws -> pure $ Syntax.Break (Ann a) ws
    Continue a ws -> pure $ Syntax.Continue (Ann a) ws
    Global a b c -> pure $ Syntax.Global (Ann a) b c
    Nonlocal a b c -> pure $ Syntax.Nonlocal (Ann a) b c
    Del a b c -> Syntax.Del (Ann a) b <$> traverse fromIR_expr c
    Import a b c -> pure $ Syntax.Import (Ann a) b c
    From a b c d e -> pure $ Syntax.From (Ann a) b c d e
    Raise a b c ->
      Syntax.Raise (Ann a) b <$>
      traverse
        (\(x, y) -> (,) <$>
          fromIR_expr x <*>
          traverseOf (traverse._2) fromIR_expr y)
        c
    Assert a b c d ->
      Syntax.Assert (Ann a) b <$>
      fromIR_expr c <*>
      traverseOf (traverse._2) fromIR_expr d

fromIR_compoundStatement
  :: AsIRError e a
  => CompoundStatement a
  -> Validation (NonEmpty e) (Syntax.CompoundStatement '[] a)
fromIR_compoundStatement st =
  case st of
    Fundef a b asyncWs c d e f g h i j ->
      (\b' g' i' -> Syntax.Fundef (Ann a) b' asyncWs c d e f g' h i') <$>
      traverse fromIR_decorator b <*>
      traverse fromIR_param g <*>
      traverseOf (traverse._2) fromIR_expr i <*>
      fromIR_suite j
    If a b c d e f g ->
      Syntax.If (Ann a) b c <$>
      fromIR_expr d <*>
      fromIR_suite e <*>
      traverse (\(x, y, z, w) -> (,,,) x y <$> fromIR_expr z <*> fromIR_suite w) f <*>
      traverseOf (traverse._3) fromIR_suite g
    While a b c d e f ->
      Syntax.While (Ann a) b c <$>
      fromIR_expr d <*>
      fromIR_suite e <*>
      traverseOf (traverse._3) fromIR_suite f
    TryExcept a b c d e f g ->
      Syntax.TryExcept (Ann a) b c <$>
      fromIR_suite d <*>
      traverse
        (\(x, y, z, w) -> (,,,) x y <$> traverse fromIR_exceptAs z <*> fromIR_suite w)
        e <*>
      traverseOf (traverse._3) fromIR_suite f <*>
      traverseOf (traverse._3) fromIR_suite g
    TryFinally a b c d e f g ->
      (\d' -> Syntax.TryFinally (Ann a) b c d' e f) <$> fromIR_suite d <*> fromIR_suite g
    For a b asyncWs c d e f g h ->
      (\d' -> Syntax.For (Ann a) b asyncWs c d' e) <$>
      fromIR_expr d <*>
      traverse fromIR_expr f <*>
      fromIR_suite g <*>
      traverseOf (traverse._3) fromIR_suite h
    ClassDef a b c d e f g ->
      (\b' -> Syntax.ClassDef (Ann a) b' c d e) <$>
      traverse fromIR_decorator b <*>
      traverseOf (traverse._2.traverse.traverse) fromIR_arg f <*>
      fromIR_suite g
    With a b asyncWs c d e ->
      Syntax.With (Ann a) b asyncWs c <$>
      traverse fromIR_withItem d <*>
      fromIR_suite e

fromIR_listItem
  :: AsIRError e a
  => Expr a
  -> Validation (NonEmpty e) (Syntax.ListItem Syntax.Expr '[] a)
fromIR_listItem (StarExpr a b c) =
  Syntax.ListUnpack (Ann a) [] b <$> fromIR_expr c
fromIR_listItem (Parens a b c d) =
  (\case
      Syntax.ListUnpack w x y z -> Syntax.ListUnpack w ((b, d) : x) y z
      Syntax.ListItem x y ->
        Syntax.ListItem (Ann a) (_Parens # Syntax.MkParens x b y d)) <$>
  fromIR_listItem c
fromIR_listItem e = (\x -> Syntax.ListItem (x ^. annot) x) <$> fromIR_expr e

fromIR_tupleItem
  :: AsIRError e a
  => Expr a
  -> Validation (NonEmpty e) (Syntax.TupleItem Syntax.Expr '[] a)
fromIR_tupleItem (StarExpr a b c) =
  Syntax.TupleUnpack (Ann a) [] b <$> fromIR_expr c
fromIR_tupleItem (Parens a b c d) =
  (\case
      Syntax.TupleUnpack w x y z -> Syntax.TupleUnpack w ((b, d) : x) y z
      Syntax.TupleItem x y ->
        Syntax.TupleItem (Ann a) (_Parens # Syntax.MkParens x b y d)) <$>
  fromIR_tupleItem c
fromIR_tupleItem e =
  (\x -> Syntax.TupleItem (x ^. annot) x) <$> fromIR_expr e

fromIR_setItem
  :: AsIRError e a
  => Expr a
  -> Validation (NonEmpty e) (Syntax.SetItem Syntax.Expr '[] a)
fromIR_setItem (StarExpr a b c) =
  Syntax.SetUnpack (Ann a) [] b <$> fromIR_expr c
fromIR_setItem (Parens a b c d) =
  (\case
      Syntax.SetUnpack w x y z -> Syntax.SetUnpack w ((b, d) : x) y z
      Syntax.SetItem x y ->
        Syntax.SetItem (Ann a) (_Parens # Syntax.MkParens x b y d)) <$>
  fromIR_setItem c
fromIR_setItem e = (\x -> Syntax.SetItem (x ^. annot) x) <$> fromIR_expr e

fromIR
  :: AsIRError e a
  => Module a
  -> Validation (NonEmpty e) (Syntax.Module '[] a)
fromIR ModuleEmpty = pure Syntax.ModuleEmpty
fromIR (ModuleBlankFinal a) = pure $ Syntax.ModuleBlankFinal a
fromIR (ModuleBlank a b c) = Syntax.ModuleBlank a b <$> fromIR c
fromIR (ModuleStatement a b) = Syntax.ModuleStatement <$> fromIR_statement a <*> fromIR b
