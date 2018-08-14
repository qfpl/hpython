{-# language LambdaCase #-}
{-# language DataKinds, KindSignatures #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language ExistentialQuantification #-}
module Language.Python.Internal.Syntax.Expr where

import Control.Lens.Cons (_last)
import Control.Lens.Fold ((^?), (^?!))
import Control.Lens.Getter ((^.), getting)
import Control.Lens.Lens (Lens, Lens', lens)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Prism (_Just, _Left, _Right)
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (Traversal, failing)
import Data.Bifunctor (bimap)
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Digit.Integral (integralDecDigits)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import GHC.Generics (Generic)

import Language.Python.Internal.Syntax.BinOp
import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Numbers
import Language.Python.Internal.Syntax.Strings
import Language.Python.Internal.Syntax.UnOp
import Language.Python.Internal.Syntax.Whitespace

-- | 'Traversal' over all the expressions in a term
class HasExprs s where
  _Exprs :: Traversal (s v a) (s '[] a) (Expr v a) (Expr '[] a)

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

data Arg (v :: [*]) a
  = PositionalArg
  { _argAnn :: a
  , _argExpr :: Expr v a
  }
  | KeywordArg
  { _argAnn :: a
  , _unsafeKeywordArgName :: Ident v a
  , _unsafeKeywordArgWhitespaceRight :: [Whitespace]
  , _argExpr :: Expr v a
  }
  | StarArg
  { _argAnn :: a
  , _unsafeStarArgWhitespace :: [Whitespace]
  , _argExpr :: Expr v a
  }
  | DoubleStarArg
  { _argAnn :: a
  , _unsafeDoubleStarArgWhitespace :: [Whitespace]
  , _argExpr :: Expr v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance IsString (Arg '[] ()) where; fromString = PositionalArg () . fromString

argExpr :: Lens (Arg v a) (Arg '[] a) (Expr v a) (Expr '[] a)
argExpr = lens _argExpr (\s a -> (coerce s) { _argExpr = a })

instance HasExprs Arg where
  _Exprs f (KeywordArg a name ws2 expr) = KeywordArg a (coerce name) ws2 <$> f expr
  _Exprs f (PositionalArg a expr) = PositionalArg a <$> f expr
  _Exprs f (StarArg a ws expr) = StarArg a ws <$> f expr
  _Exprs f (DoubleStarArg a ws expr) = StarArg a ws <$> f expr

data Comprehension (v :: [*]) a
  -- ^ <expr> <comp_for> (comp_for | comp_if)*
  = Comprehension a (Expr v a) (CompFor v a) [Either (CompFor v a) (CompIf v a)]
  deriving (Eq, Show)

instance HasTrailingWhitespace (Comprehension v a) where
  trailingWhitespace =
    lens
      (\(Comprehension _ _ a b) ->
         case b of
           [] -> a ^. trailingWhitespace
           _ -> b ^?! _last.failing (_Left.trailingWhitespace) (_Right.trailingWhitespace))
      (\(Comprehension a b c d) ws ->
         case d of
           [] -> Comprehension a b (c & trailingWhitespace .~ ws) d
           _ ->
             Comprehension a b c
               (d &
                _last.failing (_Left.trailingWhitespace) (_Right.trailingWhitespace) .~ ws))

instance Functor (Comprehension v) where
  fmap f (Comprehension a b c d) =
    Comprehension (f a) (fmap f b) (fmap f c) (fmap (bimap (fmap f) (fmap f)) d)

instance Foldable (Comprehension v) where
  foldMap f (Comprehension a b c d) =
    f a <> foldMap f b <> foldMap f c <> foldMap (bifoldMap (foldMap f) (foldMap f)) d

instance Traversable (Comprehension v) where
  traverse f (Comprehension a b c d) =
    Comprehension <$>
    f a <*>
    traverse f b <*>
    traverse f c <*>
    traverse (bitraverse (traverse f) (traverse f)) d

data CompIf (v :: [*]) a
  -- ^ 'if' <any_spaces> <expr>
  = CompIf a [Whitespace] (Expr v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (CompIf v a) where
  trailingWhitespace =
    lens
      (\(CompIf _ _ a) -> a ^. trailingWhitespace)
      (\(CompIf a b c) ws -> CompIf a b $ c & trailingWhitespace .~ ws)

data CompFor (v :: [*]) a
  -- ^ 'for' <any_spaces> <targets> 'in' <any_spaces> <expr>
  = CompFor a [Whitespace] (Expr v a) [Whitespace] (Expr v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (CompFor v a) where
  trailingWhitespace =
    lens
      (\(CompFor _ _ _ _ a) -> a ^. trailingWhitespace)
      (\(CompFor a b c d e) ws -> CompFor a b c d $ e & trailingWhitespace .~ ws)

data DictItem (v :: [*]) a
  = DictItem
  { _dictItemAnn :: a
  , _unsafeDictItemKey :: Expr v a
  , _unsafeDictItemWhitespace :: [Whitespace]
  , _unsafeDictItemvalue :: Expr v a
  }
  | DictUnpack
  { _dictItemAnn :: a
  , _unsafeDictItemUnpackWhitespace :: [Whitespace]
  , _unsafeDictItemUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (DictItem v a) where
  trailingWhitespace =
    lens
      (\(DictItem _ _ _ a) -> a ^. trailingWhitespace)
      (\(DictItem a b c d) ws -> DictItem a b c (d & trailingWhitespace .~ ws))

data Subscript (v :: [*]) a
  = SubscriptExpr (Expr v a)
  | SubscriptSlice
      -- [expr]
      (Maybe (Expr v a))
      -- ':' <spaces>
      [Whitespace]
      -- [expr]
      (Maybe (Expr v a))
      -- [':' [expr]]
      (Maybe ([Whitespace], Maybe (Expr v a)))
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasTrailingWhitespace (Subscript v a) where
  trailingWhitespace =
    lens
      (\case
          SubscriptExpr e -> e ^. trailingWhitespace
          SubscriptSlice _ b c d ->
            case d of
              Nothing ->
                case c of
                  Nothing -> b
                  Just e -> e ^. trailingWhitespace
              Just (e, f) ->
                case f of
                  Nothing -> e
                  Just g -> g ^. trailingWhitespace)
      (\x ws ->
         case x of
          SubscriptExpr e -> SubscriptExpr $ e & trailingWhitespace .~ ws
          SubscriptSlice a b c d ->
            (\(b', c', d') -> SubscriptSlice a b' c' d') $
            case d of
              Nothing ->
                case c of
                  Nothing -> (ws, c, d)
                  Just e -> (b, Just $ e & trailingWhitespace .~ ws, d)
              Just (e, f) ->
                case f of
                  Nothing -> (b, c, Just (ws, f))
                  Just g -> (b, c, Just (e, Just $ g & trailingWhitespace .~ ws)))

data ListItem v a
  = ListItem
  { _listItemAnn :: a
  , _unsafeListItemValue :: Expr v a
  }
  | ListUnpack
  { _listItemAnn :: a
  , _unsafeListUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeListUnpackWhitespace :: [Whitespace]
  , _unsafeListUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasExprs ListItem where
  _Exprs f (ListItem a b) = ListItem a <$> f b
  _Exprs f (ListUnpack a b c d) = ListUnpack a b c <$> f d

instance HasTrailingWhitespace (ListItem v a) where
  trailingWhitespace =
    lens
      (\case
          ListItem _ a -> a ^. trailingWhitespace
          ListUnpack _ [] _ a -> a ^. trailingWhitespace
          ListUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           ListItem b c -> ListItem b $ c & trailingWhitespace .~ ws
           ListUnpack b [] d e -> ListUnpack b [] d $ e & trailingWhitespace .~ ws
           ListUnpack b ((c, _) : rest) e f -> ListUnpack b ((c, ws) : rest) e f)

data SetItem v a
  = SetItem
  { _setItemAnn :: a
  , _unsafeSetItemValue :: Expr v a
  }
  | SetUnpack
  { _setItemAnn :: a
  , _unsafeSetUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeSetUnpackWhitespace :: [Whitespace]
  , _unsafeSetUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasExprs SetItem where
  _Exprs f (SetItem a b) = SetItem a <$> f b
  _Exprs f (SetUnpack a b c d) = SetUnpack a b c <$> f d

instance HasTrailingWhitespace (SetItem v a) where
  trailingWhitespace =
    lens
      (\case
          SetItem _ a -> a ^. trailingWhitespace
          SetUnpack _ [] _ a -> a ^. trailingWhitespace
          SetUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           SetItem b c -> SetItem b $ c & trailingWhitespace .~ ws
           SetUnpack b [] d e -> SetUnpack b [] d $ e & trailingWhitespace .~ ws
           SetUnpack b ((c, _) : rest) e f -> SetUnpack b ((c, ws) : rest) e f)

data TupleItem v a
  = TupleItem
  { _tupleItemAnn :: a
  , _unsafeTupleItemValue :: Expr v a
  }
  | TupleUnpack
  { _tupleItemAnn :: a
  , _unsafeTupleUnpackParens :: [([Whitespace], [Whitespace])]
  , _unsafeTupleUnpackWhitespace :: [Whitespace]
  , _unsafeTupleUnpackValue :: Expr v a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasExprs TupleItem where
  _Exprs f (TupleItem a b) = TupleItem a <$> f b
  _Exprs f (TupleUnpack a b c d) = TupleUnpack a b c <$> f d

instance HasTrailingWhitespace (TupleItem v a) where
  trailingWhitespace =
    lens
      (\case
          TupleItem _ a -> a ^. trailingWhitespace
          TupleUnpack _ [] _ a -> a ^. trailingWhitespace
          TupleUnpack _ ((_, ws) : _) _ _ -> ws)
      (\a ws ->
         case a of
           TupleItem b c -> TupleItem b $ c & trailingWhitespace .~ ws
           TupleUnpack b [] d e -> TupleUnpack b [] d $ e & trailingWhitespace .~ ws
           TupleUnpack b ((c, _) : rest) e f -> TupleUnpack b ((c, ws) : rest) e f)

data Expr (v :: [*]) a
  = Unit
  { _exprAnnotation :: a
  , _unsafeUnitWhitespaceInner :: [Whitespace]
  , _unsafeUnitWhitespaceRight :: [Whitespace]
  }
  | Lambda
  { _exprAnnotation :: a
  , _unsafeLambdaWhitespace :: [Whitespace]
  , _unsafeLambdaArgs :: CommaSep (Param v a)
  , _unsafeLambdaColon :: [Whitespace]
  , _unsafeLambdaBody :: Expr v a
  }
  | Yield
  { _exprAnnotation :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeYieldValue :: Maybe (Expr v a)
  }
  | YieldFrom
  { _exprAnnotation :: a
  , _unsafeYieldWhitespace :: [Whitespace]
  , _unsafeFromWhitespace :: [Whitespace]
  , _unsafeYieldFromValue :: Expr v a
  }
  | Ternary
  { _exprAnnotation :: a
  -- expr
  , _unsafeTernaryValue :: Expr v a
  -- 'if' spaces
  , _unsafeTernaryWhitespaceIf :: [Whitespace]
  -- expr
  , _unsafeTernaryCond :: Expr v a
  -- 'else' spaces
  , _unsafeTernaryWhitespaceElse :: [Whitespace]
  -- expr
  , _unsafeTernaryElse :: Expr v a
  }
  | ListComp
  { _exprAnnotation :: a
  -- [ spaces
  , _unsafeListCompWhitespaceLeft :: [Whitespace]
  -- comprehension
  , _unsafeListCompValue :: Comprehension v a
  -- ] spaces
  , _unsafeListCompWhitespaceRight :: [Whitespace]
  }
  | List
  { _exprAnnotation :: a
  -- [ spaces
  , _unsafeListWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeListValues :: Maybe (CommaSep1' (ListItem v a))
  -- ] spaces
  , _unsafeListWhitespaceRight :: [Whitespace]
  }
  | Dict
  { _exprAnnotation :: a
  , _unsafeDictWhitespaceLeft :: [Whitespace]
  , _unsafeDictValues :: Maybe (CommaSep1' (DictItem v a))
  , _unsafeDictWhitespaceRight :: [Whitespace]
  }
  | Set
  { _exprAnnotation :: a
  , _unsafeSetWhitespaceLeft :: [Whitespace]
  , _unsafeSetValues :: CommaSep1' (SetItem v a)
  , _unsafeSetWhitespaceRight :: [Whitespace]
  }
  | Deref
  { _exprAnnotation :: a
  -- expr
  , _unsafeDerefValueLeft :: Expr v a
  -- . spaces
  , _unsafeDerefWhitespaceLeft :: [Whitespace]
  -- ident
  , _unsafeDerefValueRight :: Ident v a
  }
  | Subscript
  { _exprAnnotation :: a
  -- expr
  , _unsafeSubscriptValueLeft :: Expr v a
  -- [ spaces
  , _unsafeSubscriptWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeSubscriptValueRight :: CommaSep1' (Subscript v a)
  -- ] spaces
  , _unsafeSubscriptWhitespaceRight :: [Whitespace]
  }
  | Call
  { _exprAnnotation :: a
  -- expr
  , _unsafeCallFunction :: Expr v a
  -- ( spaces
  , _unsafeCallWhitespaceLeft :: [Whitespace]
  -- exprs
  , _unsafeCallArguments :: Maybe (CommaSep1' (Arg v a))
  -- ) spaces
  , _unsafeCallWhitespaceRight :: [Whitespace]
  }
  | None
  { _exprAnnotation :: a
  , _unsafeNoneWhitespace :: [Whitespace]
  }
  | BinOp
  { _exprAnnotation :: a
  , _unsafeBinOpExprLeft :: Expr v a
  , _unsafeBinOpOp :: BinOp a
  , _unsafeBinOpExprRight :: Expr v a
  }
  | UnOp
  { _exprAnnotation :: a
  , _unsafeUnOpOp :: UnOp a
  , _unsafeUnOpValue :: Expr v a
  }
  | Parens
  { _exprAnnotation :: a
  -- ( spaces
  , _unsafeParensWhitespaceLeft :: [Whitespace]
  -- expr
  , _unsafeParensValue :: Expr v a
  -- ) spaces
  , _unsafeParensWhitespaceAfter :: [Whitespace]
  }
  | Ident
  { _exprAnnotation :: a
  , _unsafeIdentValue :: Ident v a
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
  | Bool
  { _exprAnnotation :: a
  , _unsafeBoolValue :: Bool
  , _unsafeBoolWhitespace :: [Whitespace]
  }
  | String
  { _exprAnnotation :: a
  , _unsafeStringValue :: NonEmpty (StringLiteral a)
  }
  | Tuple
  { _exprAnnotation :: a
  -- expr
  , _unsafeTupleHead :: TupleItem v a
  -- , spaces
  , _unsafeTupleWhitespace :: [Whitespace]
  -- [exprs]
  , _unsafeTupleTail :: Maybe (CommaSep1' (TupleItem v a))
  }
  | Not
  { _exprAnnotation :: a
  , _unsafeNotWhitespace :: [Whitespace]
  , _unsafeNotValue :: Expr v a
  }
  | Generator
  { _exprAnnotation :: a
  , _generatorValue :: Comprehension v a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasTrailingWhitespace (Expr v a) where
  trailingWhitespace =
    lens
      (\case
          Unit _ _ a -> a
          Lambda _ _ _ _ a -> a ^. trailingWhitespace
          Yield _ ws Nothing -> ws
          Yield _ _ (Just e) -> e ^. trailingWhitespace
          YieldFrom _ _ _ e -> e ^. trailingWhitespace
          Ternary _ _ _ _ _ e -> e ^. trailingWhitespace
          None _ ws -> ws
          List _ _ _ ws -> ws
          ListComp _ _ _ ws -> ws
          Deref _ _ _ a -> a ^. trailingWhitespace
          Subscript _ _ _ _ ws -> ws
          Call _ _ _ _ ws -> ws
          BinOp _ _ _ e -> e ^. trailingWhitespace
          UnOp _ _ e -> e ^. trailingWhitespace
          Parens _ _ _ ws -> ws
          Ident _ a -> a ^. getting trailingWhitespace
          Int _ _ ws -> ws
          Float _ _ ws -> ws
          Bool _ _ ws -> ws
          String _ v -> v ^. trailingWhitespace
          Not _ _ e -> e ^. trailingWhitespace
          Tuple _ _ ws Nothing -> ws
          Tuple _ _ _ (Just cs) -> cs ^. trailingWhitespace
          Dict _ _ _ ws -> ws
          Set _ _ _ ws -> ws
          Generator  _ a -> a ^. trailingWhitespace)
      (\e ws ->
        case e of
          Unit a b _ -> Unit a b ws
          Lambda a b c d f -> Lambda a b c d (f & trailingWhitespace .~ ws)
          Yield a _ Nothing -> Yield a ws Nothing
          Yield a b (Just c) -> Yield a b (Just $ c & trailingWhitespace .~ ws)
          YieldFrom a b c d -> YieldFrom a b c (d & trailingWhitespace .~ ws)
          Ternary a b c d e f -> Ternary a b c d e (f & trailingWhitespace .~ ws)
          None a _ -> None a ws
          List a b c _ -> List a b (coerce c) ws
          ListComp a b c _ -> ListComp a b (coerce c) ws
          Deref a b c d -> Deref a (coerce b) c (d & trailingWhitespace .~ ws)
          Subscript a b c d _ -> Subscript a (coerce b) c d ws
          Call a b c d _ -> Call a (coerce b) c (coerce d) ws
          BinOp a b c e -> BinOp a (coerce b) c (e & trailingWhitespace .~ ws)
          UnOp a b c -> UnOp a b (c & trailingWhitespace .~ ws)
          Parens a b c _ -> Parens a b (coerce c) ws
          Ident a b -> Ident a (b & trailingWhitespace .~ ws)
          Int a b _ -> Int a b ws
          Float a b _ -> Float a b ws
          Bool a b _ -> Bool a b ws
          String a v -> String a (v & trailingWhitespace .~ ws)
          Not a b c -> Not a b (c & trailingWhitespace .~ ws)
          Tuple a e _ Nothing -> Tuple a (coerce e) ws Nothing
          Tuple a b ws (Just cs) ->
            Tuple a (coerce b) ws (Just $ cs & trailingWhitespace .~ ws)
          Dict a b c _ -> Dict a b c ws
          Set a b c _ -> Set a b c ws
          Generator a b -> Generator a $ b & trailingWhitespace .~ ws)

instance IsString (Expr '[] ()) where
  fromString s = Ident () (MkIdent () s [])

instance Num (Expr '[] ()) where
  fromInteger n
    | n >= 0 = Int () (IntLiteralDec () $ integralDecDigits n ^?! _Right) []
    | otherwise =
        UnOp
          ()
          (Negate () [])
          (Int () (IntLiteralDec () $ integralDecDigits (-n) ^?! _Right) [])

  negate = UnOp () (Negate () [])

  (+) a = BinOp () (a & trailingWhitespace .~ [Space]) (Plus () [Space])
  (*) a = BinOp () (a & trailingWhitespace .~ [Space]) (Multiply () [Space])
  (-) a = BinOp () (a & trailingWhitespace .~ [Space]) (Minus () [Space])
  signum = undefined
  abs = undefined

instance Plated (Expr '[] a) where; plate = gplate

instance HasExprs Expr where
  _Exprs = id

shouldBracketLeft :: BinOp a -> Expr v a -> Bool
shouldBracketLeft op left =
  let
    entry = lookupOpEntry op operatorTable

    lEntry =
      case left of
        BinOp _ _ lOp _ -> Just $ lookupOpEntry lOp operatorTable
        _ -> Nothing

    leftf =
      case entry ^. opAssoc of
        R | Just (OpEntry _ prec R) <- lEntry -> prec <= entry ^. opPrec
        _ -> False

    leftf' =
      case (left, op) of
        (UnOp{}, Exp{}) -> True
        (Tuple{}, _) -> True
        (Not{}, BoolAnd{}) -> False
        (Not{}, BoolOr{}) -> False
        (Not{}, _) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (lEntry ^? _Just.opPrec)
  in
    leftf || leftf'

shouldBracketRight :: BinOp a -> Expr v a -> Bool
shouldBracketRight op right =
  let
    entry = lookupOpEntry op operatorTable

    rEntry =
      case right of
        BinOp _ _ rOp _ -> Just $ lookupOpEntry rOp operatorTable
        _ -> Nothing

    rightf =
      case entry ^. opAssoc of
        L | Just (OpEntry _ prec L) <- rEntry -> prec <= entry ^. opPrec
        _ -> False

    rightf' =
      case (op, right) of
        (_, Tuple{}) -> True
        (BoolAnd{}, Not{}) -> False
        (BoolOr{}, Not{}) -> False
        (_, Not{}) -> True
        _ -> maybe False (\p -> p < entry ^. opPrec) (rEntry ^? _Just.opPrec)
  in
    rightf || rightf'

makeLenses ''Expr
