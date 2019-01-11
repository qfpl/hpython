{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Generators.Correct where

import Control.Applicative
import Control.Lens.Cons (snoc)
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Iso (from)
import Control.Lens.Plated
import Control.Lens.Prism (_Just)
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.TH
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Function
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List.NonEmpty as NonEmpty

import GHC.Stack

import Data.VFix
import Data.VIdentity
import Language.Python.Optics
import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Import
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace
import Language.Python.Validate.Syntax (reservedWords)

import Generators.Common
import Generators.Sized

initialGenState :: GenState
initialGenState =
  GenState
  { _inFunction = Nothing
  , _inGenerator = False
  , _currentNonlocals = []
  , _willBeNonlocals = []
  , _inLoop = False
  , _inClass = False
  , _inFinally = False
  , _currentIndentation = Indents [] (Ann ())
  }

data GenState
  = GenState
  { _inFunction :: Maybe ([String], Bool)
  , _inGenerator :: Bool
  , _currentNonlocals :: [String]
  , _willBeNonlocals :: [String]
  , _inLoop :: Bool
  , _inClass :: Bool
  , _inFinally :: Bool
  , _currentIndentation :: Indents ()
  }
makeLenses ''GenState

doIndent :: (MonadGen m, MonadState GenState m) => m ()
doIndent = do
  is <- Gen.list (Range.constant 1 5) (Gen.element [Space, Tab])
  modifying (currentIndentation.indentsValue) (<> [is ^. from indentWhitespaces])

doDedent :: (MonadGen m, MonadState GenState m) => m ()
doDedent = do
  is <- use $ currentIndentation.indentsValue
  case is of
    [] -> pure ()
    _ -> assign (currentIndentation.indentsValue) $ init is

localState :: MonadState s m => m a -> m a
localState m = do
  a <- get
  b <- m
  put a
  pure b

genBlank :: MonadGen m => m (Blank ())
genBlank =
  Blank (Ann ()) <$>
  Gen.list (Range.constant 0 10) (Gen.element [Space, Tab]) <*>
  Gen.maybe genComment

genIdent :: (MonadGen m, MonadState GenState m) => m (Ident '[] ())
genIdent = do
  isAsync <- maybe False snd <$> use inFunction
  let reserved = reservedWords <> (if isAsync then ["async", "await"] else [])
  Gen.filter
    (\i -> not $ any (`isPrefixOf` _identValue i) reserved) $
    MkIdent (Ann ()) <$>
    liftA2 (:)
      (Gen.choice [Gen.alpha, pure '_'])
      (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_'])) <*>
    genWhitespaces

genDeletableTuple :: (MonadGen m, MonadState GenState m) => m (Expr '[] ()) -> m (Expr '[] ())
genDeletableTuple expr =
  fmap VIn $
  Tuple (Ann ()) <$>
  (TupleItem (Ann ()) <$> expr) <*>
  genComma <*>
  Gen.maybe (genSizedCommaSep1' $ TupleItem (Ann ()) <$> expr)

genTuple :: (MonadGen m, MonadState GenState m) => m (Expr '[] ()) -> m (Expr '[] ())
genTuple expr =
  fmap VIn $
  Tuple (Ann ()) <$>
  genTupleItem genWhitespaces expr <*>
  genComma <*>
  Gen.maybe (genSizedCommaSep1' $ genTupleItem genWhitespaces expr)

genAssignableTuple :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genAssignableTuple =
  (\(ti, tis) ws -> VIn $ Tuple (Ann ()) ti ws tis) <$> genTupleItems <*> genComma
  where
    genTupleItems =
      sizedBind
        (genTupleItem genWhitespaces genAssignable)
        (\ti -> (,) ti <$> genTupleItemsRest (has _TupleUnpack ti))

    genTupleItemsRest seen =
      Gen.sized $ \n ->
      if n == 0
      then pure Nothing
      else
        sizedBind
          (if seen
           then TupleItem (Ann ()) <$> genAssignable
           else genTupleItem genWhitespaces genAssignable)
          (\ti ->
             sizedMaybe
               (view _CommaSep1' <$> genTupleItemsRest' (seen || has _TupleUnpack ti) ti []))

    genTupleItemsRest' seen a as =
      Gen.sized $ \n ->
      if n == 0
      then (,,) a as <$> Gen.maybe genComma
      else
        sizedBind
          (if seen
           then TupleItem (Ann ()) <$> genAssignable
           else genTupleItem genWhitespaces genAssignable)
          (\ti -> do
              comma <- genComma
              genTupleItemsRest' (seen || has _TupleUnpack ti) ti ((comma, a) : as))

genModuleName :: (MonadGen m, MonadState GenState m) => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne (Ann ()) <$> genIdent ]
  [ ModuleNameMany (Ann ()) <$>
    genIdent <*>
    genDot <*>
    genModuleName
  ]

genRelativeModuleName :: (MonadGen m, MonadState GenState m) => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative (Ann ()) <$>
    Gen.nonEmpty (Range.constant 1 10) genDot
  , RelativeWithName (Ann ()) <$>
    Gen.list (Range.constant 1 10) genDot <*>
    genModuleName
  ]

genImportTargets :: (MonadGen m, MonadState GenState m) => m (ImportTargets '[] ())
genImportTargets = do
  ctxt <- get
  let
    isInFunction = isJust $ _inFunction ctxt
    isInClass = _inClass ctxt
  Gen.choice $
    [ ImportSome (Ann ()) <$>
      genSizedCommaSep1 (genImportAs genIdent genIdent)
    , ImportSomeParens (Ann ()) <$>
      genWhitespaces <*>
      genSizedCommaSep1' (genImportAs genIdent genIdent) <*>
      genWhitespaces
    ] ++
    [ ImportAll (Ann ()) <$> genWhitespaces
    | not isInFunction && not isInClass
    ]

genBlock :: (MonadGen m, MonadState GenState m) => m (Block '[] ())
genBlock = doIndent *> go <* doDedent
  where
    genLine =
      Gen.choice
        [ Right <$> genStatement
        , Left <$> ((,) <$> genBlank <*> genNewline)
        ]

    go =
      sizedBind genStatement $ \st ->
      Block <$>
        Gen.list (Range.constant 0 10) ((,) <$> genBlank <*> genNewline) <*>
        pure st <*>
        sizedList genLine

genPositionalArg :: (MonadGen m, MonadState GenState m) => m (Arg Expr '[] ())
genPositionalArg =
  sizedRecursive
    [ PositionalArg (Ann ()) <$> genExpr
    , StarArg (Ann ()) <$> genWhitespaces <*> genExpr
    ]
    []

genKeywordArg :: (MonadGen m, MonadState GenState m) => m (Arg Expr '[] ())
genKeywordArg =
  sizedRecursive
    [ KeywordArg (Ann ()) <$> genIdent <*> genWhitespaces <*> genExpr
    , DoubleStarArg (Ann ()) <$> genWhitespaces <*> genExpr
    ]
    []

genArgs :: (MonadGen m, MonadState GenState m) => m (CommaSep1' (Arg Expr '[] ()))
genArgs =
  sized4
    (\a b c d -> (a, b <> c, d) ^. _CommaSep1')
    genPositionalArg
    (sizedList $ (,) <$> genComma <*> genPositionalArg)
    (sizedList $ (,) <$> genComma <*> genKeywordArg)
    (Gen.maybe genComma)

genPositionalParams
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ This is for a lambda
  -> m (CommaSep (Param Expr '[] ()))
genPositionalParams isLambda =
  Gen.scale (max 0 . subtract 1) $
  Gen.sized $ fmap (listToCommaSep . fmap (uncurry $ PositionalParam (Ann ()))) . go []
  where
    go _    0 = pure []
    go seen n = do
      i <- Gen.filter ((`notElem` seen) . _identValue) genIdent
      mty <-
        if isLambda
        then pure Nothing
        else sizedMaybe ((,) <$> genColonAny <*> genExpr)
      ((i, mty) :) <$> go (_identValue i : seen) (n-1)

genKeywordParam
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ This is for a lambda
  -> [String]
  -> m (Param Expr '[] ())
genKeywordParam isLambda positionals =
  Gen.scale (max 0 . subtract 1) $
  KeywordParam (Ann ()) <$>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent <*>
  (if isLambda then pure Nothing else sizedMaybe ((,) <$> genColonAny <*> genExpr)) <*>
  genWhitespaces <*>
  genExpr

genStarParam
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ This is for a lambda
  -> [String]
  -> m (Param Expr '[] ())
genStarParam isLambda positionals = Gen.choice [namedStarParam, unnamedStarParam]
  where
    unnamedStarParam = UnnamedStarParam (Ann ()) <$> genWhitespaces
    namedStarParam =
      Gen.scale (max 0 . subtract 1) $ do
        ident <- Gen.filter (\i -> _identValue i `notElem` positionals) genIdent
        mty <-
          if isLambda
          then pure Nothing
          else sizedMaybe ((,) <$> genColonAny <*> genExpr)
        StarParam (Ann ()) <$>
          genWhitespaces <*>
          pure ident <*>
          pure mty

genDoubleStarParam
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ This is for a lambda
  -> [String]
  -> m (Param Expr '[] ())
genDoubleStarParam isLambda positionals =
  Gen.scale (max 0 . subtract 1) $
  DoubleStarParam (Ann ()) <$>
  genWhitespaces <*>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent <*>
  (if isLambda then pure Nothing else sizedMaybe ((,) <$> genColonAny <*> genExpr))

genParams
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ These are for a lambda
  -> m (CommaSep (Param Expr '[] ()))
genParams isLambda =
  sizedBind (genPositionalParams isLambda) $ \pparams ->
  let pparamNames = pparams ^.. folded.paramName.identValue in

  sizedBind (sizedMaybe $ genStarParam isLambda pparamNames) $ \sp ->
  let pparamNames' = pparamNames <> (sp ^.. _Just.paramName.identValue) in

  sizedBind (sizedCommaSep (genKeywordParam isLambda pparamNames')) $ \kwparams ->
  let
    pparamNames'' = pparamNames' <> kwparams ^.. folded.paramName.identValue
  in do
    kwparams' <-
      if has (folded._UnnamedStarParam) sp && null kwparams
      then CommaSepOne <$> genKeywordParam isLambda pparamNames''
      else pure kwparams

    sizedBind (sizedMaybe $ genDoubleStarParam isLambda pparamNames'') $ \dsp ->

      pure $
        pparams <> maybeToCommaSep sp <>
          kwparams' <> maybeToCommaSep dsp

genDeletableList :: (MonadState GenState m, MonadGen m) => m (Expr '[] ()) -> m (Expr '[] ())
genDeletableList genExpr' =
  Gen.shrink
    (\e -> case vout e of
        List _ _ (Just (CommaSepOne1' e _)) _ -> e ^.. _Exprs
        _ -> []) .
  fmap VIn $
  List (Ann ()) <$>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' $ ListItem (Ann ()) <$> genExpr') <*>
  genWhitespaces

genList :: (MonadState GenState m, MonadGen m) => m (Expr '[] ()) -> m (Expr '[] ())
genList genExpr' =
  Gen.shrink
    (\e -> case vout e of
        List _ _ (Just (CommaSepOne1' e _)) _ -> e ^.. _Exprs
        _ -> []) .
  fmap VIn $
  List (Ann ()) <$>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' $ genListItem genWhitespaces genExpr') <*>
  genWhitespaces

genAssignableList :: (MonadState GenState m, MonadGen m) => m (Expr '[] ())
genAssignableList =
  Gen.shrink
    (\e -> case vout e of
        List _ _ (Just (CommaSepOne1' e _)) _ -> e ^.. _Exprs
        _ -> []) .
  fmap VIn $
  List (Ann ()) <$>
  genWhitespaces <*>
  genListItems <*>
  genWhitespaces
  where
    genListItems =
      Gen.sized $ \n ->
      if n == 0
      then pure Nothing
      else
        sizedBind
          (genListItem genWhitespaces genAssignable)
          (\ti -> Just . view _CommaSep1' <$> genListItemsRest (has _ListUnpack ti) ti [])

    genListItemsRest seen a as =
      Gen.sized $ \n ->
      if n == 0
      then (,,) a as <$> Gen.maybe genComma
      else
        sizedBind
          (if seen
           then ListItem (Ann ()) <$> genAssignable
           else genListItem genWhitespaces genAssignable)
          (\ti -> do
              comma <- genComma
              genListItemsRest (seen || has _ListUnpack ti) ti ((comma, a) : as))

genParens :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genParens genExpr' =
  fmap VIn $
  Parens (Ann ()) <$> genWhitespaces <*> genExpr' <*> genWhitespaces

genDeref :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genDeref =
  fmap VIn $
  Deref (Ann ()) <$>
  genExpr <*>
  genWhitespaces <*>
  genIdent

genCompFor :: (MonadGen m, MonadState GenState m) => m (CompFor Expr '[] ())
genCompFor =
  sized2M
    (\a b ->
       (\ws1 ws2 -> CompFor (Ann ()) ws1 a ws2 b) <$>
       genWhitespaces <*>
       genWhitespaces)
    (localState $ do
        inGenerator .= True
        genAssignable)
    (Gen.filter (\e -> case vout e of; Tuple{} -> False; _ -> True) genExpr)

genCompIf :: (MonadGen m, MonadState GenState m) => m (CompIf Expr '[] ())
genCompIf =
  localState $ do
    inGenerator .= True
    CompIf (Ann ()) <$>
      genWhitespaces <*>
      sizedRecursive
        [ Gen.filter (\e -> case vout e of; Tuple{} -> False; _ -> True) genExpr ]
        []

genComprehension :: (MonadGen m, MonadState GenState m) => m (Comprehension VIdentity Expr '[] ())
genComprehension =
  sized3
    (Comprehension (Ann ()))
    (localState $ do
        inGenerator .= True
        Gen.filter (\(VIdentity e) -> case vout e of; Tuple{} -> False; _ -> True) (VIdentity <$> genExpr))
    genCompFor
    (localState $ do
        inGenerator .= True
        sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genDictComp :: (MonadGen m, MonadState GenState m) => m (Comprehension DictItem Expr '[] ())
genDictComp =
  sized3
    (Comprehension (Ann ()))
    (localState $ do
        inGenerator .= True
        DictItem (Ann ()) <$> genExpr <*> genColonAny <*> genExpr)
    genCompFor
    (localState $ do
        inGenerator .= True
        sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSetComp :: (MonadGen m, MonadState GenState m) => m (Comprehension SetItem Expr '[] ())
genSetComp =
  sized3
    (Comprehension (Ann ()))
    (localState $ do
        inGenerator .= True
        SetItem (Ann ()) <$> genExpr)
    genCompFor
    (localState $ do
        inGenerator .= True
        sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSubscriptItem :: (MonadGen m, MonadState GenState m) => m (SubscriptItem Expr '[] ())
genSubscriptItem =
  sizedRecursive
    [ SubscriptExpr <$> genExpr
    , sized3M
        (\a b c -> (\ws -> SubscriptSlice a ws b c) <$> genColon)
        (sizedMaybe genExpr)
        (sizedMaybe genExpr)
        (sizedMaybe $ (,) <$> genColon <*> sizedMaybe genExpr)
    ]
    []

genExprList :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genExprList =
  sizedBind genExpr $ \e -> do
    mes <- Gen.maybe $ sizedMaybe (genSizedCommaSep1' $ TupleItem (Ann ()) <$> genExpr)
    case mes of
      Nothing -> pure e
      Just es ->
        (\ws -> VIn $ Tuple (Ann ()) (TupleItem (Ann ()) e) ws es) <$>
        genComma

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genExpr = genExpr' False

genRawStringLiteral :: MonadGen m => m (StringLiteral ())
genRawStringLiteral =
  Gen.choice
  [ RawStringLiteral (Ann ()) <$>
    genRawStringPrefix <*>
    pure LongString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) (genPyChar $ Gen.filter (/='\0') Gen.latin1)<*>
    genWhitespaces
  , RawStringLiteral (Ann ()) <$>
    genRawStringPrefix <*>
    pure ShortString <*>
    genQuoteType <*>
    Gen.list
      (Range.constant 0 100)
      (genPyChar $ Gen.filter (`notElem` "\0\n\r") Gen.latin1) <*>
    genWhitespaces
  ]

genRawBytesLiteral :: MonadGen m => m (StringLiteral ())
genRawBytesLiteral =
  Gen.choice
  [ RawBytesLiteral (Ann ()) <$>
    genRawBytesPrefix <*>
    pure LongString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) (genPyChar $ Gen.filter (/='\0') Gen.latin1) <*>
    genWhitespaces
  , RawBytesLiteral (Ann ()) <$>
    genRawBytesPrefix <*>
    pure ShortString <*>
    genQuoteType <*>
    Gen.list
      (Range.constant 0 100)
      (genPyChar $ Gen.filter (`notElem` "\0\n\r") Gen.latin1) <*>
    genWhitespaces
  ]

genStringLiterals :: MonadGen m => m (Expr '[] ())
genStringLiterals = do
  n <- Gen.integral (Range.constant 1 5) :: MonadGen m => m Integer
  b <- Gen.bool_
  fmap VIn $ String (Ann ()) <$> go b n
  where
    ss = Gen.choice
      [ genStringLiteral $ genPyChar (Gen.filter (/='\0') Gen.latin1)
      , genRawStringLiteral
      ]
    bs = Gen.choice
      [ genBytesLiteral $ genPyChar (Gen.filter (/='\0') Gen.ascii)
      , genRawBytesLiteral
      ]
    go True 1 = pure <$> ss
    go False 1 = pure <$> bs
    go b n =
      NonEmpty.cons <$>
      (if b then ss else bs) <*>
      go b (n-1)

genExpr' :: (MonadGen m, MonadState GenState m) => Bool -> m (Expr '[] ())
genExpr' isExp = do
  isInFunction <- isJust <$> use inFunction
  isInAsyncFunction <- maybe False snd <$> use inFunction
  isAsync <- maybe False snd <$> use inFunction
  isInGenerator <- use inGenerator
  sizedRecursive
    [ genNone
    , genEllipsis
    , genUnit
    , genBool
    , if isExp then genSmallInt else genInt
    , if isExp then genSmallFloat else genFloat
    , genImag
    , VIn . Ident (Ann ()) <$> genIdent
    , genStringLiterals
    ]
    ([ genList genExpr
     , genStringLiterals
     , VIn . Generator (Ann ()) <$> genComprehension
     , fmap VIn $ ListComp (Ann ()) <$> genWhitespaces <*> genComprehension <*> genWhitespaces
     , fmap VIn $ DictComp (Ann ()) <$> genWhitespaces <*> genDictComp <*> genWhitespaces
     , fmap VIn $ SetComp (Ann ()) <$> genWhitespaces <*> genSetComp <*> genWhitespaces
     , fmap VIn $
       Dict (Ann ()) <$>
       genAnyWhitespaces <*>
       sizedMaybe (genSizedCommaSep1' $ genDictItem genExpr) <*>
       genWhitespaces
     , fmap VIn $
       Set (Ann ()) <$>
       genAnyWhitespaces <*>
       genSizedCommaSep1' (genSetItem genWhitespaces genExpr) <*>
       genWhitespaces
     , genDeref
     , genParens (genExpr' isExp)
     , sized2M
         (\a b -> fmap VIn $ (\ws1 -> Call (Ann ()) a ws1 b) <$> genWhitespaces <*> genWhitespaces)
         genExpr
         (sizedMaybe genArgs)
     , genSubscript
     , sizedBind genExpr $ \e1 ->
       sizedBind genOp $ \op ->
       sizedBind (genExpr' $ case op of; Exp{} -> True; _ -> False) $ \e2 ->
         pure . VIn $
         Binary (Ann ()) (e1 & trailingWhitespace .~ [Space]) (op & trailingWhitespace .~ [Space]) e2
     , genTuple genExpr
     , fmap VIn $ Not (Ann ()) <$> (NonEmpty.toList <$> genWhitespaces1) <*> genExpr
     , fmap VIn $ Unary (Ann ()) <$> genUnOp <*> genExpr
     , sized3M
         (\a b c ->
            fmap VIn $
            (\ws1 ws2 -> Ternary (Ann ()) a ws1 b ws2 c) <$>
            genWhitespaces <*>
            genWhitespaces)
         genExpr
         genExpr
         genExpr
     , sizedBind (genParams True) $ \a ->
         let paramIdents = a ^.. folded.paramName.identValue in
         fmap VIn $
         Lambda (Ann ()) <$>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         pure a <*>
         genColon <*>
         localState (do
           inLoop .= False
           modify $ \ctxt ->
             ctxt
             { _inFunction =
                 fmap
                   (bimap (`union` paramIdents) (const False))
                   (_inFunction ctxt) <|>
                 Just (paramIdents, False)
             , _currentNonlocals = _willBeNonlocals ctxt <> _currentNonlocals ctxt
             }
           genExpr)
     ] ++
     [ fmap VIn $
       Yield (Ann ()) <$>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         sizedCommaSep genExpr
     | (isInFunction || isInGenerator) && not isInAsyncFunction
     ] ++
     [ fmap VIn $
       YieldFrom (Ann ()) <$>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         genExpr
     | (isInFunction || isInGenerator) && not isAsync
     ] ++
     [ Gen.subtermM
         genExpr
         (\a ->
            fmap VIn $
            Await (Ann ()) <$>
            (NonEmpty.toList <$> genWhitespaces1) <*>
            pure a)
     | isAsync && not isInGenerator
     ])

genSubscript :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genSubscript =
  sized2M
    (\a b ->
       fmap VIn $
       (\ws1 -> Subscript (Ann ()) a ws1 b) <$>
       genWhitespaces <*>
       genWhitespaces)
    genExpr
    (genSizedCommaSep1' genSubscriptItem)

genDeletable :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genDeletable =
  sizedRecursive
    [ VIn . Ident (Ann ()) <$> genIdent
    ]
    [ genDeletableList genDeletable
    , genParens genDeletable
    , genDeletableTuple genDeletable
    , genDeref
    , genSubscript
    ]

genAssignable :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genAssignable =
  sizedRecursive
    [ VIn . Ident (Ann ()) <$> genIdent
    ]
    [ genParens genAssignable
    , genAssignableList
    , genAssignableTuple
    , genDeref
    , genSubscript
    ]

genAugAssignable :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genAugAssignable =
  sizedRecursive
    [ VIn . Ident (Ann ()) <$> genIdent ]
    [ genDeref
    , genSubscript
    ]

genSimpleStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (SimpleStatement '[] ())
genSimpleStatement = do
  ctxt <- get
  nonlocals <- use currentNonlocals
  sizedRecursive
    ([Pass (Ann ()) <$> genWhitespaces] <>
     [Break (Ann ()) <$> genWhitespaces | _inLoop ctxt] <>
     [Continue (Ann ()) <$> genWhitespaces | _inLoop ctxt && not (_inFinally ctxt)])
    ([ Expr (Ann ()) <$> genExpr
     , sizedBind (sizedNonEmpty $ (,) <$> genEquals <*> genAssignable) $ \a -> do
         isInFunction <- use inFunction
         when (isJust isInFunction) $
           willBeNonlocals %= ((a ^.. folded._2.cosmos._Ident.identValue) ++)
         sizedBind ((,) <$> genEquals <*> genExpr) $ \b ->
           pure $ Assign (Ann ()) (snd $ NonEmpty.head a) (NonEmpty.fromList $ snoc (NonEmpty.tail a) b)
     , sized2M
         (\a b -> AugAssign (Ann ()) a <$> genAugAssign <*> pure b)
         genAugAssignable
         genExpr
     , Global (Ann ()) <$>
       genWhitespaces1 <*>
       genSizedCommaSep1
         (maybe
            id
            (\(ps, _) -> Gen.filter ((`notElem` ps) . _identValue))
            (_inFunction ctxt)
            genIdent)
     , Del (Ann ()) <$>
       genWhitespaces <*>
       genSizedCommaSep1' genDeletable
     , Import (Ann ()) <$>
       genWhitespaces1 <*>
       genSizedCommaSep1 (genImportAs genModuleName genIdent)
     , Raise (Ann ()) <$>
       fmap NonEmpty.toList genWhitespaces1 <*>
       sizedMaybe
         ((,) <$>
           set (mapped.trailingWhitespace) [Space] genExpr <*>
           Gen.maybe ((,) <$> fmap NonEmpty.toList genWhitespaces1 <*> genExpr))
    , sized2M
        (\a b -> (\ws -> Assert (Ann ()) ws a b) <$> genWhitespaces)
        genExpr
        (sizedMaybe ((,) <$> genComma <*> genExpr))
     , From (Ann ()) <$>
       genWhitespaces <*>
       (genRelativeModuleName & mapped.trailingWhitespace .~ [Space]) <*>
       (NonEmpty.toList <$> genWhitespaces1) <*>
       genImportTargets
     ] ++
     [ do
         nonlocals <- use currentNonlocals
         Nonlocal (Ann ()) <$>
           genWhitespaces1 <*>
           genSizedCommaSep1 (Gen.element $ MkIdent (Ann ()) <$> nonlocals <*> pure [])
     | isJust (_inFunction ctxt) && not (null nonlocals)
     ] ++
     [ Return (Ann ()) <$>
       fmap NonEmpty.toList genWhitespaces1 <*>
       sizedMaybe genExpr
     | isJust (_inFunction ctxt)
     ])

genDecorator :: (MonadGen m, MonadState GenState m) => m (Decorator '[] ())
genDecorator =
  Decorator (Ann ()) <$>
  use currentIndentation <*>
  genAt <*>
  genDecoratorValue <*>
  Gen.maybe genComment <*>
  genNewline <*>
  Gen.list (Range.constant 0 10) ((,) <$> genBlank <*> genNewline)
  where
    genDecoratorValue =
      Gen.choice
      [ VIn . Ident (Ann()) <$> genIdent
      , sized2M
         (\a b ->
            fmap VIn $
            (\ws1 -> Call (Ann ()) a ws1 b) <$>
            genWhitespaces <*>
            genWhitespaces)
         genDerefs
         (sizedMaybe genArgs)
      ]

    genDerefs =
      sizedRecursive
      [ VIn . Ident (Ann ()) <$> genIdent ]
      [ fmap VIn $
        Deref (Ann ()) <$>
        genDerefs <*>
        genWhitespaces <*>
        genIdent
      ]

genCompoundStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (CompoundStatement '[] ())
genCompoundStatement =
  sizedRecursive
    [ do
        asyncWs <- Gen.maybe genWhitespaces1
        sizedBind (genParams False) $ \a ->
          let paramIdents = a ^.. folded.paramName.identValue in
          sizedBind
            (localState $ do
                modify $
                  \ctxt ->
                  ctxt
                  { _inLoop = False
                  , _inFunction =
                      fmap
                        (bimap (`union` paramIdents) (|| isJust asyncWs))
                        (_inFunction ctxt) <|>
                      Just (paramIdents, isJust asyncWs)
                  , _currentNonlocals = _willBeNonlocals ctxt <> _currentNonlocals ctxt
                  }
                genSuite genSimpleStatement genBlock) $
            \b ->
          sizedBind (sizedList genDecorator) $ \c ->
          sizedBind (sizedMaybe $ (,) <$> genWhitespaces <*> genExpr) $ \d ->
          Fundef (Ann ()) c <$>
            use currentIndentation <*>
            pure asyncWs <*>
            genWhitespaces1 <*>
            genIdent <*> genWhitespaces <*> pure a <*>
            genWhitespaces <*> pure d <*> pure b
    , sized4M
        (\a b c d -> 
          If (Ann ()) <$>
            use currentIndentation <*>
            fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
            pure b <*> pure c <*> pure d)
        genExpr
        (localState $ genSuite genSimpleStatement genBlock)
        (sizedList $
        sized2M
          (\a b ->
            (,,,) <$>
              use currentIndentation <*>
              genWhitespaces <*>
              pure a <*>
              pure b)
            genExpr
            (localState $ genSuite genSimpleStatement genBlock))
        (sizedMaybe $
          sizedBind (localState $ genSuite genSimpleStatement genBlock) $ \a ->
            (,,) <$>
            use currentIndentation <*>
            genWhitespaces <*>
            pure a)
    , sized3M
        (\a b c ->
          While (Ann ()) <$>
          use currentIndentation <*>
          fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
          pure b <*> pure c)
        genExpr
        (localState $ do
            inFinally .= False
            inLoop .= True
            genSuite genSimpleStatement genBlock)
        (sizedMaybe $
          sizedBind (localState $ genSuite genSimpleStatement genBlock) $ \a ->
            (,,) <$>
            use currentIndentation <*>
            genWhitespaces <*>
            pure a)
    , sized4M
        (\a b e1 e2 ->
          TryExcept (Ann ()) <$>
          use currentIndentation <*>
          genWhitespaces <*>
          pure a <*>
          pure b <*>
          pure e1 <*>
          pure e2)
        (genSuite genSimpleStatement genBlock)
        (do
          ls <- sizedList $
            sized2M
            (\a b -> 
                (,,,) <$>
                use currentIndentation <*>
                (NonEmpty.toList <$> genWhitespaces1) <*>
                (Just .
                ExceptAs (Ann ())
                  (a & trailingWhitespace .~ [Space]) <$>
                  Gen.maybe ((,) <$> (NonEmpty.toList <$> genWhitespaces1) <*> genIdent)) <*>
                pure b)
            genExpr
            (genSuite genSimpleStatement genBlock)
          l <-
            sized2M
              (\a b -> 
                  (,,,) <$>
                  use currentIndentation <*>
                  (NonEmpty.toList <$> genWhitespaces1) <*>
                  (case ls of
                      [] ->
                        Gen.maybe
                          (ExceptAs (Ann ())
                          (a & trailingWhitespace .~ [Space]) <$>
                          Gen.maybe
                            ((,) <$>
                              (NonEmpty.toList <$> genWhitespaces1) <*>
                              genIdent))
                      _ ->
                        Just . ExceptAs (Ann ())
                          (a & trailingWhitespace .~ [Space]) <$>
                          Gen.maybe
                            ((,) <$>
                            (NonEmpty.toList <$> genWhitespaces1) <*>
                            genIdent)) <*>
                  pure b)
              genExpr
              (genSuite genSimpleStatement genBlock)
          pure . NonEmpty.fromList $ ls ++ [l])
        (sizedMaybe $
          sizedBind (genSuite genSimpleStatement genBlock) $ \a ->
          (,,) <$>
          use currentIndentation <*>
          (NonEmpty.toList <$> genWhitespaces1) <*>
          pure a)
        (sizedMaybe . localState $ do
            modify $ inFinally .~ True
            sizedBind (genSuite genSimpleStatement genBlock) $ \a ->
              (,,) <$>
              use currentIndentation <*>
              (NonEmpty.toList <$> genWhitespaces1) <*>
              pure a)
    , sized2M
        (\a b ->
          TryFinally (Ann ()) <$>
          use currentIndentation <*>
          (NonEmpty.toList <$> genWhitespaces1) <*>
          pure a <*>
          use currentIndentation <*>
          (NonEmpty.toList <$> genWhitespaces1) <*>
          pure b)
        (genSuite genSimpleStatement genBlock)
        (localState $ do
          modify (inFinally .~ True)
          genSuite genSimpleStatement genBlock)
    , sized3M
        (\a b c ->
          ClassDef (Ann ()) a <$>
          use currentIndentation <*>
          genWhitespaces1 <*> genIdent <*>
          pure b <*>
          pure c)
        (sizedList genDecorator)
        (sizedMaybe $
            (,,) <$>
            genWhitespaces <*>
            sizedMaybe genArgs <*>
            genWhitespaces)
        (localState $ do
            modify (inClass .~ True)
            modify (inFunction .~ Nothing)
            modify (inLoop .~ False)
            genSuite genSimpleStatement genBlock)
    , do
        inAsync <- maybe False snd <$> use inFunction
        sized2M
          (\a b ->
            With (Ann ()) <$> use currentIndentation <*>
            (if inAsync then Gen.maybe genWhitespaces1 else pure Nothing) <*>
            (NonEmpty.toList <$> genWhitespaces1) <*>
            pure a <*> pure b)
          (genSizedCommaSep1 $
          WithItem (Ann ()) <$>
          genExpr <*>
          sizedMaybe ((,) <$> genWhitespaces <*> genAssignable))
          (genSuite genSimpleStatement genBlock)
    , do
        inAsync <- maybe False snd <$> use inFunction
        sized4M
          (\a b c d ->
            For (Ann ()) <$> use currentIndentation <*>
            (if inAsync then Gen.maybe genWhitespaces1 else pure Nothing) <*>
            (NonEmpty.toList <$> genWhitespaces1) <*> pure a <*>
            (NonEmpty.toList <$> genWhitespaces1) <*> pure b <*>
            pure c <*>
            pure d)
          genAssignable
          (genSizedCommaSep1' genExpr)
          (localState $ do
              inLoop .= True
              inFinally .= False
              genSuite genSimpleStatement genBlock)
          (sizedMaybe $
           (,,) <$>
           use currentIndentation <*>
           (NonEmpty.toList <$> genWhitespaces1) <*>
           genSuite genSimpleStatement genBlock)
    ]
    []

genStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (Statement '[] ())
genStatement =
  sizedRecursive
    [ SmallStatement <$>
      use currentIndentation <*>
      genSmallStatement (localState genSimpleStatement)
    ]
    [ CompoundStatement <$> localState genCompoundStatement ]

genImportAs :: (HasTrailingWhitespace (e '[] ()), MonadGen m) => m (e '[] ()) -> m (Ident '[] ()) -> m (ImportAs e '[] ())
genImportAs me genIdent =
  sized2
    (ImportAs (Ann ()))
    (set (mapped.trailingWhitespace) [Space] me)
    (sizedMaybe $ (,) <$> genWhitespaces1 <*> genIdent)
