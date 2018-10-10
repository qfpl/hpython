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
import Control.Lens.Prism (_Just, _Nothing)
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

import Language.Python.Syntax.Types (spType)
import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax

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
  , _currentIndentation = Indents [] ()
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

genIdent :: (MonadGen m, MonadState GenState m) => m (Ident '[] ())
genIdent = do
  isAsync <- maybe False snd <$> use inFunction
  let reserved = reservedWords <> (if isAsync then ["async", "await"] else [])
  Gen.filter
    (\i -> not $ any (`isPrefixOf` _identValue i) reserved) $
    MkIdent () <$>
    liftA2 (:)
      (Gen.choice [Gen.alpha, pure '_'])
      (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_'])) <*>
    genWhitespaces

genDeletableTuple :: (MonadGen m, MonadState GenState m) => m (Expr '[] ()) -> m (Expr '[] ())
genDeletableTuple expr =
  Tuple () <$>
  (TupleItem () <$> expr) <*>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' $ TupleItem () <$> expr)

genTuple :: (MonadGen m, MonadState GenState m) => m (Expr '[] ()) -> m (Expr '[] ())
genTuple expr =
  Tuple () <$>
  genTupleItem genWhitespaces expr <*>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' $ genTupleItem genWhitespaces expr)

genAssignableTuple :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genAssignableTuple =
  (\(ti, tis) ws -> Tuple () ti ws tis) <$> genTupleItems <*> genWhitespaces
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
           then TupleItem () <$> genAssignable
           else genTupleItem genWhitespaces genAssignable)
          (\ti ->
             sizedMaybe
               (view _CommaSep1' <$> genTupleItemsRest' (seen || has _TupleUnpack ti) ti []))

    genTupleItemsRest' seen a as =
      Gen.sized $ \n ->
      if n == 0
      then (,,) a as <$> Gen.maybe genWhitespaces
      else
        sizedBind
          (if seen
           then TupleItem () <$> genAssignable
           else genTupleItem genWhitespaces genAssignable)
          (\ti -> do
              ws <- genWhitespaces
              genTupleItemsRest' (seen || has _TupleUnpack ti) ti ((ws, a) : as))

genModuleName :: (MonadGen m, MonadState GenState m) => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne () <$> genIdent ]
  [ ModuleNameMany () <$>
    genIdent <*>
    genWhitespaces <*>
    genModuleName
  ]

genRelativeModuleName :: (MonadGen m, MonadState GenState m) => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative <$>
    Gen.nonEmpty (Range.constant 1 10) genDot
  , RelativeWithName <$>
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
    [ ImportSome () <$>
      genSizedCommaSep1 (genImportAs genIdent genIdent)
    , ImportSomeParens () <$>
      genWhitespaces <*>
      genSizedCommaSep1' (genImportAs genIdent genIdent) <*>
      genWhitespaces
    ] ++
    [ ImportAll () <$> genWhitespaces
    | not isInFunction && not isInClass
    ]

genBlock :: (MonadGen m, MonadState GenState m) => m (Block '[] ())
genBlock = doIndent *> go <* doDedent
  where
    genBlank =
      (,,,) () <$>
      Gen.list (Range.constant 0 10) (Gen.element [Space, Tab]) <*>
      Gen.maybe genComment <*>
      genNewline

    genLine =
      Gen.choice
        [ Right <$> genStatement
        , Left <$> genBlank
        ]

    go =
      sizedBind genStatement $ \st ->
      Block <$>
        Gen.list (Range.constant 0 10) genBlank <*>
        pure st <*>
        sizedList genLine

genPositionalArg :: (MonadGen m, MonadState GenState m) => m (Arg '[] ())
genPositionalArg =
  sizedRecursive
    [ PositionalArg () <$> genExpr
    , StarArg () <$> genWhitespaces <*> genExpr
    ]
    []

genKeywordArg :: (MonadGen m, MonadState GenState m) => m (Arg '[] ())
genKeywordArg =
  sizedRecursive
    [ KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr
    , DoubleStarArg () <$> genWhitespaces <*> genExpr
    ]
    []

genArgs :: (MonadGen m, MonadState GenState m) => m (CommaSep1' (Arg '[] ()))
genArgs =
  sized4
    (\a b c d -> (a, b <> c, d) ^. _CommaSep1')
    genPositionalArg
    (sizedList $ (,) <$> genWhitespaces <*> genPositionalArg)
    (sizedList $ (,) <$> genWhitespaces <*> genKeywordArg)
    (Gen.maybe genWhitespaces)

genPositionalParams
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ This is for a lambda
  -> m (CommaSep (Param '[] ()))
genPositionalParams isLambda =
  Gen.scale (max 0 . subtract 1) $
  Gen.sized $ fmap (listToCommaSep . fmap (uncurry $ PositionalParam ())) . go []
  where
    go _    0 = pure []
    go seen n = do
      i <- Gen.filter ((`notElem` seen) . _identValue) genIdent
      mty <-
        if isLambda
        then pure Nothing
        else sizedMaybe ((,) <$> genAnyWhitespaces <*> genExpr)
      ((i, mty) :) <$> go (_identValue i : seen) (n-1)

genKeywordParam
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ This is for a lambda
  -> [String]
  -> m (Param '[] ())
genKeywordParam isLambda positionals =
  Gen.scale (max 0 . subtract 1) $
  KeywordParam () <$>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent <*>
  (if isLambda then pure Nothing else sizedMaybe ((,) <$> genAnyWhitespaces <*> genExpr)) <*>
  genWhitespaces <*>
  genExpr

genStarParam
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ This is for a lambda
  -> [String]
  -> m (Param '[] ())
genStarParam isLambda positionals =
  Gen.scale (max 0 . subtract 1) $ do
    ident <- Gen.maybe $ Gen.filter (\i -> _identValue i `notElem` positionals) genIdent
    mty <-
      if isLambda
      then pure Nothing
      else
        case ident of
          Nothing -> pure Nothing
          _ -> sizedMaybe ((,) <$> genAnyWhitespaces <*> genExpr)
    StarParam () <$>
      genWhitespaces <*>
      pure ident <*>
      pure mty

genDoubleStarParam
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ This is for a lambda
  -> [String]
  -> m (Param '[] ())
genDoubleStarParam isLambda positionals =
  Gen.scale (max 0 . subtract 1) $
  DoubleStarParam () <$>
  genWhitespaces <*>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent <*>
  (if isLambda then pure Nothing else sizedMaybe ((,) <$> genAnyWhitespaces <*> genExpr))

genParams
  :: (MonadGen m, MonadState GenState m)
  => Bool -- ^ These are for a lambda
  -> m (CommaSep (Param '[] ()))
genParams isLambda =
  sizedBind (genPositionalParams isLambda) $ \pparams ->
  let pparamNames = pparams ^.. folded.paramName.identValue in

  sizedBind (sizedMaybe $ genStarParam isLambda pparamNames) $ \sp ->
  let pparamNames' = pparamNames <> (sp ^.. _Just.paramName.identValue) in

  sizedBind (genSizedCommaSep (genKeywordParam isLambda pparamNames')) $ \kwparams ->
  let
    pparamNames'' = pparamNames' <> kwparams ^.. folded.paramName.identValue
  in do
    kwparams' <-
      if has (folded._StarParam.spType._Nothing) sp && null kwparams
      then CommaSepOne <$> genKeywordParam isLambda pparamNames'
      else pure kwparams

    sizedBind (sizedMaybe $ genDoubleStarParam isLambda pparamNames'') $ \dsp ->

      pure $
        appendCommaSep
          (pparams `appendCommaSep` maybe CommaSepNone CommaSepOne sp)
          (kwparams' `appendCommaSep` maybe CommaSepNone CommaSepOne dsp)

genDeletableList :: (MonadState GenState m, MonadGen m) => m (Expr '[] ()) -> m (Expr '[] ())
genDeletableList genExpr' =
  Gen.shrink
    (\case
        List _ _ (Just (CommaSepOne1' e _)) _ -> e ^.. _Exprs
        _ -> []) $
  List () <$>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' $ ListItem () <$> genExpr') <*>
  genWhitespaces

genList :: (MonadState GenState m, MonadGen m) => m (Expr '[] ()) -> m (Expr '[] ())
genList genExpr' =
  Gen.shrink
    (\case
        List _ _ (Just (CommaSepOne1' e _)) _ -> e ^.. _Exprs
        _ -> []) $
  List () <$>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' $ genListItem genWhitespaces genExpr') <*>
  genWhitespaces

genAssignableList :: (MonadState GenState m, MonadGen m) => m (Expr '[] ())
genAssignableList =
  Gen.shrink
    (\case
        List _ _ (Just (CommaSepOne1' e _)) _ -> e ^.. _Exprs
        _ -> []) $
  List () <$>
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
      then (,,) a as <$> Gen.maybe genWhitespaces
      else
        sizedBind
          (if seen
           then ListItem () <$> genAssignable
           else genListItem genWhitespaces genAssignable)
          (\ti -> do
              ws <- genWhitespaces
              genListItemsRest (seen || has _ListUnpack ti) ti ((ws, a) : as))

genParens :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genParens genExpr' = Parens () <$> genWhitespaces <*> genExpr' <*> genWhitespaces

genDeref :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genDeref =
  Deref () <$>
  genExpr <*>
  genWhitespaces <*>
  genIdent

genCompFor :: (MonadGen m, MonadState GenState m) => m (CompFor '[] ())
genCompFor =
  sized2M
    (\a b ->
       (\ws1 ws2 -> CompFor () ws1 a ws2 b) <$>
       genWhitespaces <*>
       genWhitespaces)
    (localState $ do
        inGenerator .= True
        genAssignable)
    (Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr)

genCompIf :: (MonadGen m, MonadState GenState m) => m (CompIf '[] ())
genCompIf =
  localState $ do
    inGenerator .= True
    CompIf () <$>
      genWhitespaces <*>
      sizedRecursive
        [ Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr ]
        []

genComprehension :: (MonadGen m, MonadState GenState m) => m (Comprehension Expr '[] ())
genComprehension =
  sized3
    (Comprehension ())
    (localState $ do
        inGenerator .= True
        Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr)
    genCompFor
    (localState $ do
        inGenerator .= True
        sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genDictComp :: (MonadGen m, MonadState GenState m) => m (Comprehension DictItem '[] ())
genDictComp =
  sized3
    (Comprehension ())
    (localState $ do
        inGenerator .= True
        DictItem () <$> genExpr <*> genAnyWhitespaces <*> genExpr)
    genCompFor
    (localState $ do
        inGenerator .= True
        sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSetComp :: (MonadGen m, MonadState GenState m) => m (Comprehension SetItem '[] ())
genSetComp =
  sized3
    (Comprehension ())
    (localState $ do
        inGenerator .= True
        SetItem () <$> genExpr)
    genCompFor
    (localState $ do
        inGenerator .= True
        sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSubscriptItem :: (MonadGen m, MonadState GenState m) => m (Subscript '[] ())
genSubscriptItem =
  sizedRecursive
    [ SubscriptExpr <$> genExpr
    , sized3M
        (\a b c -> (\ws -> SubscriptSlice a ws b c) <$> genWhitespaces)
        (sizedMaybe genExpr)
        (sizedMaybe genExpr)
        (sizedMaybe $ (,) <$> genWhitespaces <*> sizedMaybe genExpr)
    ]
    []

genExprList :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genExprList =
  sizedBind genExpr $ \e -> do
    mes <- Gen.maybe $ sizedMaybe (genSizedCommaSep1' $ TupleItem () <$> genExpr)
    case mes of
      Nothing -> pure e
      Just es -> (\ws -> Tuple () (TupleItem () e) ws es) <$> genWhitespaces

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genExpr = genExpr' False

genRawStringLiteral :: MonadGen m => m (StringLiteral ())
genRawStringLiteral =
  Gen.choice
  [ RawStringLiteral () <$>
    genRawStringPrefix <*>
    pure LongString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) (genPyChar $ Gen.filter (/='\0') Gen.latin1)<*>
    genWhitespaces
  , RawStringLiteral () <$>
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
  [ RawBytesLiteral () <$>
    genRawBytesPrefix <*>
    pure LongString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) (genPyChar $ Gen.filter (/='\0') Gen.latin1) <*>
    genWhitespaces
  , RawBytesLiteral () <$>
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
  String () <$> go b n
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
    , Ident <$> genIdent
    , genStringLiterals
    ]
    ([ genList genExpr
     , genStringLiterals
     , Generator () <$> genComprehension
     , ListComp () <$> genWhitespaces <*> genComprehension <*> genWhitespaces
     , DictComp () <$> genWhitespaces <*> genDictComp <*> genWhitespaces
     , SetComp () <$> genWhitespaces <*> genSetComp <*> genWhitespaces
     , Dict () <$>
       genAnyWhitespaces <*>
       sizedMaybe (genSizedCommaSep1' $ genDictItem genExpr) <*>
       genWhitespaces
     , Set () <$>
       genAnyWhitespaces <*>
       genSizedCommaSep1' (genSetItem genWhitespaces genExpr) <*>
       genWhitespaces
     , genDeref
     , genParens (genExpr' isExp)
     , sized2M
         (\a b -> (\ws1 -> Call () a ws1 b) <$> genWhitespaces <*> genWhitespaces)
         genExpr
         (sizedMaybe genArgs)
     , genSubscript
     , sizedBind genExpr $ \e1 ->
       sizedBind genOp $ \op ->
       sizedBind (genExpr' $ case op of; Exp{} -> True; _ -> False) $ \e2 ->
         pure $
         BinOp () (e1 & trailingWhitespace .~ [Space]) (op & trailingWhitespace .~ [Space]) e2
     , genTuple genExpr
     , Not () <$> (NonEmpty.toList <$> genWhitespaces1) <*> genExpr
     , UnOp () <$> genUnOp <*> genExpr
     , sized3M
         (\a b c ->
           (\ws1 ws2 -> Ternary () a ws1 b ws2 c) <$>
           genWhitespaces <*>
           genWhitespaces)
         genExpr
         genExpr
         genExpr
     , sizedBind (genParams True) $ \a ->
         let paramIdents = a ^.. folded.paramName.identValue in
         Lambda () <$>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         pure a <*>
         genWhitespaces <*>
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
     [ Yield () <$>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         sizedMaybe genExpr
     | (isInFunction || isInGenerator) && not isInAsyncFunction
     ] ++
     [ YieldFrom () <$>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         genExpr
     | (isInFunction || isInGenerator) && not isAsync
     ] ++
     [ Gen.subtermM
         genExpr
         (\a ->
            Await () <$>
            (NonEmpty.toList <$> genWhitespaces1) <*>
            pure a)
     | isAsync && not isInGenerator
     ])

genSubscript :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genSubscript =
  sized2M
    (\a b -> (\ws1 -> Subscript () a ws1 b) <$> genWhitespaces <*> genWhitespaces)
    genExpr
    (genSizedCommaSep1' genSubscriptItem)

genDeletable :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genDeletable =
  sizedRecursive
    [ Ident <$> genIdent
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
    [ Ident <$> genIdent
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
    [ Ident <$> genIdent ]
    [ genDeref
    , genSubscript
    ]

genSmallStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (SmallStatement '[] ())
genSmallStatement = do
  ctxt <- get
  nonlocals <- use currentNonlocals
  sizedRecursive
    ([Pass () <$> genWhitespaces] <>
     [Break () <$> genWhitespaces | _inLoop ctxt] <>
     [Continue () <$> genWhitespaces | _inLoop ctxt && not (_inFinally ctxt)])
    ([ Expr () <$> genExpr
     , sizedBind (sizedNonEmpty $ (,) <$> genWhitespaces <*> genAssignable) $ \a -> do
         isInFunction <- use inFunction
         when (isJust isInFunction) $
           willBeNonlocals %= ((a ^.. folded._2.cosmos._Ident.identValue) ++)
         sizedBind ((,) <$> genWhitespaces <*> genExpr) $ \b ->
           pure $ Assign () (snd $ NonEmpty.head a) (NonEmpty.fromList $ snoc (NonEmpty.tail a) b)
     , sized2M
         (\a b -> AugAssign () a <$> genAugAssign <*> pure b)
         genAugAssignable
         genExpr
     , Global () <$>
       genWhitespaces1 <*>
       genSizedCommaSep1 genIdent
     , Del () <$>
       genWhitespaces1 <*>
       genSizedCommaSep1' genDeletable
     , Import () <$>
       genWhitespaces1 <*>
       genSizedCommaSep1 (genImportAs genModuleName genIdent)
     , Raise () <$>
       fmap NonEmpty.toList genWhitespaces1 <*>
       sizedMaybe
         ((,) <$>
           set (mapped.trailingWhitespace) [Space] genExpr <*>
           Gen.maybe ((,) <$> fmap NonEmpty.toList genWhitespaces1 <*> genExpr))
    , sized2M
        (\a b -> (\ws -> Assert () ws a b) <$> genWhitespaces)
        genExpr
        (sizedMaybe ((,) <$> genWhitespaces <*> genExpr))
     , From () <$>
       genWhitespaces <*>
       (genRelativeModuleName & mapped.trailingWhitespace .~ [Space]) <*>
       (NonEmpty.toList <$> genWhitespaces1) <*>
       genImportTargets
     ] ++
     [ do
         nonlocals <- use currentNonlocals
         Nonlocal () <$>
           genWhitespaces1 <*>
           genSizedCommaSep1 (Gen.element $ MkIdent () <$> nonlocals <*> pure [])
     | isJust (_inFunction ctxt) && not (null nonlocals)
     ] ++
     [ Return () <$>
       fmap NonEmpty.toList genWhitespaces1 <*>
       sizedMaybe genExpr
     | isJust (_inFunction ctxt)
     ])

genDecorator :: (MonadGen m, MonadState GenState m) => m (Decorator '[] ())
genDecorator =
  Decorator () <$>
  use currentIndentation <*>
  genWhitespaces <*>
  genDecoratorValue <*>
  Gen.maybe genComment <*>
  genNewline
  where
    genDecoratorValue =
      Gen.choice
      [ Ident <$> genIdent
      , sized2M
         (\a b -> (\ws1 -> Call () a ws1 b) <$> genWhitespaces <*> genWhitespaces)
         genDerefs
         (sizedMaybe genArgs)
      ]

    genDerefs =
      sizedRecursive
      [ Ident <$> genIdent ]
      [ Deref () <$>
        genDerefs <*>
        genWhitespaces <*>
        genIdent
      ]

genCompoundStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (CompoundStatement '[] ())
genCompoundStatement =
  localState $ do
    modify $ inFinally .~ False
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
                 genSuite genSmallStatement genBlock) $
              \b ->
            sizedBind (sizedList genDecorator) $ \c ->
            sizedBind (sizedMaybe $ (,) <$> genWhitespaces <*> genExpr) $ \d ->
            Fundef () c <$>
              use currentIndentation <*>
              pure asyncWs <*>
              genWhitespaces1 <*>
              genIdent <*> genWhitespaces <*> pure a <*>
              genWhitespaces <*> pure d <*> pure b
      , sized4M
          (\a b c d -> 
            If <$>
              pure () <*>
              use currentIndentation <*>
              fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
              pure b <*> pure c <*> pure d)
          genExpr
          (localState $ genSuite genSmallStatement genBlock)
          (sizedList $
          sized2M
            (\a b ->
              (,,,) <$>
                use currentIndentation <*>
                genWhitespaces <*>
                pure a <*>
                pure b)
              genExpr
              (localState $ genSuite genSmallStatement genBlock))
          (sizedMaybe $
          sizedBind (localState $ genSuite genSmallStatement genBlock) $ \a ->
            (,,) <$>
            use currentIndentation <*>
            genWhitespaces <*>
            pure a)
      , sized2M
          (\a b ->
            While <$>
            pure () <*>
            use currentIndentation <*>
            fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
            pure b)
          genExpr
          (localState $ (inLoop .= True) *> genSuite genSmallStatement genBlock)
      , sized4M
          (\a b e1 e2 ->
            TryExcept <$>
            pure () <*>
            use currentIndentation <*>
            genWhitespaces <*>
            pure a <*>
            pure b <*>
            pure e1 <*>
            pure e2)
          (genSuite genSmallStatement genBlock)
          (do
            ls <- sizedList $
              sized2M
              (\a b -> 
                  (,,,) <$>
                  use currentIndentation <*>
                  (NonEmpty.toList <$> genWhitespaces1) <*>
                  (Just .
                  ExceptAs ()
                    (a & trailingWhitespace .~ [Space]) <$>
                    Gen.maybe ((,) <$> (NonEmpty.toList <$> genWhitespaces1) <*> genIdent)) <*>
                  pure b)
              genExpr
              (genSuite genSmallStatement genBlock)
            l <-
              sized2M
                (\a b -> 
                    (,,,) <$>
                    use currentIndentation <*>
                    (NonEmpty.toList <$> genWhitespaces1) <*>
                    (case ls of
                        [] ->
                          Gen.maybe
                            (ExceptAs ()
                            (a & trailingWhitespace .~ [Space]) <$>
                            Gen.maybe
                              ((,) <$>
                                (NonEmpty.toList <$> genWhitespaces1) <*>
                                genIdent))
                        _ ->
                          Just . ExceptAs ()
                            (a & trailingWhitespace .~ [Space]) <$>
                            Gen.maybe
                              ((,) <$>
                              (NonEmpty.toList <$> genWhitespaces1) <*>
                              genIdent)) <*>
                    pure b)
                genExpr
                (genSuite genSmallStatement genBlock)
            pure . NonEmpty.fromList $ ls ++ [l])
          (sizedMaybe $
           sizedBind (genSuite genSmallStatement genBlock) $ \a ->
           (,,) <$>
           use currentIndentation <*>
           (NonEmpty.toList <$> genWhitespaces1) <*>
           pure a)
          (sizedMaybe . localState $ do
             modify $ inFinally .~ True
             sizedBind (genSuite genSmallStatement genBlock) $ \a ->
               (,,) <$>
               use currentIndentation <*>
               (NonEmpty.toList <$> genWhitespaces1) <*>
               pure a)
      , sized2M
          (\a b ->
            TryFinally <$>
            pure () <*> use currentIndentation <*>
            (NonEmpty.toList <$> genWhitespaces1) <*>
            pure a <*>
            use currentIndentation <*>
            (NonEmpty.toList <$> genWhitespaces1) <*>
            pure b)
          (genSuite genSmallStatement genBlock)
          (localState $ do
            modify (inFinally .~ True)
            genSuite genSmallStatement genBlock)
      , sized3M
          (\a b c ->
            ClassDef () a <$>
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
             modify (inLoop .~ False)
             genSuite genSmallStatement genBlock)
      , do
          inAsync <- maybe False snd <$> use inFunction
          sized2M
            (\a b ->
              With <$> pure () <*> use currentIndentation <*>
              (if inAsync then Gen.maybe genWhitespaces1 else pure Nothing) <*>
              (NonEmpty.toList <$> genWhitespaces1) <*>
              pure a <*> pure b)
            (genSizedCommaSep1 $
            WithItem () <$>
            genExpr <*>
            sizedMaybe ((,) <$> genWhitespaces <*> genAssignable))
            (genSuite genSmallStatement genBlock)
      , do
          inAsync <- maybe False snd <$> use inFunction
          sized4M
            (\a b c d ->
              For <$> pure () <*> use currentIndentation <*>
              (if inAsync then Gen.maybe genWhitespaces1 else pure Nothing) <*>
              (NonEmpty.toList <$> genWhitespaces1) <*> pure a <*>
              (NonEmpty.toList <$> genWhitespaces1) <*> pure b <*>
              pure c <*>
              pure d)
            genAssignable
            (genSizedCommaSep1' genExpr)
            (genSuite genSmallStatement genBlock)
            (sizedMaybe $
            (,,) <$>
            use currentIndentation <*>
            (NonEmpty.toList <$> genWhitespaces1) <*>
            (genSuite genSmallStatement genBlock))
      ]
      []

genStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (Statement '[] ())
genStatement =
  sizedRecursive
    [ sizedBind (localState genSmallStatement) $ \st ->
      sizedBind (sizedList $ (,) <$> genWhitespaces <*> localState genSmallStatement) $ \sts ->
      (\a c -> SmallStatements a st sts c) <$>
      use currentIndentation <*>
      Gen.maybe genWhitespaces <*>
      Gen.maybe genComment <*>
      (Just <$> genNewline)
    ]
    [ CompoundStatement <$> localState genCompoundStatement ]

genImportAs :: (HasTrailingWhitespace (e ()), MonadGen m) => m (e ()) -> m (Ident '[] ()) -> m (ImportAs e '[] ())
genImportAs me genIdent =
  sized2
    (ImportAs ())
    (set (mapped.trailingWhitespace) [Space] me)
    (sizedMaybe $ (,) <$> genWhitespaces1 <*> genIdent)
