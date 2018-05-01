{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Generators.Correct where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Plated
import Control.Lens.Setter
import Control.Lens.Tuple
import Control.Lens.TH
import Control.Monad.State
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Semigroup ((<>))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Stack

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax

import Generators.Common

initialGenState =
  GenState
  { _inFunction = Nothing
  , _currentNonlocals = []
  , _willBeNonlocals = []
  , _inLoop = False
  }

data GenState
  = GenState
  { _inFunction :: Maybe [String]
  , _currentNonlocals :: [String]
  , _willBeNonlocals :: [String]
  , _inLoop :: Bool
  }
makeLenses ''GenState

localState m = do
  a <- get
  b <- m
  put a
  pure b

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  Gen.filter (\i -> _identValue i `notElem` reservedWords) $
  MkIdent () <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_'])) <*>
  genWhitespaces

genModuleName :: MonadGen m => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne () <$> genIdent ]
  [ ModuleNameMany () <$>
    genIdent <*>
    genWhitespaces <*>
    genModuleName
  ]

genRelativeModuleName :: MonadGen m => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative <$>
    Gen.nonEmpty (Range.constant 1 10) genDot <*>
    genWhitespaces
  , RelativeWithName <$>
    Gen.list (Range.constant 1 10) genDot <*>
    genModuleName
  ]

genImportTargets :: MonadGen m => m (ImportTargets '[] ())
genImportTargets =
  Gen.choice
  [ ImportAll () <$> genWhitespaces
  , ImportSome () <$>
    genSizedCommaSep1 (genImportAs genIdent genIdent)
  , ImportSomeParens () <$>
    genWhitespaces <*>
    genSizedCommaSep1' (genImportAs genIdent genIdent) <*>
    genWhitespaces
  ]

genInt :: MonadGen m => m (Expr '[] ())
genInt = Int () <$> Gen.integral (Range.constant 0 (2^32)) <*> genWhitespaces

genBlock :: (MonadGen m, MonadState GenState m) => m (Block '[] ())
genBlock = do
  indent <- NonEmpty.toList <$> genWhitespaces1
  go indent
  where
    go indent =
      Gen.sized $ \n ->
      if n <= 1
        then do
          s1 <-
            Gen.choice
              [ Right <$> genStatement
              , fmap Left $ (,,) <$> genWhitespaces <*> genComment <*> genNewline
              ]
          pure . Block $ ((), indent, s1) :| []
        else do
          n' <- Gen.integral (Range.constant 1 (n-1))
          s1 <-
            Gen.resize n' $
            Gen.choice
              [ Right <$> genStatement
              , fmap Left $ (,,) <$> genWhitespaces <*> genComment <*> genNewline
              ]
          let n'' = n - n'
          b <- Gen.resize n'' (go indent)
          pure . Block $ NonEmpty.cons ((), indent, s1) (unBlock b)

genPositionalArg :: MonadGen m => m (Arg '[] ())
genPositionalArg = Gen.scale (max 0 . subtract 1) $ PositionalArg () <$> genExpr

genKeywordArg :: MonadGen m => m (Arg '[] ())
genKeywordArg =
  Gen.scale (max 0 . subtract 1) $
  KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr

genArgs :: MonadGen m => m (CommaSep (Arg '[] ()))
genArgs =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 0 n)
    n2 <- Gen.integral (Range.constant 0 $ n-n1)
    let n3 = n - n1 - n2

    pargs <- Gen.resize n1 $ genSizedCommaSep genPositionalArg
    -- sargs <- Gen.resize n2 $ genSizedCommaSep genStarArg
    kwargs <- Gen.resize n3 $ genSizedCommaSep genKeywordArg

    -- appendCommaSep pargs (appendCommaSep sargs kwargs)
    pure $ appendCommaSep pargs kwargs

genPositionalParams :: MonadGen m => m (CommaSep (Param '[] ()))
genPositionalParams =
  Gen.scale (max 0 . subtract 1) $
  Gen.sized $ \n -> do
    idents <- go [] n
    pure . listToCommaSep $ PositionalParam () <$> idents
  where
    go seen 0 = pure []
    go seen n = do
      i <- Gen.filter ((`notElem` seen) . _identValue) genIdent
      (i :) <$> go (_identValue i : seen) (n-1)

genKeywordParam :: MonadGen m => [String] -> m (Param '[] ())
genKeywordParam positionals =
  Gen.scale (max 0 . subtract 1) $
  KeywordParam () <$>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent <*>
  genWhitespaces <*>
  genExpr

genParams :: MonadGen m => m (CommaSep (Param '[] ()))
genParams =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 0 n)
    n2 <- Gen.integral (Range.constant 0 $ n-n1)
    let n3 = n - n1 - n2

    pparams <- Gen.resize n1 genPositionalParams
    -- sparams <- Gen.resize n2 $ genSizedCommaSep genStarParam
    kwparams <-
      Gen.resize n3 $
      genSizedCommaSep (genKeywordParam $ pparams ^.. folded.paramName.identValue)

    -- appendCommaSep pparams (appendCommaSep sparams kwparams)
    pure $ appendCommaSep pparams kwparams

genList :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genList genExpr' =
  Gen.shrink
    (\case
        List _ _ (CommaSepOne e) _ -> [e]
        _ -> []) $
  List () <$>
  genWhitespaces <*>
  genSizedCommaSep genExpr' <*>
  genWhitespaces

genParens :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genParens genExpr' = Parens () <$> genWhitespaces <*> genExpr' <*> genWhitespaces

genDeref :: MonadGen m => m (Expr '[] ())
genDeref =
  Gen.subtermM
    genExpr
    (\a ->
        Deref () a <$>
        genWhitespaces <*>
        genIdent)

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: MonadGen m => m (Expr '[] ())
genExpr = genExpr' False

genExpr' :: MonadGen m => Bool -> m (Expr '[] ())
genExpr' isExp = Gen.sized $ \n ->
  if n <= 1
  then
    Gen.choice
    [ genBool
    , if isExp then genSmallInt else genInt
    , Ident () <$> genIdent
    , String () <$> genStringType <*> genString <*> genWhitespaces
    ]
  else
    Gen.resize (n-1) .
    Gen.choice $
      [ genList genExpr
      , genDeref
      , genParens genExpr
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genExpr
          b <- Gen.resize (n - n') genArgs
          Call () a <$> genWhitespaces <*> pure b <*> genWhitespaces
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          op <- genOp
          Gen.subtermM2
            (Gen.resize n' genExpr)
            (Gen.resize (n - n') (genExpr' $ case op of; Exp{} -> True; _ -> False))
            (\a b ->
               BinOp () (a & whitespaceAfter .~ [Space]) <$>
               pure (op & whitespaceAfter .~ [Space]) <*>
               pure b)
      , genTuple genExpr
      ]

genAssignable :: MonadGen m => m (Expr '[] ())
genAssignable =
  Gen.scale (max 0 . subtract 1) $
  Gen.choice
    [ genList genAssignable
    , genParens genAssignable
    , genTuple genAssignable
    , Ident () <$> genIdent
    , genDeref
    ]

genSmallStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (SmallStatement '[] ())
genSmallStatement = Gen.sized $ \n -> do
  ctxt <- get
  if n <= 1
  then Gen.element $ [Pass ()] ++ [Break () | _inLoop ctxt]
  else do
    nonlocals <- use currentNonlocals
    Gen.resize (n-1) .
      Gen.choice $
        [ Expr () <$> genExpr
        , pure $ Pass ()
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 1 (n-1))
            a <- Gen.resize n' genAssignable
            isInFunction <- use inFunction
            when (isJust isInFunction) $
              willBeNonlocals %= ((a ^.. cosmos._Ident._2.identValue) ++)
            b <- Gen.resize (n - n') genExpr
            Assign () a <$> genWhitespaces <*> genWhitespaces <*> pure b
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            Global () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 genIdent)
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            Del () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 genIdent)
        , Import () <$>
          genWhitespaces1 <*>
          genSizedCommaSep1 (genImportAs genModuleName genIdent)
        , From () <$>
          genWhitespaces <*>
          genRelativeModuleName <*>
          genWhitespaces <*>
          genImportTargets
        ] ++
        [pure (Break ()) | _inLoop ctxt] ++
        [ Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 2 (n-1))
            nonlocals <- use currentNonlocals
            Nonlocal () <$>
              genWhitespaces1 <*>
              Gen.resize n' (genSizedCommaSep1 . Gen.element $ MkIdent () <$> nonlocals <*> pure [])
        | isJust (_inFunction ctxt) && not (null nonlocals)
        ] ++
        [ Return () <$>
          fmap NonEmpty.toList genWhitespaces1 <*>
          genExpr
        | isJust (_inFunction ctxt)
        ]

genCompoundStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (CompoundStatement '[] ())
genCompoundStatement =
  Gen.sized $ \n ->
  Gen.resize (n-1) .
  Gen.choice $
    [ Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 (n-1))
        a <- Gen.resize n' genParams
        let paramIdents = a ^.. folded.paramName.identValue
        b <-
          Gen.resize
            (n - n')
            (localState $ do
               (modify $ \ctxt ->
                   ctxt
                   { _inLoop = False
                   , _inFunction =
                       fmap
                         (\b -> union b paramIdents)
                         (_inFunction ctxt) <|>
                       Just paramIdents
                   , _currentNonlocals = _willBeNonlocals ctxt <> _currentNonlocals ctxt
                   })
               genBlock)
        Fundef () <$> genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
    , Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 (n-1))
        n'' <- Gen.integral (Range.constant 0 (n-n'))
        a <- Gen.resize n' genExpr
        b <- Gen.resize (n - n') (localState genBlock)
        c <-
          if n - n' - n'' == 0
          then pure Nothing
          else
            fmap Just $
            (,,,) <$>
            genWhitespaces <*>
            genWhitespaces <*>
            genNewline <*>
            Gen.resize (n - n' - n'') (localState genBlock)
        If () <$> fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b <*> pure c
    , Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 (n-1))
        a <- Gen.resize n' genExpr
        b <- Gen.resize (n - n') (localState $ (inLoop .= True) *> genBlock)
        While () <$> fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
    ]

genStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (Statement '[] ())
genStatement =
  Gen.sized $ \n ->
  if n < 4
  then
    SmallStatements <$>
    localState genSmallStatement <*>
    pure [] <*>
    Gen.maybe ((,) <$> genWhitespaces <*> genWhitespaces) <*>
    genNewline
  else
    Gen.scale (subtract 1) $
    Gen.choice
    [ CompoundStatement <$> localState genCompoundStatement
    , Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 n)
        n'' <- Gen.integral (Range.constant 0 (n-n'))
        SmallStatements <$>
          Gen.resize n' (localState genSmallStatement) <*>
          (if n'' == 0
            then pure []
            else
             Gen.list
               (Range.singleton $ unSize n'')
               (Gen.resize ((n-n') `div` n'') $
                (,,) <$> genWhitespaces <*> genWhitespaces <*> localState genSmallStatement)) <*>
          Gen.maybe ((,) <$> genWhitespaces <*> genWhitespaces) <*>
          genNewline
    ]
