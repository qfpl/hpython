{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Generators.Correct where

import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Iso (from)
import Control.Lens.Plated
import Control.Lens.Prism (_Just)
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
  , _currentIndentation = Indents [] ()
  }

data GenState
  = GenState
  { _inFunction :: Maybe [String]
  , _currentNonlocals :: [String]
  , _willBeNonlocals :: [String]
  , _inLoop :: Bool
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

localState m = do
  a <- get
  b <- m
  put a
  pure b

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  Gen.filter (\i -> not $ any (`isPrefixOf` _identValue i) reservedWords) $
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
    Gen.nonEmpty (Range.constant 1 10) genDot
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
genBlock = doIndent *> go False <* doDedent
  where
    go b =
      Gen.sized $ \n ->
      if n <= 1
        then do
          s1 <-
            Gen.choice $
              [ Right <$> genStatement ] <>
              [ fmap Left $
                (,,) <$>
                genWhitespaces <*>
                Gen.maybe genComment <*>
                genNewline
              | b ]
          pure . Block $ s1 :| []
        else do
          n' <- Gen.integral (Range.constant 1 (n-1))
          (b', s1) <-
            Gen.resize n' $
            Gen.choice
              [ (,) True . Right <$> genStatement
              , fmap ((,) False . Left) $
                (,,) <$>
                genWhitespaces <*>
                Gen.maybe genComment <*>
                genNewline
              ]
          let n'' = n - n'
          b <- Gen.resize n'' $ go b'
          pure . Block $ NonEmpty.cons s1 (unBlock b)

genPositionalArg :: MonadGen m => m (Arg '[] ())
genPositionalArg =
  Gen.scale (max 0 . subtract 1) $
  Gen.choice
    [ PositionalArg () <$> genExpr
    , StarArg () <$> genWhitespaces <*> genExpr
    ]

genKeywordArg :: MonadGen m => m (Arg '[] ())
genKeywordArg =
  Gen.scale (max 0 . subtract 1) $
  Gen.choice
    [ KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr
    , DoubleStarArg () <$> genWhitespaces <*> genExpr
    ]

genArgs :: MonadGen m => m (CommaSep (Arg '[] ()))
genArgs =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 0 n)
    n2 <- Gen.integral (Range.constant 0 $ n-n1)
    let n3 = n - n1 - n2

    pargs <- Gen.resize n1 $ genSizedCommaSep genPositionalArg
    kwargs <- Gen.resize n3 $ genSizedCommaSep genKeywordArg

    pure $ appendCommaSep pargs kwargs

genArgs1 :: MonadGen m => m (CommaSep1 (Arg '[] ()))
genArgs1 =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 0 n)
    n2 <- Gen.integral (Range.constant 0 $ n-n1)
    let n3 = n - n1 - n2

    pargs <- Gen.resize n1 $ genSizedCommaSep1 genPositionalArg
    kwargs <- Gen.resize n3 $ genSizedCommaSep1 genKeywordArg

    pure $ pargs <> kwargs

genPositionalParams :: MonadGen m => m (CommaSep (Param '[] ()))
genPositionalParams =
  Gen.scale (max 0 . subtract 1) $
  Gen.sized $ fmap (listToCommaSep . fmap (PositionalParam ())) . go []
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

genStarParam :: MonadGen m => [String] -> m (Param '[] ())
genStarParam positionals =
  Gen.scale (max 0 . subtract 1) $
  StarParam () <$>
  genWhitespaces <*>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent

genDoubleStarParam :: MonadGen m => [String] -> m (Param '[] ())
genDoubleStarParam positionals =
  Gen.scale (max 0 . subtract 1) $
  DoubleStarParam () <$>
  genWhitespaces <*>
  Gen.filter (\i -> _identValue i `notElem` positionals) genIdent

genParams :: MonadGen m => m (CommaSep (Param '[] ()))
genParams =
  Gen.sized $ \n -> do
    n1 <- Gen.integral (Range.constant 0 n)
    n2 <- Gen.integral (Range.constant 0 $ n-n1)
    let n3 = n - n1 - n2

    pparams <- Gen.resize n1 genPositionalParams
    let
      pparamNames = pparams ^.. folded.paramName.identValue
    sp <- Gen.maybe $ genStarParam pparamNames
    let
      pparamNames' = pparamNames <> (sp ^.. _Just.paramName.identValue)
    kwparams <-
      Gen.resize n3 $
      genSizedCommaSep (genKeywordParam pparamNames')
    let
      pparamNames'' = pparamNames' <> kwparams ^.. folded.paramName.identValue
    dsp <- Gen.maybe $ genDoubleStarParam pparamNames''

    pure $
      appendCommaSep
        (pparams `appendCommaSep` maybe CommaSepNone CommaSepOne sp)
        (kwparams `appendCommaSep` maybe CommaSepNone CommaSepOne dsp)

genList :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genList genExpr' =
  Gen.shrink
    (\case
        List _ _ (Just (CommaSepOne1' e _)) _ -> [e]
        _ -> []) $
  List () <$>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' genExpr') <*>
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

genCompFor :: MonadGen m => m (CompFor '[] ())
genCompFor =
  Gen.sized $ \n -> do
    s1 <- Gen.integral (Range.constant 0 . max 0 $ n-1)
    s2 <- Gen.integral (Range.constant 0 $ s1)
    CompFor () <$>
      genWhitespaces <*>
      Gen.resize s1 genAssignable <*>
      genWhitespaces <*>
      Gen.resize s2 (Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr)

genCompIf :: MonadGen m => m (CompIf '[] ())
genCompIf =
  CompIf () <$>
  genWhitespaces <*>
  Gen.scale (max 0 . subtract 1) (Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr)

genComprehension :: MonadGen m => m (Comprehension '[] ())
genComprehension =
  Gen.sized $ \n -> do
    s1 <- Gen.integral (Range.constant 0 . max 0 $ n-2)
    s2 <- Gen.integral (Range.constant 0 . max 0 $ s1-1)
    s3 <- Gen.integral (Range.constant 0 $ s2)
    s4 <- Gen.integral (Range.constant 0 10)
    Comprehension () <$>
      Gen.resize s1 (Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr) <*>
      Gen.resize s2 genCompFor <*>
      replicateM (unSize s4)
        (Gen.resize (s3 `div` s4) $
         Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

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
    , String () <$>
      Gen.maybe genStringPrefix <*>
      genStringType <*>
      genString <*>
      genWhitespaces
    ]
  else
    Gen.resize (n-1) .
    Gen.choice $
      [ genList genExpr
      , ListComp () <$> genWhitespaces <*> genComprehension <*> genWhitespaces
      , genDeref
      , genParens (genExpr' isExp)
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          a <- Gen.resize n' genExpr
          b <- Gen.resize (n - n') genArgs
          Call () a <$> genWhitespaces <*> pure b <*> genWhitespaces
      , genSubscript
      , Gen.sized $ \n -> do
          n' <- Gen.integral (Range.constant 1 (n-1))
          op <- genOp
          Gen.subtermM2
            (Gen.resize n' genExpr)
            (Gen.resize (n - n') (genExpr' $ case op of; Exp{} -> True; _ -> False))
            (\a b ->
               BinOp () (a & trailingWhitespace .~ [Space]) <$>
               pure (op & trailingWhitespace .~ [Space]) <*>
               pure b)
      , genTuple genExpr
      , Not () <$> (NonEmpty.toList <$> genWhitespaces1) <*> genExpr
      ]

genSubscript :: MonadGen m => m (Expr '[] ())
genSubscript =
  Gen.sized $ \n -> do
    n' <- Gen.integral (Range.constant 0 . max 0 $ n-1)
    a <- Gen.resize n' genExpr
    b <- Gen.resize (max 0 $ n - n') genExpr
    Subscript () a <$> genWhitespaces <*> pure b <*> genWhitespaces

genAssignable :: MonadGen m => m (Expr '[] ())
genAssignable =
  Gen.scale (max 0 . subtract 1) $
  Gen.choice
    [ genList genAssignable
    , genParens genAssignable
    , genTuple genAssignable
    , Ident () <$> genIdent
    , genDeref
    , genSubscript
    ]

genAugAssignable :: MonadGen m => m (Expr '[] ())
genAugAssignable =
  Gen.scale (max 0 . subtract 1) $
  Gen.choice
    [ Ident () <$> genIdent
    , genDeref
    , genSubscript
    ]

genSmallStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (SmallStatement '[] ())
genSmallStatement = Gen.sized $ \n -> do
  ctxt <- get
  if n <= 1
  then Gen.element $ [Pass ()] ++ [Break () | _inLoop ctxt] ++ [Continue () | _inLoop ctxt]
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
            Assign () a <$> genWhitespaces <*> pure b
        , Gen.sized $ \n -> do
            n' <- Gen.integral (Range.constant 1 (n-1))
            a <- Gen.resize n' genAugAssignable
            b <- Gen.resize (n - n') genExpr
            AugAssign () a <$> genAugAssign <*> pure b
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
          (genRelativeModuleName & mapped.trailingWhitespace .~ [Space]) <*>
          (NonEmpty.toList <$> genWhitespaces1) <*>
          genImportTargets
        , Raise () <$>
          fmap NonEmpty.toList genWhitespaces1 <*>
          Gen.maybe
            ((,) <$>
             set (mapped.trailingWhitespace) [Space] genExpr <*>
             Gen.maybe ((,) <$> fmap NonEmpty.toList genWhitespaces1 <*> genExpr))
        ] ++
        [pure (Break ()) | _inLoop ctxt] ++
        [pure (Continue ()) | _inLoop ctxt] ++
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
        Fundef <$>
          use currentIndentation <*> pure () <*>
          genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
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
            (,,,,) <$>
            use currentIndentation <*>
            genWhitespaces <*>
            genWhitespaces <*>
            genNewline <*>
            Gen.resize (n - n' - n'') (localState genBlock)
        If <$>
          use currentIndentation <*>
          pure () <*>
          fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
          genWhitespaces <*> genNewline <*> pure b <*> pure c
    , Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 (n-1))
        a <- Gen.resize n' genExpr
        b <- Gen.resize (n - n') (localState $ (inLoop .= True) *> genBlock)
        While <$>
          use currentIndentation <*>
          pure () <*>
          fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
          genWhitespaces <*> genNewline <*> pure b
    , Gen.sized $ \n -> do
        sz <- Gen.integral (Range.constant 1 5)
        n1 <- Gen.integral (Range.constant 1 $ n - 2)
        n2 <- Gen.integral (Range.constant 1 $ n - n1 - 1)
        n3 <- Gen.integral (Range.constant 1 $ n - n2 - n1)
        let remaining = n - n1 - n2 - n3
        (e1, e2) <-
          if remaining > 0
          then do
            n4 <- Gen.integral (Range.constant 0 remaining)
            e1 <- Gen.resize n4 genBlock
            e2 <- Gen.maybe (Gen.resize (remaining - n4) genBlock)
            (,) <$>
              fmap Just
                ((,,,,) <$>
                 use currentIndentation <*>
                 genWhitespaces <*>
                 genWhitespaces <*>
                 genNewline <*>
                 pure e1) <*>
              maybe
                 (pure Nothing)
                 (\e2' ->
                    fmap Just $
                    (,,,,) <$>
                    use currentIndentation <*>
                    genWhitespaces <*>
                    genWhitespaces <*>
                    genNewline <*>
                    pure e2')
                 e2
          else pure (Nothing, Nothing)
        TryExcept <$>
          use currentIndentation <*>
          pure () <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*>
          Gen.resize n1 genBlock <*>
          Gen.nonEmpty
            (Range.singleton sz)
            ((,,,,,) <$>
             use currentIndentation <*>
             (NonEmpty.toList <$> genWhitespaces1) <*>
             (ExceptAs () <$>
              (Gen.resize n2 genExpr & mapped.trailingWhitespace .~ [Space]) <*>
              Gen.maybe ((,) <$> (NonEmpty.toList <$> genWhitespaces1) <*> genIdent)) <*>
             genWhitespaces <*>
             genNewline <*>
             Gen.resize n3 genBlock) <*>
          pure e1 <*>
          pure e2
    , Gen.sized $ \n -> do
        n1 <- Gen.integral (Range.constant 1 $ n-1)
        n2 <- Gen.integral (Range.constant 1 n1)
        TryFinally <$>
          use currentIndentation <*>
          pure () <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*>
          Gen.resize n1 genBlock <*>
          use currentIndentation <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*>
          Gen.resize n2 genBlock
    , Gen.sized $ \n -> do
        n1 <- Gen.integral $ Range.constant 0 (n-1)
        ClassDef <$>
          use currentIndentation <*>
          pure () <*>
          genWhitespaces1 <*>
          genIdent <*>
          Gen.maybe
            ((,,) <$>
             genWhitespaces <*>
             (if n1 == 0
              then pure Nothing
              else fmap Just $ Gen.resize n1 genArgs1) <*>
             genWhitespaces) <*>
          genWhitespaces <*> genNewline <*>
          Gen.resize (n - n1 - 1) genBlock
    ] ++
    [ Gen.sized $ \n -> do
        n1 <- Gen.integral $ Range.constant 1 (max 1 $ n-2)
        n2 <- Gen.integral $ Range.constant 1 (max 1 $ n-n1-1)
        n3 <- Gen.integral $ Range.constant 1 (max 1 $ n-n1-n2)
        n4 <- Gen.integral $ Range.constant 0 (max 0 $ n-n1-n2-n3)
        For <$>
          use currentIndentation <*>
          pure () <*>
          (NonEmpty.toList <$> genWhitespaces1) <*>
          (Gen.resize n1 genAssignable & mapped.trailingWhitespace .~ [Space]) <*>
          (NonEmpty.toList <$> genWhitespaces1) <*>
          Gen.resize n2 genExpr <*>
          genWhitespaces <*> genNewline <*>
          Gen.resize n3 genBlock <*>
          if n4 == 0
          then pure Nothing
          else
            Gen.resize n4
              (fmap Just $
               (,,,,) <$>
               use currentIndentation <*>
               genWhitespaces <*> genWhitespaces <*>
               genNewline <*> genBlock)
    | n >= 4
    ]

genStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (Statement '[] ())
genStatement =
  Gen.sized $ \n ->
  if n < 4
  then do
    SmallStatements <$>
      use currentIndentation <*>
      localState genSmallStatement <*>
      pure [] <*>
      Gen.maybe genWhitespaces <*>
      fmap Just genNewline
  else
    Gen.scale (subtract 1) $
    Gen.choice
    [ CompoundStatement <$> localState genCompoundStatement
    , Gen.sized $ \n -> do
        n' <- Gen.integral (Range.constant 1 n)
        n'' <- Gen.integral (Range.constant 0 (n-n'))
        SmallStatements <$>
          use currentIndentation <*>
          Gen.resize n' (localState genSmallStatement) <*>
          (if n'' == 0
            then pure []
            else
             Gen.list
               (Range.singleton $ unSize n'')
               (Gen.resize ((n-n') `div` n'') $
                (,) <$> genWhitespaces <*> localState genSmallStatement)) <*>
          Gen.maybe genWhitespaces <*>
          (Just <$> genNewline)
    ]

genImportAs :: (HasTrailingWhitespace (e ()), MonadGen m) => m (e ()) -> m (Ident '[] ()) -> m (ImportAs e '[] ())
genImportAs me genIdent =
  Gen.sized $ \n -> do
    n' <- Gen.integral (Range.constant 1 n)
    let n'' = n - n'
    ImportAs () <$>
      set (mapped.trailingWhitespace) [Space] (Gen.resize n' me) <*>
      (if n'' <= 2
       then pure Nothing
       else fmap Just $ (,) <$> genWhitespaces1 <*> Gen.resize n'' genIdent)
