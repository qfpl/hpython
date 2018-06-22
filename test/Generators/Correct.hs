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
import Generators.Sized

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
genBlock = doIndent *> go <* doDedent
  where
    genLine =
      Gen.choice
        [ Right <$> genStatement
        , fmap Left $
          (,,) <$>
          genWhitespaces <*>
          Gen.maybe genComment <*>
          genNewline
        ]

    go =
      sizedBind (Right <$> genStatement) $ \st ->
      sizedBind (sizedList genLine) $ \sts ->
      Block . foldr NonEmpty.cons (st :| sts) <$> sizedList genLine

genPositionalArg :: MonadGen m => m (Arg '[] ())
genPositionalArg =
  sizedRecursive
    [ PositionalArg () <$> genExpr
    , StarArg () <$> genWhitespaces <*> genExpr
    ]
    []

genKeywordArg :: MonadGen m => m (Arg '[] ())
genKeywordArg =
  sizedRecursive
    [ KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr
    , DoubleStarArg () <$> genWhitespaces <*> genExpr
    ]
    []

genArgs :: MonadGen m => m (CommaSep (Arg '[] ()))
genArgs =
  sized2
    appendCommaSep
    (genSizedCommaSep genPositionalArg)
    (genSizedCommaSep genKeywordArg)

genArgs1 :: MonadGen m => m (CommaSep1 (Arg '[] ()))
genArgs1 =
  sized2
    (<>)
    (genSizedCommaSep1 genPositionalArg)
    (genSizedCommaSep1 genKeywordArg)

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
  sizedBind genPositionalParams $ \pparams ->
  let pparamNames = pparams ^.. folded.paramName.identValue in
  sizedBind (sizedMaybe $ genStarParam pparamNames) $ \sp ->
  let pparamNames' = pparamNames <> (sp ^.. _Just.paramName.identValue) in
  sizedBind (genSizedCommaSep (genKeywordParam pparamNames')) $ \kwparams ->
  let pparamNames'' = pparamNames' <> kwparams ^.. folded.paramName.identValue in
  sizedBind (sizedMaybe $ genDoubleStarParam pparamNames'') $ \dsp ->

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
  sized2M
    (\a b ->
       (\ws1 ws2 -> CompFor () ws1 a ws2 b) <$>
       genWhitespaces <*>
       genWhitespaces)
    genAssignable
    (Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr)

genCompIf :: MonadGen m => m (CompIf '[] ())
genCompIf =
  CompIf () <$>
  genWhitespaces <*>
  sizedRecursive
    [ Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr ]
    []

genComprehension :: MonadGen m => m (Comprehension '[] ())
genComprehension =
  sized3
    (Comprehension ())
    (Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr)
    genCompFor
    (sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSubscriptItem :: MonadGen m => m (Subscript '[] ())
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

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: MonadGen m => m (Expr '[] ())
genExpr = genExpr' False

genStringLiterals :: MonadGen m => m (Expr '[] ())
genStringLiterals = do
  n <- Gen.integral (Range.constant 1 5)
  b <- Gen.bool_
  String () <$> go b n
  where
    go True 1 = pure <$> genStringLiteral
    go False 1 = pure <$> genBytesLiteral
    go b n =
      NonEmpty.cons <$>
      (if b then genStringLiteral else genBytesLiteral) <*>
      go b (n-1)

genExpr' :: MonadGen m => Bool -> m (Expr '[] ())
genExpr' isExp =
  sizedRecursive
    [ genBool
    , if isExp then genSmallInt else genInt
    , Ident () <$> genIdent
    , genStringLiterals
    ]
    [ genList genExpr
    , genStringLiterals
    , ListComp () <$> genWhitespaces <*> genComprehension <*> genWhitespaces
    , Generator () <$> genComprehension
    , Dict () <$>
      genAnyWhitespaces <*>
      sizedMaybe (genSizedCommaSep1' $ genDictItem genExpr) <*>
      genWhitespaces
    , Set () <$> genAnyWhitespaces <*> genSizedCommaSep1' genExpr <*> genWhitespaces
    , genDeref
    , genParens (genExpr' isExp)
    , sized2M
        (\a b -> (\ws1 -> Call () a ws1 b) <$> genWhitespaces <*> genWhitespaces)
        genExpr
        genArgs
    , genSubscript
    , sizedBind genExpr $ \e1 ->
      sizedBind genOp $ \op ->
      sizedBind (genExpr' $ case op of; Exp{} -> True; _ -> False) $ \e2 ->
        pure $
        BinOp () (e1 & trailingWhitespace .~ [Space]) (op & trailingWhitespace .~ [Space]) e2
    , genTuple genExpr
    , Not () <$> (NonEmpty.toList <$> genWhitespaces1) <*> genExpr
    ]

genSubscript :: MonadGen m => m (Expr '[] ())
genSubscript =
  sized2M
    (\a b -> (\ws1 -> Subscript () a ws1 b) <$> genWhitespaces <*> genWhitespaces)
    genExpr
    (genSizedCommaSep1' genSubscriptItem)

genAssignable :: MonadGen m => m (Expr '[] ())
genAssignable =
  sizedRecursive
    [ Ident () <$> genIdent
    ]
    [ genList genAssignable
    , genParens genAssignable
    , genTuple genAssignable
    , genDeref
    , genSubscript
    ]

genAugAssignable :: MonadGen m => m (Expr '[] ())
genAugAssignable =
  sizedRecursive
    [ Ident () <$> genIdent ]
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
    (fmap pure $ [Pass ()] <> [Break () | _inLoop ctxt] <> [Continue () | _inLoop ctxt])
    ([ Expr () <$> genExpr
     , sizedBind genAssignable $ \a -> do
         isInFunction <- use inFunction
         when (isJust isInFunction) $
           willBeNonlocals %= ((a ^.. cosmos._Ident._2.identValue) ++)
         sizedBind genExpr $ \b -> Assign () a <$> genWhitespaces <*> pure b
     , sized2M
         (\a b -> AugAssign () a <$> genAugAssign <*> pure b)
         genAugAssignable
         genExpr
     , Global () <$>
       genWhitespaces1 <*>
       genSizedCommaSep1 genIdent
     , Del () <$>
       genWhitespaces1 <*>
       genSizedCommaSep1 genIdent
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
       sizedMaybe
         ((,) <$>
           set (mapped.trailingWhitespace) [Space] genExpr <*>
           Gen.maybe ((,) <$> fmap NonEmpty.toList genWhitespaces1 <*> genExpr))
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
       genExpr
     | isJust (_inFunction ctxt)
     ])

genCompoundStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (CompoundStatement '[] ())
genCompoundStatement =
  sizedRecursive
    [ sizedBind genParams $ \a ->
      let paramIdents = a ^.. folded.paramName.identValue in
      sizedBind
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
            genBlock) $
        \b ->
      Fundef <$>
        use currentIndentation <*> pure () <*>
        genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
        genWhitespaces <*> genWhitespaces <*> genNewline <*> pure b
    , sized4M
        (\a b c d -> 
           If <$>
             use currentIndentation <*>
             pure () <*>
             fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
             genWhitespaces <*> genNewline <*>
             pure b <*> pure c <*> pure d)
        genExpr
        (localState genBlock)
        (sizedList $
         sized2M
           (\a b ->
            (,,,,,) <$>
              use currentIndentation <*>
              genWhitespaces <*>
              pure a <*>
              genWhitespaces <*>
              genNewline <*>
              pure b)
            genExpr
            (localState genBlock))
        (sizedMaybe $
         sizedBind (localState genBlock) $ \a ->
          (,,,,) <$>
          use currentIndentation <*>
          genWhitespaces <*>
          genWhitespaces <*>
          genNewline <*>
          pure a)
    , sized2M
        (\a b ->
          While <$>
          use currentIndentation <*>
          pure () <*>
          fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
          genWhitespaces <*> genNewline <*> pure b)
        genExpr
        (localState $ (inLoop .= True) *> genBlock)
    , sized4M
        (\a b e1 e2 ->
          TryExcept <$>
          use currentIndentation <*>
          pure () <*>
          genWhitespaces <*> genWhitespaces <*> genNewline <*>
          pure a <*>
          pure b <*>
          pure e1 <*>
          pure e2)
        genBlock
        (sizedNonEmpty $
         sized2M
           (\a b -> 
            (,,,,,) <$>
            use currentIndentation <*>
            (NonEmpty.toList <$> genWhitespaces1) <*>
            (ExceptAs ()
              (a & trailingWhitespace .~ [Space]) <$>
              Gen.maybe ((,) <$> (NonEmpty.toList <$> genWhitespaces1) <*> genIdent)) <*>
            genWhitespaces <*>
            genNewline <*>
            pure b)
           genExpr
           genBlock)
        (sizedMaybe $
         sizedBind genBlock $ \a ->
         (,,,,) <$>
         use currentIndentation <*>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         genWhitespaces <*>
         genNewline <*>
         pure a)
        (sizedMaybe $
         sizedBind genBlock $ \a ->
         (,,,,) <$>
         use currentIndentation <*>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         genWhitespaces <*>
         genNewline <*>
         pure a)
    , sized2M
        (\a b ->
           TryFinally <$>
           use currentIndentation <*> pure () <*>
           (NonEmpty.toList <$> genWhitespaces1) <*> genWhitespaces <*> genNewline <*>
           pure a <*>
           use currentIndentation <*>
           (NonEmpty.toList <$> genWhitespaces1) <*> genWhitespaces <*> genNewline <*>
           pure b)
        genBlock
        genBlock
    , sized2M
        (\a b ->
           ClassDef <$>
           use currentIndentation <*> pure () <*>
           genWhitespaces1 <*> genIdent <*>
           pure a <*>
           genWhitespaces <*> genNewline <*>
           pure b)
        (sizedMaybe $
         (,,) <$>
         genWhitespaces <*>
         sizedMaybe genArgs1 <*>
         genWhitespaces)
        genBlock
    , sized4M
        (\a b c d ->
           For <$> use currentIndentation <*> pure () <*>
           (NonEmpty.toList <$> genWhitespaces1) <*> pure a <*>
           (NonEmpty.toList <$> genWhitespaces1) <*> pure b <*>
           genWhitespaces <*> genNewline <*>
           pure c <*>
           pure d)
        genAssignable
        genExpr
        genBlock
        (sizedMaybe $
         (,,,,) <$>
         use currentIndentation <*>
         (NonEmpty.toList <$> genWhitespaces1) <*> genWhitespaces <*> genNewline <*>
         genBlock)
    ]
    []

genStatement
  :: (HasCallStack, MonadGen m, MonadState GenState m)
  => m (Statement '[] ())
genStatement =
  sizedRecursive
    [ sizedBind (localState genSmallStatement) $ \st ->
      sizedBind (sizedList $ (,) <$> genWhitespaces <*> localState genSmallStatement) $ \sts ->
      (\a b c -> SmallStatements a st sts b (Just c)) <$>
      use currentIndentation <*>
      Gen.maybe genWhitespaces <*>
      genNewline
    ]
    [ CompoundStatement <$> localState genCompoundStatement ]

genImportAs :: (HasTrailingWhitespace (e ()), MonadGen m) => m (e ()) -> m (Ident '[] ()) -> m (ImportAs e '[] ())
genImportAs me genIdent =
  sized2
    (ImportAs ())
    (set (mapped.trailingWhitespace) [Space] me)
    (sizedMaybe $ (,) <$> genWhitespaces1 <*> genIdent)
