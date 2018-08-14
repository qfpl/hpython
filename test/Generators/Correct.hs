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
import Data.Digit.HeXaDeCiMaL
import Data.Digit.Enum
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Semigroup ((<>))
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Shrink as Shrink
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
  , _inGenerator = False
  , _currentNonlocals = []
  , _willBeNonlocals = []
  , _inLoop = False
  , _currentIndentation = Indents [] ()
  }

data GenState
  = GenState
  { _inFunction :: Maybe [String]
  , _inGenerator :: Bool
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

localState :: MonadState s m => m a -> m a
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

genTuple :: (MonadGen m, MonadState GenState m) => m (Expr '[] ()) -> m (Expr '[] ())
genTuple expr =
  Tuple () <$>
  genTupleItem genWhitespaces expr <*>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' $ genTupleItem genWhitespaces expr)

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

genBlock :: (MonadGen m, MonadState GenState m) => m (Block '[] ())
genBlock =
  doIndent *>
  Gen.shrink (\(Block (a :| as)) -> Block . (a :|) <$> Shrink.list as) go <*
  doDedent
  where
    genLine =
      Gen.choice
        [ Right <$> genStatement
        , fmap Left $
          (,) <$>
          genWhitespaces <*>
          genNewline
        ]

    go =
      sizedBind (Right <$> genStatement) $ \st ->
      sizedBind (sizedList genLine) $ \sts ->
      Block . foldr NonEmpty.cons (st :| sts) <$> sizedList genLine

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

genPositionalParams :: MonadGen m => m (CommaSep (Param '[] ()))
genPositionalParams =
  Gen.scale (max 0 . subtract 1) $
  Gen.sized $ fmap (listToCommaSep . fmap (PositionalParam ())) . go []
  where
    go seen 0 = pure []
    go seen n = do
      i <- Gen.filter ((`notElem` seen) . _identValue) genIdent
      (i :) <$> go (_identValue i : seen) (n-1)

genKeywordParam :: (MonadGen m, MonadState GenState m) => [String] -> m (Param '[] ())
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

genParams :: (MonadGen m, MonadState GenState m) => m (CommaSep (Param '[] ()))
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
    genAssignable
    (Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr)

genCompIf :: (MonadGen m, MonadState GenState m) => m (CompIf '[] ())
genCompIf =
  CompIf () <$>
  genWhitespaces <*>
  sizedRecursive
    [ Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr ]
    []

genComprehension :: (MonadGen m, MonadState GenState m) => m (Comprehension '[] ())
genComprehension =
  sized3
    (Comprehension ())
    (Gen.filter (\case; Tuple{} -> False; _ -> True) genExpr)
    genCompFor
    (sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

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

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genExpr = genExpr' False

genStringLiterals :: MonadGen m => m (Expr '[] ())
genStringLiterals = do
  n <- Gen.integral (Range.constant 1 5)
  b <- Gen.bool_
  String () <$> go b n
  where
    ss = Gen.choice [genStringLiteral genPyChar, genRawStringLiteral]
    bs = Gen.choice [genBytesLiteral genPyChar, genRawBytesLiteral]
    go True 1 = pure <$> ss
    go False 1 = pure <$> bs
    go b n =
      NonEmpty.cons <$>
      (if b then ss else bs) <*>
      go b (n-1)

genExpr' :: (MonadGen m, MonadState GenState m) => Bool -> m (Expr '[] ())
genExpr' isExp = do
  isInFunction <- isJust <$> gets _inFunction
  isInGenerator <- gets _inGenerator
  sizedRecursive
    [ genUnit
    , genBool
    , if isExp then genSmallInt else genInt
    , if isExp then genSmallFloat else genFloat
    , Ident () <$> genIdent
    , genStringLiterals
    ]
    ([ genList genExpr
     , genStringLiterals
     , ListComp () <$> genWhitespaces <*> genComprehension <*> genWhitespaces
     , Generator () <$>
       localState (modify (\ctxt -> ctxt { _inGenerator = True }) *> genComprehension)
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
     , sizedBind genParams $ \a ->
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
                   (`union` paramIdents)
                   (_inFunction ctxt) <|>
                 Just paramIdents
             , _currentNonlocals = _willBeNonlocals ctxt <> _currentNonlocals ctxt
             }
           genExpr)
     ] ++
     [ Yield () <$>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         sizedMaybe genExpr
     | isInFunction || isInGenerator
     ] ++
     [ YieldFrom () <$>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         genExpr
     | isInFunction || isInGenerator
     ])

genSubscript :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
genSubscript =
  sized2M
    (\a b -> (\ws1 -> Subscript () a ws1 b) <$> genWhitespaces <*> genWhitespaces)
    genExpr
    (genSizedCommaSep1' genSubscriptItem)

genAssignable :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
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

genAugAssignable :: (MonadGen m, MonadState GenState m) => m (Expr '[] ())
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
     , sizedBind (sizedNonEmpty $ (,) <$> genWhitespaces <*> genAssignable) $ \a -> do
         isInFunction <- use inFunction
         when (isJust isInFunction) $
           willBeNonlocals %= ((a ^.. folded._2.cosmos._Ident._2.identValue) ++)
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
       genSizedCommaSep1' genExpr
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
    , sized2M
        (\a b -> (\ws -> Assert () ws a b) <$> genWhitespaces)
        genExpr
        (sizedMaybe ((,) <$> genWhitespaces <*> genExpr))
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
  genNewline
  where
    genDecoratorValue =
      Gen.choice
      [ Ident () <$> genIdent
      , sized2M
         (\a b -> (\ws1 -> Call () a ws1 b) <$> genWhitespaces <*> genWhitespaces)
         genDerefs
         (sizedMaybe genArgs)
      ]

    genDerefs =
      sizedRecursive
      [ Ident () <$> genIdent ]
      [ Deref () <$>
        genDerefs <*>
        genWhitespaces <*>
        genIdent
      ]

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
            (genSuite genSmallStatement genBlock)) $
        \b ->
      sizedBind (sizedList genDecorator) $ \c ->
      Fundef () c <$>
        use currentIndentation <*>
        genWhitespaces1 <*> genIdent <*> genWhitespaces <*> pure a <*>
        genWhitespaces <*> pure b
    , sized4M
        (\a b c d -> 
           If <$>
             use currentIndentation <*>
             pure () <*>
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
          use currentIndentation <*>
          pure () <*>
          fmap NonEmpty.toList genWhitespaces1 <*> pure a <*>
          pure b)
        genExpr
        (localState $ (inLoop .= True) *> genSuite genSmallStatement genBlock)
    , sized4M
        (\a b e1 e2 ->
          TryExcept <$>
          use currentIndentation <*>
          pure () <*>
          genWhitespaces <*>
          pure a <*>
          pure b <*>
          pure e1 <*>
          pure e2)
        (genSuite genSmallStatement genBlock)
        (sizedNonEmpty $
         sized2M
           (\a b -> 
            (,,,) <$>
            use currentIndentation <*>
            (NonEmpty.toList <$> genWhitespaces1) <*>
            (ExceptAs ()
              (a & trailingWhitespace .~ [Space]) <$>
              Gen.maybe ((,) <$> (NonEmpty.toList <$> genWhitespaces1) <*> genIdent)) <*>
            pure b)
           genExpr
           (genSuite genSmallStatement genBlock))
        (sizedMaybe $
         sizedBind (genSuite genSmallStatement genBlock) $ \a ->
         (,,) <$>
         use currentIndentation <*>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         pure a)
        (sizedMaybe $
         sizedBind (genSuite genSmallStatement genBlock) $ \a ->
         (,,) <$>
         use currentIndentation <*>
         (NonEmpty.toList <$> genWhitespaces1) <*>
         pure a)
    , sized2M
        (\a b ->
           TryFinally <$>
           use currentIndentation <*> pure () <*>
           (NonEmpty.toList <$> genWhitespaces1) <*>
           pure a <*>
           use currentIndentation <*>
           (NonEmpty.toList <$> genWhitespaces1) <*>
           pure b)
        (genSuite genSmallStatement genBlock)
        (genSuite genSmallStatement genBlock)
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
        (genSuite genSmallStatement genBlock)
    , sized2M
        (\a b ->
           With <$> use currentIndentation <*> pure () <*>
           (NonEmpty.toList <$> genWhitespaces1) <*>
           pure a <*> pure b)
        (genSizedCommaSep1 $
         WithItem () <$>
         genExpr <*>
         sizedMaybe ((,) <$> genWhitespaces <*> genAssignable))
        (genSuite genSmallStatement genBlock)
    , sized4M
        (\a b c d ->
           For <$> use currentIndentation <*> pure () <*>
           (NonEmpty.toList <$> genWhitespaces1) <*> pure a <*>
           (NonEmpty.toList <$> genWhitespaces1) <*> pure b <*>
           pure c <*>
           pure d)
        genAssignable
        genExpr
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
  Gen.shrink
    (\case
        SmallStatements a b c e f -> (\c' -> SmallStatements a b c' e f) <$> Shrink.list c
        _ -> []) $
  sizedRecursive
    [ sizedBind (localState genSmallStatement) $ \st ->
      sizedBind (sizedList $ (,) <$> genWhitespaces <*> localState genSmallStatement) $ \sts ->
      (\a c -> SmallStatements a st sts c . Right) <$>
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

genPyChar :: MonadGen m => m PyChar
genPyChar =
  Gen.choice
  [ pure Char_newline
  , Char_octal <$> Gen.element enumOctal <*> Gen.element enumOctal
  , Char_hex <$> genHexDigit <*> genHexDigit
  , Char_uni16 <$>
    genHexDigit <*>
    genHexDigit <*>
    genHexDigit <*>
    genHexDigit
  , do
      a <- genHexDigit
      b <- case a of
        HeXDigit1 -> pure HeXDigit0
        _ -> genHexDigit
      Char_uni32 HeXDigit0 HeXDigit0 a b <$>
        genHexDigit <*>
        genHexDigit <*>
        genHexDigit <*>
        genHexDigit
  , pure Char_esc_bslash
  , pure Char_esc_singlequote
  , pure Char_esc_doublequote
  , pure Char_esc_a
  , pure Char_esc_b
  , pure Char_esc_f
  , pure Char_esc_n
  , pure Char_esc_r
  , pure Char_esc_t
  , pure Char_esc_v
  , Char_lit <$> Gen.filter (/='\0') Gen.latin1
  ]
