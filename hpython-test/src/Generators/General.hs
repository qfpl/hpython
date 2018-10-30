{-# language DataKinds, TypeFamilies #-}
module Generators.General where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Iso (from)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.Internal.Syntax
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Whitespace

import Generators.Common
import Generators.Sized

genBlank :: MonadGen m => m (Blank ())
genBlank = Blank () <$> genWhitespaces <*> Gen.maybe genComment

genTuple :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genTuple expr =
  Tuple () <$>
  genTupleItem genWhitespaces expr <*>
  genWhitespaces <*>
  Gen.maybe (genSizedCommaSep1' $ genTupleItem genWhitespaces expr)

genParam :: MonadGen m => m (Expr '[] ()) -> m (Param '[] ())
genParam genExpr =
  sizedRecursive
    [ StarParam () <$>
      genWhitespaces <*>
      Gen.maybe genIdent <*>
      sizedMaybe ((,) <$> genAnyWhitespaces <*> genExpr)
    , DoubleStarParam () <$>
      genWhitespaces <*>
      genIdent <*>
      sizedMaybe ((,) <$> genAnyWhitespaces <*> genExpr)
    ]
    [ KeywordParam () <$>
      genIdent <*>
      sizedMaybe ((,) <$> genAnyWhitespaces <*> genExpr) <*>
      genWhitespaces <*>
      genExpr
    ]

genArg :: MonadGen m => m (Expr '[] ()) -> m (Arg '[] ())
genArg genExpr =
  sizedRecursive
    [ PositionalArg () <$> genExpr
    , KeywordArg () <$> genIdent <*> genWhitespaces <*> genExpr
    , StarArg () <$> genWhitespaces <*> genExpr
    , DoubleStarArg () <$> genWhitespaces <*> genExpr
    ]
    []

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
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

genBlock :: MonadGen m => m (Block '[] ())
genBlock =
  Block <$>
  Gen.list
    (Range.constant 0 10)
    ((,) <$> genBlank <*> genNewline) <*>
  genStatement <*>
  sizedList
    (Gen.choice
     [ Right <$> genStatement
     , fmap Left $ (,) <$> genBlank <*> genNewline
     ])

genCompFor :: MonadGen m => m (CompFor '[] ())
genCompFor =
  sized2M
    (\a b ->
        (\ws1 ws2 -> CompFor () ws1 a ws2 b) <$>
        genWhitespaces <*>
        genWhitespaces)
    genExpr
    genExpr

genCompIf :: MonadGen m => m (CompIf '[] ())
genCompIf =
  CompIf () <$>
  genWhitespaces <*>
  Gen.scale (max 0 . subtract 1) genExpr

genComprehension :: MonadGen m => m (e '[] ()) -> m (Comprehension e '[] ())
genComprehension me =
  sized3
    (Comprehension ())
    me
    genCompFor
    (sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSubscript :: MonadGen m => m (Subscript '[] ())
genSubscript =
  sizedRecursive
    [ SubscriptExpr <$> genExpr
    , sized3M
        (\a b c -> (\ws -> SubscriptSlice a ws b c) <$> genWhitespaces)
        (sizedMaybe genExpr)
        (sizedMaybe genExpr)
        (sizedMaybe $ (,) <$> genWhitespaces <*> sizedMaybe genExpr)
    ]
    []

genDictComp :: MonadGen m => m (Comprehension DictItem '[] ())
genDictComp =
  sized3
    (Comprehension ())
    (genDictItem genExpr)
    genCompFor
    (sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSetComp :: MonadGen m => m (Comprehension SetItem '[] ())
genSetComp =
  sized3
    (Comprehension ())
    (genSetItem genAnyWhitespaces genExpr)
    genCompFor
    (sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

-- | This is necessary to prevent generating exponentials that will take forever to evaluate
-- when python does constant folding
genExpr :: MonadGen m => m (Expr '[] ())
genExpr = genExpr' False

genPyChar' :: MonadGen m => m PyChar
genPyChar' = genPyChar Gen.unicode

genRawStringLiteral :: MonadGen m => m (StringLiteral ())
genRawStringLiteral =
  Gen.choice
  [ RawStringLiteral () <$>
    genRawStringPrefix <*>
    pure LongString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) genPyChar' <*>
    genWhitespaces
  , RawStringLiteral () <$>
    genRawStringPrefix <*>
    pure ShortString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) genPyChar' <*>
    genWhitespaces
  ]

genRawBytesLiteral :: MonadGen m => m (StringLiteral ())
genRawBytesLiteral =
  Gen.choice
  [ RawBytesLiteral () <$>
    genRawBytesPrefix <*>
    pure LongString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) genPyChar' <*>
    genWhitespaces
  , RawBytesLiteral () <$>
    genRawBytesPrefix <*>
    pure ShortString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) genPyChar' <*>
    genWhitespaces
  ]


genExpr' :: MonadGen m => Bool -> m (Expr '[] ())
genExpr' isExp =
  sizedRecursive
    [ genNone
    , genEllipsis
    , genUnit
    , genBool
    , if isExp then genSmallInt else genInt
    , if isExp then genSmallFloat else genFloat
    , genImag
    , Ident <$> genIdent
    , String () <$>
      Gen.nonEmpty
        (Range.constant 1 5)
        (Gen.choice
           [ genStringLiteral genPyChar'
           , genBytesLiteral genPyChar'
           , genRawStringLiteral
           , genRawBytesLiteral
           ])
    ]
    [ List () <$>
      genWhitespaces <*>
      Gen.maybe (genSizedCommaSep1' $ genListItem genWhitespaces genExpr) <*>
      genWhitespaces
    , Dict () <$>
      genWhitespaces <*>
      sizedMaybe (genSizedCommaSep1' $ genDictItem genExpr) <*>
      genWhitespaces
    , Set () <$>
      genWhitespaces <*>
      genSizedCommaSep1' (genSetItem genWhitespaces genExpr) <*>
      genWhitespaces
    , ListComp () <$>
      genWhitespaces <*>
      genComprehension genExpr <*>
      genWhitespaces
    , DictComp () <$>
      genWhitespaces <*>
      genDictComp <*>
      genWhitespaces
    , SetComp () <$>
      genWhitespaces <*>
      genSetComp <*>
      genWhitespaces
    , Generator () <$> genComprehension genExpr
    , Gen.subtermM
        genExpr
        (\a -> Await () <$> genWhitespaces <*> pure a)
    , Gen.subtermM
        genExpr
        (\a ->
            Deref () <$>
            pure a <*>
            genWhitespaces <*>
            genIdent)
    , sized2M
        (\a b ->
           (\ws1 ws2 -> Call () a ws1 b ws2) <$>
           genWhitespaces <*>
           genWhitespaces)
        genExpr
        (sizedMaybe . genSizedCommaSep1' $ genArg genExpr)
    , sized2M
        (\a b ->
           (\ws1 ws2 -> Subscript () a ws1 b ws2) <$>
           genWhitespaces <*>
           genWhitespaces)
        genExpr
        (genSizedCommaSep1' genSubscript)
    , sizedBind genExpr $ \a -> do
        op <- genOp
        sizedBind (genExpr' $ case op of; Exp{} -> True; _ -> False) $ \b ->
          pure $ BinOp () a op b
    , Gen.subtermM
        (genExpr' isExp)
        (\a -> Parens () <$> genWhitespaces <*> pure a <*> genWhitespaces)
    , genTuple genExpr
    , Not () <$> genWhitespaces <*> genExpr
    , UnOp () <$> genUnOp <*> genExpr
    , sized3M
        (\a b c ->
           (\ws1 ws2 -> Ternary () a ws1 b ws2 c) <$>
           genWhitespaces <*>
           genWhitespaces)
        genExpr
        genExpr
        genExpr
    , Yield () <$> genWhitespaces <*> sizedMaybe genExpr
    , YieldFrom () <$> genWhitespaces <*> genWhitespaces <*> genExpr
    , Gen.subtermM
        genExpr
        (\a ->
           Lambda () <$>
           genWhitespaces <*>
           genSizedCommaSep (genParam genExpr) <*>
           genWhitespaces <*>
           pure a)
    ]

genSmallStatement :: MonadGen m => m (SmallStatement '[] ())
genSmallStatement =
  sizedRecursive
    [ Pass () <$> genWhitespaces
    , Break () <$> genWhitespaces
    , Continue () <$> genWhitespaces
    ]
    [ Expr () <$> genExpr
    , sized2
        (Assign ())
        genExpr
        (sizedNonEmpty ((,) <$> genWhitespaces <*> genExpr))
    , sized2M
        (\a b -> (\aa -> AugAssign () a aa b) <$> genAugAssign)
        genExpr
        genExpr
    , Global () <$>
        genWhitespaces1 <*>
        genSizedCommaSep1 genIdent
    , Del () <$>
      genWhitespaces1 <*>
      genSizedCommaSep1' genExpr
    , Nonlocal () <$>
      genWhitespaces1 <*>
      genSizedCommaSep1 genIdent
    , Return () <$>
      genWhitespaces <*>
      sizedMaybe genExpr
    , Import () <$>
      genWhitespaces1 <*>
      genSizedCommaSep1 (genImportAs genModuleName genIdent)
    , From () <$>
      genWhitespaces <*>
      genRelativeModuleName <*>
      genWhitespaces <*>
      genImportTargets
    , Raise () <$>
      genWhitespaces <*>
      sizedMaybe ((,) <$> genExpr <*> Gen.maybe ((,) <$> genWhitespaces <*> genExpr))
    , sized2M
        (\a b -> (\ws -> Assert () ws a b) <$> genWhitespaces)
        genExpr
        (sizedMaybe ((,) <$> genWhitespaces <*> genExpr))
    ]

genDecorator :: MonadGen m => m (Decorator '[] ())
genDecorator =
  Decorator () <$>
  genIndents <*>
  genWhitespaces <*>
  genExpr <*>
  Gen.maybe genComment <*>
  genNewline <*>
  Gen.list (Range.constant 0 10) ((,) <$> genBlank <*> genNewline)

genCompoundStatement
  :: MonadGen m
  => m (CompoundStatement '[] ())
genCompoundStatement =
  sizedRecursive
    [ sized4M
        (\a b c d ->
           Fundef () a <$> genIndents <*>
           Gen.maybe genWhitespaces1 <*>
           genWhitespaces1 <*> genIdent <*>
           genWhitespaces <*> pure b <*>
           genWhitespaces <*> pure c <*> pure d)
        (sizedList genDecorator)
        (genSizedCommaSep $ genParam genExpr)
        (sizedMaybe $ (,) <$> genWhitespaces <*> genExpr)
        (genSuite genSmallStatement genBlock)
    , sized4M
        (\a b c d ->
           If <$> pure () <*> genIndents <*> genWhitespaces <*>
           pure a <*> pure b <*> pure c <*> pure d)
        genExpr
        (genSuite genSmallStatement genBlock)
        (sizedList $
         sized2M
           (\a b -> (,,,) <$> genIndents <*> genWhitespaces <*> pure a <*> pure b)
           genExpr
           (genSuite genSmallStatement genBlock))
        (sizedMaybe $
         (,,) <$>
         genIndents <*> genWhitespaces <*> genSuite genSmallStatement genBlock)
    , sized3M
        (\a b c ->
           While <$>
           pure () <*> genIndents <*>
           genWhitespaces <*> pure a <*> pure b <*> pure c)
        genExpr
        (genSuite genSmallStatement genBlock)
        (sizedMaybe $
         (,,) <$>
         genIndents <*> genWhitespaces <*> genSuite genSmallStatement genBlock)
    , sized4M
        (\a b c d ->
           TryExcept <$> pure () <*> genIndents <*> genWhitespaces <*>
           pure a <*> pure b <*> pure c <*> pure d)
        (genSuite genSmallStatement genBlock)
        (sizedNonEmpty $
         sized2M
           (\a b ->
              (,,,) <$> genIndents <*> genWhitespaces <*>
              Gen.maybe
                (ExceptAs () <$> pure a <*> Gen.maybe ((,) <$> genWhitespaces <*> genIdent)) <*>
              pure b)
           genExpr
           (genSuite genSmallStatement genBlock))
        (sizedMaybe $
         (,,) <$> genIndents <*> genWhitespaces <*>
         genSuite genSmallStatement genBlock)
        (sizedMaybe $
         (,,) <$> genIndents <*> genWhitespaces <*>
         genSuite genSmallStatement genBlock)
    , sized2M
        (\a b -> 
           TryFinally <$>
           pure () <*>
           genIndents <*>
           genWhitespaces <*>
           pure a <*>
           genIndents <*>
           genWhitespaces <*>
           pure b)
        (genSuite genSmallStatement genBlock)
        (genSuite genSmallStatement genBlock)
    , sized3M
        (\a b c ->
           ClassDef () a <$> genIndents <*> genWhitespaces1 <*> genIdent <*>
           Gen.maybe ((,,) <$> genWhitespaces <*> pure b <*> genWhitespaces) <*>
           pure c)
        (sizedList genDecorator)
        (sizedMaybe $ genSizedCommaSep1' $ genArg genExpr)
        (genSuite genSmallStatement genBlock)
    , sized4M
        (\a b c d ->
           For <$>
           pure () <*> genIndents <*> Gen.maybe genWhitespaces1 <*>
           genWhitespaces <*> pure a <*>
           genWhitespaces <*> pure b <*>
           pure c <*> pure d)
        genExpr
        (genSizedCommaSep1' genExpr)
        (genSuite genSmallStatement genBlock)
        (sizedMaybe $
         (,,) <$>
         genIndents <*> genWhitespaces <*> genSuite genSmallStatement genBlock)
    , sized2M
        (\a b ->
           With <$>
           pure () <*> genIndents <*>
           Gen.maybe genWhitespaces1 <*> genWhitespaces <*> pure a <*> pure b)
        (genSizedCommaSep1 $
         WithItem () <$>
         genExpr <*>
         sizedMaybe ((,) <$> genWhitespaces <*> genExpr))
        (genSuite genSmallStatement genBlock)
    ]
    []

genStatement :: MonadGen m => m (Statement '[] ())
genStatement =
  sizedRecursive
    [ SimpleStatement <$> genIndents <*> genSimpleStatement genSmallStatement ]
    [ CompoundStatement <$> genCompoundStatement ]

genIndent :: MonadGen m => m Indent
genIndent =
  view (from indentWhitespaces) <$> genWhitespaces

genIndents :: MonadGen m => m (Indents ())
genIndents = (\is -> Indents is ()) <$> Gen.list (Range.constant 0 10) genIndent

genModule :: MonadGen m => m (Module '[] ())
genModule =
  sizedRecursive
    [ pure ModuleEmpty
    , ModuleBlankFinal <$> genBlank
    ]
    [ ModuleBlank <$>
      genBlank <*>
      genNewline <*>
      genModule
    , sized2
        ModuleStatement
        genStatement
        genModule
    ]

genImportAs :: MonadGen m => m (e ()) -> m (Ident '[] ()) -> m (ImportAs e '[] ())
genImportAs me genIdent =
  sized2M
    (\a b -> ImportAs () a <$> pure b)
    me
    (sizedMaybe $ (,) <$> genWhitespaces1 <*> genIdent)
