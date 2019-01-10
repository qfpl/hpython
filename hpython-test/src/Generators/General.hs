{-# language DataKinds, TypeFamilies #-}
module Generators.General where

import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Iso (from)
import Control.Lens.Review (review)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.DSL
import Language.Python.Optics
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Import
import Language.Python.Syntax.Module
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace

import Generators.Common
import Generators.Sized

genBlank :: MonadGen m => m (Blank ())
genBlank = Blank (Ann ()) <$> genWhitespaces <*> Gen.maybe genComment

genTuple :: MonadGen m => m (Expr '[] ()) -> m (Expr '[] ())
genTuple expr =
  Tuple (Ann ()) <$>
  genTupleItem genWhitespaces expr <*>
  genComma <*>
  Gen.maybe (genSizedCommaSep1' $ genTupleItem genWhitespaces expr)

genParam :: MonadGen m => m (Expr '[] ()) -> m (Param '[] ())
genParam genExpr =
  sizedRecursive
    [ StarParam (Ann ()) <$>
      genWhitespaces <*>
      genIdent <*>
      sizedMaybe ((,) <$> genColonAny <*> genExpr)
    , UnnamedStarParam (Ann ()) <$>
      genWhitespaces
    , DoubleStarParam (Ann ()) <$>
      genWhitespaces <*>
      genIdent <*>
      sizedMaybe ((,) <$> genColonAny <*> genExpr)
    ]
    [ KeywordParam (Ann ()) <$>
      genIdent <*>
      sizedMaybe ((,) <$> genColonAny <*> genExpr) <*>
      genWhitespaces <*>
      genExpr
    ]

genArg :: MonadGen m => m (Expr '[] ()) -> m (Arg '[] ())
genArg genExpr =
  sizedRecursive
    [ PositionalArg (Ann ()) <$> genExpr
    , KeywordArg (Ann ()) <$> genIdent <*> genWhitespaces <*> genExpr
    , StarArg (Ann ()) <$> genWhitespaces <*> genExpr
    , DoubleStarArg (Ann ()) <$> genWhitespaces <*> genExpr
    ]
    []

genIdent :: MonadGen m => m (Ident '[] ())
genIdent =
  MkIdent (Ann ()) <$>
  liftA2 (:)
    (Gen.choice [Gen.alpha, pure '_'])
    (Gen.list (Range.constant 0 49) (Gen.choice [Gen.alphaNum, pure '_'])) <*>
  genWhitespaces

genModuleName :: MonadGen m => m (ModuleName '[] ())
genModuleName =
  Gen.recursive Gen.choice
  [ ModuleNameOne (Ann ()) <$> genIdent ]
  [ ModuleNameMany (Ann ()) <$>
    genIdent <*>
    genDot <*>
    genModuleName
  ]

genRelativeModuleName :: MonadGen m => m (RelativeModuleName '[] ())
genRelativeModuleName =
  Gen.choice
  [ Relative (Ann ()) <$>
    Gen.nonEmpty (Range.constant 1 10) genDot
  , RelativeWithName (Ann ()) <$>
    Gen.list (Range.constant 1 10) genDot <*>
    genModuleName
  ]

genImportTargets :: MonadGen m => m (ImportTargets '[] ())
genImportTargets =
  Gen.choice
  [ ImportAll (Ann ()) <$> genWhitespaces
  , ImportSome (Ann ()) <$>
    genSizedCommaSep1 (genImportAs genIdent genIdent)
  , ImportSomeParens (Ann ()) <$>
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
        (\ws1 ws2 -> CompFor (Ann ()) ws1 a ws2 b) <$>
        genWhitespaces <*>
        genWhitespaces)
    genExpr
    genExpr

genCompIf :: MonadGen m => m (CompIf '[] ())
genCompIf =
  CompIf (Ann ()) <$>
  genWhitespaces <*>
  Gen.scale (max 0 . subtract 1) genExpr

genComprehension :: MonadGen m => m (e '[] ()) -> m (Comprehension e '[] ())
genComprehension me =
  sized3
    (Comprehension (Ann ()))
    me
    genCompFor
    (sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSubscript :: MonadGen m => m (Subscript '[] ())
genSubscript =
  sizedRecursive
    [ SubscriptExpr <$> genExpr
    , sized3M
        (\a b c -> (\ws -> SubscriptSlice a ws b c) <$> genColon)
        (sizedMaybe genExpr)
        (sizedMaybe genExpr)
        (sizedMaybe $ (,) <$> genColon <*> sizedMaybe genExpr)
    ]
    []

genDictComp :: MonadGen m => m (Comprehension DictItem '[] ())
genDictComp =
  sized3
    (Comprehension (Ann ()))
    (genDictItem genExpr)
    genCompFor
    (sizedList $ Gen.choice [Left <$> genCompFor, Right <$> genCompIf])

genSetComp :: MonadGen m => m (Comprehension SetItem '[] ())
genSetComp =
  sized3
    (Comprehension (Ann ()))
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
  [ RawStringLiteral (Ann ()) <$>
    genRawStringPrefix <*>
    pure LongString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) genPyChar' <*>
    genWhitespaces
  , RawStringLiteral (Ann ()) <$>
    genRawStringPrefix <*>
    pure ShortString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) genPyChar' <*>
    genWhitespaces
  ]

genRawBytesLiteral :: MonadGen m => m (StringLiteral ())
genRawBytesLiteral =
  Gen.choice
  [ RawBytesLiteral (Ann ()) <$>
    genRawBytesPrefix <*>
    pure LongString <*>
    genQuoteType <*>
    Gen.list (Range.constant 0 100) genPyChar' <*>
    genWhitespaces
  , RawBytesLiteral (Ann ()) <$>
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
    , Ident (Ann ()) <$> genIdent
    , String (Ann ()) <$>
      Gen.nonEmpty
        (Range.constant 1 5)
        (Gen.choice
           [ genStringLiteral genPyChar'
           , genBytesLiteral genPyChar'
           , genRawStringLiteral
           , genRawBytesLiteral
           ])
    ]
    [ List (Ann ()) <$>
      genWhitespaces <*>
      Gen.maybe (genSizedCommaSep1' $ genListItem genWhitespaces genExpr) <*>
      genWhitespaces
    , Dict (Ann ()) <$>
      genWhitespaces <*>
      sizedMaybe (genSizedCommaSep1' $ genDictItem genExpr) <*>
      genWhitespaces
    , Set (Ann ()) <$>
      genWhitespaces <*>
      genSizedCommaSep1' (genSetItem genWhitespaces genExpr) <*>
      genWhitespaces
    , ListComp (Ann ()) <$>
      genWhitespaces <*>
      genComprehension genExpr <*>
      genWhitespaces
    , DictComp (Ann ()) <$>
      genWhitespaces <*>
      genDictComp <*>
      genWhitespaces
    , SetComp (Ann ()) <$>
      genWhitespaces <*>
      genSetComp <*>
      genWhitespaces
    , Generator (Ann ()) <$> genComprehension genExpr
    , Gen.subtermM
        genExpr
        (\a -> Await (Ann ()) <$> genWhitespaces <*> pure a)
    , Gen.subtermM
        genExpr
        (\a ->
            Deref (Ann ()) <$>
            pure a <*>
            genWhitespaces <*>
            genIdent)
    , sized2M
        (\a b ->
           (\ws1 ws2 -> Call (Ann ()) a ws1 b ws2) <$>
           genWhitespaces <*>
           genWhitespaces)
        genExpr
        (sizedMaybe . genSizedCommaSep1' $ genArg genExpr)
    , sized2M
        (\a b ->
           (\ws1 ws2 -> Subscript (Ann ()) a ws1 b ws2) <$>
           genWhitespaces <*>
           genWhitespaces)
        genExpr
        (genSizedCommaSep1' genSubscript)
    , sizedBind genExpr $ \a -> do
        op <- genOp
        sizedBind (genExpr' $ case op of; Exp{} -> True; _ -> False) $ \b ->
          pure $ BinOp (Ann ()) a op b
    , Gen.subtermM
        (genExpr' isExp)
        (\a -> Parens (Ann ()) <$> genWhitespaces <*> pure a <*> genWhitespaces)
    , genTuple genExpr
    , Not (Ann ()) <$> genWhitespaces <*> genExpr
    , UnOp (Ann ()) <$> genUnOp <*> genExpr
    , sized3M
        (\a b c ->
           (\ws1 ws2 -> Ternary (Ann ()) a ws1 b ws2 c) <$>
           genWhitespaces <*>
           genWhitespaces)
        genExpr
        genExpr
        genExpr
    , Yield (Ann ()) <$> genWhitespaces <*> sizedCommaSep genExpr
    , YieldFrom (Ann ()) <$> genWhitespaces <*> genWhitespaces <*> genExpr
    , Gen.subtermM
        genExpr
        (\a ->
           Lambda (Ann ()) <$>
           genWhitespaces <*>
           sizedCommaSep (genParam genExpr) <*>
           genColon <*>
           pure a)
    ]

genSimpleStatement :: MonadGen m => m (SimpleStatement '[] ())
genSimpleStatement =
  sizedRecursive
    [ Pass (Ann ()) <$> genWhitespaces
    , Break (Ann ()) <$> genWhitespaces
    , Continue (Ann ()) <$> genWhitespaces
    ]
    [ Expr (Ann ()) <$> genExpr
    , sized2
        (Assign (Ann ()))
        genExpr
        (sizedNonEmpty ((,) <$> genEquals <*> genExpr))
    , sized2M
        (\a b -> (\aa -> AugAssign (Ann ()) a aa b) <$> genAugAssign)
        genExpr
        genExpr
    , Global (Ann ()) <$>
        genWhitespaces1 <*>
        genSizedCommaSep1 genIdent
    , Del (Ann ()) <$>
      genWhitespaces <*>
      genSizedCommaSep1' genExpr
    , Nonlocal (Ann ()) <$>
      genWhitespaces1 <*>
      genSizedCommaSep1 genIdent
    , Return (Ann ()) <$>
      genWhitespaces <*>
      sizedMaybe genExpr
    , Import (Ann ()) <$>
      genWhitespaces1 <*>
      genSizedCommaSep1 (genImportAs genModuleName genIdent)
    , From (Ann ()) <$>
      genWhitespaces <*>
      genRelativeModuleName <*>
      genWhitespaces <*>
      genImportTargets
    , Raise (Ann ()) <$>
      genWhitespaces <*>
      sizedMaybe ((,) <$> genExpr <*> Gen.maybe ((,) <$> genWhitespaces <*> genExpr))
    , sized2M
        (\a b -> (\ws -> Assert (Ann ()) ws a b) <$> genWhitespaces)
        genExpr
        (sizedMaybe ((,) <$> genComma <*> genExpr))
    ]

genDecorator :: MonadGen m => m (Decorator '[] ())
genDecorator =
  Decorator (Ann ()) <$>
  genIndents <*>
  genAt <*>
  genExpr <*>
  Gen.maybe genComment <*>
  genNewline <*>
  Gen.list (Range.constant 0 10) ((,) <$> genBlank <*> genNewline)

genFundef :: MonadGen m => m (Raw Fundef)
genFundef =
  sized4M
    (\a b c d ->
        MkFundef (Ann ()) a <$> genIndents <*>
        Gen.maybe genWhitespaces1 <*>
        genWhitespaces1 <*> genIdent <*>
        genWhitespaces <*> pure b <*>
        genWhitespaces <*> pure c <*> pure d)
    (sizedList genDecorator)
    (sizedCommaSep $ genParam genExpr)
    (sizedMaybe $ (,) <$> genWhitespaces <*> genExpr)
    (genSuite genSimpleStatement genBlock)

genCompoundStatement
  :: MonadGen m
  => m (CompoundStatement '[] ())
genCompoundStatement =
  sizedRecursive
    [ review _Fundef <$> genFundef
    , sized4M
        (\a b c d ->
           If (Ann ()) <$> genIndents <*> genWhitespaces <*>
           pure a <*> pure b <*> pure c <*> pure d)
        genExpr
        (genSuite genSimpleStatement genBlock)
        (sizedList $
         sized2M
           (\a b -> (,,,) <$> genIndents <*> genWhitespaces <*> pure a <*> pure b)
           genExpr
           (genSuite genSimpleStatement genBlock))
        (sizedMaybe $
         (,,) <$>
         genIndents <*> genWhitespaces <*> genSuite genSimpleStatement genBlock)
    , sized3M
        (\a b c ->
           While (Ann ()) <$>
           genIndents <*>
           genWhitespaces <*> pure a <*> pure b <*> pure c)
        genExpr
        (genSuite genSimpleStatement genBlock)
        (sizedMaybe $
         (,,) <$>
         genIndents <*> genWhitespaces <*> genSuite genSimpleStatement genBlock)
    , sized4M
        (\a b c d ->
           TryExcept (Ann ()) <$> genIndents <*> genWhitespaces <*>
           pure a <*> pure b <*> pure c <*> pure d)
        (genSuite genSimpleStatement genBlock)
        (sizedNonEmpty $
         sized2M
           (\a b ->
              (,,,) <$> genIndents <*> genWhitespaces <*>
              Gen.maybe
                (ExceptAs (Ann ()) <$> pure a <*> Gen.maybe ((,) <$> genWhitespaces <*> genIdent)) <*>
              pure b)
           genExpr
           (genSuite genSimpleStatement genBlock))
        (sizedMaybe $
         (,,) <$> genIndents <*> genWhitespaces <*>
         genSuite genSimpleStatement genBlock)
        (sizedMaybe $
         (,,) <$> genIndents <*> genWhitespaces <*>
         genSuite genSimpleStatement genBlock)
    , sized2M
        (\a b -> 
           TryFinally (Ann ()) <$>
           genIndents <*>
           genWhitespaces <*>
           pure a <*>
           genIndents <*>
           genWhitespaces <*>
           pure b)
        (genSuite genSimpleStatement genBlock)
        (genSuite genSimpleStatement genBlock)
    , sized3M
        (\a b c ->
           ClassDef (Ann ()) a <$> genIndents <*> genWhitespaces1 <*> genIdent <*>
           Gen.maybe ((,,) <$> genWhitespaces <*> pure b <*> genWhitespaces) <*>
           pure c)
        (sizedList genDecorator)
        (sizedMaybe $ genSizedCommaSep1' $ genArg genExpr)
        (genSuite genSimpleStatement genBlock)
    , sized4M
        (\a b c d ->
           For (Ann ()) <$>
           genIndents <*> Gen.maybe genWhitespaces1 <*>
           genWhitespaces <*> pure a <*>
           genWhitespaces <*> pure b <*>
           pure c <*> pure d)
        genExpr
        (genSizedCommaSep1' genExpr)
        (genSuite genSimpleStatement genBlock)
        (sizedMaybe $
         (,,) <$>
         genIndents <*> genWhitespaces <*> genSuite genSimpleStatement genBlock)
    , sized2M
        (\a b ->
           With (Ann ()) <$>
           genIndents <*>
           Gen.maybe genWhitespaces1 <*> genWhitespaces <*> pure a <*> pure b)
        (genSizedCommaSep1 $
         WithItem (Ann ()) <$>
         genExpr <*>
         sizedMaybe ((,) <$> genWhitespaces <*> genExpr))
        (genSuite genSimpleStatement genBlock)
    ]
    []

genStatement :: MonadGen m => m (Statement '[] ())
genStatement =
  sizedRecursive
    [ SmallStatement <$> genIndents <*> genSmallStatement genSimpleStatement ]
    [ CompoundStatement <$> genCompoundStatement ]

genIndent :: MonadGen m => m Indent
genIndent =
  view (from indentWhitespaces) <$> genWhitespaces

genIndents :: MonadGen m => m (Indents ())
genIndents = (\is -> Indents is (Ann ())) <$> Gen.list (Range.constant 0 10) genIndent

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

genImportAs :: MonadGen m => m (e '[] ()) -> m (Ident '[] ()) -> m (ImportAs e '[] ())
genImportAs me genIdent =
  sized2M
    (\a b -> ImportAs (Ann ()) a <$> pure b)
    me
    (sizedMaybe $ (,) <$> genWhitespaces1 <*> genIdent)
