{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}
{-# language TypeSynonymInstances, FlexibleInstances #-}
{-# language TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

{-|
Module      : Language.Python.Validate.Syntax
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Syntax
  ( module Data.Validation
  , module Language.Python.Validate.Syntax.Error
    -- * Main validation functions
  , Syntax, ValidateSyntax, runValidateSyntax
  , validateModuleSyntax
  , validateStatementSyntax
  , validateExprSyntax
    -- * Miscellany
    -- ** Extra types
  , SyntaxContext(..), FunctionInfo(..), inLoop, inFunction, inGenerator, inParens
  , runValidateSyntax'
  , initialSyntaxContext
    -- ** Extra functions
  , reservedWords
  , canAssignTo
  , deleteBy'
  , deleteFirstsBy'
  , localNonlocals
    -- ** Validation functions
  , validateArgsSyntax
  , validateBlockSyntax
  , validateCompoundStatementSyntax
  , validateComprehensionSyntax
  , validateDecoratorSyntax
  , validateDictItemSyntax
  , validateExceptAsSyntax
  , validateIdentSyntax
  , validateImportAsSyntax
  , validateImportTargetsSyntax
  , validateListItemSyntax
  , validateParamsSyntax
  , validateSetItemSyntax
  , validateSimpleStatementSyntax
  , validateStringLiteralSyntax
  , validateSubscriptSyntax
  , validateSuiteSyntax
  , validateTupleItemSyntax
  , validateWhitespace
  )
where

import Data.Validation

import Control.Applicative ((<|>), liftA2)
import Control.Lens.Cons (snoc, _init)
import Control.Lens.Fold
  ((^..), (^?), (^?!), folded, allOf, toListOf, anyOf, lengthOf, has)
import Control.Lens.Getter ((^.), getting, view)
import Control.Lens.Prism (_Right, _Just)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.~), (%~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Lens.Traversal (traverseOf)
import Control.Monad (when)
import Control.Monad.State (State, put, modify, get, evalState)
import Control.Monad.Reader (ReaderT, local, ask, runReaderT)
import Data.Char (isAscii, ord)
import Data.Foldable (toList, traverse_)
import Data.Bitraversable (bitraverse)
import Data.Functor.Compose (Compose(..))
import Data.List (intersect, union)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Semigroup (Semigroup(..))
import Data.Validate.Monadic (ValidateM(..), bindVM, liftVM0, liftVM1, errorVM, errorVM1)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Optics
import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Import
import Language.Python.Syntax.Module
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace
import Language.Python.Validate.Syntax.Error

deleteBy' :: (a -> b -> Bool) -> a -> [b] -> [b]
deleteBy' _ _ [] = []
deleteBy' eq a (b:bs) = if a `eq` b then bs else b : deleteBy' eq a bs

deleteFirstsBy' :: (a -> b -> Bool) -> [a] -> [b] -> [a]
deleteFirstsBy' eq = foldl (flip (deleteBy' (flip eq)))

reservedWords :: [String]
reservedWords =
  [ "False"
  , "class"
  , "finally"
  , "is"
  , "return"
  , "None"
  , "continue"
  , "for"
  , "lambda"
  , "try"
  , "True"
  , "def"
  , "from"
  , "nonlocal"
  , "while"
  , "and"
  , "del"
  , "global"
  , "not"
  , "with"
  , "as"
  , "elif"
  , "if"
  , "or"
  , "yield"
  , "assert"
  , "else"
  , "import"
  , "pass"
  , "break"
  , "except"
  , "in"
  , "raise"
  ]

data Syntax

data FunctionInfo
  = FunctionInfo
  { _functionParams :: [String]
  , _asyncFunction :: Bool
  }
makeLenses ''FunctionInfo

data SyntaxContext
  = SyntaxContext
  { _inLoop :: Bool
  , _inFinally :: Bool
  , _inFunction :: Maybe FunctionInfo
  , _inGenerator :: Bool
  , _inClass :: Bool
  , _inParens :: Bool
  }
makeLenses ''SyntaxContext

type ValidateSyntax e = ValidateM (NonEmpty e) (ReaderT SyntaxContext (State [String]))

runValidateSyntax :: ValidateSyntax e a -> Validation (NonEmpty e) a
runValidateSyntax = runValidateSyntax' initialSyntaxContext []

runValidateSyntax' :: SyntaxContext -> [String] -> ValidateSyntax e a -> Validation (NonEmpty e) a
runValidateSyntax' ctxt nlscope =
  flip evalState nlscope .
  flip runReaderT ctxt . getCompose .
  unValidateM

localNonlocals :: ([String] -> [String]) -> ValidateSyntax e a -> ValidateSyntax e a
localNonlocals f v =
  ValidateM . Compose $ do
    before <- get
    modify f
    res <- getCompose $ unValidateM v
    put before
    pure res

initialSyntaxContext :: SyntaxContext
initialSyntaxContext =
  SyntaxContext
  { _inLoop = False
  , _inFinally = False
  , _inFunction = Nothing
  , _inGenerator = False
  , _inClass = False
  , _inParens = False
  }

validateIdentSyntax
  :: AsSyntaxError e a
  => Ident a
  -> ValidateSyntax e (Ident a)
validateIdentSyntax (MkIdent a name ws)
  | not (all isAscii name) = errorVM1 (_BadCharacter # (getAnn a, name))
  | null name = errorVM1 (_EmptyIdentifier # getAnn a)
  | otherwise =
      bindVM (liftVM0 $ view inFunction) $ \fi ->
        let
          reserved =
            reservedWords <>
            if fromMaybe False (fi ^? _Just.asyncFunction)
            then ["async", "await"]
            else []
        in
          if (name `elem` reserved)
            then errorVM1 (_IdentifierReservedWord # (getAnn a, name))
            else pure $ MkIdent a name ws

validateWhitespace
  :: (AsSyntaxError e a, Foldable f)
  => a
  -> f Whitespace
  -> ValidateSyntax e (f Whitespace)
validateWhitespace ann ws =
  liftVM0 ask `bindVM` \ctxt ->
  if _inParens ctxt
  then pure ws
  else if
    any
      (\case
          Newline{} -> True
          Comment{} -> False
          Continued{} -> False
          Tab -> False
          Space -> False)
      ws
  then errorVM1 (_UnexpectedNewline # ann)
  else if
    any
      (\case
          Newline{} -> False
          Comment{} -> True
          Continued{} -> False
          Tab -> False
          Space -> False)
      ws
  then errorVM1 (_UnexpectedComment # ann)
  else pure ws

validateAt
  :: (AsSyntaxError e a)
  => a
  -> At
  -> ValidateSyntax e At
validateAt a (MkAt ws) = MkAt <$> validateWhitespace a ws

validateComma
  :: (AsSyntaxError e a)
  => a
  -> Comma
  -> ValidateSyntax e Comma
validateComma a (MkComma ws) = MkComma <$> validateWhitespace a ws

validateDot
  :: (AsSyntaxError e a)
  => a
  -> Dot
  -> ValidateSyntax e Dot
validateDot a (MkDot ws) = MkDot <$> validateWhitespace a ws

validateColon
  :: (AsSyntaxError e a)
  => a
  -> Colon
  -> ValidateSyntax e Colon
validateColon a (MkColon ws) = MkColon <$> validateWhitespace a ws

validateSemicolon
  :: AsSyntaxError e a
  => Semicolon a
  -> ValidateSyntax e (Semicolon a)
validateSemicolon (MkSemicolon a ws) =
  MkSemicolon a <$> validateWhitespace (getAnn a) ws

validateEquals
  :: AsSyntaxError e a
  => a
  -> Equals
  -> ValidateSyntax e Equals
validateEquals a (MkEquals ws) = MkEquals <$> validateWhitespace a ws

validateAssignmentSyntax
  :: AsSyntaxError e a
  => a
  -> Expr a
  -> ValidateSyntax e (Expr a)
validateAssignmentSyntax a ex =
  (if
     lengthOf (getting $ _Tuple.tupleItems._TupleUnpack) ex > 1 ||
     lengthOf (getting $ _List.listItems._ListUnpack) ex > 1
   then errorVM1 $ _ManyStarredTargets # a
   else pure ()) *>
  (if canAssignTo ex
   then validateExprSyntax ex
   else errorVM1 $ _CannotAssignTo # (a, ex))

validateCompForSyntax
  :: AsSyntaxError e a
  => CompFor a
  -> ValidateSyntax e (CompFor a)
validateCompForSyntax (CompFor a b c d e) =
  (\c' -> CompFor a b c' d) <$>
  liftVM1 (local $ inGenerator .~ True) (validateAssignmentSyntax (getAnn a) c) <*>
  validateExprSyntax e

validateCompIfSyntax
  :: AsSyntaxError e a
  => CompIf a
  -> ValidateSyntax e (CompIf a)
validateCompIfSyntax (CompIf a b c) =
  CompIf a b <$> liftVM1 (local $ inGenerator .~ True) (validateExprSyntax c)

validateComprehensionSyntax
  :: AsSyntaxError e a
  => (ex a -> ValidateSyntax e (ex a))
  -> Comprehension ex a
  -> ValidateSyntax e (Comprehension ex a)
validateComprehensionSyntax f (Comprehension a b c d) =
  Comprehension a <$>
  liftVM1 (local $ inGenerator .~ True) (f b) <*>
  validateCompForSyntax c <*>
  liftVM1
    (local $ inGenerator .~ True)
    (traverse
      (bitraverse validateCompForSyntax validateCompIfSyntax)
      d)

validateStringPyChar
  :: AsSyntaxError e a
  => a
  -> PyChar
  -> ValidateSyntax e PyChar
validateStringPyChar a (Char_lit '\0') =
  errorVM1 $ _NullByte # a
validateStringPyChar _ a = pure a

validateBytesPyChar
  :: AsSyntaxError e a
  => a
  -> PyChar
  -> ValidateSyntax e PyChar
validateBytesPyChar a (Char_lit '\0') =
  errorVM1 $ _NullByte # a
validateBytesPyChar a (Char_lit c) | ord c >= 128 =
  errorVM1 $ _NonAsciiInBytes # (a, c)
validateBytesPyChar _ a = pure a

validateStringLiteralSyntax
  :: AsSyntaxError e a
  => StringLiteral a
  -> ValidateSyntax e (StringLiteral a)
validateStringLiteralSyntax (StringLiteral a b c d e f) =
  StringLiteral a b c d <$>
  traverse (validateStringPyChar $ getAnn a) e <*>
  validateWhitespace (getAnn a) f
validateStringLiteralSyntax (BytesLiteral a b c d e f) =
  BytesLiteral a b c d <$>
  traverse (validateBytesPyChar $ getAnn a) e <*>
  validateWhitespace (getAnn a) f
validateStringLiteralSyntax (RawStringLiteral a b c d e f) =
  RawStringLiteral a b c d e <$>
  validateWhitespace (getAnn a) f
validateStringLiteralSyntax (RawBytesLiteral a b c d e f) =
  RawBytesLiteral a b c d e <$>
  validateWhitespace (getAnn a) f

validateDictItemSyntax
  :: AsSyntaxError e a
  => DictItem a
  -> ValidateSyntax e (DictItem a)
validateDictItemSyntax (DictItem a b c d) =
  (\b' -> DictItem a b' c) <$>
  validateExprSyntax b <*>
  validateExprSyntax d
validateDictItemSyntax (DictUnpack a b c) =
  DictUnpack a <$>
  validateWhitespace (getAnn a) b <*>
  validateExprSyntax c

validateSubscriptSyntax
  :: AsSyntaxError e a
  => Subscript a
  -> ValidateSyntax e (Subscript a)
validateSubscriptSyntax (SubscriptExpr e) = SubscriptExpr <$> validateExprSyntax e
validateSubscriptSyntax (SubscriptSlice a b c d) =
  (\a' -> SubscriptSlice a' b) <$>
  traverse validateExprSyntax a <*>
  traverse validateExprSyntax c <*>
  traverseOf (traverse._2.traverse) validateExprSyntax d

validateListItemSyntax
  :: AsSyntaxError e a
  => ListItem a
  -> ValidateSyntax e (ListItem a)
validateListItemSyntax (ListItem a b) = ListItem a <$> validateExprSyntax b
validateListItemSyntax (ListUnpack a b c d) =
  ListUnpack a <$>
  traverseOf (traverse._2) (validateWhitespace $ getAnn a) b <*>
  validateWhitespace (getAnn a) c <*>
  validateExprSyntax d

validateSetItemSyntax
  :: AsSyntaxError e a
  => SetItem a
  -> ValidateSyntax e (SetItem a)
validateSetItemSyntax (SetItem a b) = SetItem a <$> validateExprSyntax b
validateSetItemSyntax (SetUnpack a b c d) =
  SetUnpack a <$>
  traverseOf (traverse._2) (validateWhitespace $ getAnn a) b <*>
  validateWhitespace (getAnn a) c <*>
  validateExprSyntax d

validateTupleItemSyntax
  :: AsSyntaxError e a
  => TupleItem a
  -> ValidateSyntax e (TupleItem a)
validateTupleItemSyntax (TupleItem a b) = TupleItem a <$> validateExprSyntax b
validateTupleItemSyntax (TupleUnpack a b c d) =
  TupleUnpack a <$>
  traverseOf (traverse._2) (validateWhitespace $ getAnn a) b <*>
  validateWhitespace (getAnn a) c <*>
  validateExprSyntax d

validateExprSyntax
  :: AsSyntaxError e a
  => Expr a
  -> ValidateSyntax e (Expr a)
validateExprSyntax (Unit a b c) =
  Unit a <$>
  liftVM1 (local $ inParens .~ True) (validateWhitespace (getAnn a) b) <*>
  validateWhitespace (getAnn a) c
validateExprSyntax (Lambda a b c d e) =
  let
    paramIdents = c ^.. folded.paramName.identValue
  in
    Lambda a <$>
    validateWhitespace (getAnn a) b <*>
    validateParamsSyntax True c <*>
    validateColon (getAnn a) d <*>
    liftVM1
      (local $
       \ctxt ->
          ctxt
          { _inLoop = False
          , _inFunction =
              fmap
                ((functionParams %~ (`union` paramIdents)) . (asyncFunction .~ False))
                (_inFunction ctxt) <|>
              Just (FunctionInfo paramIdents False)
          })
      (validateExprSyntax e)
validateExprSyntax (Yield a b c) =
  Yield a <$>
  validateWhitespace (getAnn a) b <*
  (liftVM0 ask `bindVM` \ctxt ->
      case _inFunction ctxt of
        Nothing
          | _inGenerator ctxt -> pure ()
          | otherwise -> errorVM1 (_YieldOutsideGenerator # getAnn a)
        Just info ->
          if info^.asyncFunction
          then errorVM1 $ _YieldInsideCoroutine # getAnn a
          else pure ()) <*>
  traverse validateExprSyntax c
validateExprSyntax (YieldFrom a b c d) =
  YieldFrom a <$>
  validateWhitespace (getAnn a) b <*>
  validateWhitespace (getAnn a) c <*
  (liftVM0 ask `bindVM` \ctxt ->
      case _inFunction ctxt of
        Nothing
          | _inGenerator ctxt -> pure ()
          | otherwise -> errorVM1 (_YieldOutsideGenerator # getAnn a)
        Just fi ->
          if fi ^. asyncFunction
          then errorVM1 (_YieldFromInsideCoroutine # getAnn a)
          else pure ()) <*>
  validateExprSyntax d
validateExprSyntax (Ternary a b c d e f) =
  (\b' d' f' -> Ternary a b' c d' e f') <$>
  validateExprSyntax b <*>
  validateExprSyntax d <*>
  validateExprSyntax f
validateExprSyntax (Subscript a b c d e) =
  (\b' d' -> Subscript a b' c d' e) <$>
  validateExprSyntax b <*>
  traverse validateSubscriptSyntax d
validateExprSyntax (Not a ws e) =
  Not a <$>
  validateWhitespace (getAnn a) ws <*>
  validateExprSyntax e
validateExprSyntax (Parens a ws1 e ws2) =
  Parens a ws1 <$>
  liftVM1 (local $ inParens .~ True) (validateExprSyntax e) <*>
  validateWhitespace (getAnn a) ws2
validateExprSyntax (Bool a b ws) = pure $ Bool a b ws
validateExprSyntax (UnOp a op expr) =
  UnOp a op <$> validateExprSyntax expr
validateExprSyntax (String a strLits) =
  if
    all
      (\case
          StringLiteral{} -> True
          RawStringLiteral{} -> True
          _ -> False)
      strLits
      ||
    all
      (\case
          BytesLiteral{} -> True
          RawBytesLiteral{} -> True
          _ -> False)
      strLits
  then
    String a <$> traverse validateStringLiteralSyntax strLits
  else
    errorVM1 (_Can'tJoinStringAndBytes # getAnn a)
validateExprSyntax (Int a n ws) = pure $ Int a n ws
validateExprSyntax (Float a n ws) = pure $ Float a n ws
validateExprSyntax (Imag a n ws) = pure $ Imag a n ws
validateExprSyntax (Ident a name) = Ident a <$> validateIdentSyntax name
validateExprSyntax (List a ws1 exprs ws2) =
  List a ws1 <$>
  liftVM1
    (local $ inParens .~ True)
    (traverseOf (traverse.traverse) validateListItemSyntax exprs) <*>
  validateWhitespace (getAnn a) ws2
validateExprSyntax (ListComp a ws1 comp ws2) =
  liftVM1
    (local $ inParens .~ True)
    (ListComp a ws1 <$>
     validateComprehensionSyntax validateExprSyntax comp) <*>
  validateWhitespace (getAnn a) ws2
validateExprSyntax (Generator a comp) =
  Generator a <$> validateComprehensionSyntax validateExprSyntax comp
validateExprSyntax (Await a ws expr) =
  bindVM (liftVM0 ask) $ \ctxt ->
  Await a <$>
  validateWhitespace (getAnn a) ws <*
  (if not $ fromMaybe False (ctxt ^? inFunction._Just.asyncFunction)
   then errorVM1 $ _AwaitOutsideCoroutine # getAnn a
   else pure () *>
   if ctxt^.inGenerator
   then errorVM1 $ _AwaitInsideComprehension # getAnn a
   else pure ()) <*>
  validateExprSyntax expr
validateExprSyntax (Deref a expr ws1 name) =
  Deref a <$>
  validateExprSyntax expr <*>
  validateWhitespace (getAnn a) ws1 <*>
  validateIdentSyntax name
validateExprSyntax (Call a expr ws args ws2) =
  Call a <$>
  validateExprSyntax expr <*>
  liftVM1 (local $ inParens .~ True) (validateWhitespace (getAnn a) ws) <*>
  liftVM1 (local $ inParens .~ True) (traverse validateArgsSyntax args) <*>
  validateWhitespace (getAnn a) ws2
validateExprSyntax (None a ws) = None a <$> validateWhitespace (getAnn a) ws
validateExprSyntax (Ellipsis a ws) = Ellipsis a <$> validateWhitespace (getAnn a) ws
validateExprSyntax (BinOp a e1 op e2) =
  BinOp a <$>
  validateExprSyntax e1 <*>
  pure op <*>
  validateExprSyntax e2
validateExprSyntax (Tuple a b comma d) =
  Tuple a <$>
  validateTupleItemSyntax b <*>
  validateComma (getAnn a) comma <*>
  traverseOf (traverse.traverse) validateTupleItemSyntax d
validateExprSyntax (DictComp a ws1 comp ws2) =
  liftVM1
    (local $ inParens .~ True)
    (DictComp a ws1 <$>
     validateComprehensionSyntax dictItem comp) <*>
  validateWhitespace (getAnn a) ws2
  where
    dictItem (DictUnpack a _ _) = errorVM1 (_InvalidDictUnpacking # getAnn a)
    dictItem a = validateDictItemSyntax a
validateExprSyntax (Dict a b c d) =
  Dict a b <$>
  liftVM1
    (local $ inParens .~ True)
    (traverseOf (traverse.traverse) validateDictItemSyntax c) <*>
  validateWhitespace (getAnn a) d
validateExprSyntax (SetComp a ws1 comp ws2) =
  liftVM1
    (local $ inParens .~ True)
    (SetComp a ws1 <$>
     validateComprehensionSyntax setItem comp) <*>
  validateWhitespace (getAnn a) ws2
  where
    setItem (SetUnpack a _ _ _) = errorVM1 (_InvalidSetUnpacking # getAnn a)
    setItem a = validateSetItemSyntax a
validateExprSyntax (Set a b c d) =
  Set a b <$>
  liftVM1
    (local $ inParens .~ True)
    (traverse validateSetItemSyntax c) <*>
  validateWhitespace (getAnn a) d

validateBlockSyntax
  :: AsSyntaxError e a
  => Block a
  -> ValidateSyntax e (Block a)
validateBlockSyntax (Block x b bs) =
  Block x <$>
  validateStatementSyntax b <*>
  traverseOf (traverse._Right) validateStatementSyntax bs

validateSuiteSyntax
  :: AsSyntaxError e a
  => Suite a
  -> ValidateSyntax e (Suite a)
validateSuiteSyntax (SuiteMany a b c d e) =
  (\b' -> SuiteMany a b' c d) <$>
  validateColon (getAnn a) b <*>
  validateBlockSyntax e
validateSuiteSyntax (SuiteOne a b c) =
  SuiteOne a <$>
  validateColon (getAnn a) b <*>
  validateSmallStatementSyntax c

validateDecoratorSyntax
  :: AsSyntaxError e a
  => Decorator a
  -> ValidateSyntax e (Decorator a)
validateDecoratorSyntax (Decorator a b c d e f g) =
  (\c' d' -> Decorator a b c' d' e f) <$>
  validateAt (getAnn a) c <*>
  isDecoratorValue d <*>
  traverseOf (traverse._1) validateBlankSyntax g
  where
    someDerefs Ident{} = True
    someDerefs (Deref _ a _ _) = someDerefs a
    someDerefs _ = False

    isDecoratorValue e@(Call _ a _ _ _) | someDerefs a = pure $ unsafeCoerce e
    isDecoratorValue e | someDerefs e = pure $ unsafeCoerce e
    isDecoratorValue _ = errorVM1 (_MalformedDecorator # getAnn a)

validateBlankSyntax :: AsSyntaxError e a => Blank a -> ValidateSyntax e (Blank a)
validateBlankSyntax (Blank a ws cmt) =
  (\ws' -> Blank a ws' cmt) <$>
  validateWhitespace (getAnn a) ws

validateCompoundStatementSyntax
  :: forall e a
   . AsSyntaxError e a
  => CompoundStatement a
  -> ValidateSyntax e (CompoundStatement a)
validateCompoundStatementSyntax (Fundef a decos idnts asyncWs ws1 name ws2 params ws3 mty body) =
  let
    paramIdents = params ^.. folded.paramName.identValue
  in
    (\decos' -> Fundef a decos' idnts) <$>
    traverse validateDecoratorSyntax decos <*>
    traverse (validateWhitespace $ getAnn a) asyncWs <*>
    validateWhitespace (getAnn a) ws1 <*>
    validateIdentSyntax name <*>
    pure ws2 <*>
    liftVM1 (local $ inParens .~ True) (validateParamsSyntax False params) <*>
    pure ws3 <*>
    traverse (bitraverse (validateWhitespace $ getAnn a) validateExprSyntax) mty <*>
    localNonlocals id
      (liftVM1
         (local $
          \ctxt ->
            ctxt
            { _inLoop = False
            , _inFunction =
                fmap
                  ((functionParams %~ (`union` paramIdents)) .
                   (asyncFunction %~ (|| isJust asyncWs)))
                  (_inFunction ctxt) <|>
                Just (FunctionInfo paramIdents $ isJust asyncWs)
            })
         (validateSuiteSyntax body))
validateCompoundStatementSyntax (If a idnts ws1 expr body elifs body') =
  If a idnts <$>
  validateWhitespace (getAnn a) ws1 <*>
  validateExprSyntax expr <*>
  validateSuiteSyntax body <*>
  traverse
    (\(a, b, c, d) ->
       (\c' -> (,,,) a b c') <$>
       validateExprSyntax c <*>
       validateSuiteSyntax d)
    elifs <*>
  traverseOf (traverse._3) validateSuiteSyntax body'
validateCompoundStatementSyntax (While a idnts ws1 expr body els) =
  While a idnts <$>
  validateWhitespace (getAnn a) ws1 <*>
  validateExprSyntax expr <*>
  liftVM1 (local $ (inFinally .~ False) . (inLoop .~ True)) (validateSuiteSyntax body) <*>
  traverseOf (traverse._3) validateSuiteSyntax els
validateCompoundStatementSyntax (TryExcept a idnts b e f k l) =
  TryExcept a idnts <$>
  validateWhitespace (getAnn a) b <*>
  validateSuiteSyntax e <*>
  traverse
    (\(idnts, f, g, j) ->
       (,,,) idnts <$>
       validateWhitespace (getAnn a) f <*>
       traverse validateExceptAsSyntax g <*>
       validateSuiteSyntax j)
    f <*
  (if anyOf (_init.folded._3) isNothing $ NonEmpty.toList f
   then errorVM1 $ _DefaultExceptMustBeLast # getAnn a
   else pure ()) <*>
  traverse
    (\(idnts, x, w) ->
       (,,) idnts <$>
       validateWhitespace (getAnn a) x <*>
       validateSuiteSyntax w)
    k <*>
  traverse
    (\(idnts, x, w) ->
       (,,) idnts <$>
       validateWhitespace (getAnn a) x <*>
       liftVM1 (local $ inFinally .~ True) (validateSuiteSyntax w))
    l
validateCompoundStatementSyntax (TryFinally a idnts b e idnts2 f i) =
  TryFinally a idnts <$>
  validateWhitespace (getAnn a) b <*>
  validateSuiteSyntax e <*> pure idnts2 <*>
  validateWhitespace (getAnn a) f <*>
  liftVM1 (local $ inFinally .~ True) (validateSuiteSyntax i)
validateCompoundStatementSyntax (ClassDef a decos idnts b c d g) =
  liftVM1 (local $ inLoop .~ False) $
  (\decos' -> ClassDef a decos' idnts) <$>
  traverse validateDecoratorSyntax decos <*>
  validateWhitespace (getAnn a) b <*>
  validateIdentSyntax c <*>
  traverse
    (\(x, y, z) ->
       (,,) <$>
       validateWhitespace (getAnn a) x <*>
       traverse
         (liftVM1 (local $ inParens .~ True) . validateArgsSyntax)
         y <*>
       validateWhitespace (getAnn a) z)
    d <*>
  liftVM1
    (local $ (inClass .~ True) . (inFunction .~ Nothing))
    (validateSuiteSyntax g)
validateCompoundStatementSyntax (For a idnts asyncWs b c d e h i) =
  bindVM (liftVM0 ask) $ \ctxt ->
  For a idnts <$
  (if isJust asyncWs && not (fromMaybe False $ ctxt ^? inFunction._Just.asyncFunction)
   then errorVM1 (_AsyncForOutsideCoroutine # getAnn a)
   else pure ()) <*>
  traverse (validateWhitespace $ getAnn a) asyncWs <*>
  validateWhitespace (getAnn a) b <*>
  validateAssignmentSyntax (getAnn a) c <*>
  validateWhitespace (getAnn a) d <*>
  traverse validateExprSyntax e <*>
  liftVM1
    (local $ (inFinally .~ False) . (inLoop .~ True))
    (validateSuiteSyntax h) <*>
  traverse
    (\(idnts, x, w) ->
       (,,) idnts <$>
       validateWhitespace (getAnn a) x <*>
       validateSuiteSyntax w)
    i
validateCompoundStatementSyntax (With a b asyncWs c d e) =
  bindVM (liftVM0 ask) $ \ctxt ->
  With a b <$
  (if isJust asyncWs && not (fromMaybe False $ ctxt ^? inFunction._Just.asyncFunction)
   then errorVM1 (_AsyncWithOutsideCoroutine # getAnn a)
   else pure ()) <*>
  traverse (validateWhitespace $ getAnn a) asyncWs <*>
  validateWhitespace (getAnn a) c <*>
  traverse
    (\(WithItem a b c) ->
        WithItem a <$>
        validateExprSyntax b <*>
        traverse
          (\(ws, b) ->
             (,) <$>
             validateWhitespace (getAnn a) ws <*>
             validateAssignmentSyntax (getAnn a) b)
          c)
    d <*>
  validateSuiteSyntax e

validateExceptAsSyntax
  :: AsSyntaxError e a
  => ExceptAs a
  -> ValidateSyntax e (ExceptAs a)
validateExceptAsSyntax (ExceptAs ann e f) =
  ExceptAs ann <$>
  validateExprSyntax e <*>
  traverse
    (\(a, b) ->
       (,) <$>
       validateWhitespace (getAnn ann) a <*>
       validateIdentSyntax b)
    f

validateImportAsSyntax
  :: AsSyntaxError e a
  => (t a -> ValidateSyntax e (t' a))
  -> ImportAs t a
  -> ValidateSyntax e (ImportAs t' a)
validateImportAsSyntax v (ImportAs x a b) =
  ImportAs x <$>
  v a <*>
  traverse
    (\(c, d) ->
       (,) <$>
       (c <$ validateWhitespace (getAnn x) (NonEmpty.toList c)) <*>
       validateIdentSyntax d)
    b

validateImportTargetsSyntax
  :: AsSyntaxError e a
  => ImportTargets a
  -> ValidateSyntax e (ImportTargets a)
validateImportTargetsSyntax (ImportAll a ws) =
  bindVM (liftVM0 ask) $ \ctxt ->
  if ctxt ^. inClass || has (inFunction._Just) ctxt
    then errorVM1 $ _WildcardImportInDefinition # getAnn a
    else ImportAll a <$> validateWhitespace (getAnn a) ws
validateImportTargetsSyntax (ImportSome a cs) =
  ImportSome a <$> traverse (validateImportAsSyntax validateIdentSyntax) cs
validateImportTargetsSyntax (ImportSomeParens a ws1 cs ws2) =
  liftVM1
    (local $ inParens .~ True)
    (ImportSomeParens a <$>
     validateWhitespace (getAnn a) ws1 <*>
     traverse (validateImportAsSyntax validateIdentSyntax) cs) <*>
  validateWhitespace (getAnn a) ws2

validateModuleNameSyntax
  :: forall e a
   . AsSyntaxError e a
  => ModuleName a
  -> ValidateSyntax e (ModuleName a)
validateModuleNameSyntax (ModuleNameOne a b) =
  ModuleNameOne a <$> validateIdentSyntax b
validateModuleNameSyntax (ModuleNameMany a b c d) =
  ModuleNameMany a <$>
  validateIdentSyntax b <*>
  validateDot (getAnn a) c <*>
  validateModuleNameSyntax d

validateRelativeModuleNameSyntax
  :: forall e a
   . AsSyntaxError e a
  => RelativeModuleName a
  -> ValidateSyntax e (RelativeModuleName a)
validateRelativeModuleNameSyntax (RelativeWithName ann dots mn) =
  RelativeWithName ann <$>
  traverse (validateDot $ getAnn ann) dots <*>
  validateModuleNameSyntax mn
validateRelativeModuleNameSyntax (Relative ann dots) =
  Relative ann <$>
  traverse (validateDot $ getAnn ann) dots

validateSimpleStatementSyntax
  :: forall e a
   . AsSyntaxError e a
  => SimpleStatement a
  -> ValidateSyntax e (SimpleStatement a)
validateSimpleStatementSyntax (Assert a b c d) =
  Assert a <$>
  validateWhitespace (getAnn a) b <*>
  validateExprSyntax c <*>
  traverseOf (traverse._2) validateExprSyntax d
validateSimpleStatementSyntax (Raise a ws f) =
  Raise a <$>
  validateWhitespace (getAnn a) ws <*>
  traverse
    (\(b, c) ->
       (,) <$>
       validateExprSyntax b <*>
       traverse
         (\(d, e) ->
            (,) <$>
            validateWhitespace (getAnn a) d <*>
            validateExprSyntax e)
         c)
    f
validateSimpleStatementSyntax (Return a ws expr) =
  liftVM0 ask `bindVM` \sctxt ->
    case _inFunction sctxt of
      Just{} ->
        Return a <$>
        validateWhitespace (getAnn a) ws <*>
        traverse validateExprSyntax expr
      _ -> errorVM1 (_ReturnOutsideFunction # getAnn a)
validateSimpleStatementSyntax (Expr a expr) =
  Expr a <$>
  validateExprSyntax expr
validateSimpleStatementSyntax (Assign a lvalue rs) =
  liftVM0 ask `bindVM` \sctxt ->
    let
      assigns =
        if isJust (_inFunction sctxt)
        then
          (lvalue : (snd <$> NonEmpty.init rs)) ^..
          folded.assignTargets.identValue
        else []
    in
      Assign a <$>
      validateAssignmentSyntax (getAnn a) lvalue <*>
      ((\a b -> case a of; [] -> pure b; a : as -> a :| (snoc as b)) <$>
       traverse
         (\(ws, b) ->
            (,) <$>
            validateEquals (getAnn a) ws <*>
            validateAssignmentSyntax (getAnn a) b)
         (NonEmpty.init rs) <*>
       (\(ws, b) -> (,) <$> validateEquals (getAnn a) ws <*> validateExprSyntax b)
         (NonEmpty.last rs)) <*
      liftVM0 (modify (assigns ++))
validateSimpleStatementSyntax (AugAssign a lvalue aa rvalue) =
  AugAssign a <$>
  (if canAssignTo lvalue
    then case lvalue of
      Ident{} -> validateExprSyntax lvalue
      Deref{} -> validateExprSyntax lvalue
      Subscript{} -> validateExprSyntax lvalue
      _ -> errorVM1 (_CannotAugAssignTo # (getAnn a, lvalue))
    else errorVM1 (_CannotAssignTo # (getAnn a, lvalue))) <*>
  pure aa <*>
  validateExprSyntax rvalue
validateSimpleStatementSyntax (Pass a ws) =
  Pass a <$> validateWhitespace (getAnn a) ws
validateSimpleStatementSyntax (Break a ws) =
  Break a <$
  (liftVM0 ask `bindVM` \sctxt ->
     if _inLoop sctxt
     then pure ()
     else errorVM1 (_BreakOutsideLoop # getAnn a)) <*>
  validateWhitespace (getAnn a) ws
validateSimpleStatementSyntax (Continue a ws) =
  Continue a <$
  (liftVM0 ask `bindVM` \sctxt ->
     (if _inLoop sctxt
      then pure ()
      else errorVM1 (_ContinueOutsideLoop # getAnn a)) *>
     (if _inFinally sctxt
      then errorVM1 (_ContinueInsideFinally # getAnn a)
      else pure ())) <*>
  validateWhitespace (getAnn a) ws
validateSimpleStatementSyntax (Global a ws ids) =
  liftVM0 ask `bindVM` \ctx ->
  let
    params = ctx ^.. inFunction.folded.functionParams.folded
  in
    Global a ws <$>
    traverse
      (\i ->
         let
           ival = i ^. getting identValue
         in
         (if ival `elem` params
          then errorVM1 $ _ParameterMarkedGlobal # (getAnn a, ival)
          else pure ()) *>
         validateIdentSyntax i)
      ids
validateSimpleStatementSyntax (Nonlocal a ws ids) =
  liftVM0 ask `bindVM` \sctxt ->
  liftVM0 get `bindVM` \nls ->
  (case deleteFirstsBy' (\a -> (==) (a ^. identValue)) (ids ^.. folded) nls of
     [] -> pure ()
     ids -> traverse_ (\e -> errorVM1 (_NoBindingNonlocal # e)) ids) *>
  case sctxt ^? inFunction._Just.functionParams of
    Nothing -> errorVM1 (_NonlocalOutsideFunction # getAnn a)
    Just params ->
      case intersect params (ids ^.. folded.identValue) of
        [] -> Nonlocal a ws <$> traverse validateIdentSyntax ids
        bad -> errorVM1 (_ParametersNonlocal # (getAnn a, bad))
validateSimpleStatementSyntax (Del a ws ids) =
  Del a ws <$>
  traverse
    (\x ->
       validateExprSyntax x <*
       if canDelete x
       then pure ()
       else errorVM1 $ _CannotDelete # (getAnn a, x))
    ids
validateSimpleStatementSyntax (Import a ws mns) =
  Import a ws <$>
  traverse (validateImportAsSyntax validateModuleNameSyntax) mns
validateSimpleStatementSyntax (From a ws1 mn ws2 ts) =
  From a ws1 <$>
  validateRelativeModuleNameSyntax mn <*>
  validateWhitespace (getAnn a) ws2 <*>
  validateImportTargetsSyntax ts

canDelete :: Expr a -> Bool
canDelete None{} = False
canDelete Ellipsis{} = False
canDelete UnOp{} = False
canDelete Int{} = False
canDelete Call{} = False
canDelete BinOp{} = False
canDelete Bool{} = False
canDelete Unit{} = False
canDelete Yield{} = False
canDelete YieldFrom{} = False
canDelete Ternary{} = False
canDelete ListComp{} = False
canDelete DictComp{} = False
canDelete Dict{} = False
canDelete SetComp{} = False
canDelete Set{} = False
canDelete Lambda{} = False
canDelete Float{} = False
canDelete Imag{} = False
canDelete Not{} = False
canDelete Generator{} = False
canDelete Await{} = False
canDelete String{} = False
canDelete (Parens _ _ a _) = canDelete a
canDelete (List _ _ a _) =
  all (allOf (folded._Exprs) canDelete) a &&
  not (any (\case; ListUnpack{} -> True; _ -> False) $ a ^.. folded.folded)
canDelete (Tuple _ a _ b) =
  all
    canDelete
    ((a ^?! _Exprs) : toListOf (folded.folded._Exprs) b) &&
  not (any (\case; TupleUnpack{} -> True; _ -> False) $ a : toListOf (folded.folded) b)
canDelete Deref{} = True
canDelete Subscript{} = True
canDelete Ident{} = True

validateSmallStatementSyntax
  :: AsSyntaxError e a
  => SmallStatement a
  -> ValidateSyntax e (SmallStatement a)
validateSmallStatementSyntax (MkSmallStatement s ss sc cmt nl) =
  (\s' ss' sc' -> MkSmallStatement s' ss' sc' cmt nl) <$>
  validateSimpleStatementSyntax s <*>
  traverse (bitraverse validateSemicolon validateSimpleStatementSyntax) ss <*>
  traverse validateSemicolon sc

validateStatementSyntax
  :: AsSyntaxError e a
  => Statement a
  -> ValidateSyntax e (Statement a)
validateStatementSyntax (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementSyntax c
validateStatementSyntax (SmallStatement idnts a) =
  SmallStatement idnts <$> validateSmallStatementSyntax a

canAssignTo :: Expr a -> Bool
canAssignTo None{} = False
canAssignTo Ellipsis{} = False
canAssignTo UnOp{} = False
canAssignTo Int{} = False
canAssignTo Call{} = False
canAssignTo BinOp{} = False
canAssignTo Bool{} = False
canAssignTo Unit{} = False
canAssignTo Yield{} = False
canAssignTo YieldFrom{} = False
canAssignTo Ternary{} = False
canAssignTo ListComp{} = False
canAssignTo DictComp{} = False
canAssignTo Dict{} = False
canAssignTo SetComp{} = False
canAssignTo Set{} = False
canAssignTo Lambda{} = False
canAssignTo Float{} = False
canAssignTo Imag{} = False
canAssignTo Not{} = False
canAssignTo Generator{} = False
canAssignTo Await{} = False
canAssignTo String{} = False
canAssignTo (Parens _ _ a _) = canAssignTo a
canAssignTo (List _ _ a _) =
  all (allOf (folded._Exprs) canAssignTo) a
canAssignTo (Tuple _ a _ b) =
  all canAssignTo ((a ^?! _Exprs) : toListOf (folded.folded._Exprs) b)
canAssignTo Deref{} = True
canAssignTo Subscript{} = True
canAssignTo Ident{} = True

validateArgsSyntax
  :: AsSyntaxError e a
  => CommaSep1' (Arg a)
  -> ValidateSyntax e (CommaSep1' (Arg a))
validateArgsSyntax e = unsafeCoerce e <$ go [] False False (toList e)
  where
    go
      :: AsSyntaxError e a
      => [String]
      -- ^ Have we seen a keyword argument?
      -> Bool
      -- ^ Have we seen a **argument?
      -> Bool
      -> [Arg a]
      -> ValidateSyntax e [Arg a]
    go _ _ _ [] = pure []
    go names False False (PositionalArg a expr : args) =
      liftA2 (:)
        (PositionalArg a <$> validateExprSyntax expr)
        (go names False False args)
    go names seenKeyword seenUnpack (PositionalArg a expr : args) =
      when seenKeyword (errorVM1 (_PositionalAfterKeywordArg # (getAnn a, expr))) *>
      when seenUnpack (errorVM1 (_PositionalAfterKeywordUnpacking # (getAnn a, expr))) *>
      go names seenKeyword seenUnpack args
    go names seenKeyword False (StarArg a ws expr : args) =
      liftA2 (:)
        (StarArg a <$> validateWhitespace (getAnn a) ws <*> validateExprSyntax expr)
        (go names seenKeyword False args)
    go names seenKeyword seenUnpack (StarArg a _ expr : args) =
      when seenKeyword (errorVM1 (_PositionalAfterKeywordArg # (getAnn a, expr))) *>
      when seenUnpack (errorVM1 (_PositionalAfterKeywordUnpacking # (getAnn a, expr))) *>
      go names seenKeyword seenUnpack args
    go names _ seenUnpack (KeywordArg a name ws2 expr : args)
      | _identValue name `elem` names =
          errorVM1 (_DuplicateArgument # (getAnn a, _identValue name)) <*>
          validateIdentSyntax name <*>
          go names True seenUnpack args
      | otherwise =
          liftA2 (:)
            (KeywordArg a <$>
             validateIdentSyntax name <*>
             pure ws2 <*>
             validateExprSyntax expr)
            (go (_identValue name:names) True seenUnpack args)
    go names seenKeyword _ (DoubleStarArg a ws expr : args) =
      liftA2 (:)
        (DoubleStarArg a <$>
         validateWhitespace (getAnn a) ws <*>
         validateExprSyntax expr)
        (go names seenKeyword True args)

newtype HaveSeenStarArg = HaveSeenStarArg Bool
newtype HaveSeenKeywordArg = HaveSeenKeywordArg Bool
newtype HaveSeenEmptyStarArg a = HaveSeenEmptyStarArg (Maybe a)

validateParamsSyntax
  :: forall e a
   . AsSyntaxError e a
  => Bool -- ^ These are the parameters to a lambda
  -> CommaSep (Param a)
  -> ValidateSyntax e (CommaSep (Param a))
validateParamsSyntax isLambda e =
  unsafeCoerce e <$
  go
    []
    (HaveSeenStarArg False)
    (HaveSeenEmptyStarArg Nothing)
    (HaveSeenKeywordArg False)
    (toList e)
  where
    checkTy
      :: a
      -> Maybe (Colon, Expr a)
      -> ValidateSyntax e (Maybe (Colon, Expr a))
    checkTy a mty =
      if isLambda
      then traverse (\_ -> errorVM1 (_TypedParamInLambda # a)) mty
      else traverseOf (traverse._2) validateExprSyntax mty

    go
      :: [String] -- identifiers that we've seen
      -> HaveSeenStarArg -- have we seen a star argument?
      -> HaveSeenEmptyStarArg a -- have we seen an empty star argument?
      -> HaveSeenKeywordArg -- have we seen a keyword parameter?
      -> [Param a]
      -> ValidateSyntax e [Param a]
    go _ _ (HaveSeenEmptyStarArg b) _ [] =
      case b of
        Nothing -> pure []
        Just b' -> errorVM1 $ _NoKeywordsAfterEmptyStarArg # b'
    go names bsa besa bkw@(HaveSeenKeywordArg False) (PositionalParam a name mty : params)
      | _identValue name `elem` names =
          errorVM1 (_DuplicateArgument # (getAnn a, _identValue name)) <*>
          validateIdentSyntax name <*>
          checkTy (getAnn a) mty <*>
          go (_identValue name:names) bsa besa bkw params
      | otherwise =
          liftA2
            (:)
            (PositionalParam a <$>
             validateIdentSyntax name <*>
             checkTy (getAnn a) mty)
            (go (_identValue name:names) bsa besa bkw params)
    go names (HaveSeenStarArg b) besa bkw (StarParam a _ name mty : params)
      | _identValue name `elem` names =
          if b
          then
            errorVM1 (_ManyStarredParams # getAnn a) <*>
            errorVM1 (_DuplicateArgument # (getAnn a, _identValue name)) <*>
            validateIdentSyntax name <*>
            checkTy (getAnn a) mty <*>
            go
              (_identValue name:names)
              (HaveSeenStarArg True)
              besa
              bkw
              params
          else
            errorVM1 (_DuplicateArgument # (getAnn a, _identValue name)) <*>
            validateIdentSyntax name <*>
            checkTy (getAnn a) mty <*>
            go
              (_identValue name:names)
              (HaveSeenStarArg True)
              besa
              bkw
              params
      | otherwise =
          if b
          then
            errorVM1 (_ManyStarredParams # getAnn a) <*>
            validateIdentSyntax name *>
            checkTy (getAnn a) mty *>
            go
              (_identValue name:names)
              (HaveSeenStarArg True)
              besa
              bkw
              params
          else
            validateIdentSyntax name *>
            checkTy (getAnn a) mty *>
            go
              (_identValue name:names)
              (HaveSeenStarArg True)
              besa
              bkw
              params
    go names (HaveSeenStarArg b) _ bkw (UnnamedStarParam a _ : params) =
      if b
      then
        errorVM1 (_ManyStarredParams # getAnn a) <*>
        go
          names
          (HaveSeenStarArg True)
          (HaveSeenEmptyStarArg $ Just $ getAnn a)
          bkw
          params
      else
        go
          names
          (HaveSeenStarArg True)
          (HaveSeenEmptyStarArg $ Just $ getAnn a)
          bkw
          params
    go names bsa besa bkw@(HaveSeenKeywordArg True) (PositionalParam a name mty : params) =
      let
        name' = _identValue name
        errs =
          foldr (<|)
            (_PositionalAfterKeywordParam # (getAnn a, name') :| [])
            [_DuplicateArgument # (getAnn a, name') | name' `elem` names]
      in
        errorVM errs <*>
        checkTy (getAnn a) mty <*>
        go (name':names) bsa besa bkw params
    go names bsa _ _ (KeywordParam a name mty ws2 expr : params)
      | _identValue name `elem` names =
          errorVM1 (_DuplicateArgument # (getAnn a, _identValue name)) <*>
          checkTy (getAnn a) mty <*>
          go names bsa (HaveSeenEmptyStarArg Nothing) (HaveSeenKeywordArg True) params
      | otherwise =
          liftA2 (:)
            (KeywordParam a <$>
             validateIdentSyntax name <*>
             checkTy (getAnn a) mty <*>
             pure ws2 <*>
             validateExprSyntax expr)
            (go
               (_identValue name:names)
               bsa
               (HaveSeenEmptyStarArg Nothing)
               (HaveSeenKeywordArg True)
               params)
    go names bsa besa bkw [DoubleStarParam a ws name mty]
      | _identValue name `elem` names =
          errorVM1 (_DuplicateArgument # (getAnn a, _identValue name)) <*>
          checkTy (getAnn a) mty <*
          go names bsa besa bkw []
      | otherwise =
          fmap pure $
          DoubleStarParam a ws <$>
          validateIdentSyntax name <*>
          checkTy (getAnn a) mty <*
          go names bsa besa bkw []
    go names bsa besa bkw (DoubleStarParam a _ name mty : _) =
      (if _identValue name `elem` names
       then errorVM1 (_DuplicateArgument # (getAnn a, _identValue name))
       else pure ()) *>
      errorVM1 (_UnexpectedDoubleStarParam # (getAnn a, _identValue name)) <*>
      checkTy (getAnn a) mty <*
      go names bsa besa bkw []

validateModuleSyntax
  :: AsSyntaxError e a
  => Module a
  -> ValidateSyntax e (Module a)
validateModuleSyntax m =
  case m of
    ModuleEmpty -> pure ModuleEmpty
    ModuleBlankFinal a -> ModuleBlankFinal <$> validateBlankSyntax a
    ModuleBlank a b c ->
      (\a' -> ModuleBlank a' b) <$>
      validateBlankSyntax a <*>
      validateModuleSyntax c
    ModuleStatement a b ->
     ModuleStatement <$>
     validateStatementSyntax a <*>
     validateModuleSyntax b
