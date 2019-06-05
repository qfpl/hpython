{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}

{-|
Module      : Language.Python.Validate.Indentation
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Indentation
  ( module Data.Validation
  , module Language.Python.Validate.Indentation.Error
    -- * Main validation functions
  , ValidateIndentation, runValidateIndentation
  , validateModuleIndentation
  , validateStatementIndentation
  , validateExprIndentation
    -- * Miscellany
    -- ** Extra types
  , NextIndent(..)
    -- ** Extra functions
  , equivalentIndentation
  , runValidateIndentation'
    -- ** Validation functions
  , validateArgsIndentation
  , validateBlockIndentation
  , validateCompoundStatementIndentation
  , validateDecoratorIndentation
  , validateExceptAsIndentation
  , validateParamsIndentation
  , validateSuiteIndentation
  )
where

import Data.Validation

import Control.Lens.Fold ((^?!), folded)
import Control.Lens.Getter ((^.))
import Control.Lens.Prism (_Right)
import Control.Lens.Review ((#))
import Control.Lens.Setter (over, mapped)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Tuple (_1, _2)
import Control.Monad.State (State, evalState, get, put)
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty (NonEmpty(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Validate.Monadic (ValidateM(..), liftVM0, errorVM, errorVM1)
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Optics
import Language.Python.Syntax.Ann
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Module
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Whitespace
import Language.Python.Validate.Indentation.Error

-- | "The next line must be..."
data NextIndent
  = GreaterThan
  | EqualTo
  deriving (Eq, Show)

type ValidateIndentation e = ValidateM (NonEmpty e) (State (NextIndent, [Indent]))

runValidateIndentation :: ValidateIndentation e a -> Validation (NonEmpty e) a
runValidateIndentation = runValidateIndentation' EqualTo []

runValidateIndentation' :: NextIndent -> [Indent] -> ValidateIndentation e a -> Validation (NonEmpty e) a
runValidateIndentation' ni is =
  flip evalState (ni, is) .
  getCompose .
  unValidateM

withNextIndent
  :: (NextIndent -> [Indent] -> ValidateIndentation e a)
  -> ValidateIndentation e a
withNextIndent f =
  ValidateM . Compose $
    get >>= getCompose . unValidateM . uncurry f

checkIndent :: AsIndentationError e a => Indents a -> ValidateIndentation e (Indents a)
checkIndent i =
  withNextIndent $ \ni i' ->
  let
    a = i ^. indentsAnn
    ii = fold (i ^. indentsValue)
    ii' = fold i'
    absolute1Comparison = compare (absoluteIndentLevel 1 ii) (absoluteIndentLevel 1 ii')
    absolute8Comparison = compare (absoluteIndentLevel 8 ii) (absoluteIndentLevel 8 ii')
  in
    case ni of
      GreaterThan ->
        case (absolute1Comparison, absolute8Comparison) of
          (GT, GT) -> pure i
          (GT, _) -> errorVM $ pure (_TabError # getAnn a)
          (_, GT) -> errorVM $ pure (_TabError # getAnn a)
          (EQ, EQ) -> errorVM $ pure (_ExpectedGreaterThan # (i', i))
          (_, EQ) -> errorVM $ pure (_TabError # getAnn a)
          (EQ, _) -> errorVM $ pure (_TabError # getAnn a)
          (LT, LT) -> errorVM $ pure (_ExpectedGreaterThan # (i', i))
      EqualTo ->
        case (absolute1Comparison, absolute8Comparison) of
          (EQ, EQ) -> pure i
          (EQ, _) -> errorVM $ pure (_TabError # getAnn a)
          (_, EQ) -> errorVM $ pure (_TabError # getAnn a)
          (GT, GT) -> errorVM $ pure (_ExpectedEqualTo # (i', i))
          (_, GT) -> errorVM $ pure (_TabError # getAnn a)
          (GT, _) -> errorVM $ pure (_TabError # getAnn a)
          (LT, LT) -> errorVM $ pure (_ExpectedEqualTo # (i', i))

setNextIndent :: NextIndent -> [Indent] -> ValidateIndentation e ()
setNextIndent ni is = liftVM0 $ put (ni, is)

equivalentIndentation :: [Whitespace] -> [Whitespace] -> Bool
equivalentIndentation [] [] = True
equivalentIndentation (x:_) [] =
  case x of
    Continued _ _ -> True
    _ -> False
equivalentIndentation [] (y:_) =
  case y of
    Continued _ _ -> True
    _ -> False
equivalentIndentation (x:xs) (y:ys) =
  case (x, y) of
    (Space, Space) -> equivalentIndentation xs ys
    (Tab, Tab) -> equivalentIndentation xs ys
    (Continued _ _, Continued _ _) -> True
    _ -> False

validateBlankIndentation
  :: forall e a.
     AsIndentationError e a
  => Blank a
  -> ValidateIndentation e (Blank a)
validateBlankIndentation (Blank a ws cmt) =
  if any (\case; Continued{} -> True; _ -> False) ws
  then errorVM1 $ _EmptyContinuedLine # getAnn a
  else pure $ Blank a ws cmt

validateBlockIndentation
  :: forall e a.
     AsIndentationError e a
  => Block a
  -> ValidateIndentation e (Block a)
validateBlockIndentation (Block x b bs) =
  (\x' (b' :| bs') ->
     case b' of
       Right b'' -> Block x' b'' bs'
       _ -> error "impossible") <$>
  traverseOf (traverse._1) validateBlankIndentation x <*>
  go False (Right b) bs
  where
    is = (Right b:|bs) ^?! folded._Right._Indents.indentsValue

    go
      :: Bool
      -> Either
           (Blank a, Newline)
           (Statement a)
      -> [Either (Blank a, Newline) (Statement a)]
      -> ValidateIndentation e
         (NonEmpty
            (Either
               (Blank a, Newline)
               (Statement a)))
    go flag (Left e) rest =
        case rest of
          [] ->
            pure . Left <$>
            traverseOf _1 validateBlankIndentation e
          r : rs ->
            NonEmpty.cons . Left <$>
            traverseOf _1 validateBlankIndentation e <*>
            go flag r rs
    go flag (Right st) rest =
      let
        validated =
          Right <$
          (if flag then setNextIndent EqualTo is else pure ()) <*>
          validateStatementIndentation st
      in
      case rest of
        [] -> (:| []) <$> validated
        r : rs -> NonEmpty.cons <$> validated <*> go True r rs

validateSuiteIndentation
  :: AsIndentationError e a
  => Indents a
  -> Suite a
  -> ValidateIndentation e (Suite a)
validateSuiteIndentation idnt (SuiteMany ann a b c d) =
  SuiteMany ann a b c <$
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation d
validateSuiteIndentation _ (SuiteOne ann a b) =
  SuiteOne ann a <$> validateSmallStatementIndentation b

validateExprIndentation
  :: AsIndentationError e a
  => Expr a
  -> ValidateIndentation e (Expr a)
validateExprIndentation e = pure $ unsafeCoerce e

validateParamsIndentation
  :: AsIndentationError e a
  => CommaSep (Param a)
  -> ValidateIndentation e (CommaSep (Param a))
validateParamsIndentation e = pure $ unsafeCoerce e

validateArgsIndentation
  :: AsIndentationError e a
  => CommaSep (Arg a)
  -> ValidateIndentation e (CommaSep (Arg a))
validateArgsIndentation e = pure $ unsafeCoerce e

validateExceptAsIndentation
  :: AsIndentationError e a
  => ExceptAs a
  -> ValidateIndentation e (ExceptAs a)
validateExceptAsIndentation (ExceptAs ann e f) =
  ExceptAs ann <$>
  validateExprIndentation e <*>
  pure (over (traverse._2) coerce f)

validateDecoratorIndentation
  :: AsIndentationError e a
  => Decorator a
  -> ValidateIndentation e (Decorator a)
validateDecoratorIndentation (Decorator a b c d e f g) =
  (\b' -> Decorator a b' c (unsafeCoerce d) e f) <$>
  checkIndent b <*>
  traverseOf (traverse._1) validateBlankIndentation g

validateCompoundStatementIndentation
  :: forall e a
   . AsIndentationError e a
  => CompoundStatement a
  -> ValidateIndentation e (CompoundStatement a)
validateCompoundStatementIndentation (Fundef a decos idnt asyncWs ws1 name ws2 params ws3 mty s) =
  (\decos' idnt' params' ->
     Fundef a decos' idnt' asyncWs ws1 (coerce name) ws2 params' ws3 (unsafeCoerce mty)) <$>
  traverse validateDecoratorIndentation decos <*>
  checkIndent idnt <*>
  validateParamsIndentation params <*>
  validateSuiteIndentation idnt s
validateCompoundStatementIndentation (If a idnt ws1 expr s elifs body1) =
  (\idnt' -> If a idnt' ws1) <$>
  checkIndent idnt <*>
  validateExprIndentation expr <*>
  validateSuiteIndentation idnt s <*>
  traverse
    (\(idnt2, a, b, c) ->
       (,,,) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*>
       pure a <*>
       validateExprIndentation b <*>
       validateSuiteIndentation idnt c)
    elifs <*>
  traverse
    (\(idnt2, a, b) ->
       (,,) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*>
       pure a <*>
       validateSuiteIndentation idnt b)
    body1
validateCompoundStatementIndentation (While a idnt ws1 expr s els) =
  (\idnt' expr' -> While a idnt' ws1 expr') <$>
  checkIndent idnt <*>
  validateExprIndentation expr <*>
  validateSuiteIndentation idnt s <*>
  traverse
    (\(idnt2, a, b) ->
       (,,) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*>
       pure a <*>
       validateSuiteIndentation idnt b)
    els
validateCompoundStatementIndentation (TryExcept a idnt b c d e f) =
  (\idnt' -> TryExcept a idnt' b) <$>
  checkIndent idnt <*>
  validateSuiteIndentation idnt c <*>
  traverse
    (\(a, b, c, d) ->
       (\a' -> (,,,) a' b) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent a <*>
       traverse validateExceptAsIndentation c <*>
       validateSuiteIndentation idnt d)
    d <*
  setNextIndent EqualTo (idnt ^. indentsValue) <*>
  traverse
    (\(idnt2, a, b) ->
       (\idnt2' -> (,,) idnt2' a) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*>
       validateSuiteIndentation idnt b)
    e <*
  setNextIndent EqualTo (idnt ^. indentsValue) <*>
  traverse
    (\(idnt2, a, b) ->
       (\idnt2' -> (,,) idnt2' a) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*>
       validateSuiteIndentation idnt b)
    f
validateCompoundStatementIndentation (TryFinally a idnt b c idnt2 d e) =
  (\idnt' c' idnt2' -> TryFinally a idnt' b c' idnt2' d) <$>
  checkIndent idnt <*>
  validateSuiteIndentation idnt c <*
  setNextIndent EqualTo (idnt ^. indentsValue) <*>
  checkIndent idnt2 <*>
  validateSuiteIndentation idnt e
validateCompoundStatementIndentation (For a idnt asyncWs b c d e h i) =
  (\idnt' c' -> For a idnt' asyncWs b c' d) <$>
  checkIndent idnt <*>
  validateExprIndentation c <*>
  traverse validateExprIndentation e <*>
  validateSuiteIndentation idnt h <*
  setNextIndent EqualTo (idnt ^. indentsValue) <*>
  traverse
    (\(idnt2, a, b) ->
       (\idnt2' -> (,,) idnt2' a) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*>
       validateSuiteIndentation idnt b)
    i
validateCompoundStatementIndentation (ClassDef a decos idnt b c d e) =
  (\decos' idnt' ->
     ClassDef a decos' idnt' b (coerce c) (unsafeCoerce d)) <$>
  traverse validateDecoratorIndentation decos <*>
  checkIndent idnt <*>
  validateSuiteIndentation idnt e
validateCompoundStatementIndentation (With a idnt asyncWs b c d) =
  (\idnt' -> With a idnt' asyncWs b) <$>
  checkIndent idnt <*>
  traverse validateWithItemIndentation c <*>
  validateSuiteIndentation idnt d

validateWithItemIndentation
  :: AsIndentationError e a
  => WithItem a
  -> ValidateIndentation e (WithItem a)
validateWithItemIndentation a = pure $ unsafeCoerce a

validateSmallStatementIndentation
  :: AsIndentationError e a
  => SmallStatement a
  -> ValidateIndentation e (SmallStatement a)
validateSmallStatementIndentation (MkSmallStatement a b c d e) =
  pure $ MkSmallStatement (unsafeCoerce a) (over (mapped._2) unsafeCoerce b) c d e

validateStatementIndentation
  :: AsIndentationError e a
  => Statement a
  -> ValidateIndentation e (Statement a)
validateStatementIndentation (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementIndentation c
validateStatementIndentation (SmallStatement idnt a) =
  SmallStatement <$>
  checkIndent idnt <*>
  validateSmallStatementIndentation a

validateModuleIndentation
  :: AsIndentationError e a
  => Module a
  -> ValidateIndentation e (Module a)
validateModuleIndentation m =
  case m of
    ModuleEmpty -> pure ModuleEmpty
    ModuleBlankFinal a ->
      ModuleBlankFinal <$>
      validateBlankIndentation a
    ModuleBlank a b c ->
      (\a' -> ModuleBlank a' b) <$>
      validateBlankIndentation a <*>
      validateModuleIndentation c
    ModuleStatement a b ->
     ModuleStatement <$
     setNextIndent EqualTo [] <*>
     validateStatementIndentation a <*>
     validateModuleIndentation b
