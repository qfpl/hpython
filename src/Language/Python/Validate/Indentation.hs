{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
module Language.Python.Validate.Indentation where

import Control.Lens ((#), _Wrapped, over, _2, traverseOf, _Right)
import Control.Lens.Fold ((^?!), folded)
import Control.Lens.Getter ((^.))
import Control.Monad.State (State, evalState, get, put)
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Type.Set
import Data.Validate
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation.Error

data Indentation

-- | "The next line must be..."
data NextIndent
  = GreaterThan
  | EqualTo
  | LessThan
  deriving (Eq, Show)

newtype ValidateIndentation e a
  = ValidateIndentation
  { unValidateIndentation :: Compose (State (NextIndent, [Indent])) (Validate [e]) a
  } deriving (Functor, Applicative)

runValidateIndentation :: ValidateIndentation e a -> Validate [e] a
runValidateIndentation = runValidateIndentation' EqualTo []

runValidateIndentation' :: NextIndent -> [Indent] -> ValidateIndentation e a -> Validate [e] a
runValidateIndentation' ni is =
  flip evalState (ni, is) .
  getCompose .
  unValidateIndentation

withNextIndent
  :: (NextIndent -> [Indent] -> ValidateIndentation e a)
  -> ValidateIndentation e a
withNextIndent f =
  ValidateIndentation . Compose $
    get >>= getCompose . unValidateIndentation . uncurry f

indentationError :: [e] -> ValidateIndentation e a
indentationError = ValidateIndentation . Compose . pure . Failure

checkIndent :: AsIndentationError e v a => Indents a -> ValidateIndentation e (Indents a)
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
          (GT, _) -> indentationError [_TabError # a]
          (_, GT) -> indentationError [_TabError # a]
          (EQ, EQ) -> indentationError [_ExpectedGreaterThan # (i', i)]
          (_, EQ) -> indentationError [_TabError # a]
          (EQ, _) -> indentationError [_TabError # a]
          (LT, LT) -> indentationError [_ExpectedGreaterThan # (i', i)]
      EqualTo ->
        case (absolute1Comparison, absolute8Comparison) of
          (EQ, EQ) -> pure i
          (EQ, _) -> indentationError [_TabError # a]
          (_, EQ) -> indentationError [_TabError # a]
          (GT, GT) -> indentationError [_ExpectedEqualTo # (i', i)]
          (_, GT) -> indentationError [_TabError # a]
          (GT, _) -> indentationError [_TabError # a]
          (LT, LT) -> indentationError [_ExpectedEqualTo # (i', i)]
      LessThan ->
        case (absolute1Comparison, absolute8Comparison) of
          (LT, LT) -> pure i
          (LT, _) -> indentationError [_TabError # a]
          (_, LT) -> indentationError [_TabError # a]
          (EQ, EQ) -> indentationError [_ExpectedDedent # a]
          (_, EQ) -> indentationError [_TabError # a]
          (EQ, _) -> indentationError [_TabError # a]
          (GT, GT) -> indentationError [_ExpectedDedent # a]

setNextIndent :: NextIndent -> [Indent] -> ValidateIndentation e ()
setNextIndent ni is = ValidateIndentation . Compose $ pure () <$ put (ni, is)

equivalentIndentation :: [Whitespace] -> [Whitespace] -> Bool
equivalentIndentation [] [] = True
equivalentIndentation (x:xs) [] =
  case x of
    Continued _ _ -> True
    _ -> False
equivalentIndentation [] (y:ys) =
  case y of
    Continued _ _ -> True
    _ -> False
equivalentIndentation (x:xs) (y:ys) =
  case (x, y) of
    (Space, Space) -> equivalentIndentation xs ys
    (Tab, Tab) -> equivalentIndentation xs ys
    (Continued _ _, Continued _ _) -> True
    _ -> False

validateBlockIndentation
  :: AsIndentationError e v a
  => Block v a
  -> ValidateIndentation e (Block (Nub (Indentation ': v)) a)
validateBlockIndentation (Block (b :| bs)) =
  Block <$> go False b bs
  where
    is = (b:|bs) ^?! folded._Right.unvalidated._Indents.indentsValue

    go flag (Left e) rest =
      case rest of
        [] -> pure $ Left e :| []
        r : rs -> NonEmpty.cons (Left e) <$> go flag r rs
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

validateExprIndentation
  :: AsIndentationError e v a
  => Expr v a
  -> ValidateIndentation e (Expr (Nub (Indentation ': v)) a)
validateExprIndentation e = pure $ coerce e

validateParamsIndentation
  :: AsIndentationError e v a
  => CommaSep (Param v a)
  -> ValidateIndentation e (CommaSep (Param (Nub (Indentation ': v)) a))
validateParamsIndentation e = pure $ coerce e

validateArgsIndentation
  :: AsIndentationError e v a
  => CommaSep (Arg v a)
  -> ValidateIndentation e (CommaSep (Arg (Nub (Indentation ': v)) a))
validateArgsIndentation e = pure $ coerce e

validateExceptAsIndentation
  :: AsIndentationError e v a
  => ExceptAs v a
  -> ValidateIndentation e (ExceptAs (Nub (Indentation ': v)) a)
validateExceptAsIndentation (ExceptAs ann e f) =
  ExceptAs ann <$>
  validateExprIndentation e <*>
  pure (over (traverse._2) coerce f)

validateCompoundStatementIndentation
  :: AsIndentationError e v a
  => CompoundStatement v a
  -> ValidateIndentation e (CompoundStatement (Nub (Indentation ': v)) a)
validateCompoundStatementIndentation (Fundef idnt a ws1 name ws2 params ws3 ws4 nl body) =
  (\idnt' params' body' -> Fundef idnt' a ws1 (coerce name) ws2 params' ws3 ws4 nl body') <$>
  checkIndent idnt <*>
  validateParamsIndentation params <*
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation body
validateCompoundStatementIndentation (If idnt a ws1 expr ws3 nl body body1) =
  (\idnt' expr' body' body1' -> If idnt' a ws1 expr' ws3 nl body' body1') <$>
  checkIndent idnt <*>
  validateExprIndentation expr <*
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation body <*>
  traverse
    (\(idnt2, a, b, c, d) ->
       (\idnt2' -> (,,,,) idnt2' a b c) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*
       setNextIndent GreaterThan (idnt ^. indentsValue) <*>
       validateBlockIndentation d)
    body1
validateCompoundStatementIndentation (While idnt a ws1 expr ws3 nl body) =
  (\idnt' expr' body' -> While idnt' a ws1 expr' ws3 nl body') <$>
  checkIndent idnt <*>
  validateExprIndentation expr <*
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation body
validateCompoundStatementIndentation (TryExcept idnt a b c d e f k l) =
  (\idnt' -> TryExcept idnt' a b c d) <$>
  checkIndent idnt <*
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation e <*>
  traverse
    (\(a, b, c, d, e, f) ->
       (\a' c' -> (,,,,,) a' b c' d e) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent a <*>
       validateExceptAsIndentation c <*
       setNextIndent GreaterThan (idnt ^. indentsValue) <*>
       validateBlockIndentation f)
    f <*
  setNextIndent EqualTo (idnt ^. indentsValue) <*>
  traverse
    (\(idnt2, a, b, c, d) ->
       (\idnt2' -> (,,,,) idnt2' a b c) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*
       setNextIndent GreaterThan (idnt ^. indentsValue) <*>
       validateBlockIndentation d)
    k <*
  setNextIndent EqualTo (idnt ^. indentsValue) <*>
  traverse
    (\(idnt2, a, b, c, d) ->
       (\idnt2' -> (,,,,) idnt2' a b c) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*
       setNextIndent GreaterThan (idnt ^. indentsValue) <*>
       validateBlockIndentation d)
    l
validateCompoundStatementIndentation (TryFinally idnt a b c d e idnt2 f g h i) =
  (\idnt' e' idnt2' -> TryFinally idnt' a b c d e' idnt2' f g h) <$>
  checkIndent idnt <*
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation e <*
  setNextIndent EqualTo (idnt ^. indentsValue) <*>
  checkIndent idnt2 <*
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation i
validateCompoundStatementIndentation (For idnt a b c d e f g h i) =
  (\idnt' c' e' -> For idnt' a b c' d e' f g) <$>
  checkIndent idnt <*>
  validateExprIndentation c <*>
  validateExprIndentation e <*
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation h <*
  setNextIndent EqualTo (idnt ^. indentsValue) <*>
  traverse
    (\(idnt2, a, b, c, d) ->
       (\idnt2' -> (,,,,) idnt2' a b c) <$
       setNextIndent EqualTo (idnt ^. indentsValue) <*>
       checkIndent idnt2 <*
       setNextIndent GreaterThan (idnt ^. indentsValue) <*>
       validateBlockIndentation d)
    i
validateCompoundStatementIndentation (ClassDef idnt a b c d e f g) =
  (\idnt' -> ClassDef idnt' a b (coerce c) (coerce d) e f) <$>
  checkIndent idnt <*
  setNextIndent GreaterThan (idnt ^. indentsValue) <*>
  validateBlockIndentation g

validateStatementIndentation
  :: AsIndentationError e v a
  => Statement v a
  -> ValidateIndentation e (Statement (Nub (Indentation ': v)) a)
validateStatementIndentation (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementIndentation c
validateStatementIndentation s@SmallStatements{} = pure $ coerce s

validateModuleIndentation
  :: AsIndentationError e v a
  => Module v a
  -> ValidateIndentation e (Module (Nub (Indentation ': v)) a)
validateModuleIndentation =
  traverseOf (_Wrapped.traverse._Right) validateStatementIndentation
