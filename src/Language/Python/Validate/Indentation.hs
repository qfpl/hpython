{-# language DataKinds #-}
{-# language TypeOperators #-}
module Language.Python.Validate.Indentation where

import Control.Applicative
import Control.Lens ((#), _Wrapped, view, from, _4, traverseOf, _Right)
import Data.Coerce
import Data.Type.Set
import Data.Validate

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Internal.Syntax
import Language.Python.Validate.Indentation.Error

data Indentation

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
  -> Validate [e] (Block (Nub (Indentation ': v)) a)
validateBlockIndentation a =
  view (from _Wrapped) . NonEmpty.fromList <$>
  go Nothing (NonEmpty.toList $ view _Wrapped a)
  where
    go _ [] = pure []
    go a ((ann, ws, st):xs)
      | null ws = Failure [_ExpectedIndent # ann] <*> go a xs
      | otherwise =
          case a of
            Nothing ->
              liftA2 (:)
                ((,,) ann ws <$> validateStatementIndentation st)
                (go (Just ws) xs)
            Just ws'
              | equivalentIndentation ws ws' ->
                  liftA2 (:)
                    ((,,) ann ws <$> validateStatementIndentation st)
                    (go a xs)
              | otherwise -> Failure [_WrongIndent # (ws', ws, ann)] <*> go a xs

validateExprIndentation
  :: AsIndentationError e v a
  => Expr v a
  -> Validate [e] (Expr (Nub (Indentation ': v)) a)
validateExprIndentation e = pure $ coerce e

validateParamsIndentation
  :: AsIndentationError e v a
  => CommaSep (Param v a)
  -> Validate [e] (CommaSep (Param (Nub (Indentation ': v)) a))
validateParamsIndentation e = pure $ coerce e

validateArgsIndentation
  :: AsIndentationError e v a
  => CommaSep (Arg v a)
  -> Validate [e] (CommaSep (Arg (Nub (Indentation ': v)) a))
validateArgsIndentation e = pure $ coerce e

validateCompoundStatementIndentation
  :: AsIndentationError e v a
  => CompoundStatement v a
  -> Validate [e] (CompoundStatement (Nub (Indentation ': v)) a)
validateCompoundStatementIndentation (Fundef a ws1 name ws2 params ws3 ws4 nl body) =
  Fundef a ws1 (coerce name) ws2 <$>
  validateParamsIndentation params <*>
  pure ws3 <*>
  pure ws4 <*>
  pure nl <*>
  validateBlockIndentation body
validateCompoundStatementIndentation (If a ws1 expr ws2 ws3 nl body body') =
  If a ws1 <$>
  validateExprIndentation expr <*>
  pure ws2 <*>
  pure ws3 <*>
  pure nl <*>
  validateBlockIndentation body <*>
  traverseOf (traverse._4) validateBlockIndentation body'
validateCompoundStatementIndentation (While a ws1 expr ws2 ws3 nl body) =
  While a ws1 <$>
  validateExprIndentation expr <*>
  pure ws2 <*>
  pure ws3 <*>
  pure nl <*>
  validateBlockIndentation body

validateStatementIndentation
  :: AsIndentationError e v a
  => Statement v a
  -> Validate [e] (Statement (Nub (Indentation ': v)) a)
validateStatementIndentation (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementIndentation c
validateStatementIndentation s@SmallStatements{} = pure $ coerce s

validateModuleIndentation
  :: AsIndentationError e v a
  => Module v a
  -> Validate [e] (Module (Nub (Indentation ': v)) a)
validateModuleIndentation =
  traverseOf (_Wrapped.traverse._Right) validateStatementIndentation
