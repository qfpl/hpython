{-# language DataKinds, TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
module Language.Python.Validate.Scope where

import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Review
import Control.Lens.Setter
import Control.Lens.TH
import Control.Lens.Traversal
import Control.Lens.Wrapped
import Control.Monad.State
import Data.Coerce
import Data.Functor.Compose
import Data.String
import Data.Type.Set
import Data.Trie (Trie)
import Data.Validate

import qualified Data.Trie as Trie

import Language.Python.Internal.Syntax
import Language.Python.Validate.Scope.Error

data Scope

newtype ScopeContext = ScopeContext { unScopeContext :: Trie () }
  deriving (Eq, Show)
makeWrapped ''ScopeContext

newtype ValidateScope e a
  = ValidateScope
  { unValidateScope :: Compose (State ScopeContext) (Validate [e]) a
  } deriving (Functor, Applicative)

runValidateScope :: ScopeContext -> ValidateScope e a -> Validate [e] a
runValidateScope ctxt (ValidateScope s) = evalState (getCompose s) ctxt

insertScope :: String -> ValidateScope e ()
insertScope s =
  ValidateScope . Compose . fmap pure $
  modify (over _Wrapped $ Trie.insert (fromString s) ())

inScope :: MonadState ScopeContext m => String -> m Bool
inScope s = Trie.member (fromString s) . unScopeContext <$> get

initialScopeContext :: ScopeContext
initialScopeContext = ScopeContext Trie.empty

validateStatementScope
  :: AsScopeError e v a
  => Statement v a
  -> ValidateScope e (Statement (Nub (Scope ': v)) a)
validateStatementScope (Fundef a ws1 name ws2 params ws3 ws4 nl body) =
  Fundef a ws1 (coerce name) ws2 <$>
  traverse validateParamScope params <*>
  pure ws3 <*>
  pure ws4 <*>
  pure nl <*
  traverse insertScope (toListOf (folded.getting paramName.getting identValue) params) <*
  insertScope (name ^. getting identValue) <*>
  validateBlockScope body

validateIdentScope
  :: AsScopeError e v a
  => Ident v a
  -> ValidateScope e (Ident (Nub (Scope ': v)) a)
validateIdentScope i@(MkIdent a s) =
  ValidateScope . Compose $ do
    res <- inScope s
    if res
      then pure . pure $ MkIdent a s
      else pure $ Failure [_NotInScope # i]

validateParamScope
  :: AsScopeError e v a
  => Param v a
  -> ValidateScope e (Param (Nub (Scope ': v)) a)
validateParamScope (PositionalParam a ident) =
  pure . PositionalParam a $ coerce ident
validateParamScope (KeywordParam a ident ws1 ws2 expr) =
  KeywordParam a (coerce ident) ws1 ws2 <$> validateExprScope expr

validateBlockScope
  :: AsScopeError e v a
  => Block v a
  -> ValidateScope e (Block (Nub (Scope ': v)) a)
validateBlockScope = _

validateExprScope
  :: AsScopeError e v a
  => Expr v a
  -> ValidateScope e (Expr (Nub (Scope ': v)) a)
validateExprScope = _
