{-# language DataKinds, TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
module Language.Python.Validate.Scope where

import Control.Arrow
import Control.Applicative
import Control.Lens.Fold
import Control.Lens.Getter
import Control.Lens.Lens
import Control.Lens.Plated
import Control.Lens.Prism
import Control.Lens.Review
import Control.Lens.Setter
import Control.Lens.TH
import Control.Lens.Tuple
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

import Language.Python.Internal.Optics
import Language.Python.Internal.Syntax
import Language.Python.Validate.Scope.Error

data Scope

data Binding = Clean | Dirty
  deriving (Eq, Ord, Show)

data ScopeContext a
  = ScopeContext
  { _scGlobalScope :: !(Trie a)
  , _scLocalScope :: !(Trie a)
  , _scImmediateScope :: !(Trie a)
  }
  deriving (Eq, Show)
makeLenses ''ScopeContext

initialScopeContext :: ScopeContext a
initialScopeContext = ScopeContext Trie.empty Trie.empty Trie.empty

newtype ValidateScope ann e a
  = ValidateScope
  { unValidateScope :: Compose (State (ScopeContext ann)) (Validate [e]) a
  } deriving (Functor, Applicative)

runValidateScope :: ScopeContext ann -> ValidateScope ann e a -> Validate [e] a
runValidateScope ctxt (ValidateScope s) = evalState (getCompose s) ctxt

scopeErrors :: [e] -> ValidateScope ann e a
scopeErrors = ValidateScope . Compose . pure . Failure

extendScope
  :: Setter' (ScopeContext ann) (Trie ann)
  -> [(ann, String)]
  -> ValidateScope ann e ()
extendScope l s =
  ValidateScope . Compose . fmap pure $ do
  gs <- use scGlobalScope
  let t = buildTrie gs Trie.empty
  modify (over l (t `Trie.unionL`))
  where
    buildTrie gs t =
       foldr
       (\(ann, a) b ->
          let
            a' = fromString a
          in
            if Trie.member a' gs
            then b
            else Trie.insert a' ann b)
       t
       s

locallyOver
  :: Lens' (ScopeContext ann) b
  -> (b -> b)
  -> ValidateScope ann e a
  -> ValidateScope ann e a
locallyOver l f m =
  ValidateScope . Compose $ do
    before <- use l
    modify (l %~ f)
    getCompose (unValidateScope m) <* modify (l .~ before)

scopeContext
  :: Lens' (ScopeContext ann) b
  -> ValidateScope ann e b
scopeContext l =
  ValidateScope . Compose . fmap pure $ use l

bindValidateScope
  :: ValidateScope ann e a
  -> (a -> ValidateScope ann e b)
  -> ValidateScope ann e b
bindValidateScope v f =
  ValidateScope . Compose $ do
    a <- getCompose (unValidateScope v)
    case a of
      Failure e -> pure $ Failure e
      Success x -> getCompose . unValidateScope $ f x

locallyExtendOver
  :: Lens' (ScopeContext ann) (Trie ann)
  -> [(ann, String)]
  -> ValidateScope ann e a
  -> ValidateScope ann e a
locallyExtendOver l s m = locallyOver l id $ extendScope l s *> m

inScope :: String -> ValidateScope ann e (Maybe (Binding, ann))
inScope s =
  ValidateScope . Compose . fmap pure $ do
    gs <- use scGlobalScope
    ls <- use scLocalScope
    is <- use scImmediateScope
    let
      s' = fromString s
      inls = Trie.lookup s' ls
      ings = Trie.lookup s' gs
    pure $
      ((,) Clean <$> Trie.lookup s' is) <|>
      (ings *> ((,) Clean <$> inls)) <|>
      ((,) Clean <$> ings) <|>
      ((,) Dirty <$> inls)

validateCompoundStatementScope
  :: AsScopeError e v a
  => CompoundStatement v a
  -> ValidateScope a e (CompoundStatement (Nub (Scope ': v)) a)
validateCompoundStatementScope (Fundef a ws1 name ws2 params ws3 ws4 nl body) =
  (locallyOver scLocalScope (const Trie.empty) $
   locallyOver scImmediateScope (const Trie.empty) $
     Fundef a ws1 (coerce name) ws2 <$>
     traverse validateParamScope params <*>
     pure ws3 <*>
     pure ws4 <*>
     pure nl <*>
     locallyExtendOver
       scGlobalScope
       ((_identAnnotation &&& _identValue) name :
         toListOf (folded.getting paramName.to (_identAnnotation &&& _identValue)) params)
       (validateBlockScope body)) <*
  extendScope scLocalScope [(_identAnnotation &&& _identValue) name] <*
  extendScope scImmediateScope [(_identAnnotation &&& _identValue) name]
validateCompoundStatementScope (If a ws1 e ws2 ws3 nl b melse) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (If a ws1 <$>
     validateExprScope e <*>
     pure ws2 <*>
     pure ws3 <*>
     pure nl <*>
     validateBlockScope b <*>
     traverseOf (traverse._4) validateBlockScope melse)))
validateCompoundStatementScope (While a ws1 e ws2 ws3 nl b) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (While a ws1 <$>
     validateExprScope e <*>
     pure ws2 <*>
     pure ws3 <*>
     pure nl <*>
     validateBlockScope b)))

validateSmallStatementScope
  :: AsScopeError e v a
  => SmallStatement v a
  -> ValidateScope a e (SmallStatement (Nub (Scope ': v)) a)
validateSmallStatementScope (Return a ws e) = Return a ws <$> validateExprScope e
validateSmallStatementScope (Expr a e) = Expr a <$> validateExprScope e
validateSmallStatementScope (Assign a l ws1 ws2 r) =
  let
    ls = l ^.. unvalidated.cosmos._Ident._2.to (_identAnnotation &&& _identValue)
  in
  (Assign a (coerce l) ws1 ws2 <$> validateExprScope r) <*
  extendScope scLocalScope ls <*
  extendScope scImmediateScope ls
validateSmallStatementScope (Global a _ _) = scopeErrors [_FoundGlobal # a]
validateSmallStatementScope (Nonlocal a _ _) = scopeErrors [_FoundNonlocal # a]
validateSmallStatementScope (Del a ws cs) =
  scopeErrors [_FoundDel # a] <*>
  traverse validateIdentScope cs
validateSmallStatementScope s@Pass{} = pure $ coerce s
validateSmallStatementScope s@Break{} = pure $ coerce s
validateSmallStatementScope s@Import{} = pure $ coerce s
validateSmallStatementScope s@From{} = pure $ coerce s

validateStatementScope
  :: AsScopeError e v a
  => Statement v a
  -> ValidateScope a e (Statement (Nub (Scope ': v)) a)
validateStatementScope (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementScope c
validateStatementScope (SmallStatements s ss sc nl) =
  SmallStatements <$>
  validateSmallStatementScope s <*>
  traverseOf (traverse._3) validateSmallStatementScope ss <*>
  pure sc <*>
  pure nl

validateIdentScope
  :: AsScopeError e v a
  => Ident v a
  -> ValidateScope a e (Ident (Nub (Scope ': v)) a)
validateIdentScope i@(MkIdent a s ws) =
  inScope s `bindValidateScope`
  \context ->
  case context of
    Just (Clean, _) -> pure $ MkIdent a s ws
    Just (Dirty, ann)-> scopeErrors [_FoundDynamic # (ann, i)]
    Nothing -> scopeErrors [_NotInScope # i]

validateArgScope
  :: AsScopeError e v a
  => Arg v a
  -> ValidateScope a e (Arg (Nub (Scope ': v)) a)
validateArgScope (PositionalArg a e) =
  PositionalArg a <$> validateExprScope e
validateArgScope (KeywordArg a ident ws2 expr) =
  KeywordArg a (coerce ident) ws2 <$> validateExprScope expr

validateParamScope
  :: AsScopeError e v a
  => Param v a
  -> ValidateScope a e (Param (Nub (Scope ': v)) a)
validateParamScope (PositionalParam a ident) =
  pure . PositionalParam a $ coerce ident
validateParamScope (KeywordParam a ident ws2 expr) =
  KeywordParam a (coerce ident) ws2 <$> validateExprScope expr

validateBlockScope
  :: AsScopeError e v a
  => Block v a
  -> ValidateScope a e (Block (Nub (Scope ': v)) a)
validateBlockScope (Block b) =
  Block <$> traverseOf (traverse._3._Right) validateStatementScope b

validateExprScope
  :: AsScopeError e v a
  => Expr v a
  -> ValidateScope a e (Expr (Nub (Scope ': v)) a)
validateExprScope (List a ws1 es ws2) =
  List a ws1 <$>
  traverse validateExprScope es <*>
  pure ws2
validateExprScope (Deref a e ws1 r) =
  Deref a <$>
  validateExprScope e <*>
  pure ws1 <*>
  validateIdentScope r
validateExprScope (Call a e ws1 as ws2) =
  Call a <$>
  validateExprScope e <*>
  pure ws1 <*>
  traverse validateArgScope as <*>
  pure ws2
validateExprScope (BinOp a l op r) =
  BinOp a <$>
  validateExprScope l <*>
  pure op <*>
  validateExprScope r
validateExprScope (Negate a ws e) =
  Negate a ws <$>
  validateExprScope e
validateExprScope (Parens a ws1 e ws2) =
  Parens a ws1 <$>
  validateExprScope e <*>
  pure ws2
validateExprScope (Ident a i) =
  Ident a <$>
  validateIdentScope i
validateExprScope (Tuple a b ws d) =
  Tuple a <$>
  validateExprScope b <*>
  pure ws <*>
  traverseOf (traverse.traverse) validateExprScope d
validateExprScope e@None{} = pure $ coerce e
validateExprScope e@Int{} = pure $ coerce e
validateExprScope e@Bool{} = pure $ coerce e
validateExprScope e@String{} = pure $ coerce e

validateModuleScope
  :: AsScopeError e v a
  => Module v a
  -> ValidateScope a e (Module (Nub (Scope ': v)) a)
validateModuleScope =
  traverseOf (_Wrapped.traverse._Right) validateStatementScope
