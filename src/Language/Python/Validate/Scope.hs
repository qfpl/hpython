{-# language DataKinds, TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
module Language.Python.Validate.Scope where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Control.Lens.Fold ((^..), toListOf, folded)
import Control.Lens.Getter ((^.), to, getting, use)
import Control.Lens.Lens (Lens')
import Control.Lens.Plated (cosmos)
import Control.Lens.Prism (_Right, _Just)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((%~), (.~), Setter', mapped, over)
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_2, _5)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad.State (State, modify, evalState)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.String (fromString)
import Data.Type.Set (Nub)
import Data.Trie (Trie)
import Data.Validate (Validate(..))

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

validateExceptAsScope
  :: AsScopeError e v a
  => ExceptAs v a
  -> ValidateScope a e (ExceptAs (Nub (Scope ': v)) a)
validateExceptAsScope (ExceptAs ann e f) =
  ExceptAs ann <$>
  validateExprScope e <*>
  pure (over (mapped._2) coerce f)

validateCompoundStatementScope
  :: AsScopeError e v a
  => CompoundStatement v a
  -> ValidateScope a e (CompoundStatement (Nub (Scope ': v)) a)
validateCompoundStatementScope (Fundef idnts a ws1 name ws2 params ws3 ws4 nl body) =
  (locallyOver scLocalScope (const Trie.empty) $
   locallyOver scImmediateScope (const Trie.empty) $
     Fundef idnts a ws1 (coerce name) ws2 <$>
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
validateCompoundStatementScope (If idnts a ws1 e ws3 nl b elifs melse) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (If idnts a ws1 <$>
     validateExprScope e <*>
     pure ws3 <*>
     pure nl <*>
     validateBlockScope b <*>
     traverse
       (\(a, b, c, d, e, f) ->
          (\c' -> (,,,,,) a b c' d e) <$>
          validateExprScope c <*>
          validateBlockScope f)
       elifs <*>
     traverseOf (traverse._5) validateBlockScope melse)))
validateCompoundStatementScope (While idnts a ws1 e ws3 nl b) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (While idnts a ws1 <$>
     validateExprScope e <*>
     pure ws3 <*>
     pure nl <*>
     validateBlockScope b)))
validateCompoundStatementScope (TryExcept idnts a b c d e f k l) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (TryExcept idnts a b c d <$>
     validateBlockScope e <*>
     traverse
       (\(idnts, ws, g, h, i, j) ->
          (,,,,,) idnts ws <$>
          validateExceptAsScope g <*>
          pure h <*>
          pure i <*>
          locallyExtendOver
            scGlobalScope
            (toListOf (exceptAsName._Just._2.to (_identAnnotation &&& _identValue)) g)
            (validateBlockScope j))
       f <*>
     traverseOf (traverse._5) validateBlockScope k <*>
     traverseOf (traverse._5) validateBlockScope l)))
validateCompoundStatementScope (TryFinally idnts a b c d e idnts2 f g h i) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (TryFinally idnts a b c d <$>
     validateBlockScope e <*>
     pure idnts2 <*>
     pure f <*> pure g <*> pure h <*>
     validateBlockScope i)))
validateCompoundStatementScope (For idnts a b c d e f g h i) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty) $
    For idnts a b <$>
    (coerce c <$
     traverse
       (\s ->
          inScope (s ^. identValue) `bindValidateScope` \res ->
          maybe (pure ()) (\_ -> scopeErrors [_BadShadowing # coerce s]) res)
       (c ^.. unvalidated.cosmos._Ident._2)) <*>
    pure d <*>
    validateExprScope e <*>
    pure f <*> pure g <*>
    (let
       ls = c ^.. unvalidated.cosmos._Ident._2.to (_identAnnotation &&& _identValue)
     in
       extendScope scLocalScope ls *>
       extendScope scImmediateScope ls *>
       validateBlockScope h) <*>
    traverseOf (traverse._5) validateBlockScope i))
validateCompoundStatementScope (ClassDef idnts a b c d e f g) =
  ClassDef idnts a b (coerce c) <$>
  traverseOf (traverse._2.traverse.traverse) validateArgScope d <*>
  pure e <*> pure f <*>
  validateBlockScope g <*
  extendScope scImmediateScope [c ^. to (_identAnnotation &&& _identValue)]

validateSmallStatementScope
  :: AsScopeError e v a
  => SmallStatement v a
  -> ValidateScope a e (SmallStatement (Nub (Scope ': v)) a)
validateSmallStatementScope (Raise a ws f) =
  Raise a ws <$>
  traverse
    (\(b, c) ->
       (,) <$>
       validateExprScope b <*>
       traverseOf (traverse._2) validateExprScope c)
    f
validateSmallStatementScope (Return a ws e) = Return a ws <$> validateExprScope e
validateSmallStatementScope (Expr a e) = Expr a <$> validateExprScope e
validateSmallStatementScope (Assign a l ws2 r) =
  let
    ls = l ^.. unvalidated.assignTargets.to (_identAnnotation &&& _identValue)
  in
  (\l' -> Assign a l' ws2) <$>
  validateAssignExprScope l <*>
  validateExprScope r <*
  extendScope scLocalScope ls <*
  extendScope scImmediateScope ls
validateSmallStatementScope (AugAssign a l aa r) =
  (\l' -> AugAssign a l' aa) <$>
  validateExprScope l <*>
  validateExprScope r
validateSmallStatementScope (Global a _ _) = scopeErrors [_FoundGlobal # a]
validateSmallStatementScope (Nonlocal a _ _) = scopeErrors [_FoundNonlocal # a]
validateSmallStatementScope (Del a ws cs) =
  Del a ws <$
  traverse_ (\case; Ident ann _-> scopeErrors [_DeletedIdent # ann]; _ -> pure ()) cs <*>
  traverse validateExprScope cs
validateSmallStatementScope s@Pass{} = pure $ coerce s
validateSmallStatementScope s@Break{} = pure $ coerce s
validateSmallStatementScope s@Continue{} = pure $ coerce s
validateSmallStatementScope s@Import{} = pure $ coerce s
validateSmallStatementScope s@From{} = pure $ coerce s

validateStatementScope
  :: AsScopeError e v a
  => Statement v a
  -> ValidateScope a e (Statement (Nub (Scope ': v)) a)
validateStatementScope (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementScope c
validateStatementScope (SmallStatements idnts s ss sc nl) =
  SmallStatements idnts <$>
  validateSmallStatementScope s <*>
  traverseOf (traverse._2) validateSmallStatementScope ss <*>
  pure sc <*>
  pure nl

validateIdentScope
  :: AsScopeError e v a
  => Ident v a
  -> ValidateScope a e (Ident (Nub (Scope ': v)) a)
validateIdentScope i =
  inScope (_identValue i) `bindValidateScope`
  \context ->
  case context of
    Just (Clean, _) -> pure $ coerce i
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
validateArgScope (StarArg a ws e) =
  StarArg a ws <$> validateExprScope e
validateArgScope (DoubleStarArg a ws e) =
  DoubleStarArg a ws <$> validateExprScope e

validateParamScope
  :: AsScopeError e v a
  => Param v a
  -> ValidateScope a e (Param (Nub (Scope ': v)) a)
validateParamScope (PositionalParam a ident) =
  pure . PositionalParam a $ coerce ident
validateParamScope (KeywordParam a ident ws2 expr) =
  KeywordParam a (coerce ident) ws2 <$> validateExprScope expr
validateParamScope a@StarParam{} = pure $ coerce a
validateParamScope a@DoubleStarParam{} = pure $ coerce a

validateBlockScope
  :: AsScopeError e v a
  => Block v a
  -> ValidateScope a e (Block (Nub (Scope ': v)) a)
validateBlockScope (Block b) =
  Block <$> traverseOf (traverse._Right) validateStatementScope b

validateComprehensionScope
  :: AsScopeError e v a
  => Comprehension v a
  -> ValidateScope a e (Comprehension (Nub (Scope ': v)) a)
validateComprehensionScope (Comprehension a b c d) =
  locallyOver scGlobalScope id $
    (\c' d' b' -> Comprehension a b' c' d') <$>
    validateCompForScope c <*>
    traverse (bitraverse validateCompForScope validateCompIfScope) d <*>
    validateExprScope b
  where
    validateCompForScope
      :: AsScopeError e v a
      => CompFor v a
      -> ValidateScope a e (CompFor (Nub (Scope ': v)) a)
    validateCompForScope (CompFor a b c d e) =
      (\c' -> CompFor a b c' d) <$>
      validateAssignExprScope c <*>
      validateExprScope e <*
      extendScope
        scGlobalScope
        (c ^.. unvalidated.assignTargets.to (_identAnnotation &&& _identValue))

    validateCompIfScope
      :: AsScopeError e v a
      => CompIf v a
      -> ValidateScope a e (CompIf (Nub (Scope ': v)) a)
    validateCompIfScope (CompIf a b c) =
      CompIf a b <$> validateExprScope c

validateAssignExprScope
  :: AsScopeError e v a
  => Expr v a
  -> ValidateScope a e (Expr (Nub (Scope ': v)) a)
validateAssignExprScope (Subscript a e1 ws1 e2 ws2) =
  (\e1' e2' -> Subscript a e1' ws1 e2' ws2) <$>
  validateAssignExprScope e1 <*>
  traverse validateSubscriptScope e2
validateAssignExprScope (List a ws1 es ws2) =
  List a ws1 <$>
  traverseOf (traverse.traverse) validateAssignExprScope es <*>
  pure ws2
validateAssignExprScope (Deref a e ws1 r) =
  Deref a <$>
  validateExprScope e <*>
  pure ws1 <*>
  validateIdentScope r
validateAssignExprScope (Parens a ws1 e ws2) =
  Parens a ws1 <$>
  validateAssignExprScope e <*>
  pure ws2
validateAssignExprScope (Tuple a b ws d) =
  Tuple a <$>
  validateAssignExprScope b <*>
  pure ws <*>
  traverseOf (traverse.traverse) validateAssignExprScope d
validateAssignExprScope e@Yield{} = pure $ coerce e
validateAssignExprScope e@YieldFrom{} = pure $ coerce e
validateAssignExprScope e@Not{} = pure $ coerce e
validateAssignExprScope e@ListComp{} = pure $ coerce e
validateAssignExprScope e@Call{} = pure $ coerce e
validateAssignExprScope e@Negate{} = pure $ coerce e
validateAssignExprScope e@BinOp{} = pure $ coerce e
validateAssignExprScope e@Ident{} = pure $ coerce e
validateAssignExprScope e@None{} = pure $ coerce e
validateAssignExprScope e@Int{} = pure $ coerce e
validateAssignExprScope e@Bool{} = pure $ coerce e
validateAssignExprScope e@String{} = pure $ coerce e
validateAssignExprScope e@Dict{} = pure $ coerce e
validateAssignExprScope e@Set{} = pure $ coerce e
validateAssignExprScope e@Generator{} = pure $ coerce e
validateAssignExprScope e@Ternary{} = pure $ coerce e

validateDictItemScope
  :: AsScopeError e v a
  => DictItem v a
  -> ValidateScope a e (DictItem (Nub (Scope ': v)) a)
validateDictItemScope (DictItem a b c d) =
  (\b' -> DictItem a b' c) <$>
  validateExprScope b <*>
  validateExprScope d

validateSubscriptScope
  :: AsScopeError e v a
  => Subscript v a
  -> ValidateScope a e (Subscript (Nub (Scope ': v)) a)
validateSubscriptScope (SubscriptExpr e) = SubscriptExpr <$> validateExprScope e
validateSubscriptScope (SubscriptSlice a b c d) =
  (\a' -> SubscriptSlice a' b) <$>
  traverse validateExprScope a <*>
  traverse validateExprScope c <*>
  traverseOf (traverse._2.traverse) validateExprScope d

validateExprScope
  :: AsScopeError e v a
  => Expr v a
  -> ValidateScope a e (Expr (Nub (Scope ': v)) a)
validateExprScope (Yield a b c) =
  Yield a b <$> traverse validateExprScope c
validateExprScope (YieldFrom a b c d) =
  YieldFrom a b c <$> validateExprScope d
validateExprScope (Ternary a b c d e f) =
  (\b' d' f' -> Ternary a b' c d' e f') <$>
  validateExprScope b <*>
  validateExprScope d <*>
  validateExprScope f
validateExprScope (Subscript a b c d e) =
  (\b' d' -> Subscript a b' c d' e) <$>
  validateExprScope b <*>
  traverse validateSubscriptScope d
validateExprScope (Not a ws e) = Not a ws <$> validateExprScope e
validateExprScope (List a ws1 es ws2) =
  List a ws1 <$>
  traverseOf (traverse.traverse) validateExprScope es <*>
  pure ws2
validateExprScope (ListComp a ws1 comp ws2) =
  ListComp a ws1 <$>
  validateComprehensionScope comp <*>
  pure ws2
validateExprScope (Generator a comp) =
  Generator a <$>
  validateComprehensionScope comp
validateExprScope (Deref a e ws1 r) =
  Deref a <$>
  validateExprScope e <*>
  pure ws1 <*>
  validateIdentScope r
validateExprScope (Call a e ws1 as ws2) =
  Call a <$>
  validateExprScope e <*>
  pure ws1 <*>
  traverseOf (traverse.traverse) validateArgScope as <*>
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
validateExprScope (Dict a b c d) =
  (\c' -> Dict a b c' d) <$> traverseOf (traverse.traverse) validateDictItemScope c
validateExprScope (Set a b c d) =
  (\c' -> Set a b c' d) <$> traverse validateExprScope c

validateModuleScope
  :: AsScopeError e v a
  => Module v a
  -> ValidateScope a e (Module (Nub (Scope ': v)) a)
validateModuleScope =
  traverseOf (_Wrapped.traverse._Right) validateStatementScope
