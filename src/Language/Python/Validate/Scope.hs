{-# language DataKinds, TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
module Language.Python.Validate.Scope where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Control.Lens.Cons (snoc)
import Control.Lens.Fold ((^..), toListOf, folded)
import Control.Lens.Getter ((^.), to, getting, use)
import Control.Lens.Lens (Lens')
import Control.Lens.Plated (cosmos)
import Control.Lens.Prism (_Right, _Just)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((%~), (.~), Setter', mapped, over)
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_2, _3)
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad.State (State, modify, evalState)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (fromString)
import Data.Type.Set (Nub)
import Data.Trie (Trie)
import Data.Validate (Validate(..))

import qualified Data.List.NonEmpty as NonEmpty
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

validateSuiteScope
  :: AsScopeError e v a
  => Suite v a
  -> ValidateScope a e (Suite (Nub (Scope ': v)) a)
validateSuiteScope (SuiteMany ann a b d) = SuiteMany ann a b <$> validateBlockScope d
validateSuiteScope (SuiteOne ann a b d) =
  SuiteOne ann a <$> validateSmallStatementScope b <*> pure d

validateDecoratorScope
  :: AsScopeError e v a
  => Decorator v a
  -> ValidateScope a e (Decorator (Nub (Scope ': v)) a)
validateDecoratorScope (Decorator a b c d e) =
  Decorator a b c <$>
  validateExprScope d <*>
  pure e

validateCompoundStatementScope
  :: AsScopeError e v a
  => CompoundStatement v a
  -> ValidateScope a e (CompoundStatement (Nub (Scope ': v)) a)
validateCompoundStatementScope (Fundef a decos idnts ws1 name ws2 params ws3 s) =
  (locallyOver scLocalScope (const Trie.empty) $
   locallyOver scImmediateScope (const Trie.empty) $
     (\decos' -> Fundef a decos' idnts ws1 (coerce name) ws2) <$>
     traverse validateDecoratorScope decos <*>
     traverse validateParamScope params <*>
     pure ws3 <*>
     locallyExtendOver
       scGlobalScope
       ((_identAnnotation &&& _identValue) name :
         toListOf (folded.getting paramName.to (_identAnnotation &&& _identValue)) params)
       (validateSuiteScope s)) <*
  extendScope scLocalScope [(_identAnnotation &&& _identValue) name] <*
  extendScope scImmediateScope [(_identAnnotation &&& _identValue) name]
validateCompoundStatementScope (If idnts a ws1 e b elifs melse) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (If idnts a ws1 <$>
     validateExprScope e <*>
     validateSuiteScope b <*>
     traverse
       (\(a, b, c, d) ->
          (\c' -> (,,,) a b c') <$>
          validateExprScope c <*>
          validateSuiteScope d)
       elifs <*>
     traverseOf (traverse._3) validateSuiteScope melse)))
validateCompoundStatementScope (While idnts a ws1 e b) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (While idnts a ws1 <$>
     validateExprScope e <*>
     validateSuiteScope b)))
validateCompoundStatementScope (TryExcept idnts a b e f k l) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (TryExcept idnts a b <$>
     validateSuiteScope e <*>
     traverse
       (\(idnts, ws, g, h) ->
          (,,,) idnts ws <$>
          validateExceptAsScope g <*>
          locallyExtendOver
            scGlobalScope
            (toListOf (exceptAsName._Just._2.to (_identAnnotation &&& _identValue)) g)
            (validateSuiteScope h))
       f <*>
     traverseOf (traverse._3) validateSuiteScope k <*>
     traverseOf (traverse._3) validateSuiteScope l)))
validateCompoundStatementScope (TryFinally idnts a b e idnts2 f i) =
  scopeContext scLocalScope `bindValidateScope` (\ls ->
  scopeContext scImmediateScope `bindValidateScope` (\is ->
  locallyOver scGlobalScope (`Trie.unionR` Trie.unionR ls is) $
  locallyOver scImmediateScope (const Trie.empty)
    (TryFinally idnts a b <$>
     validateSuiteScope e <*>
     pure idnts2 <*>
     pure f <*>
     validateSuiteScope i)))
validateCompoundStatementScope (For idnts a b c d e h i) =
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
    (let
       ls = c ^.. unvalidated.cosmos._Ident._2.to (_identAnnotation &&& _identValue)
     in
       extendScope scLocalScope ls *>
       extendScope scImmediateScope ls *>
       validateSuiteScope h) <*>
    traverseOf (traverse._3) validateSuiteScope i))
validateCompoundStatementScope (ClassDef a decos idnts b c d g) =
  (\decos' -> ClassDef a decos' idnts b (coerce c)) <$>
  traverse validateDecoratorScope decos <*>
  traverseOf (traverse._2.traverse.traverse) validateArgScope d <*>
  validateSuiteScope g <*
  extendScope scImmediateScope [c ^. to (_identAnnotation &&& _identValue)]
validateCompoundStatementScope (With a b c d e) =
  let
    names =
      d ^..
      folded.unvalidated.to _withItemBinder.folded._2.
      assignTargets.to (_identAnnotation &&& _identValue)
  in
    With a b c <$>
    traverse
      (\(WithItem a b c) ->
         WithItem a <$>
         validateExprScope b <*>
         traverseOf (traverse._2) validateAssignExprScope c)
      d <*
    extendScope scLocalScope names <*
    extendScope scImmediateScope names <*>
    validateSuiteScope e

validateSmallStatementScope
  :: AsScopeError e v a
  => SmallStatement v a
  -> ValidateScope a e (SmallStatement (Nub (Scope ': v)) a)
validateSmallStatementScope (Assert a b c d) =
  Assert a b <$>
  validateExprScope c <*>
  traverseOf (traverse._2) validateExprScope d
validateSmallStatementScope (Raise a ws f) =
  Raise a ws <$>
  traverse
    (\(b, c) ->
       (,) <$>
       validateExprScope b <*>
       traverseOf (traverse._2) validateExprScope c)
    f
validateSmallStatementScope (Return a ws e) = Return a ws <$> traverse validateExprScope e
validateSmallStatementScope (Expr a e) = Expr a <$> validateExprScope e
validateSmallStatementScope (Assign a l rs) =
  let
    ls =
      (l : (snd <$> NonEmpty.init rs)) ^..
      folded.unvalidated.assignTargets.to (_identAnnotation &&& _identValue)
  in
  Assign a <$>
  validateAssignExprScope l <*>
  ((\a b -> case a of; [] -> b :| []; a : as -> a :| snoc as b) <$>
   traverseOf (traverse._2) validateAssignExprScope (NonEmpty.init rs) <*>
   (\(ws, b) -> (,) ws <$> validateExprScope b) (NonEmpty.last rs)) <*
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
  traverseOf (traverse.traverse) listItem es <*>
  pure ws2
  where
    listItem (ListItem a b) = ListItem a <$> validateAssignExprScope b
    listItem (ListUnpack a b c) = ListUnpack a b <$> validateAssignExprScope c
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
  tupleItem b <*>
  pure ws <*>
  traverseOf (traverse.traverse) tupleItem d
  where
    tupleItem (TupleItem a b) = TupleItem a <$> validateAssignExprScope b
    tupleItem (TupleUnpack a b c d) = TupleUnpack a b c <$> validateAssignExprScope d
validateAssignExprScope e@Unit{} = pure $ coerce e
validateAssignExprScope e@Lambda{} = pure $ coerce e
validateAssignExprScope e@Yield{} = pure $ coerce e
validateAssignExprScope e@YieldFrom{} = pure $ coerce e
validateAssignExprScope e@Not{} = pure $ coerce e
validateAssignExprScope e@ListComp{} = pure $ coerce e
validateAssignExprScope e@Call{} = pure $ coerce e
validateAssignExprScope e@UnOp{} = pure $ coerce e
validateAssignExprScope e@BinOp{} = pure $ coerce e
validateAssignExprScope e@Ident{} = pure $ coerce e
validateAssignExprScope e@None{} = pure $ coerce e
validateAssignExprScope e@Int{} = pure $ coerce e
validateAssignExprScope e@Float{} = pure $ coerce e
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
validateDictItemScope (DictUnpack a b c) =
  DictUnpack a b <$> validateExprScope c

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

validateListItemScope
  :: AsScopeError e v a
  => ListItem v a
  -> ValidateScope a e (ListItem (Nub (Scope ': v)) a)
validateListItemScope (ListItem a b) = ListItem a <$> validateExprScope b
validateListItemScope (ListUnpack a b c) = ListUnpack a b <$> validateExprScope c

validateSetItemScope
  :: AsScopeError e v a
  => SetItem v a
  -> ValidateScope a e (SetItem (Nub (Scope ': v)) a)
validateSetItemScope (SetItem a b) = SetItem a <$> validateExprScope b
validateSetItemScope (SetUnpack a b c) = SetUnpack a b <$> validateExprScope c

validateTupleItemScope
  :: AsScopeError e v a
  => TupleItem v a
  -> ValidateScope a e (TupleItem (Nub (Scope ': v)) a)
validateTupleItemScope (TupleItem a b) = TupleItem a <$> validateExprScope b
validateTupleItemScope (TupleUnpack a b c d) = TupleUnpack a b c <$> validateExprScope d

validateExprScope
  :: AsScopeError e v a
  => Expr v a
  -> ValidateScope a e (Expr (Nub (Scope ': v)) a)
validateExprScope (Lambda a b c d e) =
  Lambda a b <$>
  traverse validateParamScope c <*>
  pure d <*>
  validateExprScope e
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
  traverseOf (traverse.traverse) validateListItemScope es <*>
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
validateExprScope (UnOp a op e) =
  UnOp a op <$>
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
  validateTupleItemScope b <*>
  pure ws <*>
  traverseOf (traverse.traverse) validateTupleItemScope d
validateExprScope e@None{} = pure $ coerce e
validateExprScope e@Int{} = pure $ coerce e
validateExprScope e@Float{} = pure $ coerce e
validateExprScope e@Bool{} = pure $ coerce e
validateExprScope e@String{} = pure $ coerce e
validateExprScope e@Unit{} = pure $ coerce e
validateExprScope (Dict a b c d) =
  (\c' -> Dict a b c' d) <$> traverseOf (traverse.traverse) validateDictItemScope c
validateExprScope (Set a b c d) =
  (\c' -> Set a b c' d) <$> traverse validateSetItemScope c

validateModuleScope
  :: AsScopeError e v a
  => Module v a
  -> ValidateScope a e (Module (Nub (Scope ': v)) a)
validateModuleScope =
  traverseOf (_Wrapped.traverse._Right) validateStatementScope
