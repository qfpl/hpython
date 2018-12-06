{-# language DataKinds, TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables, TypeApplications #-}

{-|
Module      : Language.Python.Validate.Scope
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Scope
  ( module Data.Validation
  , module Language.Python.Validate.Scope.Error
    -- * Main validation functions
  , Scope, ValidateScope, runValidateScope
  , validateModuleScope
  , validateStatementScope
  , validateExprScope
    -- * Miscellany
    -- ** Extra types
  , ScopeContext(..), scGlobalScope, scLocalScope, scImmediateScope
  , runValidateScope'
  , initialScopeContext
  , Binding(..)
    -- ** Extra functions
  , inScope
  , extendScope
  , locallyOver
  , locallyExtendOver
    -- ** Validation functions
  , validateArgScope
  , validateAssignExprScope
  , validateBlockScope
  , validateCompoundStatementScope
  , validateComprehensionScope
  , validateDecoratorScope
  , validateDictItemScope
  , validateExceptAsScope
  , validateIdentScope
  , validateListItemScope
  , validateParamScope
  , validateSetItemScope
  , validateSimpleStatementScope
  , validateSubscriptScope
  , validateSuiteScope
  , validateTupleItemScope
  )
where

import Data.Validation

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
import Control.Monad.State (MonadState, State, evalState, modify)
import Data.Bitraversable (bitraverse)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.String (fromString)
import Data.Type.Set (Nub)
import Data.Validate.Monadic (ValidateM(..), runValidateM, bindVM, liftVM0, errorVM1)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map

import Language.Python.Optics
import Language.Python.Optics.Validated (unvalidated)
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Module
import Language.Python.Validate.Scope.Error

data Scope

data Binding = Clean | Dirty
  deriving (Eq, Ord, Show)

data ScopeContext a
  = ScopeContext
  { _scGlobalScope :: !(Map ByteString a)
  , _scLocalScope :: !(Map ByteString a)
  , _scImmediateScope :: !(Map ByteString a)
  }
  deriving (Eq, Show)
makeLenses ''ScopeContext

initialScopeContext :: ScopeContext a
initialScopeContext = ScopeContext Map.empty Map.empty Map.empty

type ValidateScope ann e = ValidateM (NonEmpty e) (State (ScopeContext ann))

runValidateScope :: ValidateScope ann e a -> Validation (NonEmpty e) a
runValidateScope = runValidateScope' initialScopeContext

runValidateScope' :: ScopeContext ann -> ValidateScope ann e a -> Validation (NonEmpty e) a
runValidateScope' s = flip evalState s . runValidateM

extendScope
  :: Setter' (ScopeContext ann) (Map ByteString ann)
  -> [(ann, String)]
  -> ValidateScope ann e ()
extendScope l s =
  liftVM0 $ do
    gs <- use scGlobalScope
    let t = buildMap gs Map.empty
    modify (over l (t `unionL`))
  where
    buildMap gs t =
      foldr
      (\(ann, a) b ->
          let
            a' = fromString a
          in
            if Map.member a' gs
            then b
            else Map.insert a' ann b)
      t
      s

locallyOver
  :: Lens' (ScopeContext ann) b
  -> (b -> b)
  -> ValidateScope ann e a
  -> ValidateScope ann e a
locallyOver l f m =
  ValidateM . Compose $ do
    before <- use l
    modify (l %~ f)
    getCompose (unValidateM m) <* modify (l .~ before)

locallyExtendOver
  :: Lens' (ScopeContext ann) (Map ByteString ann)
  -> [(ann, String)]
  -> ValidateScope ann e a
  -> ValidateScope ann e a
locallyExtendOver l s m = locallyOver l id $ extendScope l s *> m

inScope
  :: MonadState (ScopeContext ann) m
  => String
  -> m (Maybe (Binding, ann))
inScope s = do
  gs <- use scGlobalScope
  ls <- use scLocalScope
  is <- use scImmediateScope
  let
    s' = fromString s
    inls = Map.lookup s' ls
    ings = Map.lookup s' gs
  pure $
    ((,) Clean <$> Map.lookup s' is) <|>
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
validateSuiteScope (SuiteMany ann a b c d) = SuiteMany ann a b c <$> validateBlockScope d
validateSuiteScope (SuiteOne ann a b) =
  SuiteOne ann a <$> validateSmallStatementScope b

validateDecoratorScope
  :: AsScopeError e v a
  => Decorator v a
  -> ValidateScope a e (Decorator (Nub (Scope ': v)) a)
validateDecoratorScope (Decorator a b c d e f g) =
  (\d' -> Decorator a b c d' e f g) <$>
  validateExprScope d

validateCompoundStatementScope
  :: forall e v a
   . AsScopeError e v a
  => CompoundStatement v a
  -> ValidateScope a e (CompoundStatement (Nub (Scope ': v)) a)
validateCompoundStatementScope (Fundef a decos idnts asyncWs ws1 name ws2 params ws3 mty s) =
  (locallyOver scLocalScope (const Map.empty) $
   locallyOver scImmediateScope (const Map.empty) $
     (\decos' -> Fundef a decos' idnts asyncWs ws1 (coerce name) ws2) <$>
     traverse validateDecoratorScope decos <*>
     traverse validateParamScope params <*>
     pure ws3 <*>
     traverseOf (traverse._2) validateExprScope mty <*>
     locallyExtendOver
       scGlobalScope
       ((_identAnn &&& _identValue) name :
         toListOf (folded.getting paramName.to (_identAnn &&& _identValue)) params)
       (validateSuiteScope s)) <*
  extendScope scLocalScope [(_identAnn &&& _identValue) name] <*
  extendScope scImmediateScope [(_identAnn &&& _identValue) name]
validateCompoundStatementScope (If idnts a ws1 e b elifs melse) =
  use scLocalScope `bindVM` (\ls ->
  use scImmediateScope `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` unionR ls is) $
  locallyOver scImmediateScope (const Map.empty)
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
validateCompoundStatementScope (While idnts a ws1 e b els) =
  use scLocalScope `bindVM` (\ls ->
  use scImmediateScope `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` unionR ls is) $
  locallyOver scImmediateScope (const Map.empty)
    (While idnts a ws1 <$>
     validateExprScope e <*>
     validateSuiteScope b <*>
     traverseOf (traverse._3) validateSuiteScope els)))
validateCompoundStatementScope (TryExcept idnts a b e f k l) =
  use scLocalScope `bindVM` (\ls ->
  use scImmediateScope `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` unionR ls is) $
  locallyOver scImmediateScope (const Map.empty)
    (TryExcept idnts a b <$>
     validateSuiteScope e <*>
     traverse
       (\(idnts, ws, g, h) ->
          (,,,) idnts ws <$>
          traverse validateExceptAsScope g <*>
          locallyExtendOver
            scGlobalScope
            (toListOf (folded.exceptAsName._Just._2.to (_identAnn &&& _identValue)) g)
            (validateSuiteScope h))
       f <*>
     traverseOf (traverse._3) validateSuiteScope k <*>
     traverseOf (traverse._3) validateSuiteScope l)))
validateCompoundStatementScope (TryFinally idnts a b e idnts2 f i) =
  use scLocalScope `bindVM` (\ls ->
  use scImmediateScope `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` unionR ls is) $
  locallyOver scImmediateScope (const Map.empty)
    (TryFinally idnts a b <$>
     validateSuiteScope e <*>
     pure idnts2 <*>
     pure f <*>
     validateSuiteScope i)))
validateCompoundStatementScope (For idnts a asyncWs b c d e h i) =
  use scLocalScope `bindVM` (\ls ->
  use scImmediateScope `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` unionR ls is) $
  locallyOver scImmediateScope (const Map.empty) $
    For @(Nub (Scope ': v)) idnts a asyncWs b <$>
    (unsafeCoerce c <$
     traverse
       (\s ->
          inScope (s ^. identValue) `bindVM` \res ->
          maybe (pure ()) (\_ -> errorVM1 (_BadShadowing # coerce s)) res)
       (c ^.. unvalidated.cosmos._Ident)) <*>
    pure d <*>
    traverse validateExprScope e <*>
    (let
       ls = c ^.. unvalidated.cosmos._Ident.to (_identAnn &&& _identValue)
     in
       extendScope scLocalScope ls *>
       extendScope scImmediateScope ls *>
       validateSuiteScope h) <*>
    traverseOf (traverse._3) validateSuiteScope i))
validateCompoundStatementScope (ClassDef a decos idnts b c d g) =
  (\decos' -> ClassDef @(Nub (Scope ': v)) a decos' idnts b (coerce c)) <$>
  traverse validateDecoratorScope decos <*>
  traverseOf (traverse._2.traverse.traverse) validateArgScope d <*>
  validateSuiteScope g <*
  extendScope scImmediateScope [c ^. to (_identAnn &&& _identValue)]
validateCompoundStatementScope (With a b asyncWs c d e) =
  let
    names =
      d ^..
      folded.unvalidated.to _withItemBinder.folded._2.
      assignTargets.to (_identAnn &&& _identValue)
  in
    With @(Nub (Scope ': v)) a b asyncWs c <$>
    traverse
      (\(WithItem a b c) ->
         WithItem @(Nub (Scope ': v)) a <$>
         validateExprScope b <*>
         traverseOf (traverse._2) validateAssignExprScope c)
      d <*
    extendScope scLocalScope names <*
    extendScope scImmediateScope names <*>
    validateSuiteScope e

validateSimpleStatementScope
  :: AsScopeError e v a
  => SimpleStatement v a
  -> ValidateScope a e (SimpleStatement (Nub (Scope ': v)) a)
validateSimpleStatementScope (Assert a b c d) =
  Assert a b <$>
  validateExprScope c <*>
  traverseOf (traverse._2) validateExprScope d
validateSimpleStatementScope (Raise a ws f) =
  Raise a ws <$>
  traverse
    (\(b, c) ->
       (,) <$>
       validateExprScope b <*>
       traverseOf (traverse._2) validateExprScope c)
    f
validateSimpleStatementScope (Return a ws e) = Return a ws <$> traverse validateExprScope e
validateSimpleStatementScope (Expr a e) = Expr a <$> validateExprScope e
validateSimpleStatementScope (Assign a l rs) =
  let
    ls =
      (l : (snd <$> NonEmpty.init rs)) ^..
      folded.unvalidated.assignTargets.to (_identAnn &&& _identValue)
  in
  Assign a <$>
  validateAssignExprScope l <*>
  ((\a b -> case a of; [] -> b :| []; a : as -> a :| snoc as b) <$>
   traverseOf (traverse._2) validateAssignExprScope (NonEmpty.init rs) <*>
   (\(ws, b) -> (,) ws <$> validateExprScope b) (NonEmpty.last rs)) <*
  extendScope scLocalScope ls <*
  extendScope scImmediateScope ls
validateSimpleStatementScope (AugAssign a l aa r) =
  (\l' -> AugAssign a l' aa) <$>
  validateExprScope l <*>
  validateExprScope r
validateSimpleStatementScope (Global a _ _) = errorVM1 (_FoundGlobal # a)
validateSimpleStatementScope (Nonlocal a _ _) = errorVM1 (_FoundNonlocal # a)
validateSimpleStatementScope (Del a ws cs) =
  Del a ws <$
  traverse_
    (\case; Ident a -> errorVM1 (_DeletedIdent # (a ^. identAnn)); _ -> pure ())
    cs <*>
  traverse validateExprScope cs
validateSimpleStatementScope s@Pass{} = pure $ unsafeCoerce s
validateSimpleStatementScope s@Break{} = pure $ unsafeCoerce s
validateSimpleStatementScope s@Continue{} = pure $ unsafeCoerce s
validateSimpleStatementScope s@Import{} = pure $ unsafeCoerce s
validateSimpleStatementScope s@From{} = pure $ unsafeCoerce s

validateSmallStatementScope
  :: AsScopeError e v a
  => SmallStatement v a
  -> ValidateScope a e (SmallStatement (Nub (Scope ': v)) a)
validateSmallStatementScope (MkSmallStatement s ss sc cmt nl) =
  (\s' ss' -> MkSmallStatement s' ss' sc cmt nl) <$>
  validateSimpleStatementScope s <*>
  traverseOf (traverse._2) validateSimpleStatementScope ss

validateStatementScope
  :: AsScopeError e v a
  => Statement v a
  -> ValidateScope a e (Statement (Nub (Scope ': v)) a)
validateStatementScope (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementScope c
validateStatementScope (SmallStatement idnts a) =
  SmallStatement idnts <$> validateSmallStatementScope a

validateIdentScope
  :: AsScopeError e v a
  => Ident v a
  -> ValidateScope a e (Ident (Nub (Scope ': v)) a)
validateIdentScope i =
  inScope (_identValue i) `bindVM`
  \context ->
  case context of
    Just (Clean, _) -> pure $ coerce i
    Just (Dirty, ann)-> errorVM1 (_FoundDynamic # (ann, i))
    Nothing -> errorVM1 (_NotInScope # i)

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
validateParamScope (PositionalParam a ident mty) =
  PositionalParam a (coerce ident) <$>
  traverseOf (traverse._2) validateExprScope mty
validateParamScope (KeywordParam a ident mty ws2 expr) =
  KeywordParam a (coerce ident) <$>
  traverseOf (traverse._2) validateExprScope mty <*>
  pure ws2 <*>
  validateExprScope expr
validateParamScope (StarParam a b c d) =
  StarParam a b (coerce c) <$>
  traverseOf (traverse._2) validateExprScope d
validateParamScope (UnnamedStarParam a b) = pure $ UnnamedStarParam a b
validateParamScope (DoubleStarParam a b c d) =
  DoubleStarParam a b (coerce c) <$>
  traverseOf (traverse._2) validateExprScope d

validateBlockScope
  :: AsScopeError e v a
  => Block v a
  -> ValidateScope a e (Block (Nub (Scope ': v)) a)
validateBlockScope (Block x b bs) =
  Block x <$>
  validateStatementScope b <*>
  traverseOf (traverse._Right) validateStatementScope bs

validateComprehensionScope
  :: AsScopeError e v a
  => (ex v a -> ValidateScope a e (ex (Nub (Scope ': v)) a))
  -> Comprehension ex v a
  -> ValidateScope a e (Comprehension ex (Nub (Scope ': v)) a)
validateComprehensionScope f (Comprehension a b c d) =
  locallyOver scGlobalScope id $
    (\c' d' b' -> Comprehension a b' c' d') <$>
    validateCompForScope c <*>
    traverse (bitraverse validateCompForScope validateCompIfScope) d <*>
    f b
  where
    validateCompForScope
      :: AsScopeError e v a
      => CompFor v a
      -> ValidateScope a e (CompFor (Nub (Scope ': v)) a)
    validateCompForScope (CompFor a b c d e) =
      (\c' -> CompFor a b c' d) <$>
      validateAssignExprScope c <*>
      validateExprScope e <*
      extendScope scGlobalScope
        (c ^.. unvalidated.assignTargets.to (_identAnn &&& _identValue))

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
    listItem (ListUnpack a b c d) = ListUnpack a b c <$> validateAssignExprScope d
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
validateAssignExprScope e@Unit{} = pure $ unsafeCoerce e
validateAssignExprScope e@Lambda{} = pure $ unsafeCoerce e
validateAssignExprScope e@Yield{} = pure $ unsafeCoerce e
validateAssignExprScope e@YieldFrom{} = pure $ unsafeCoerce e
validateAssignExprScope e@Not{} = pure $ unsafeCoerce e
validateAssignExprScope e@ListComp{} = pure $ unsafeCoerce e
validateAssignExprScope e@Call{} = pure $ unsafeCoerce e
validateAssignExprScope e@UnOp{} = pure $ unsafeCoerce e
validateAssignExprScope e@BinOp{} = pure $ unsafeCoerce e
validateAssignExprScope e@Ident{} = pure $ unsafeCoerce e
validateAssignExprScope e@None{} = pure $ unsafeCoerce e
validateAssignExprScope e@Ellipsis{} = pure $ unsafeCoerce e
validateAssignExprScope e@Int{} = pure $ unsafeCoerce e
validateAssignExprScope e@Float{} = pure $ unsafeCoerce e
validateAssignExprScope e@Imag{} = pure $ unsafeCoerce e
validateAssignExprScope e@Bool{} = pure $ unsafeCoerce e
validateAssignExprScope e@String{} = pure $ unsafeCoerce e
validateAssignExprScope e@DictComp{} = pure $ unsafeCoerce e
validateAssignExprScope e@Dict{} = pure $ unsafeCoerce e
validateAssignExprScope e@SetComp{} = pure $ unsafeCoerce e
validateAssignExprScope e@Set{} = pure $ unsafeCoerce e
validateAssignExprScope e@Generator{} = pure $ unsafeCoerce e
validateAssignExprScope e@Await{} = pure $ unsafeCoerce e
validateAssignExprScope e@Ternary{} = pure $ unsafeCoerce e

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
validateListItemScope (ListUnpack a b c d) = ListUnpack a b c <$> validateExprScope d

validateSetItemScope
  :: AsScopeError e v a
  => SetItem v a
  -> ValidateScope a e (SetItem (Nub (Scope ': v)) a)
validateSetItemScope (SetItem a b) = SetItem a <$> validateExprScope b
validateSetItemScope (SetUnpack a b c d) = SetUnpack a b c <$> validateExprScope d

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
  validateComprehensionScope validateExprScope comp <*>
  pure ws2
validateExprScope (Generator a comp) =
  Generator a <$>
  validateComprehensionScope validateExprScope comp
validateExprScope (Await a ws expr) = Await a ws <$> validateExprScope expr
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
validateExprScope (Ident i) = Ident <$> validateIdentScope i
validateExprScope (Tuple a b ws d) =
  Tuple a <$>
  validateTupleItemScope b <*>
  pure ws <*>
  traverseOf (traverse.traverse) validateTupleItemScope d
validateExprScope e@None{} = pure $ unsafeCoerce e
validateExprScope e@Ellipsis{} = pure $ unsafeCoerce e
validateExprScope e@Int{} = pure $ unsafeCoerce e
validateExprScope e@Float{} = pure $ unsafeCoerce e
validateExprScope e@Imag{} = pure $ unsafeCoerce e
validateExprScope e@Bool{} = pure $ unsafeCoerce e
validateExprScope e@String{} = pure $ unsafeCoerce e
validateExprScope e@Unit{} = pure $ unsafeCoerce e
validateExprScope (DictComp a ws1 comp ws2) =
  DictComp a ws1 <$>
  validateComprehensionScope validateDictItemScope comp <*>
  pure ws2
validateExprScope (Dict a b c d) =
  (\c' -> Dict a b c' d) <$> traverseOf (traverse.traverse) validateDictItemScope c
validateExprScope (SetComp a ws1 comp ws2) =
  SetComp a ws1 <$>
  validateComprehensionScope validateSetItemScope comp <*>
  pure ws2
validateExprScope (Set a b c d) =
  (\c' -> Set a b c' d) <$> traverse validateSetItemScope c

validateModuleScope
  :: AsScopeError e v a
  => Module v a
  -> ValidateScope a e (Module (Nub (Scope ': v)) a)
validateModuleScope m =
  case m of
    ModuleEmpty -> pure ModuleEmpty
    ModuleBlankFinal a -> pure $ ModuleBlankFinal a
    ModuleBlank a b c -> ModuleBlank a b <$> validateModuleScope c
    ModuleStatement a b ->
     ModuleStatement <$>
     validateStatementScope a <*>
     validateModuleScope b

unionL :: Ord k => Map k v -> Map k v -> Map k v
unionL = Map.unionWith const

unionR :: Ord k => Map k v -> Map k v -> Map k v
unionR = Map.unionWith (const id)
