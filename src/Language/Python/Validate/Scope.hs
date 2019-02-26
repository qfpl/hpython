{-# language DataKinds, TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language MultiParamTypeClasses #-}

{-|
Module      : Language.Python.Validate.Scope
Copyright   : (C) CSIRO 2017-2019
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
  , Level(..), Entry(..)
    -- ** Extra functions
  , runValidateScope'
  , definitionScope
  , controlScope
  , inScope
  , lookupScope
  , localScope
  , extendScope
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

import Control.Lens.Cons (snoc)
import Control.Lens.Fold ((^..), toListOf, folded)
import Control.Lens.Getter ((^.), to, getting)
import Control.Lens.Plated (cosmos)
import Control.Lens.Prism (_Right, _Just)
import Control.Lens.Review ((#))
import Control.Lens.Setter (mapped, over)
import Control.Lens.Tuple (_2, _3)
import Control.Lens.Traversal (traverseOf)
import Control.Monad.State (State, evalState, modify, get, put)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Data.Bitraversable (bitraverse)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Sequence ((|>), Seq)
import Data.String (fromString)
import Data.Type.Set (Nub)
import Data.Validate.Monadic
  (ValidateM(..), runValidateM, bindVM, liftVM0, liftVM1, errorVM1)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Sequence as Seq

import Language.Python.Optics
import Language.Python.Optics.Validated (unvalidated)
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Module
import Language.Python.Validate.Scope.Error

data Scope

data Level
  = Toplevel
  | Definition
  | Control
  deriving (Eq, Show)

data Entry a
  = Entry
  { _entryValue :: a
  , _entryPath :: !(Seq Level)
  } deriving (Eq, Show, Functor)

type ValidateScope ann e
  = ValidateM
      (NonEmpty e)
      (ReaderT (Seq Level) (State (Map ByteString (Entry ann))))

runValidateScope :: ValidateScope ann e a -> Validation (NonEmpty e) a
runValidateScope = runValidateScope' [Toplevel] mempty

runValidateScope' ::
  Seq Level -> -- ^ Path
  Map ByteString (Entry ann) -> -- ^ Context
  ValidateScope ann e a -> -- ^ Validation action
  Validation (NonEmpty e) a
runValidateScope' path s = flip evalState s . flip runReaderT path . runValidateM

localScope ::
  (Map ByteString (Entry ann) -> Map ByteString (Entry ann)) ->
  ValidateScope ann e a ->
  ValidateScope ann e a
localScope f m =
  liftVM0 get `bindVM` \s ->
  liftVM0 (modify f) *>
  m <* liftVM0 (put s)

-- | Run a validation action for a definition block
definitionScope :: ValidateScope ann e a -> ValidateScope ann e a
definitionScope = liftVM1 (local (|> Definition)) . localScope id

-- | Run a validation action for a flow control block
controlScope :: ValidateScope ann e a -> ValidateScope ann e a
controlScope = liftVM1 (local (|> Control))

-- | Add some entries to the context, using the current depth and level
extendScope :: [Ident v ann] -> ValidateScope ann e ()
extendScope entries =
  liftVM0 ask `bindVM` \path ->
  liftVM0 . modify $ \scope ->
    foldr
      (\ident ->
         Map.insert (fromString $ _identValue ident) (Entry (ident ^. annot_) path))
      scope
      entries

inScope :: String -> ValidateScope ann e Bool
inScope = fmap isJust . lookupScope

lookupScope :: String -> ValidateScope ann e (Maybe (Entry ann))
lookupScope s = Map.lookup (fromString s) <$> liftVM0 get 

validateExceptAsScope
  :: AsScopeError e a
  => ExceptAs v a
  -> ValidateScope a e (ExceptAs (Nub (Scope ': v)) a)
validateExceptAsScope (ExceptAs ann e f) =
  ExceptAs ann <$>
  validateExprScope e <*>
  pure (over (mapped._2) coerce f)

validateSuiteScope
  :: AsScopeError e a
  => Suite v a
  -> ValidateScope a e (Suite (Nub (Scope ': v)) a)
validateSuiteScope (SuiteMany ann a b c d) = SuiteMany ann a b c <$> validateBlockScope d
validateSuiteScope (SuiteOne ann a b) =
  SuiteOne ann a <$> validateSmallStatementScope b

validateDecoratorScope
  :: AsScopeError e a
  => Decorator v a
  -> ValidateScope a e (Decorator (Nub (Scope ': v)) a)
validateDecoratorScope (Decorator a b c d e f g) =
  (\d' -> Decorator a b c d' e f g) <$>
  validateExprScope d

parallel2 ::
  ValidateScope a e x ->
  ValidateScope a e y ->
  ValidateScope a e (x, y)
parallel2 a b =
  liftVM0 get `bindVM` \st ->
  ((,) <$>
   ((,) <$ liftVM0 (put st) <*> a <*> liftVM0 get) <*>
   ((,) <$ liftVM0 (put st) <*> b <*> liftVM0 get)) `bindVM`
  \((ares, ast), (bres, bst)) ->
  (ares, bres) <$
    liftVM0
    (put $
     Map.unionWith
       (\e1 e2 ->
          if Seq.length (_entryPath e1) < Seq.length (_entryPath e2)
          then e2
          else e1)
       ast
       bst)

parallelList ::
  (x -> ValidateScope a e y) ->
  [x] ->
  ValidateScope a e [y]
parallelList _ [] = pure []
parallelList f (x:xs) = uncurry (:) <$> parallel2 (f x) (parallelList f xs)

parallelNonEmpty ::
  (x -> ValidateScope a e y) ->
  NonEmpty x ->
  ValidateScope a e (NonEmpty y)
parallelNonEmpty f (x:|xs) = uncurry (:|) <$> parallel2 (f x) (parallelList f xs)

parallel3 ::
  ValidateScope a e x1 ->
  ValidateScope a e x2 ->
  ValidateScope a e x3 ->
  ValidateScope a e (x1, x2, x3)
parallel3 a b c = (\(a', (b', c')) -> (a', b', c')) <$> parallel2 a (parallel2 b c)

parallel4 ::
  ValidateScope a e x1 ->
  ValidateScope a e x2 ->
  ValidateScope a e x3 ->
  ValidateScope a e x4 ->
  ValidateScope a e (x1, x2, x3, x4)
parallel4 a b c d =
  (\(a', (b', c', d')) -> (a', b', c', d')) <$> parallel2 a (parallel3 b c d)

validateCompoundStatementScope
  :: forall e v a
   . AsScopeError e a
  => CompoundStatement v a
  -> ValidateScope a e (CompoundStatement (Nub (Scope ': v)) a)
validateCompoundStatementScope (Fundef a decos idnts asyncWs ws1 name ws2 params ws3 mty s) =
  (\decos' -> Fundef a decos' idnts asyncWs ws1 (coerce name) ws2) <$>
  traverse validateDecoratorScope decos <*>
  traverse validateParamScope params <*>
  pure ws3 <*>
  traverseOf (traverse._2) validateExprScope mty <*>
  definitionScope
    (extendScope (name : toListOf (folded.getting paramName) params) *>
     validateSuiteScope s) <*
  extendScope [name]
validateCompoundStatementScope (If idnts a ws1 e b elifs melse) =
  (\e' (b', elifs', melse') -> If idnts a ws1 e' b' elifs' melse') <$>
  validateExprScope e <*>
  parallel3
    (controlScope $ validateSuiteScope b)
    (parallelList
     (\(a, b, c, d) ->
       (,,,) a b <$>
       validateExprScope c <*>
       controlScope (validateSuiteScope d))
     elifs)
    (traverseOf (traverse._3) (controlScope . validateSuiteScope) melse)
validateCompoundStatementScope (While idnts a ws1 e b els) =
  While idnts a ws1 <$>
  validateExprScope e <*>
  controlScope (validateSuiteScope b) <*>
  traverseOf (traverse._3) (controlScope . validateSuiteScope) els
validateCompoundStatementScope (TryExcept idnts a b e f k l) =
  (\(e', f', k', l') -> TryExcept idnts a b e' f' k' l') <$>
  parallel4
    (controlScope $ validateSuiteScope e)
    (parallelNonEmpty
       (\(idnts, ws, g, h) ->
         (,,,) idnts ws <$>
         traverse validateExceptAsScope g <*>
         controlScope
         (extendScope (toListOf (folded.exceptAsName._Just._2) g) *>
          validateSuiteScope h))
       f)
    (traverseOf (traverse._3) (controlScope . validateSuiteScope) k)
    (traverseOf (traverse._3) (controlScope . validateSuiteScope) l)
validateCompoundStatementScope (TryFinally idnts a b e idnts2 f i) =
  (\(e', i') -> TryFinally idnts a b e' idnts2 f i') <$>
  parallel2
    (controlScope $ validateSuiteScope e)
    (controlScope $ validateSuiteScope i)
validateCompoundStatementScope (For idnts a asyncWs b c d e h i) =
  let
    cs = c ^.. unvalidated.cosmos._Ident
  in
    (\c' d' e' (h', i') -> For idnts a asyncWs b c' d' e' h' i') <$>
    (unsafeCoerce c <$
     traverse
       (\s ->
         inScope (s ^. identValue) `bindVM` \res ->
         if res then errorVM1 (_BadShadowing # coerce s) else pure ())
       cs) <*>
    pure d <*>
    traverse validateExprScope e <*>
    parallel2
      (controlScope $
       extendScope (toList cs) *>
       validateSuiteScope h)
      (traverseOf (traverse._3) (controlScope . validateSuiteScope) i)
validateCompoundStatementScope (ClassDef a decos idnts b c d g) =
  (\decos' -> ClassDef a decos' idnts b (coerce c)) <$>
  traverse validateDecoratorScope decos <*>
  traverseOf (traverse._2.traverse.traverse) validateArgScope d <*>
  definitionScope (validateSuiteScope g) <*
  extendScope [c]
validateCompoundStatementScope (With a b asyncWs c d e) =
  let
    names =
      d ^..
      folded.unvalidated.to _withItemBinder.folded._2.
      assignTargets
  in
    With a b asyncWs c <$>
    traverse
      (\(WithItem a b c) ->
         WithItem @(Nub (Scope ': v)) a <$>
         validateExprScope b <*>
         traverseOf (traverse._2) validateAssignExprScope c)
      d <*
    extendScope names <*>
    controlScope (validateSuiteScope e)

validateSimpleStatementScope
  :: AsScopeError e a
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
  Assign a <$>
  validateAssignExprScope l <*>
  ((\a b -> case a of; [] -> b :| []; a : as -> a :| snoc as b) <$>
   traverseOf (traverse._2) validateAssignExprScope (NonEmpty.init rs) <*>
   (\(ws, b) -> (,) ws <$> validateExprScope b) (NonEmpty.last rs))
validateSimpleStatementScope (AugAssign a l aa r) =
  (\l' -> AugAssign a l' aa) <$>
  validateExprScope l <*>
  validateExprScope r
validateSimpleStatementScope (Global a _ _) = errorVM1 (_FoundGlobal # getAnn a)
validateSimpleStatementScope (Nonlocal a _ _) = errorVM1 (_FoundNonlocal # getAnn a)
validateSimpleStatementScope (Del a ws cs) =
  Del a ws <$
  traverse_
    (\case; Ident a _ -> errorVM1 (_DeletedIdent # getAnn a); _ -> pure ())
    cs <*>
  traverse validateExprScope cs
validateSimpleStatementScope s@Pass{} = pure $ unsafeCoerce s
validateSimpleStatementScope s@Break{} = pure $ unsafeCoerce s
validateSimpleStatementScope s@Continue{} = pure $ unsafeCoerce s
validateSimpleStatementScope s@Import{} = pure $ unsafeCoerce s
validateSimpleStatementScope s@From{} = pure $ unsafeCoerce s

validateSmallStatementScope
  :: AsScopeError e a
  => SmallStatement v a
  -> ValidateScope a e (SmallStatement (Nub (Scope ': v)) a)
validateSmallStatementScope (MkSmallStatement s ss sc cmt nl) =
  (\s' ss' -> MkSmallStatement s' ss' sc cmt nl) <$>
  validateSimpleStatementScope s <*>
  traverseOf (traverse._2) validateSimpleStatementScope ss

validateStatementScope
  :: AsScopeError e a
  => Statement v a
  -> ValidateScope a e (Statement (Nub (Scope ': v)) a)
validateStatementScope (CompoundStatement c) =
  CompoundStatement <$> validateCompoundStatementScope c
validateStatementScope (SmallStatement idnts a) =
  SmallStatement idnts <$> validateSmallStatementScope a

validateIdentScope
  :: AsScopeError e a
  => Ident v a
  -> ValidateScope a e (Ident (Nub (Scope ': v)) a)
validateIdentScope i =
  lookupScope (_identValue i) `bindVM` \res ->
  liftVM0 ask `bindVM` \curPath ->
    case res of
      Nothing -> errorVM1 (_NotInScope # (i ^. unvalidated))
      Just (Entry ann path) ->
        coerce i <$
        if Seq.length curPath < Seq.length path
        then errorVM1 (_FoundDynamic # (ann, i ^. unvalidated))
        else pure ()

validateArgScope
  :: AsScopeError e a
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
  :: AsScopeError e a
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
  :: AsScopeError e a
  => Block v a
  -> ValidateScope a e (Block (Nub (Scope ': v)) a)
validateBlockScope (Block x b bs) =
  Block x <$>
  validateStatementScope b <*>
  traverseOf (traverse._Right) validateStatementScope bs

validateComprehensionScope
  :: AsScopeError e a
  => (ex v a -> ValidateScope a e (ex (Nub (Scope ': v)) a))
  -> Comprehension ex v a
  -> ValidateScope a e (Comprehension ex (Nub (Scope ': v)) a)
validateComprehensionScope f (Comprehension a b c d) =
  controlScope $
  (\c' d' b' -> Comprehension a b' c' d') <$>
  validateCompForScope c <*>
  traverse (bitraverse validateCompForScope validateCompIfScope) d <*>
  f b
  where
    validateCompForScope
      :: AsScopeError e a
      => CompFor v a
      -> ValidateScope a e (CompFor (Nub (Scope ': v)) a)
    validateCompForScope (CompFor a b c d e) =
      (\c' -> CompFor a b c' d) <$>
      validateAssignExprScope c <*>
      validateExprScope e <*
      extendScope (c ^.. unvalidated.assignTargets)

    validateCompIfScope
      :: AsScopeError e a
      => CompIf v a
      -> ValidateScope a e (CompIf (Nub (Scope ': v)) a)
    validateCompIfScope (CompIf a b c) =
      CompIf a b <$> validateExprScope c

validateAssignExprScope
  :: AsScopeError e a
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
validateAssignExprScope (Ident a (MkIdent b c d)) =
  lookupScope c `bindVM` \res ->
  liftVM0 ask `bindVM` \curPath ->
  Ident a (MkIdent b c d) <$
  case res of
    Nothing -> extendScope [MkIdent b c d]
    Just (Entry _ path)->
      if Seq.length curPath < Seq.length path
      then extendScope [MkIdent b c d]
      else pure ()
validateAssignExprScope e@Unit{} = pure $ unsafeCoerce e
validateAssignExprScope e@Lambda{} = pure $ unsafeCoerce e
validateAssignExprScope e@Yield{} = pure $ unsafeCoerce e
validateAssignExprScope e@YieldFrom{} = pure $ unsafeCoerce e
validateAssignExprScope e@Not{} = pure $ unsafeCoerce e
validateAssignExprScope e@ListComp{} = pure $ unsafeCoerce e
validateAssignExprScope e@Call{} = pure $ unsafeCoerce e
validateAssignExprScope e@UnOp{} = pure $ unsafeCoerce e
validateAssignExprScope e@BinOp{} = pure $ unsafeCoerce e
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
  :: AsScopeError e a
  => DictItem v a
  -> ValidateScope a e (DictItem (Nub (Scope ': v)) a)
validateDictItemScope (DictItem a b c d) =
  (\b' -> DictItem a b' c) <$>
  validateExprScope b <*>
  validateExprScope d
validateDictItemScope (DictUnpack a b c) =
  DictUnpack a b <$> validateExprScope c

validateSubscriptScope
  :: AsScopeError e a
  => Subscript v a
  -> ValidateScope a e (Subscript (Nub (Scope ': v)) a)
validateSubscriptScope (SubscriptExpr e) = SubscriptExpr <$> validateExprScope e
validateSubscriptScope (SubscriptSlice a b c d) =
  (\a' -> SubscriptSlice a' b) <$>
  traverse validateExprScope a <*>
  traverse validateExprScope c <*>
  traverseOf (traverse._2.traverse) validateExprScope d

validateListItemScope
  :: AsScopeError e a
  => ListItem v a
  -> ValidateScope a e (ListItem (Nub (Scope ': v)) a)
validateListItemScope (ListItem a b) = ListItem a <$> validateExprScope b
validateListItemScope (ListUnpack a b c d) = ListUnpack a b c <$> validateExprScope d

validateSetItemScope
  :: AsScopeError e a
  => SetItem v a
  -> ValidateScope a e (SetItem (Nub (Scope ': v)) a)
validateSetItemScope (SetItem a b) = SetItem a <$> validateExprScope b
validateSetItemScope (SetUnpack a b c d) = SetUnpack a b c <$> validateExprScope d

validateTupleItemScope
  :: AsScopeError e a
  => TupleItem v a
  -> ValidateScope a e (TupleItem (Nub (Scope ': v)) a)
validateTupleItemScope (TupleItem a b) = TupleItem a <$> validateExprScope b
validateTupleItemScope (TupleUnpack a b c d) = TupleUnpack a b c <$> validateExprScope d

validateExprScope
  :: AsScopeError e a
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
validateExprScope (Ident a i) = Ident a <$> validateIdentScope i
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
  :: AsScopeError e a
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
