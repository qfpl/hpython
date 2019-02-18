{-# language DataKinds, TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables, TypeApplications #-}

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
  , validateExprScope_
    -- * Miscellany
    -- ** Calculating module exports
  , getGlobals
  , moduleEntry
    -- ** Extra types
  , ScopeContext(..), scGlobalScope, scLocalScope, scImmediateScope
  , runValidateScope'
  , emptyScopeContext
  , builtinsScopeContext
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
  , validateExprScope
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
import Control.Lens.Fold ((^..), toListOf, folded, foldrOf, preview)
import Control.Lens.Getter ((^.), view, to, getting, use)
import Control.Lens.Lens (Lens')
import Control.Lens.Plated (cosmos)
import Control.Lens.Prism (_Right, _Just)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((%~), (.~), Setter', mapped, over)
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Lens.Traversal (traverseOf)
import Control.Monad.State (MonadState, State, evalState, modify)
import Data.Bitraversable (bitraverse)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (traverse_, fold)
import Data.Function ((&))
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
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Module
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Types
import Language.Python.Validate.Scope.Error

data Scope

data Binding ann
  = Clean (Maybe ann)
  | Dirty ann
  deriving (Eq, Ord, Show)

data Entry a
  = Entry
  { _entryAttrs :: !(Map ByteString (Entry a))
  , _entryLocation :: a
  } deriving (Eq, Show, Functor)

entry :: a -> Entry a
entry = Entry mempty

data GlobalEntry a
  = GlobalEntryDone
  { _geAttrsDone :: !(Map ByteString (Entry a))
  , _geLocation :: Maybe a
  }
  | GlobalEntryMore
  { _geAttrsMore :: !(Map ByteString (GlobalEntry a))
  , _geLocation :: Maybe a
  } deriving (Eq, Show, Functor)

-- |
-- Extract the identifiers that are definitely in a module's global scope
--
-- Currently these are:
--
-- - The targets of top-level assignments
-- - Top-level class definitions, and their attributes recursively
-- - Top-level function definitions
getGlobals :: Module v a -> Map ByteString (Entry a)
getGlobals = go
  where
    go :: HasStatements s => s v a -> Map ByteString (Entry a)
    go =
      foldrOf
        (getting _Statements)
        (\a b ->
          case a of
            CompoundStatement s ->
              fold $

              (\x ->
                 Map.insert
                   (x ^. cdName.getting identValue.to fromString)
                   (Entry (go $ x ^. cdBody) $ x ^. annot_)
                   b) <$>
              preview (getting _ClassDef) s

              <|>

              (\x ->
                 Map.insert
                   (x ^. fdName.getting identValue.to fromString)
                   (entry $ x ^. annot_)
                   b) <$>
              preview (getting _Fundef) s

            SmallStatement _ s ->
              foldrOf
                (getting (_SimpleStatements._Assign).to unfoldAssign._1.folded.getting assignTargets)
                (\a' -> Map.insert (a' ^. getting identValue.to fromString) (entry $ a' ^. annot_))
                b
                s)
        mempty

-- |
-- Creates a 'GlobalEntry' for a 'Module', which lists all the in-scope identifiers
-- for that module.
--
-- If the module name is a path (e.g. @a.b.c@) and no renaming is supplied
-- then it creates nested entries so we can use the regular dereferencing machinery.
--
-- If a renaming is given, then that renaming will be the key in the symbol table.
moduleEntry ::
  ModuleName v a -> -- ^ Module name
  Maybe (Ident v a) -> -- ^ Optional renaming
  Module v a -> -- ^ The module
  (ByteString, GlobalEntry a)
moduleEntry _ (Just i) mod =
  ( i ^. getting identValue.to fromString
  , GlobalEntryDone (getGlobals mod) Nothing
  )
moduleEntry (ModuleNameOne _ i) Nothing mod =
  ( i ^. getting identValue.to fromString
  , GlobalEntryDone (getGlobals mod) Nothing
  )
moduleEntry (ModuleNameMany _ i _ rest) Nothing mod =
  let
    (n, e) = moduleEntry rest Nothing mod
  in
    ( i ^. getting identValue.to fromString
    , GlobalEntryMore (Map.singleton n e) Nothing
    )

builtinEntry :: Map ByteString (Entry a) -> GlobalEntry a
builtinEntry a = GlobalEntryDone a Nothing

globalEntry :: a -> GlobalEntry a
globalEntry = GlobalEntryDone mempty . Just

toGlobalEntry :: Entry a -> GlobalEntry a
toGlobalEntry e =
  GlobalEntryDone
  { _geAttrsDone = _entryAttrs e
  , _geLocation = Just $ _entryLocation e
  }

data ScopeContext a
  = ScopeContext
  { _scGlobalScope :: !(Map ByteString (GlobalEntry a))
  , _scLocalScope :: !(Map ByteString (Entry a))
  , _scImmediateScope :: !(Map ByteString (Entry a))
  } deriving (Eq, Show)
makeLenses ''ScopeContext

emptyScopeContext :: ScopeContext a
emptyScopeContext = ScopeContext Map.empty Map.empty Map.empty

builtins :: Map ByteString (GlobalEntry a)
builtins =
  Map.fromList
    [ ("abs", builtinEntry mempty)
    , ("dict", builtinEntry mempty)
    , ("help", builtinEntry mempty)
    , ("min", builtinEntry mempty)
    , ("setattr", builtinEntry mempty)
    , ("all", builtinEntry mempty)
    , ("dir", builtinEntry mempty)
    , ("hex", builtinEntry mempty)
    , ("next", builtinEntry mempty)
    , ("slice", builtinEntry mempty)
    , ("any", builtinEntry mempty)
    , ("divmod", builtinEntry mempty)
    , ("id", builtinEntry mempty)
    , ("object", builtinEntry mempty)
    , ("sorted", builtinEntry mempty)
    , ("ascii", builtinEntry mempty)
    , ("enumerate", builtinEntry mempty)
    , ("input", builtinEntry mempty)
    , ("oct", builtinEntry mempty)
    , ("staticmethod", builtinEntry mempty)
    , ("bin", builtinEntry mempty)
    , ("eval", builtinEntry mempty)
    , ("int", builtinEntry mempty)
    , ("open", builtinEntry mempty)
    , ("str", builtinEntry mempty)
    , ("bool", builtinEntry mempty)
    , ("exec", builtinEntry mempty)
    , ("isinstance", builtinEntry mempty)
    , ("ord", builtinEntry mempty)
    , ("sum", builtinEntry mempty)
    , ("bytearray", builtinEntry mempty)
    , ("filter", builtinEntry mempty)
    , ("issubclass", builtinEntry mempty)
    , ("pow", builtinEntry mempty)
    , ("super", builtinEntry mempty)
    , ("bytes", builtinEntry mempty)
    , ("float", builtinEntry mempty)
    , ("iter", builtinEntry mempty)
    , ("print", builtinEntry mempty)
    , ("tuple", builtinEntry mempty)
    , ("callable", builtinEntry mempty)
    , ("format", builtinEntry mempty)
    , ("len", builtinEntry mempty)
    , ("property", builtinEntry mempty)
    , ("type", builtinEntry mempty)
    , ("chr", builtinEntry mempty)
    , ("frozenset", builtinEntry mempty)
    , ("list", builtinEntry mempty)
    , ("range", builtinEntry mempty)
    , ("vars", builtinEntry mempty)
    , ("classmethod", builtinEntry mempty)
    , ("getattr", builtinEntry mempty)
    , ("locals", builtinEntry mempty)
    , ("repr", builtinEntry mempty)
    , ("zip", builtinEntry mempty)
    , ("compile", builtinEntry mempty)
    , ("globals", builtinEntry mempty)
    , ("map", builtinEntry mempty)
    , ("reversed", builtinEntry mempty)
    , ("__import__", builtinEntry mempty)
    , ("complex", builtinEntry mempty)
    , ("hasattr", builtinEntry mempty)
    , ("max", builtinEntry mempty)
    , ("round", builtinEntry mempty)
    , ("delattr", builtinEntry mempty)
    , ("hash", builtinEntry mempty)
    , ("memoryview", builtinEntry mempty)
    , ("set", builtinEntry mempty)
    ]

globals :: Map ByteString (GlobalEntry a)
globals =
  Map.fromList
  [ ("__builtins__", GlobalEntryMore builtins Nothing)
  , ("__name__", builtinEntry mempty)
  , ("__spec__", builtinEntry mempty)
  , ("__doc__", builtinEntry mempty)
  , ("__package__", builtinEntry mempty)
  , ("__loader__", builtinEntry mempty)
  ]

builtinsScopeContext :: ScopeContext a
builtinsScopeContext =
  emptyScopeContext
  { _scGlobalScope = builtins <> globals
  }

type ValidateScope ann e = ValidateM (NonEmpty e) (State (ScopeContext ann))

runValidateScope ::
  Map ByteString (GlobalEntry ann) -> -- ^ Extensions to the global scope
  ValidateScope ann e a -> -- ^ Validation action
  Validation (NonEmpty e) a
runValidateScope exts =
  runValidateScope' $
  builtinsScopeContext &
    scGlobalScope %~ (\x -> Map.unionWith const x exts)

runValidateScope' :: ScopeContext ann -> ValidateScope ann e a -> Validation (NonEmpty e) a
runValidateScope' s = flip evalState s . runValidateM

extendScope
  :: Setter' (ScopeContext ann) (Map ByteString x)
  -> [(x, String)]
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
  :: Lens' (ScopeContext ann) (Map ByteString x)
  -> [(x, String)]
  -> ValidateScope ann e a
  -> ValidateScope ann e a
locallyExtendOver l s m = locallyOver l id $ extendScope l s *> m

inScope
  :: MonadState (ScopeContext ann) m
  => String
  -> m (Maybe (Binding ann))
inScope s = do
  gs <- use scGlobalScope
  ls <- use scLocalScope
  is <- use scImmediateScope
  let
    s' = fromString s
    inis = _entryLocation <$> Map.lookup s' is
    inls = _entryLocation <$> Map.lookup s' ls
    ings = _geLocation <$> Map.lookup s' gs
  pure $
    (Clean . Just <$> inis) <|>
    (ings *> (Clean . Just <$> inls)) <|>
    (Clean <$> ings) <|>
    (Dirty <$> inls)

validateExceptAsScope
  :: AsScopeError e a
  => ExceptAs v a
  -> ValidateScope a e (ExceptAs (Nub (Scope ': v)) a)
validateExceptAsScope (ExceptAs ann e f) =
  ExceptAs ann <$>
  validateExprScope_ e <*>
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
  validateExprScope_ d

validateCompoundStatementScope
  :: forall e v a
   . AsScopeError e a
  => CompoundStatement v a
  -> ValidateScope a e (CompoundStatement (Nub (Scope ': v)) a)
validateCompoundStatementScope (Fundef a decos idnts asyncWs ws1 name ws2 params ws3 mty s) =
  (locallyOver scLocalScope (const Map.empty) $
   locallyOver scImmediateScope (const Map.empty) $
     (\decos' -> Fundef a decos' idnts asyncWs ws1 (coerce name) ws2) <$>
     traverse validateDecoratorScope decos <*>
     traverse validateParamScope params <*>
     pure ws3 <*>
     traverseOf (traverse._2) validateExprScope_ mty <*>
     locallyExtendOver
       scGlobalScope
       ((globalEntry . view annot_ &&& _identValue) name :
         toListOf
           (folded.getting paramName.to (globalEntry . view annot_ &&& _identValue))
           params)
       (validateSuiteScope s)) <*
  extendScope scLocalScope [(entry . view annot_ &&& _identValue) name] <*
  extendScope scImmediateScope [(entry . view annot_ &&& _identValue) name]
validateCompoundStatementScope (If idnts a ws1 e b elifs melse) =
  liftVM0 (use scLocalScope) `bindVM` (\ls ->
  liftVM0 (use scImmediateScope) `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` fmap toGlobalEntry (unionR ls is)) $
  locallyOver scImmediateScope (const Map.empty)
    (If idnts a ws1 <$>
     validateExprScope_ e <*>
     validateSuiteScope b <*>
     traverse
       (\(a, b, c, d) ->
          (\c' -> (,,,) a b c') <$>
          validateExprScope_ c <*>
          validateSuiteScope d)
       elifs <*>
     traverseOf (traverse._3) validateSuiteScope melse)))
validateCompoundStatementScope (While idnts a ws1 e b els) =
  liftVM0 (use scLocalScope) `bindVM` (\ls ->
  liftVM0 (use scImmediateScope) `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` fmap toGlobalEntry (unionR ls is)) $
  locallyOver scImmediateScope (const Map.empty)
    (While idnts a ws1 <$>
     validateExprScope_ e <*>
     validateSuiteScope b <*>
     traverseOf (traverse._3) validateSuiteScope els)))
validateCompoundStatementScope (TryExcept idnts a b e f k l) =
  liftVM0 (use scLocalScope) `bindVM` (\ls ->
  liftVM0 (use scImmediateScope) `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` fmap toGlobalEntry (unionR ls is)) $
  locallyOver scImmediateScope (const Map.empty)
    (TryExcept idnts a b <$>
     validateSuiteScope e <*>
     traverse
       (\(idnts, ws, g, h) ->
          (,,,) idnts ws <$>
          traverse validateExceptAsScope g <*>
          locallyExtendOver
            scGlobalScope
            (toListOf
               (folded.exceptAsName._Just._2.to (globalEntry . view annot_ &&& _identValue))
               g)
            (validateSuiteScope h))
       f <*>
     traverseOf (traverse._3) validateSuiteScope k <*>
     traverseOf (traverse._3) validateSuiteScope l)))
validateCompoundStatementScope (TryFinally idnts a b e idnts2 f i) =
  liftVM0 (use scLocalScope) `bindVM` (\ls ->
  liftVM0 (use scImmediateScope) `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` fmap toGlobalEntry (unionR ls is)) $
  locallyOver scImmediateScope (const Map.empty)
    (TryFinally idnts a b <$>
     validateSuiteScope e <*>
     pure idnts2 <*>
     pure f <*>
     validateSuiteScope i)))
validateCompoundStatementScope (For idnts a asyncWs b c d e h i) =
  liftVM0 (use scLocalScope) `bindVM` (\ls ->
  liftVM0 (use scImmediateScope) `bindVM` (\is ->
  locallyOver scGlobalScope (`unionR` fmap toGlobalEntry (unionR ls is)) $
  locallyOver scImmediateScope (const Map.empty) $
    For @(Nub (Scope ': v)) idnts a asyncWs b <$>
    (unsafeCoerce c <$
     traverse
       (\s ->
          liftVM0 (inScope $ s ^. identValue) `bindVM` \res ->
          maybe (pure ()) (\_ -> errorVM1 (_BadShadowing # coerce s)) res)
       (c ^.. unvalidated.cosmos._Ident)) <*>
    pure d <*>
    traverse validateExprScope_ e <*>
    (let
       ls = c ^.. unvalidated.cosmos._Ident.to (entry . view annot_ &&& _identValue)
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
  extendScope scImmediateScope [c ^. to (entry . view annot_ &&& _identValue)]
validateCompoundStatementScope (With a b asyncWs c d e) =
  let
    names =
      d ^..
      folded.unvalidated.to _withItemBinder.folded._2.
      assignTargets.to (entry . view annot_ &&& _identValue)
  in
    With @(Nub (Scope ': v)) a b asyncWs c <$>
    traverse
      (\(WithItem a b c) ->
         WithItem @(Nub (Scope ': v)) a <$>
         validateExprScope_ b <*>
         traverseOf (traverse._2) validateAssignExprScope c)
      d <*
    extendScope scLocalScope names <*
    extendScope scImmediateScope names <*>
    validateSuiteScope e

validateSimpleStatementScope
  :: AsScopeError e a
  => SimpleStatement v a
  -> ValidateScope a e (SimpleStatement (Nub (Scope ': v)) a)
validateSimpleStatementScope (Assert a b c d) =
  Assert a b <$>
  validateExprScope_ c <*>
  traverseOf (traverse._2) validateExprScope_ d
validateSimpleStatementScope (Raise a ws f) =
  Raise a ws <$>
  traverse
    (\(b, c) ->
       (,) <$>
       validateExprScope_ b <*>
       traverseOf (traverse._2) validateExprScope_ c)
    f
validateSimpleStatementScope (Return a ws e) =
  Return a ws <$> traverse validateExprScope_ e
validateSimpleStatementScope (Expr a e) =
  Expr a <$> validateExprScope_ e
validateSimpleStatementScope (Assign a l rs) =
  let
    ls =
      (l : (snd <$> NonEmpty.init rs)) ^..
      folded.unvalidated.assignTargets.to (entry . view annot_ &&& _identValue)
  in
  Assign a <$>
  validateAssignExprScope l <*>
  ((\a b -> case a of; [] -> b :| []; a : as -> a :| snoc as b) <$>
   traverseOf (traverse._2) validateAssignExprScope (NonEmpty.init rs) <*>
   (\(ws, b) -> (,) ws <$> validateExprScope_ b) (NonEmpty.last rs)) <*
  extendScope scLocalScope ls <*
  extendScope scImmediateScope ls
validateSimpleStatementScope (AugAssign a l aa r) =
  (\l' -> AugAssign a l' aa) <$>
  validateExprScope_ l <*>
  validateExprScope_ r
validateSimpleStatementScope (Global a _ _) = errorVM1 (_FoundGlobal # getAnn a)
validateSimpleStatementScope (Nonlocal a _ _) = errorVM1 (_FoundNonlocal # getAnn a)
validateSimpleStatementScope (Del a ws cs) =
  Del a ws <$
  traverse_
    (\case; Ident a _ -> errorVM1 (_DeletedIdent # getAnn a); _ -> pure ())
    cs <*>
  traverse validateExprScope_ cs
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
  liftVM0 (inScope $ _identValue i) `bindVM`
  \context ->
  case context of
    Just (Clean _) -> pure $ coerce i
    Just (Dirty ann)-> errorVM1 (_FoundDynamic # (ann, i ^. unvalidated))
    Nothing -> errorVM1 (_NotInScope # (i ^. unvalidated))

validateArgScope
  :: AsScopeError e a
  => Arg v a
  -> ValidateScope a e (Arg (Nub (Scope ': v)) a)
validateArgScope (PositionalArg a e) =
  PositionalArg a <$> validateExprScope_ e
validateArgScope (KeywordArg a ident ws2 expr) =
  KeywordArg a (coerce ident) ws2 <$> validateExprScope_ expr
validateArgScope (StarArg a ws e) =
  StarArg a ws <$> validateExprScope_ e
validateArgScope (DoubleStarArg a ws e) =
  DoubleStarArg a ws <$> validateExprScope_ e

validateParamScope
  :: AsScopeError e a
  => Param v a
  -> ValidateScope a e (Param (Nub (Scope ': v)) a)
validateParamScope (PositionalParam a ident mty) =
  PositionalParam a (coerce ident) <$>
  traverseOf (traverse._2) validateExprScope_ mty
validateParamScope (KeywordParam a ident mty ws2 expr) =
  KeywordParam a (coerce ident) <$>
  traverseOf (traverse._2) validateExprScope_ mty <*>
  pure ws2 <*>
  validateExprScope_ expr
validateParamScope (StarParam a b c d) =
  StarParam a b (coerce c) <$>
  traverseOf (traverse._2) validateExprScope_ d
validateParamScope (UnnamedStarParam a b) = pure $ UnnamedStarParam a b
validateParamScope (DoubleStarParam a b c d) =
  DoubleStarParam a b (coerce c) <$>
  traverseOf (traverse._2) validateExprScope_ d

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
  locallyOver scGlobalScope id $
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
      validateExprScope_ e <*
      extendScope scGlobalScope
        (c ^..
         unvalidated.assignTargets.to (globalEntry . view annot_ &&& _identValue))

    validateCompIfScope
      :: AsScopeError e a
      => CompIf v a
      -> ValidateScope a e (CompIf (Nub (Scope ': v)) a)
    validateCompIfScope (CompIf a b c) =
      CompIf a b <$> validateExprScope_ c

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
  validateExprScope_ e <*>
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
  :: AsScopeError e a
  => DictItem v a
  -> ValidateScope a e (DictItem (Nub (Scope ': v)) a)
validateDictItemScope (DictItem a b c d) =
  (\b' -> DictItem a b' c) <$>
  validateExprScope_ b <*>
  validateExprScope_ d
validateDictItemScope (DictUnpack a b c) =
  DictUnpack a b <$> validateExprScope_ c

validateSubscriptScope
  :: AsScopeError e a
  => Subscript v a
  -> ValidateScope a e (Subscript (Nub (Scope ': v)) a)
validateSubscriptScope (SubscriptExpr e) = SubscriptExpr <$> validateExprScope_ e
validateSubscriptScope (SubscriptSlice a b c d) =
  (\a' -> SubscriptSlice a' b) <$>
  traverse validateExprScope_ a <*>
  traverse validateExprScope_ c <*>
  traverseOf (traverse._2.traverse) validateExprScope_ d

validateListItemScope
  :: AsScopeError e a
  => ListItem v a
  -> ValidateScope a e (ListItem (Nub (Scope ': v)) a)
validateListItemScope (ListItem a b) = ListItem a <$> validateExprScope_ b
validateListItemScope (ListUnpack a b c d) = ListUnpack a b c <$> validateExprScope_ d

validateSetItemScope
  :: AsScopeError e a
  => SetItem v a
  -> ValidateScope a e (SetItem (Nub (Scope ': v)) a)
validateSetItemScope (SetItem a b) = SetItem a <$> validateExprScope_ b
validateSetItemScope (SetUnpack a b c d) = SetUnpack a b c <$> validateExprScope_ d

validateTupleItemScope
  :: AsScopeError e a
  => TupleItem v a
  -> ValidateScope a e (TupleItem (Nub (Scope ': v)) a)
validateTupleItemScope (TupleItem a b) = TupleItem a <$> validateExprScope_ b
validateTupleItemScope (TupleUnpack a b c d) = TupleUnpack a b c <$> validateExprScope_ d

validateExprScope_
  :: AsScopeError e a
  => Expr v a
  -> ValidateScope a e (Expr (Nub (Scope ': v)) a)
validateExprScope_ = fmap fst . validateExprScope

unrefined :: Applicative f => f a -> f (a, Maybe (Map ByteString (GlobalEntry x)))
unrefined m = (,) <$> m <*> pure Nothing

-- | Validate an expressions scope, and return a possible refinement to the scope
--
-- For example, the identifier @a@ might be a module name, in which case @a.x@ would
-- reference one of its definitions. When we validate the @a@ inside @a.x@, we
-- return the scope associated with it and check that @x@ occurs in that scope.
validateExprScope ::
  AsScopeError e a =>
  Expr v a ->
  ValidateScope a e
    (Expr (Nub (Scope ': v)) a, Maybe (Map ByteString (GlobalEntry a)))
validateExprScope (Lambda a b c d e) =
  unrefined $
  Lambda a b <$>
  traverse validateParamScope c <*>
  pure d <*>
  validateExprScope_ e
validateExprScope (Yield a b c) =
  unrefined $ Yield a b <$> traverse validateExprScope_ c
validateExprScope (YieldFrom a b c d) =
  unrefined $ YieldFrom a b c <$> validateExprScope_ d
validateExprScope (Ternary a b c d e f) =
  unrefined $
  (\b' d' f' -> Ternary a b' c d' e f') <$>
  validateExprScope_ b <*>
  validateExprScope_ d <*>
  validateExprScope_ f
validateExprScope (Subscript a b c d e) =
  unrefined $
  (\b' d' -> Subscript a b' c d' e) <$>
  validateExprScope_ b <*>
  traverse validateSubscriptScope d
validateExprScope (Not a ws e) =
  unrefined $ Not a ws <$> validateExprScope_ e
validateExprScope (List a ws1 es ws2) =
  unrefined $
  List a ws1 <$>
  traverseOf (traverse.traverse) validateListItemScope es <*>
  pure ws2
validateExprScope (ListComp a ws1 comp ws2) =
  unrefined $
  ListComp a ws1 <$>
  validateComprehensionScope validateExprScope_ comp <*>
  pure ws2
validateExprScope (Generator a comp) =
  unrefined $
  Generator a <$>
  validateComprehensionScope validateExprScope_ comp
validateExprScope (Await a ws expr) =
  unrefined $
  Await a ws <$> validateExprScope_ expr
validateExprScope (Deref a e ws1 (MkIdent ann i ws)) =
  validateExprScope e `bindVM` \(e', mscope) ->
    (,) (Deref a e' ws1 $ MkIdent ann i ws) <$>
    case mscope of
      Nothing -> pure Nothing
      Just scope ->
        case Map.lookup (fromString i) scope of
          Nothing -> errorVM1 $ _NotInScope # MkIdent ann i ws
          Just (GlobalEntryDone layer _) ->
            pure . Just $ fmap toGlobalEntry layer
          Just (GlobalEntryMore layer _) ->
            pure . Just $ layer
validateExprScope (Call a e ws1 as ws2) =
  unrefined $
  Call a <$>
  validateExprScope_ e <*>
  pure ws1 <*>
  traverseOf (traverse.traverse) validateArgScope as <*>
  pure ws2
validateExprScope (BinOp a l op r) =
  unrefined $
  BinOp a <$>
  validateExprScope_ l <*>
  pure op <*>
  validateExprScope_ r
validateExprScope (UnOp a op e) =
  unrefined $
  UnOp a op <$>
  validateExprScope_ e
validateExprScope (Parens a ws1 e ws2) =
  (\(e', sc) -> (Parens a ws1 e' ws2, sc)) <$> validateExprScope e
validateExprScope (Ident a i) =
  (,) <$>
  (Ident a <$> validateIdentScope i) <*>
  fmap
    (fmap
       (\case
           GlobalEntryDone es _ -> toGlobalEntry <$> es
           GlobalEntryMore es _ -> es) .
     Map.lookup (i ^. getting identValue . to fromString))
    (liftVM0 $ use scGlobalScope)
validateExprScope (Tuple a b ws d) =
  unrefined $
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
  unrefined $
  DictComp a ws1 <$>
  validateComprehensionScope validateDictItemScope comp <*>
  pure ws2
validateExprScope (Dict a b c d) =
  unrefined $
  (\c' -> Dict a b c' d) <$>
  traverseOf (traverse.traverse) validateDictItemScope c
validateExprScope (SetComp a ws1 comp ws2) =
  unrefined $
  SetComp a ws1 <$>
  validateComprehensionScope validateSetItemScope comp <*>
  pure ws2
validateExprScope (Set a b c d) =
  unrefined $
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

unionL :: Ord k => Map k v -> Map k v -> Map k v
unionL = Map.unionWith const

unionR :: Ord k => Map k v -> Map k v -> Map k v
unionR = Map.unionWith (const id)
