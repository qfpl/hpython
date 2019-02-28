{-# language DataKinds, TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}

{-|
Module      : Language.Python.Validate.Scope
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Note: we try our very best to check that the program is scope safe, but without
a type checker we have to make some concessions. The main one is that
variables (function parameters/targets of assignment) have no attribute
information associated with them, and thus every dereference of a variable
is considered valid.

For example, this code will pass scope checking:

@
def f(x):
  print(x.a)

b = 1
print(b.a)
@

Unfortunately, it's not practical to reject these sorts of usage.

This means that *there are programs that pass the scope checker, but throw
AttributeErrors at runtime*.

-}
module Language.Python.Validate.Scope
  ( module Data.Validation
  , module Language.Python.Validate.Scope.Error
    -- * Main validation functions
  , Scope, ValidateScope, runValidateScope
  , validateModuleScope
  , validateStatementScope
  , validateExprScope_
    -- ** Builtin and global variables
  , builtins
  , globals
    -- * Miscellany
    -- ** Calculating module exports
  , getGlobals
  , moduleEntryMap
  , moduleEntry
    -- ** Extra types
  , Level(..)
  , Env(..)
  , envPath, envInClass
  , Entry(..), EntryType(..)
    -- ** Extra functions
  , runValidateScope'
  , isStaticmethod
  , isClassmethod
    -- *** Entry operations
  , entryPath
  , lookupEntry, setEntry, updateEntry
  , isClassEntry, isFunctionEntry, isParamEntry
    -- *** Scope operations
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

import Control.Applicative ((<|>))
import Control.Lens.Cons (snoc)
import Control.Lens.Fold
  ((^..), toListOf, folded, foldrOf, preview, filtered, anyOf)
import Control.Lens.Getter ((^.), to, getting, view)
import Control.Lens.Plated (cosmos)
import Control.Lens.Prism (_Right, _Just)
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.~), (%~), mapped, over)
import Control.Lens.TH (makeLensesFor, makeLenses)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Lens.Traversal (traverseOf)
import Control.Monad (unless)
import Control.Monad.State (State, evalState, modify, get, put)
import Control.Monad.Reader (ReaderT, runReaderT, local)
import Data.Bitraversable (bitraverse)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (toList, traverse_, fold)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Sequence ((|>), Seq, ViewL(..))
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
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Types
import Language.Python.Validate.Scope.Error

data Scope

data Level
  = Toplevel
  | Definition
  | Control
  deriving (Eq, Show)

toplevelPath :: Seq Level -> Bool
toplevelPath lvl =
  case Seq.viewl lvl of
    EmptyL -> False
    Toplevel :< rest -> Seq.null rest
    _ -> False

data EntryType
  = ClassEntry
  | FunctionEntry
  | VarEntry
  | ParamEntry
  deriving (Eq, Show)

data Entry a
  = Entry
  { _entryValue :: a
  , _entryType :: EntryType
  , _entryPath :: !(Seq Level)
  , _subEntries :: Map ByteString (Entry a)
  }
  | GlobalEntry
  { _subEntries :: Map ByteString (Entry a)
  } deriving (Eq, Show, Functor)

makeLensesFor [("_subEntries", "subEntries")] ''Entry

isClassEntry :: Entry a -> Bool
isClassEntry (Entry _ ClassEntry _ _) = True
isClassEntry _ = False

isFunctionEntry :: Entry a -> Bool
isFunctionEntry (Entry _ FunctionEntry _ _) = True
isFunctionEntry _ = False

isParamEntry :: Entry a -> Bool
isParamEntry (Entry _ ParamEntry _ _) = True
isParamEntry _ = False

lookupEntry ::
  Seq ByteString ->
  Map ByteString (Entry a) ->
  Maybe (Entry a)
lookupEntry path =
  case Seq.viewl path of
    EmptyL -> error "lookupEntry: empty path"
    p :< ps -> go p ps
  where
    go ::
      ByteString ->
      Seq ByteString ->
      Map ByteString (Entry a) ->
      Maybe (Entry a)
    go p ps m =
      case Seq.viewl ps of
        EmptyL -> Map.lookup p m
        p' :< ps' -> Map.lookup p m >>= go p' ps' . _subEntries

updateEntry ::
  Seq ByteString ->
  (Entry a -> Entry a) ->
  Map ByteString (Entry a) ->
  Map ByteString (Entry a)
updateEntry path =
  case Seq.viewl path of
    EmptyL -> error "setEntry: empty path"
    p :< ps -> go p ps
  where
    go ::
      ByteString ->
      Seq ByteString ->
      (Entry a -> Entry a) ->
      Map ByteString (Entry a) ->
      Map ByteString (Entry a)
    go p ps f =
      case Seq.viewl ps of
        EmptyL -> Map.update (Just . f) p
        p' :< ps' -> Map.update (Just . over subEntries (go p' ps' f)) p

setEntry ::
  Seq ByteString ->
  Entry a ->
  Map ByteString (Entry a) ->
  Map ByteString (Entry a)
setEntry path =
  case Seq.viewl path of
    EmptyL -> error "setEntry: empty path"
    p :< ps -> go p ps
  where
    go ::
      ByteString ->
      Seq ByteString ->
      Entry a ->
      Map ByteString (Entry a) ->
      Map ByteString (Entry a)
    go p ps e =
      case Seq.viewl ps of
        EmptyL -> Map.alter (const $ Just e) p
        p' :< ps' ->
          Map.alter
            (\mentry ->
               Just $
               case mentry of
                 Nothing -> GlobalEntry $ go p' ps' e mempty
                 Just entry ->
                   entry { _subEntries = go p' ps' e $ _subEntries entry })
            p

-- | The 'entryPath' for a 'GlobalEntry' is @['Toplevel']@
entryPath :: Entry a -> Seq Level
entryPath GlobalEntry{} = [Toplevel]
entryPath (Entry _ _ a _) = a

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
                   (Entry (x ^. annot_) ClassEntry [Toplevel] (go $ x ^. cdBody))
                   b) <$>
              preview (getting _ClassDef) s

              <|>

              (\x ->
                 Map.insert
                   (x ^. fdName.getting identValue.to fromString)
                   (Entry (x ^. annot_) FunctionEntry [Toplevel] mempty)
                   b) <$>
              preview (getting _Fundef) s

            SmallStatement _ s ->
              foldrOf
                (getting (_SimpleStatements._Assign).to unfoldAssign._1.folded.getting assignTargets)
                (\a' ->
                   Map.insert
                     (a' ^. getting identValue.to fromString)
                     (Entry (a' ^. annot_) VarEntry [Toplevel] mempty))
                b
                s)
        mempty

-- |
-- Creates an 'Entry' for a 'Module', which lists all the in-scope identifiers
-- for that module.
--
-- If the module name is a path (e.g. @a.b.c@) and no renaming is supplied
-- then it creates nested entries so we can use the regular dereferencing machinery.
--
-- If a renaming is given, then that renaming will be the key in the symbol table.
moduleEntryMap ::
  ModuleName v a -> -- ^ Module name
  Maybe (Ident v a) -> -- ^ Optional renaming
  Map ByteString (Entry a) -> -- ^ In-scope values
  (ByteString, Entry a)
moduleEntryMap _ (Just i) vals =
  ( i ^. getting identValue.to fromString
  , GlobalEntry vals
  )
moduleEntryMap (ModuleNameOne _ i) Nothing vals =
  ( i ^. getting identValue.to fromString
  , GlobalEntry vals
  )
moduleEntryMap (ModuleNameMany _ i _ rest) Nothing vals =
  let
    (n, e) = moduleEntryMap rest Nothing vals
  in
    ( i ^. getting identValue.to fromString
    , GlobalEntry $ Map.singleton n e
    )

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
  (ByteString, Entry a)
moduleEntry a b = moduleEntryMap a b . getGlobals

builtins :: Map ByteString (Entry a)
builtins = Map.fromList $ (, GlobalEntry mempty) <$> names
  where
    names :: [ByteString]
    names =
      [ "abs"
      , "dict"
      , "help"
      , "min"
      , "setattr"
      , "all"
      , "dir"
      , "hex"
      , "next"
      , "slice"
      , "any"
      , "divmod"
      , "id"
      , "object"
      , "sorted"
      , "ascii"
      , "enumerate"
      , "input"
      , "oct"
      , "staticmethod"
      , "bin"
      , "eval"
      , "int"
      , "open"
      , "str"
      , "bool"
      , "exec"
      , "isinstance"
      , "ord"
      , "sum"
      , "bytearray"
      , "filter"
      , "issubclass"
      , "pow"
      , "super"
      , "bytes"
      , "float"
      , "iter"
      , "print"
      , "tuple"
      , "callable"
      , "format"
      , "len"
      , "property"
      , "type"
      , "chr"
      , "frozenset"
      , "list"
      , "range"
      , "vars"
      , "classmethod"
      , "getattr"
      , "locals"
      , "repr"
      , "zip"
      , "compile"
      , "globals"
      , "map"
      , "reversed"
      , "__import__"
      , "complex"
      , "hasattr"
      , "max"
      , "round"
      , "delattr"
      , "hash"
      , "memoryview"
      , "set"
      -- Builtin exceptions
      , "ArithmeticError"
      , "AssertionError"
      , "AttributeError"
      , "BaseException"
      , "BlockingIOError"
      , "BrokenPipeError"
      , "BufferError"
      , "BytesWarning"
      , "ChildProcessError"
      , "ConnectionAbortedError"
      , "ConnectionError"
      , "ConnectionRefusedError"
      , "ConnectionResetError"
      , "DeprecationWarning"
      , "EOFError"
      , "Ellipsis"
      , "EnvironmentError"
      , "Exception"
      , "False"
      , "FileExistsError"
      , "FileNotFoundError"
      , "FloatingPointError"
      , "FutureWarning"
      , "GeneratorExit"
      , "IOError"
      , "ImportError"
      , "ImportWarning"
      , "IndentationError"
      , "IndexError"
      , "InterruptedError"
      , "IsADirectoryError"
      , "KeyError"
      , "KeyboardInterrupt"
      , "LookupError"
      , "MemoryError"
      , "NameError"
      , "None"
      , "NotADirectoryError"
      , "NotImplemented"
      , "NotImplementedError"
      , "OSError"
      , "OverflowError"
      , "PendingDeprecationWarning"
      , "PermissionError"
      , "ProcessLookupError"
      , "RecursionError"
      , "ReferenceError"
      , "ResourceWarning"
      , "RuntimeError"
      , "RuntimeWarning"
      , "StopAsyncIteration"
      , "StopIteration"
      , "SyntaxError"
      , "SyntaxWarning"
      , "SystemError"
      , "SystemExit"
      , "TabError"
      , "TimeoutError"
      , "True"
      , "TypeError"
      , "UnboundLocalError"
      , "UnicodeDecodeError"
      , "UnicodeEncodeError"
      , "UnicodeError"
      , "UnicodeTranslateError"
      , "UnicodeWarning"
      , "UserWarning"
      , "ValueError"
      , "Warning"
      , "ZeroDivisionError"
      ]

globals :: Map ByteString (Entry a)
globals =
  Map.fromList $
  [ ("__builtins__", GlobalEntry builtins) ] <>

  fmap (, GlobalEntry mempty)
  [ "__name__"
  , "__spec__"
  , "__doc__"
  , "__package__"
  , "__loader__"
  ]

data Env
  = Env
  { _envPath :: Seq Level
  , _envInClass :: Bool
  } deriving (Eq, Show)
makeLenses ''Env

type ValidateScope ann e
  = ValidateM
      (NonEmpty e)
      (ReaderT Env (State (Map ByteString (Entry ann))))

runValidateScope ::
  Map ByteString (Entry ann) -> -- ^ Extensions to the context
  ValidateScope ann e a ->
  Validation (NonEmpty e) a
runValidateScope exts =
  runValidateScope' [Toplevel] False (exts <> globals <> builtins)

runValidateScope' ::
  Seq Level -> -- ^ Path
  Bool -> -- ^ Are we checking a class body?
  Map ByteString (Entry ann) -> -- ^ Context
  ValidateScope ann e a -> -- ^ Validation action
  Validation (NonEmpty e) a
runValidateScope' path cls s =
  flip evalState s .
  flip runReaderT (Env path cls) .
  runValidateM

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
definitionScope = liftVM1 (local (envPath %~ (|> Definition))) . localScope id

-- | Run a validation action for a flow control block
controlScope :: ValidateScope ann e a -> ValidateScope ann e a
controlScope = liftVM1 (local (envPath %~ (|> Control)))

-- | Add some entries to the context, using the current depth and level
extendScope :: EntryType -> [Ident v ann] -> ValidateScope ann e ()
extendScope ety entries =
  liftVM0 (view envPath) `bindVM` \path ->
  liftVM0 . modify $ \scope ->
    foldr
      (\ident ->
         Map.insert
           (fromString $ _identValue ident)
           (Entry (ident ^. annot_) ety path mempty))
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
  validateExprScope_ e <*>
  pure (over (mapped._2) coerce f)

validateSuiteScope
  :: AsScopeError e a
  => Suite v a
  -> ValidateScope a e (Suite (Nub (Scope ': v)) a)
validateSuiteScope (SuiteMany ann a b c d) =
  SuiteMany ann a b c <$> validateBlockScope d
validateSuiteScope (SuiteOne ann a b) =
  SuiteOne ann a <$> validateSmallStatementScope b

validateDecoratorScope
  :: AsScopeError e a
  => Decorator v a
  -> ValidateScope a e (Decorator (Nub (Scope ': v)) a)
validateDecoratorScope (Decorator a b c d e f g) =
  (\d' -> Decorator a b c d' e f g) <$>
  validateExprScope_ d

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
          if Seq.length (entryPath e1) < Seq.length (entryPath e2)
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

isStaticmethod :: Fundef v a -> Bool
isStaticmethod =
  anyOf
    (fdDecorators.folded.decoratorExpr.getting (_Ident.identValue))
    (== "staticmethod")

isClassmethod :: Fundef v a -> Bool
isClassmethod =
  anyOf
    (fdDecorators.folded.decoratorExpr.getting (_Ident.identValue))
    (== "classmethod")

validateCompoundStatementScope
  :: forall e v a
   . AsScopeError e a
  => CompoundStatement v a
  -> ValidateScope a e (CompoundStatement (Nub (Scope ': v)) a)
validateCompoundStatementScope (Fundef a decos idnts asyncWs ws1 name ws2 params ws3 mty s) =
  liftVM0 (view envInClass) `bindVM` \inClass ->
  liftVM1 (local $ envInClass .~ False) $
  (\decos' -> Fundef a decos' idnts asyncWs ws1 (coerce name) ws2) <$>
  traverse validateDecoratorScope decos <*>
  traverse validateParamScope params <*>
  pure ws3 <*>
  traverseOf (traverse._2) validateExprScope_ mty <*>
  definitionScope
    (unless inClass (extendScope FunctionEntry [name]) *>
     extendScope ParamEntry (toListOf (folded.getting paramName) params) *>
     validateSuiteScope s) <*
  extendScope FunctionEntry [name]
validateCompoundStatementScope (If idnts a ws1 e b elifs melse) =
  liftVM1 (local $ envInClass .~ False) $
  (\e' (b', elifs', melse') -> If idnts a ws1 e' b' elifs' melse') <$>
  validateExprScope_ e <*>
  parallel3
    (controlScope $ validateSuiteScope b)
    (parallelList
     (\(a, b, c, d) ->
       (,,,) a b <$>
       validateExprScope_ c <*>
       controlScope (validateSuiteScope d))
     elifs)
    (traverseOf (traverse._3) (controlScope . validateSuiteScope) melse)
validateCompoundStatementScope (While idnts a ws1 e b els) =
  liftVM1 (local $ envInClass .~ False) $
  While idnts a ws1 <$>
  validateExprScope_ e <*>
  controlScope (validateSuiteScope b) <*>
  traverseOf (traverse._3) (controlScope . validateSuiteScope) els
validateCompoundStatementScope (TryExcept idnts a b e f k l) =
  liftVM1 (local $ envInClass .~ False) $
  (\(e', f', k', l') -> TryExcept idnts a b e' f' k' l') <$>
  parallel4
    (controlScope $ validateSuiteScope e)
    (parallelNonEmpty
       (\(idnts, ws, g, h) ->
         (,,,) idnts ws <$>
         traverse validateExceptAsScope g <*>
         controlScope
         (extendScope VarEntry (toListOf (folded.exceptAsName._Just._2) g) *>
          validateSuiteScope h))
       f)
    (traverseOf (traverse._3) (controlScope . validateSuiteScope) k)
    (traverseOf (traverse._3) (controlScope . validateSuiteScope) l)
validateCompoundStatementScope (TryFinally idnts a b e idnts2 f i) =
  liftVM1 (local $ envInClass .~ False) $
  (\(e', i') -> TryFinally idnts a b e' idnts2 f i') <$>
  parallel2
    (controlScope $ validateSuiteScope e)
    (controlScope $ validateSuiteScope i)
validateCompoundStatementScope (For idnts a asyncWs b c d e h i) =
  liftVM1 (local $ envInClass .~ False) $
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
    traverse validateExprScope_ e <*>
    parallel2
      (controlScope $
       extendScope VarEntry (toList cs) *>
       validateSuiteScope h)
      (traverseOf (traverse._3) (controlScope . validateSuiteScope) i)
validateCompoundStatementScope (ClassDef a decos idnts b c d g) =
  liftVM1 (local $ envInClass .~ False) $
  (\decos' -> ClassDef a decos' idnts b (coerce c)) <$>
  traverse validateDecoratorScope decos <*>
  traverseOf (traverse._2.traverse.traverse) validateArgScope d <*
  extendScope ClassEntry [c] <*
  liftVM0
    (view envPath >>= \path ->
     modify (addClassVars path) *> modify (addClassDefs path)) <*>
  definitionScope (liftVM1 (local $ envInClass .~ True) $ validateSuiteScope g)
  where
    addClassVars path st =
       foldrOf
         (getting (_Statements._SmallStatement)._2.
          getting (_SimpleStatements._Assign).
          to unfoldAssign._1.folded.
          getting assignTargets)
         (\a ->
            updateEntry
              [c ^. getting identValue . to fromString]
              (over subEntries $
               Map.insert
                 (a ^. getting identValue . to fromString)
                 (Entry (a ^. annot_) VarEntry (path |> Definition) mempty)))
         st
         g
    addClassDefs path st =
       foldrOf
         (getting (_Statements._Fundef).
          filtered ((||) <$> isStaticmethod <*> isClassmethod).fdName)
         (\a ->
            updateEntry
              [c ^. getting identValue . to fromString]
              (over subEntries $
               Map.insert
                 (a ^. getting identValue . to fromString)
                 (Entry (a ^. annot_) VarEntry (path |> Definition) mempty)))
         st
         g
validateCompoundStatementScope (With a b asyncWs c d e) =
  liftVM1 (local $ envInClass .~ False) $
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
         validateExprScope_ b <*>
         traverseOf (traverse._2) validateAssignExprScope c)
      d <*
    extendScope VarEntry names <*>
    controlScope (validateSuiteScope e)

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
  Assign a <$>
  validateAssignExprScope l <*>
  ((\a b -> case a of; [] -> b :| []; a : as -> a :| snoc as b) <$>
   traverseOf (traverse._2) validateAssignExprScope (NonEmpty.init rs) <*>
   (\(ws, b) -> (,) ws <$> validateExprScope_ b) (NonEmpty.last rs))
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
  lookupScope (_identValue i) `bindVM` \res ->
  liftVM0 (view envPath) `bindVM` \curPath ->
    case res of
      Nothing -> errorVM1 (_NotInScope # (i ^. unvalidated))
      Just (Entry ann _ path _) ->
        coerce i <$
        if Seq.length curPath < Seq.length path
        then errorVM1 (_FoundDynamic # (ann, i ^. unvalidated))
        else pure ()
      Just (GlobalEntry _) -> pure $ coerce i

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
      validateExprScope_ e <*
      extendScope VarEntry (c ^.. unvalidated.assignTargets)

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
  validateExprScope e `bindVM` \(e', mattrs) ->
  Deref a e' ws1 (coerce r) <$
  case mattrs of
    Nothing -> pure ()
    Just (ety, attrsPath, attrs) ->
      let ix = r ^. getting identValue . to fromString in
      case Map.lookup ix attrs of
        Just{} -> pure ()
        Nothing ->
          liftVM0 (view envPath) `bindVM` \path ->
          liftVM0 get `bindVM` \scope ->
          if
            toplevelPath path &&
            maybe
              False
              ((||) <$> isFunctionEntry <*> isClassEntry)
              (lookupEntry attrsPath scope)
          then
            liftVM0 . modify $
            setEntry
              (attrsPath |> ix)
              (Entry (r ^. annot_) VarEntry path mempty)
          else
            case ety of
              -- when it comes to variables/parameters, all bets are off
              -- without a type system.
              --
              -- just let it through :(
              Just ParamEntry -> pure ()
              Just VarEntry -> pure ()
              _ -> errorVM1 (_MissingAttribute # (e ^. unvalidated, r ^. unvalidated))
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
  liftVM0 (view envPath) `bindVM` \curPath ->
  Ident a (MkIdent b c d) <$
  case res of
    Nothing -> extendScope VarEntry [MkIdent b c d]
    Just (Entry _ _ path _)->
      if Seq.length curPath < Seq.length path
      then extendScope VarEntry [MkIdent b c d]
      else pure ()
    Just GlobalEntry{} -> pure ()
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

unrefined ::
  Applicative f =>
  f a ->
  f (a, Maybe (Maybe EntryType, Seq ByteString, Map ByteString (Entry x)))
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
    ( Expr (Nub (Scope ': v)) a
    , Maybe (Maybe EntryType, Seq ByteString, Map ByteString (Entry a))
    )
validateExprScope (Lambda ann ws params col body) =
  unrefined $
  Lambda ann ws <$>
  traverse validateParamScope params <*>
  pure col <*>
  definitionScope
    (extendScope ParamEntry (toListOf (folded.getting paramName) params) *>
     validateExprScope_ body)
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
      Just (ety, path, scope) ->
        let ix = fromString i in
        case Map.lookup ix scope of
          Nothing ->
            case ety of
              Just ParamEntry -> pure $ Just (ety, path |> ix, mempty)
              Just VarEntry -> pure $ Just (ety, path |> ix, mempty)
              _ ->
                errorVM1 $ _MissingAttribute # (e ^. unvalidated, MkIdent ann i ws)
          Just entry ->
            pure $
            Just
            ( case entry of
                GlobalEntry{} -> Nothing
                Entry _ ety' _ _ -> Just ety'
            , path |> ix
            , _subEntries entry
            )
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
       (\entry ->
          ( case entry of; GlobalEntry{} -> Nothing; Entry _ ety _ _ -> Just ety
          , [i ^. getting identValue . to fromString]
          , _subEntries entry
          )))
    (lookupScope $ i ^. getting identValue)
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
