{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-|
Module      : Language.Python.Import
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

`import`ing machinery
-}

module Language.Python.Import
  ( module Language.Python.Import.Error
    -- * Configuration
  , SearchConfig(..), mkSearchConfig
    -- * Finding and Loading
  , Importer, runImporter
  , ModuleInfo(..)
  , LoadResult(..)
  , findAndLoadAll
  , findModule
  , loadModule
  , relativeImport
  )
where

import Control.Lens.Fold ((^..))
import Control.Lens.Getter ((^.), getting, to, view)
import Control.Lens.Prism (_Left, _Right)
import Control.Lens.Review ((#), un, review, re)
import Control.Lens.Traversal (failing)
import Control.Lens.Tuple (_2)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State  (StateT, evalStateT, get, modify)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Functor.Classes (liftEq, liftCompare)
import Data.IORef (IORef, readIORef, newIORef)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.String (fromString)
import Data.Validation (validation, bindValidation)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

import Language.Python.Import.Error

import Data.Type.Set (Member)
import Language.Python.Import.Builtins (lookupBuiltinModule)
import Language.Python.Optics
  ( _Statements, _SmallStatement, _SimpleStatements
  , _Import, _FromImport
  )
import Language.Python.Optics.Validated (unvalidated, demoted)
import Language.Python.Parse (SrcInfo, readModule)
import Language.Python.Syntax.Ann (annot, annot_)
import Language.Python.Syntax.Ident (Ident(..), identValue)
import Language.Python.Syntax.Import
  (ImportTargets(..), ImportAs(..), _importAsName, _importAsQual)
import Language.Python.Syntax.Module (Module)
import Language.Python.Syntax.ModuleNames
  ( ModuleName, RelativeModuleName(..)
  , moduleNameInit, moduleNameAppend, moduleNameSnoc
  , makeModuleName, unfoldModuleName
  )
import Language.Python.Syntax.Punctuation (Dot(..))
import Language.Python.Syntax.Types (Import(..), FromImport(..))
import Language.Python.Validate
  ( Indentation, Syntax, Scope
  , runValidateIndentation, runValidateSyntax, runValidateScope
  , validateModuleIndentation, validateModuleSyntax, validateModuleScope
  )
import Language.Python.Validate.Scope
  ( GlobalEntry(..), moduleEntry, moduleEntryMap
  , getGlobals, toGlobalEntry
  )

data SearchConfig
  = SearchConfig
  { _scSearchPaths :: [FilePath]
  } deriving (Eq, Show)

mkSearchConfig ::
  FilePath -> -- ^ Python executable path, e.g. @/usr/bin/python3.5@
  FilePath -> -- ^ Current directory
  SearchConfig
mkSearchConfig pp curdir =
  SearchConfig
  { _scSearchPaths = curdir : fmap (pp </>) paths
  }
  where
    paths =
      [ "lib" </> "python3.5"
      , "lib" </> "python3.5" </> "lib-dynload"
      , "lib" </> "python3.5" </> "site-packages"
      ]

data ModuleInfo a
  = FileModuleInfo
  { _miName :: ModuleName '[] a
  , _miFile :: FilePath
  }
  | PackageModuleInfo
  { _miName :: ModuleName '[] a
  , _miFile :: FilePath
  }
  | BuiltinModuleInfo
  { _miName :: ModuleName '[] a
  } deriving Show

newtype CacheKey = CacheKey (ModuleName '[] SrcInfo)
instance Eq CacheKey where
  CacheKey a == CacheKey b = liftEq (\_ _ -> True) a b
instance Ord CacheKey where
  CacheKey a `compare` CacheKey b = liftCompare (\_ _ -> EQ) a b

data CacheValue
  = CacheValue
  { _cvInfo :: ModuleInfo SrcInfo
  , _cvData :: Module '[Syntax, Indentation] SrcInfo
  }

newtype ImportCache
  = ImportCache
  { unImportCache :: Map CacheKey (IORef CacheValue)
  }

-- |
-- The module importer provides a cache to avoid constantly re-loading
-- and re-checkign files
newtype Importer a
  = Importer (StateT ImportCache IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runImporter :: Importer a -> IO a
runImporter (Importer m) = evalStateT m $ ImportCache mempty

-- |
-- Find a module by first looking in the cache, and then searching the paths
-- specified by 'SearchConfig'
findModule ::
  forall e v.
  ( Member Syntax v
  , AsImportError e SrcInfo
  ) =>
  SearchConfig ->
  ModuleName v SrcInfo ->
  Importer (Either e (ModuleInfo SrcInfo))
findModule sc mn = checkCacheThen (search $ _scSearchPaths sc)
  where
    checkCacheThen :: Importer (Either e (ModuleInfo SrcInfo)) -> Importer (Either e (ModuleInfo SrcInfo))
    checkCacheThen next = do
      ImportCache cache <- Importer get
      let res = Map.lookup (CacheKey $ mn ^. unvalidated) cache
      case res of
        Nothing -> next
        Just ref -> do
          value <- liftIO $ readIORef ref
          pure . Right $ _cvInfo value

    search :: [FilePath] -> Importer (Either e (ModuleInfo SrcInfo))
    search []
      | Just{} <- lookupBuiltinModule mn =
          pure . Right $ BuiltinModuleInfo (mn ^. unvalidated)
      | otherwise = pure . Left $ _ImportNotFound.un unvalidated # mn
    search (path : rest) = do
      let
        pkg = path </> moduleFileName </> "__init__" <.> "py"
        file = path </> moduleFileName <.> "py"
      b1 <- liftIO $ doesFileExist pkg
      if b1
        then pure $ Right PackageModuleInfo{ _miName = mn ^. unvalidated, _miFile = pkg }
        else do
          b2 <- liftIO $ doesFileExist file
          if b2
            then pure $ Right FileModuleInfo{ _miName = mn ^. unvalidated, _miFile = file }
            else search rest

    moduleDirs :: ([String], String)
    moduleDirs =
      bimap
        (fmap (^. getting identValue))
        (^. getting identValue)
        (unfoldModuleName mn)

    moduleFileName :: FilePath
    moduleFileName = foldr (</>) (snd moduleDirs) (fst moduleDirs)

data LoadResult v
  = LoadedBuiltin (Map ByteString (GlobalEntry SrcInfo))
  | LoadedModule (Module v SrcInfo)

-- |
-- If the module is in the cache, return it immediately. Otherwise, load it from disk
-- and validate it
loadModule ::
  AsImportError e SrcInfo =>
  ModuleInfo SrcInfo ->
  Importer (Either e (LoadResult '[Syntax, Indentation]))
loadModule (BuiltinModuleInfo mn)
  | Just mod <- lookupBuiltinModule mn = pure . Right $ LoadedBuiltin mod
  | otherwise = pure $ Left $ _ImportNotFound # mn
loadModule mi =
  runExceptT $ do
    ImportCache cache <- lift $ Importer get
    let res = Map.lookup (CacheKey $ _miName mi) cache
    case res of
      Just ref -> do
        value <- liftIO $ readIORef ref
        pure $ LoadedModule $ _cvData value
      Nothing -> do
        mod <-
          ExceptT $
          validation (Left . review _ImportParseErrors) Right <$>
          liftIO (readModule $ _miFile mi)

        mod' <-
          ExceptT . pure .
          validation (Left . review _ImportValidationErrors) Right $
          bindValidation
            (runValidateIndentation (validateModuleIndentation mod))
            (runValidateSyntax . validateModuleSyntax)

        ref <- liftIO . newIORef $ CacheValue mi mod'

        lift . Importer . modify $
          ImportCache .
          Map.insert (CacheKey $ _miName mi) ref .
          unImportCache

        pure $ LoadedModule mod'

relativeImport ::
  forall e.
  (AsImportError e SrcInfo) =>
  SearchConfig -> -- ^ Search config
  ModuleName '[Syntax, Indentation] SrcInfo -> -- ^ Current module name
  RelativeModuleName '[Syntax, Indentation] SrcInfo -> -- ^ Target module (relative)
  ImportTargets '[Syntax, Indentation] SrcInfo -> -- ^ Import targets
  -- Importer (Either e (Map ByteString (GlobalEntry SrcInfo)))
  Importer (Either e (Map ByteString (GlobalEntry SrcInfo)))
relativeImport sc mod targetMod targets =
  runExceptT $ do
    absTarget <- absoluteTargetMod mod targetMod
    case targets of
      -- from ? import *
      --
      -- Brings all names in ? into scope, along with the list of names
      -- specified in the __all__ variable (if it is a package module). The
      -- names from the __all__ variable brought in as submodules, so they
      -- need to be loaded
      ImportAll{} -> do
        (_, targetMod) <- ExceptT $ findAndLoadAll sc absTarget
        case targetMod of
          LoadedBuiltin mapping -> pure mapping
          LoadedModule mod -> pure $ toGlobalEntry <$> getGlobals mod
      ImportSome _ is -> exposeTargets absTarget is
      ImportSomeParens _ _ is _ -> exposeTargets absTarget is
  where
    -- Grab the prefix of a module e.g.   a.b.c ~> a.b
    --
    -- If the module has no prefix (it's a top-level module), then an error is thrown
    modulePrefix ::
      ModuleName v SrcInfo ->
      ExceptT e Importer (ModuleName v SrcInfo, Dot)
    modulePrefix mn =
      let
        (mprefix, _) = moduleNameInit mn
      in
        case mprefix of
          -- The current module is a top-level module (it has no prefix), so a
          -- relative import doesn't make sense
          Nothing ->
            throwError $
            _ImportRelativeFromToplevel # (mn ^. unvalidated, targetMod ^. annot_)
          Just (prefix, dot) -> pure (prefix, dot)

    -- Calculate the absolute name of the target module, given the current
    -- module and a relative name
    --
    -- For each leading dot in the relative name, we take the prefix of the current
    -- module. If there is only one leading dot, we join the prefix with relative
    -- target name (if it exists).
    --
    -- If there is more than one leading dot, we repeat this process
    -- recursively with one less dot, setting the new current module to the prefix
    -- of the old one.
    --
    -- absoluteTargetMod(a.b.c, .d)  ~>  a.b.d
    -- absoluteTargetMod(a.b.c, .)  ~>  a.b
    -- absoluteTargetMod(a.b.c, ..)  ~>  a
    -- absoluteTargetMod(a.b.c, ...)  ~>  error
    -- absoluteTargetMod(a.b.c, ..d)  ~>  a.d
    -- absoluteTargetMod(a.b.c, ..d.e)  ~>  a.d.e
    absoluteTargetMod ::
      ModuleName v SrcInfo ->
      RelativeModuleName v SrcInfo ->
      ExceptT e Importer (ModuleName v SrcInfo)
    absoluteTargetMod _ (RelativeWithName _ [] mn) = pure mn
    absoluteTargetMod mn (RelativeWithName ann (_:ds) mn') = do
      (prefix, dot) <- modulePrefix mn
      case ds of
        -- there are no more 'parent dots' to follow
        [] -> pure $ moduleNameAppend prefix (dot, mn')
        -- there are more 'parent dots', so we keep jumping up the module name
        _ : _ ->
          absoluteTargetMod prefix (RelativeWithName ann ds mn')
    absoluteTargetMod mn (Relative ann (_ :| ds)) = do
      (prefix, _) <- modulePrefix mn
      case ds of
        [] -> pure prefix
        dd : dds ->
          absoluteTargetMod prefix (Relative ann (dd :| dds))

    -- Given a target module and a sequence of import targets,
    -- generate a map where all the targets have been loaded and bound
    --
    -- If the module is a package module, then these names are sourced from its
    -- scope first, and then from its submodules if those names aren't bound.
    --
    -- The target might have a renaming, in which case it is loaded from the
    -- proper location, then provided in the map under the new name
    exposeTargets ::
      Foldable f =>
      ModuleName '[Syntax, Indentation] SrcInfo ->
      f (ImportAs Ident '[Syntax, Indentation] SrcInfo) ->
      ExceptT e Importer (Map ByteString (GlobalEntry SrcInfo))
    exposeTargets mn tgts = do
      found <- lift $ findModule @e sc mn
      scope <-
        -- if the target module was found, then we can try to load its contents
        -- into scope
        --
        -- otherwsise, we have to let it fall through to searching for submodules
        case found of
          Left{} -> pure mempty
          Right{} -> do
            tgt <- snd <$> ExceptT (findAndLoadAll sc mn)
            case tgt of
              LoadedBuiltin mapping -> pure mapping
              LoadedModule mod -> pure $ toGlobalEntry <$> getGlobals mod
      foldr
        (\(ImportAs _ name qual) mb ->
           let
             namebs = name ^. getting identValue.to fromString
             entryname =
               maybe namebs (fromString . view (getting identValue) . snd) qual
           in
             case Map.lookup namebs scope of
               -- the identifier was found in the global scope
               Just val -> Map.insert entryname val <$> mb
               -- the identifier wasn't found in the global scope, but we're reading
               -- a package module so we should now try to load submodules
               Nothing -> do
                 let newTarget = moduleNameSnoc mn (MkDot [], name ^. annot, name)
                 submod <- snd <$> ExceptT (findAndLoadAll sc newTarget)
                 case submod of
                   LoadedBuiltin mapping ->
                    uncurry
                      Map.insert
                      (moduleEntryMap
                         (makeModuleName name [])
                         (snd <$> qual)
                         mapping) <$>
                      mb
                   LoadedModule mod ->
                    uncurry
                      Map.insert
                      (moduleEntry
                         (makeModuleName name [])
                         (snd <$> qual)
                         (mod ^. demoted @Scope)) <$>
                      mb)
        (pure mempty)
        tgts

toplevelImports ::
  Module v SrcInfo ->
  [Either (Import v SrcInfo) (FromImport v SrcInfo)]
toplevelImports mod =
  mod ^..
  getting _Statements.
  getting _SmallStatement._2.
  getting _SimpleStatements.
  failing (getting _Import.re _Left) (getting _FromImport.re _Right)

-- |
-- Find and load a module and its immediate dependencies
findAndLoadAll ::
  forall e.
  (AsImportError e SrcInfo) =>
  SearchConfig ->
  ModuleName '[Syntax, Indentation] SrcInfo ->
  Importer
    (Either e (ModuleInfo SrcInfo, (LoadResult '[Scope, Syntax, Indentation])))
findAndLoadAll sc mn =
  runExceptT $ do
    minfo <- ExceptT $ findModule sc mn
    loadRes <- ExceptT $ loadModule minfo

    case loadRes of
      LoadedBuiltin scope -> pure (minfo, LoadedBuiltin scope)
      LoadedModule mod -> do

        let tlImports = toplevelImports mod

        deps <-
          traverse
            (\case
              Left (MkImport _ _ is) ->
                Left <$>
                foldr
                  (\ias b -> do
                      let n = _importAsName ias
                      (_, tgt) <- ExceptT $ findAndLoadAll sc (_importAsName ias)
                      -- it's fine to skip skip scope checking for these two
                      (( unsafeCoerce n -- module name
                       , unsafeCoerce . snd <$> _importAsQual ias -- optional renaming
                       , tgt
                       ) :) <$>
                        b)
                  (pure [])
                  is
              Right (MkFromImport _ _ rmn _ its) -> do
                mapping <- ExceptT $ relativeImport sc mn rmn its
                pure $ Right mapping)
            tlImports

        let
          scope =
            foldr
              (\a b ->
                b <> -- latter entries take precedence, so we merge the rest first
                either
                  (foldr
                     (\(a, b, c) ->
                        case c of
                          LoadedBuiltin d -> uncurry Map.insert $ moduleEntryMap a b d
                          LoadedModule d -> uncurry Map.insert $ moduleEntry a b d)
                     Map.empty)
                  id
                  a)
              Map.empty
              deps

        mod' <-
          ExceptT . pure $
          validation
            (Left . review _ImportValidationErrors)
            Right
            (runValidateScope scope $ validateModuleScope mod)

        pure (minfo, LoadedModule mod')
