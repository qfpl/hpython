{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

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
  , LoadedModule(..)
  , findAndLoadAll
  , findAndLoad
  , findModule
  , loadModule
  )
where

import Control.Lens.Fold ((^..), folded)
import Control.Lens.Getter ((^.), getting, to)
import Control.Lens.Review ((#), un, review)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State  (StateT, evalStateT, get, modify)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Functor.Classes (liftEq, liftCompare)
import Data.IORef (IORef, readIORef, newIORef)
import Data.Map (Map)
import Data.Validation (validation, bindValidation)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>), takeDirectory)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

import Data.Type.Set (Member)
import Language.Python.Import.Error
import Language.Python.Optics (_Statements, _SimpleStatements, _Import)
import Language.Python.Optics.Validated (unvalidated)
import Language.Python.Parse (SrcInfo, readModule)
import Language.Python.Syntax.Ident (identValue)
import Language.Python.Syntax.Import (_importAsName, _importAsQual)
import Language.Python.Syntax.Module (Module)
import Language.Python.Syntax.ModuleNames (ModuleName, unfoldModuleName)
import Language.Python.Syntax.Statement (Statement(..))
import Language.Python.Syntax.Types (importNames)
import Language.Python.Validate
  ( Indentation, Syntax, Scope
  , runValidateIndentation, runValidateSyntax, runValidateScope
  , validateModuleIndentation, validateModuleSyntax, validateModuleScope
  )
import Language.Python.Validate.Scope (moduleEntry)

data SearchConfig
  = SearchConfig
  { _scPythonPath :: FilePath
  , _scSearchPaths :: [FilePath]
  }

mkSearchConfig ::
  FilePath -> -- ^ Python executable path, e.g. @/usr/bin/python3.5@
  FilePath -> -- ^ Current directory
  SearchConfig
mkSearchConfig pp curdir =
  SearchConfig
  { _scPythonPath = pp
  , _scSearchPaths = curdir : fmap (takeDirectory pp </>) paths
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
    search [] = pure . Left $ _ImportNotFound.un unvalidated # mn
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

-- |
-- If the module is in the cache, return it immediately. Otherwise, load it from disk
-- and validate it
loadModule ::
  AsImportError e SrcInfo =>
  ModuleInfo SrcInfo ->
  Importer (Either e (Module '[Syntax, Indentation] SrcInfo))
loadModule mi =
  runExceptT $ do
    ImportCache cache <- lift $ Importer get
    let res = Map.lookup (CacheKey $ _miName mi) cache
    case res of
      Just ref -> do
        value <- liftIO $ readIORef ref
        pure $ _cvData value
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

        pure mod'

findAndLoad ::
  forall e v.
  ( Member Syntax v
  , AsImportError e SrcInfo
  ) =>
  SearchConfig ->
  ModuleName v SrcInfo ->
  Importer (Either e (Module '[Syntax, Indentation] SrcInfo))
findAndLoad sc mn =
  runExceptT $ do
    minfo <- ExceptT $ findModule sc mn
    ExceptT $ loadModule minfo

data LoadedModule v a
  = LoadedModule
  { _lmInfo :: ModuleInfo a
  , _lmTarget :: Module v a
  , _lmDependencies :: [(ModuleInfo a, Module v a)]
  } deriving Show

-- |
-- Find and load a module and its immediate dependencies
findAndLoadAll ::
  forall e v.
  (Member Syntax v, AsImportError e SrcInfo) =>
  SearchConfig ->
  ModuleName v SrcInfo ->
  Importer (Either e (LoadedModule '[Scope, Syntax, Indentation] SrcInfo))
findAndLoadAll sc mn =
  runExceptT $ do
    minfo <- ExceptT $ findModule sc mn
    mod <- ExceptT $ loadModule minfo

    let
      tlImports =
        mod ^..
        getting _Statements .
        to (\case
               SmallStatement _ s -> s ^.. getting _SimpleStatements
               _ -> []) .
        folded .
        getting _Import .
        importNames .
        folded

    deps <-
      traverse
        (\ias -> do
           let n = _importAsName ias
           res <- ExceptT $ findAndLoadAll sc (_importAsName ias)
           pure
             -- it's fine to skip skip scope checking for these two
             ( unsafeCoerce n
             , unsafeCoerce . snd <$> _importAsQual ias
             , _lmInfo res
             , _lmTarget res
             ))
        tlImports

    let
      scope =
        foldr
          (\(a, b, _, c) -> uncurry Map.insert $ moduleEntry a b c)
          Map.empty
          deps

    mod' <-
      ExceptT . pure $
      validation
        (Left . review _ImportValidationErrors)
        Right
        (runValidateScope scope $ validateModuleScope mod)

    pure $ LoadedModule minfo mod' ((\(_, _, c, d) -> (c, d)) <$> deps)
