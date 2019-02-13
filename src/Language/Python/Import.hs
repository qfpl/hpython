{-# language DataKinds #-}
{-# language FlexibleContexts #-}
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
  , ModuleInfo(..)
  , findModule
  , loadModule
  )
where

import Control.Lens.Getter ((^.), getting)
import Control.Lens.Review ((#), un, review)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (bimap)
import Data.Validation (validation, bindValidation)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>), takeDirectory)

import Data.Type.Set (Member)
import Language.Python.Import.Error
import Language.Python.Optics.Validated (unvalidated)
import Language.Python.Parse (SrcInfo, readModule)
import Language.Python.Syntax.Ident (identValue)
import Language.Python.Syntax.Module (Module)
import Language.Python.Syntax.ModuleNames (ModuleName, unfoldModuleName)
import Language.Python.Validate
  ( Indentation, Syntax
  , runValidateIndentation, runValidateSyntax
  , validateModuleIndentation, validateModuleSyntax
  )

{-

Find the location of python3.5 ?? Or have that as an argument
If the location is a symbol link, then the target is used

If $PYTHONHOME is set then it goes to prefix and exec_prefix
If $PYTHONHOME=X:Y then prefix=X, exec_prefix=Y

Afterward, we search upward. The directory that contains
`lib/python$VERSION/os.py` is the prefix, and the directory that
contains `lib/python$VERSION/lib-dynload` is the exec prefix

if user_site_directory is true, then don't add site-packages to sys.path

-}

data SearchConfig
  = SearchConfig
  { _scPythonPath :: FilePath
  , _scSearchPaths :: [FilePath]
  }

mkSearchConfig ::
  FilePath -> -- ^ Python executable path, e.g. @/usr/bin/python3.5@
  SearchConfig
mkSearchConfig pp =
  SearchConfig
  { _scPythonPath = pp
  , _scSearchPaths = fmap (takeDirectory pp </>) paths
  }
  where
    paths =
      [ ""
      , "lib" </> "python3.5"
      , "lib" </> "python3.5" </> "lib-dynload"
      , "lib" </> "python3.5" </> "site-packages"
      ]

data ModuleInfo
  = ModuleInfo
  { _miFile :: FilePath
  }

-- |
-- Find a module by looking in the paths specified by 'SearchConfig'
findModule ::
  forall v e a.
  ( Member Syntax v
  , AsImportError e a
  ) =>
  SearchConfig ->
  ModuleName v a ->
  IO (Either e ModuleInfo)
findModule sc mn = search (_scSearchPaths sc)
  where
    search :: [FilePath] -> IO (Either e ModuleInfo)
    search [] = pure . Left $ _ImportNotFound.un unvalidated # mn
    search (path : rest) = do
      let file = path </> moduleFileName
      b <- doesFileExist file
      if b
        then pure $ Right ModuleInfo{ _miFile = file }
        else search rest

    moduleDirs :: ([String], String)
    moduleDirs =
      bimap
        (fmap (^. getting identValue))
        (^. getting identValue)
        (unfoldModuleName mn)

    moduleFileName :: FilePath
    moduleFileName = foldr (</>) (snd moduleDirs <.> "py") (fst moduleDirs)

-- |
-- Load and validate a module
loadModule ::
  AsImportError e SrcInfo =>
  ModuleInfo ->
  IO (Either e (Module '[Syntax, Indentation] SrcInfo))
loadModule mi =
  runExceptT $ do
    mod <-
      ExceptT $
      validation (Left . review _ImportParseErrors) Right <$>
      readModule (_miFile mi)

    ExceptT . pure .
      validation (Left . review _ImportValidationErrors) Right $
      bindValidation
        (runValidateIndentation (validateModuleIndentation mod))
        (runValidateSyntax . validateModuleSyntax)