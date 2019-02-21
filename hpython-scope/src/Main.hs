{-# language DataKinds #-}
module Main where

import Debug.Trace

import Control.Applicative (optional)
import Control.Monad (void, guard)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Foldable (for_)
import Options.Applicative
  ( Parser, strOption, short, long, value
  , info, progDesc, fullDesc
  , execParser
  )
import System.Directory
  ( listDirectory, doesDirectoryExist, findExecutable, getCurrentDirectory
  , getSymbolicLinkTarget, pathIsSymbolicLink, doesFileExist
  )
import System.FilePath ((</>), (<.>), dropExtension, takeDirectory, takeExtension)

import Language.Python.Parse (SrcInfo, initialSrcInfo)
import Language.Python.Syntax (Ann(..), ModuleName, Ident(..), makeModuleName)
import Language.Python.Import (ImportError, Importer, runImporter, mkSearchConfig, findAndLoadAll)
import Language.Python.Validate (Syntax, Indentation)

data Config
  = Config
  { _configPython :: Maybe FilePath
  , _configRoot :: FilePath
  }

dirParser :: Parser FilePath
dirParser =
  strOption $
  short 'd' <>
  long "dir" <>
  value "."

pythonParser :: Parser (Maybe FilePath)
pythonParser =
  optional .
  strOption $
  short 'p' <>
  long "path"

configParser :: Parser Config
configParser = Config <$> pythonParser <*> dirParser

findPythonPath :: IO (Maybe FilePath)
findPythonPath =
  runMaybeT $ do
  exe <- MaybeT $ findExecutable "python3.5"
  sym <- liftIO $ pathIsSymbolicLink exe
  start <-
    if sym
    then liftIO $ getSymbolicLinkTarget exe
    else pure exe
  go (takeDirectory start)
  where
    go dir = do
      foundLib <- liftIO $ doesFileExist $ dir </> "lib" </> "python3.5" </> "os" <.> "py"
      if foundLib
        then pure dir
        else do
          let dir' = takeDirectory dir
          guard (dir /= dir')
          go dir'

main :: IO ()
main = do
  config <-
    execParser . info configParser $
    progDesc "Syntax and scope checker for python projects" <>
    fullDesc
  mpath <- maybe findPythonPath (pure . Just) (_configPython config)
  pythonPath <- maybe (error "can't find python executable") pure mpath
  dir <- getCurrentDirectory
  res <- checkFrom (traceShowId pythonPath) dir (_configRoot config)
  case res of
    Right () -> putStrLn "No errors found."
    Left e -> print e

toModuleName :: [String] -> FilePath -> ModuleName '[Syntax, Indentation] SrcInfo
toModuleName prefix filename =
  case prefix of
    [] ->
      makeModuleName (mkIdent name) []
    p:ps ->
      makeModuleName (mkIdent p) (fmap (\i -> ([], mkIdent i)) $ ps <> [name])
  where
    name = dropExtension filename
    mkIdent n = MkIdent (Ann $ initialSrcInfo "<scopecheck>") n []

checkFrom ::
  FilePath -> -- ^ Path to @python@ executable
  FilePath -> -- ^ Current directory
  FilePath -> -- ^ Directory to begin checking
  IO (Either (ImportError SrcInfo) ())
checkFrom pypath curDir start =
  runImporter . runExceptT $ go (curDir </> start) [start]
  where
    go ::
      FilePath ->
      [String] ->
      ExceptT (ImportError SrcInfo) Importer ()
    go dir prefix = do
      files <- liftIO $ listDirectory dir
      for_ files $ \file ->
        if takeExtension file == ".py"
        then
          void . ExceptT $
          findAndLoadAll (traceShowId $ mkSearchConfig pypath curDir) (toModuleName prefix file)
        else do
          b <- liftIO $ doesDirectoryExist file
          if b
            then go (dir </> file) (prefix <> [file])
            else pure ()
