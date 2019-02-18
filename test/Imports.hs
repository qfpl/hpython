{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
module Imports (importsTests) where

import Hedgehog

import Control.Exception (bracket_)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Text (Text)
import System.Directory (removeFile, getCurrentDirectory)

import qualified Data.Text.IO as Text

import Language.Python.DSL
import Language.Python.Import
import Language.Python.Parse
import Language.Python.Render
import Language.Python.Syntax
import Language.Python.Validate

importsTests :: Group
importsTests = $$discover

withFiles :: [(FilePath, Text)] -> IO a -> IO a
withFiles files =
  bracket_
    (traverse_ (uncurry Text.writeFile) files)
    (traverse_ (removeFile . fst) files)

prop_imports_1 :: Property
prop_imports_1 =
  withTests 1 . property $ do
    let
      mname :: ModuleName '[Scope, Syntax, Indentation] SrcInfo
      mname = makeModuleName (MkIdent (Ann $ initialSrcInfo "<unknown>") "b" []) []
    dir <- liftIO getCurrentDirectory
    res <-
      liftIO . withFiles files . runImporter $
      findAndLoadAll @(ImportError SrcInfo) (mkSearchConfig "" dir) mname
    case res of
      Right{} -> success
      Left e -> do
        annotateShow e
        failure
  where
    files =
      [ ( "a.py"
        , showModule $
          module_
          [ line_ (var_ "a" .= 1)
          ]
        )
      , ( "b.py"
        , showModule $
          module_
          [ line_ $ import_ (pure "a")
          , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "a"]
          ]
        )
      ]

prop_imports_2 :: Property
prop_imports_2 =
  withTests 1 . property $ do
    let
      mname :: ModuleName '[Scope, Syntax, Indentation] SrcInfo
      mname = makeModuleName (MkIdent (Ann $ initialSrcInfo "<unknown>") "b" []) []
    dir <- liftIO getCurrentDirectory
    res <-
      liftIO . withFiles files . runImporter $
      findAndLoadAll @(ImportError SrcInfo) (mkSearchConfig "" dir) mname
    case res of
      Right{} -> failure
      Left{} -> success
  where
    files =
      [ ( "a.py"
        , showModule $
          module_
          [ line_ (var_ "b" .= 1)
          ]
        )
      , ( "b.py"
        , showModule $
          module_
          [ line_ $ import_ (pure "a")
          , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "a"]
          ]
        )
      ]

prop_imports_3 :: Property
prop_imports_3 =
  withTests 1 . property $ do
    let
      mname :: ModuleName '[Scope, Syntax, Indentation] SrcInfo
      mname = makeModuleName (MkIdent (Ann $ initialSrcInfo "<unknown>") "a" []) []
    dir <- liftIO getCurrentDirectory
    res <-
      liftIO . withFiles files . runImporter $
      findAndLoadAll @(ImportError SrcInfo) (mkSearchConfig "" dir) mname
    case res of
      Right{} -> failure
      Left{} -> success
  where
    files =
      [ ( "a.py"
        , showModule $
          module_
          [ line_ $ call_ (var_ "print") [p_ $ var_ "a"]
          ]
        )
      ]

prop_imports_4 :: Property
prop_imports_4 =
  withTests 1 . property $ do
    let
      mname :: ModuleName '[Scope, Syntax, Indentation] SrcInfo
      mname = makeModuleName (MkIdent (Ann $ initialSrcInfo "<unknown>") "b" []) []
    dir <- liftIO getCurrentDirectory
    res <-
      liftIO . withFiles files . runImporter $
      findAndLoadAll @(ImportError SrcInfo) (mkSearchConfig "" dir) mname
    case res of
      Right{} -> success
      Left e -> do
        annotateShow e
        failure
  where
    files =
      [ ( "a.py"
        , showModule $
          module_
          [ line_ (var_ "a" .= 1)
          ]
        )
      , ( "b.py"
        , showModule $
          module_
          [ line_ $ importAs_ (pure "a" `as_` "b")
          , line_ $ call_ (var_ "print") [p_ $ var_ "b" /> "a"]
          ]
        )
      ]

prop_imports_5 :: Property
prop_imports_5 =
  withTests 1 . property $ do
    let
      mname :: ModuleName '[Scope, Syntax, Indentation] SrcInfo
      mname = makeModuleName (MkIdent (Ann $ initialSrcInfo "<unknown>") "b" []) []
    dir <- liftIO getCurrentDirectory
    res <-
      liftIO . withFiles files . runImporter $
      findAndLoadAll @(ImportError SrcInfo) (mkSearchConfig "" dir) mname
    case res of
      Right{} -> failure
      Left{} -> success
  where
    files =
      [ ( "a.py"
        , showModule $
          module_
          [ line_ (var_ "a" .= 1)
          ]
        )
      , ( "b.py"
        , showModule $
          module_
          [ line_ $ importAs_ (pure "a" `as_` "b")
          , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "a"]
          ]
        )
      ]