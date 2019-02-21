{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
module Imports (importsTests) where

import Hedgehog

import Control.Exception (bracket_)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import System.FilePath ((</>))
import System.Directory
  ( removeDirectoryRecursive
  , getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing
  )

import qualified Data.Text.IO as Text

import Language.Python.DSL
import Language.Python.Import
import Language.Python.Parse
import Language.Python.Render
import Language.Python.Syntax
import Language.Python.Validate

importsTests :: Group
importsTests = $$discover

withFiles :: [(FilePath, FilePath, Text)] -> IO a -> IO a
withFiles files m = do
  top <- getCurrentDirectory
  bracket_
    (do
        createDirectoryIfMissing False (top </> "sandbox")
        setCurrentDirectory (top </> "sandbox")
        traverse_
          (\(a, b, c) -> do
              createDirectoryIfMissing True a
              Text.writeFile (a </> b) c)
          files)
    (do
       setCurrentDirectory top
       removeDirectoryRecursive (top </> "sandbox"))
    m

importingSucceeds :: NonEmpty String -> [(FilePath, FilePath, Text)] -> Property
importingSucceeds (mn :| mns) files =
  withTests 1 . property $ do
  let
    mname :: ModuleName '[Syntax, Indentation] SrcInfo
    mname =
      makeModuleName
        (MkIdent (Ann $ initialSrcInfo "<unknown>") mn [])
        ((\x -> ([], MkIdent (Ann $ initialSrcInfo "<unknown>") x [])) <$> mns)
  res <-
    liftIO . withFiles files . runImporter $ do
      dir <- liftIO getCurrentDirectory
      findAndLoadAll @(ImportError SrcInfo) (mkSearchConfig "" dir) mname
  case res of
    Right{} -> success
    Left e -> do
      annotateShow e
      failure

importingFails :: NonEmpty String -> [(FilePath, FilePath, Text)] -> Property
importingFails (mn :| mns) files =
  withTests 1 . property $ do
  let
    mname :: ModuleName '[Syntax, Indentation] SrcInfo
    mname =
      makeModuleName
        (MkIdent (Ann $ initialSrcInfo "<unknown>") mn [])
        ((\x -> ([], MkIdent (Ann $ initialSrcInfo "<unknown>") x [])) <$> mns)
  res <-
    liftIO . withFiles files . runImporter $ do
      dir <- liftIO getCurrentDirectory
      findAndLoadAll @(ImportError SrcInfo) (mkSearchConfig "" dir) mname
  case res of
    Right{} -> failure
    Left{} -> success

prop_imports_1 :: Property
prop_imports_1 =
  importingSucceeds
    (pure "b")
    [ ( ".", "a.py"
      , showModule $
        module_
        [ line_ (var_ "a" .= 1)
        ]
      )
    , ( ".", "b.py"
      , showModule $
        module_
        [ line_ $ import_ (pure "a")
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "a"]
        ]
      )
    ]

prop_imports_2 :: Property
prop_imports_2 =
  importingFails
    (pure "b")
    [ ( ".", "a.py"
      , showModule $
        module_
        [ line_ (var_ "b" .= 1)
        ]
      )
    , ( ".", "b.py"
      , showModule $
        module_
        [ line_ $ import_ (pure "a")
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "a"]
        ]
      )
    ]

prop_imports_3 :: Property
prop_imports_3 =
  importingFails
    (pure "a")
    [ ( ".", "a.py"
      , showModule $
        module_
        [ line_ $ call_ (var_ "print") [p_ $ var_ "a"]
        ]
      )
    ]

prop_imports_4 :: Property
prop_imports_4 =
  importingSucceeds
    (pure "b")
    [ ( ".", "a.py"
      , showModule $
        module_
        [ line_ (var_ "a" .= 1)
        ]
      )
    , ( ".", "b.py"
      , showModule $
        module_
        [ line_ $ importAs_ (pure "a" `as_` "b")
        , line_ $ call_ (var_ "print") [p_ $ var_ "b" /> "a"]
        ]
      )
    ]

prop_imports_5 :: Property
prop_imports_5 =
  importingFails
    (pure "b")
    [ ( ".", "a.py"
      , showModule $
        module_
        [ line_ (var_ "a" .= 1)
        ]
      )
    , ( ".", "b.py"
      , showModule $
        module_
        [ line_ $ importAs_ (pure "a" `as_` "b")
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "a"]
        ]
      )
    ]

prop_imports_6 :: Property
prop_imports_6 =
  importingSucceeds
    (pure "c")
    [ ( "a", "b.py"
      , showModule $
        module_
        [ line_ (var_ "c" .= 1)
        ]
      )
    , ( ".", "c.py"
      , showModule $
        module_
        [ line_ $ import_ ("a" :| ["b"])
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b" /> "c"]
        ]
      )
    ]

prop_imports_7 :: Property
prop_imports_7 =
  importingSucceeds
    (pure "c")
    [ ( "a" </> "b", "__init__.py"
      , showModule $
        module_
        [ line_ (var_ "c" .= 1)
        ]
      )
    , ( ".", "c.py"
      , showModule $
        module_
        [ line_ $ import_ ("a" :| ["b"])
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b" /> "c"]
        ]
      )
    ]

prop_imports_8 :: Property
prop_imports_8 =
  importingFails
    (pure "c")
    [ ( "a", "b.py"
      , showModule $
        module_
        [ line_ (var_ "d" .= 1)
        ]
      )
    , ( ".", "c.py"
      , showModule $
        module_
        [ line_ $ import_ ("a" :| ["b"])
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b" /> "c"]
        ]
      )
    ]

prop_imports_9 :: Property
prop_imports_9 =
  importingFails
    (pure "c")
    [ ( "a" </> "b", "__init__.py"
      , showModule $
        module_
        [ line_ (var_ "d" .= 1)
        ]
      )
    , ( ".", "c.py"
      , showModule $
        module_
        [ line_ $ import_ ("a" :| ["b"])
        , line_ $ call_ (var_ "print") [p_ $ var_ "a" /> "b" /> "c"]
        ]
      )
    ]

prop_imports_10 :: Property
prop_imports_10 =
  importingSucceeds
    (pure "c")
    [ ( "a", "b.py"
      , showModule $
        module_
        [ line_ (var_ "c" .= 1)
        ]
      )
    , ( ".", "c.py"
      , showModule $
        module_
        [ line_ $ fromImport_ ([], "a" :| ["b"]) (pure $ "c")
        , line_ $ call_ (var_ "print") [p_ $ var_ "c"]
        ]
      )
    ]

prop_imports_11 :: Property
prop_imports_11 =
  importingSucceeds
    (pure "c")
    [ ( "a" </> "b", "__init__.py"
      , showModule $
        module_
        [ line_ (var_ "c" .= 1)
        ]
      )
    , ( ".", "c.py"
      , showModule $
        module_
        [ line_ $ fromImport_ ([], "a" :| ["b"]) (pure $ "c")
        , line_ $ call_ (var_ "print") [p_ $ var_ "c"]
        ]
      )
    ]

prop_imports_12 :: Property
prop_imports_12 =
  importingSucceeds
    ("a" :| ["b"])
    [ ( "a", "b.py"
      , showModule $
        module_
        [ line_ $ fromImport_ (pure dot_, ["c"]) (pure $ "d")
        , line_ $ call_ (var_ "print") [p_ $ var_ "d"]
        ]
      )
    , ( "a", "c.py"
      , showModule $
        module_
        [ line_ (var_ "d" .= 1)
        ]
      )
    ]

prop_imports_13 :: Property
prop_imports_13 =
  importingSucceeds
    ("a" :| ["b"])
    [ ( "a", "b.py"
      , showModule $
        module_
        [ line_ $ fromImport_ (pure dot_, ["c"]) (pure $ "d")
        , line_ $ call_ (var_ "print") [p_ $ var_ "d"]
        ]
      )
    , ( "a" </> "c", "__init__.py"
      , showModule $
        module_
        [ line_ (var_ "d" .= 1)
        ]
      )
    ]

prop_imports_14 :: Property
prop_imports_14 =
  importingFails
    ("a" :| ["b"])
    [ ( "a", "b.py"
      , showModule $
        module_
        [ line_ $ fromImport_ (pure dot_, ["c"]) (pure $ "d")
        , line_ $ call_ (var_ "print") [p_ $ var_ "d"]
        ]
      )
    , ( "a" </> "c", "__init__.py"
      , showModule $
        module_
        [ line_ (var_ "x" .= 1)
        ]
      )
    ]

prop_imports_15 :: Property
prop_imports_15 =
  importingFails
    ("a" :| ["b"])
    [ ( "a", "b.py"
      , showModule $
        module_
        [ line_ $ fromImport_ (pure dot_, ["c"]) (pure $ "d")
        , line_ $ call_ (var_ "print") [p_ $ var_ "d"]
        ]
      )
    , ( "a", "c.py"
      , showModule $
        module_
        [ line_ (var_ "x" .= 1)
        ]
      )
    ]

prop_imports_16 :: Property
prop_imports_16 =
  importingSucceeds
    ("a" :| ["d"])
    [ ( "a" </> "b", "c.py"
      , showModule $
        module_
        [ line_ (var_ "x" .= 1)
        ]
      )
    , ( "a", "d.py"
      , showModule $
        module_
        [ line_ $ fromImport_ (pure dot_, ["b"]) (pure "c")
        , line_ $ call_ (var_ "print") [p_ $ var_ "c"]
        , line_ $ call_ (var_ "print") [p_ $ var_ "c" /> "x"]
        ]
      )
    ]

prop_imports_17 :: Property
prop_imports_17 =
  importingFails
    ("a" :| ["d"])
    [ ( "a" </> "b", "c.py"
      , showModule $
        module_
        [ line_ (var_ "x" .= 1)
        ]
      )
    , ( "a", "d.py"
      , showModule $
        module_
        [ line_ $ fromImport_ (pure dot_, ["b"]) (pure "c")
        , line_ $ call_ (var_ "print") [p_ $ var_ "c"]
        , line_ $ call_ (var_ "print") [p_ $ var_ "c" /> "y"]
        ]
      )
    ]

prop_imports_18 :: Property
prop_imports_18 =
  importingFails
    ("a" :| ["d"])
    [ ( "a" </> "b", "c.py"
      , showModule $
        module_
        [ line_ (var_ "x" .= 1)
        ]
      )
    , ( "a", "d.py"
      , showModule $
        module_
        [ line_ $ fromImport_ (pure dot_, ["b"]) (pure "c")
        , line_ $ call_ (var_ "print") [p_ $ var_ "d"]
        , line_ $ call_ (var_ "print") [p_ $ var_ "c" /> "x"]
        ]
      )
    ]

prop_imports_19 :: Property
prop_imports_19 =
  importingFails
    ("a" :| ["d"])
    [ ( "a" </> "b", "c.py"
      , showModule $
        module_
        [ line_ (var_ "x" .= 1)
        ]
      )
    , ( "a", "d.py"
      , showModule $
        module_
        [ line_ $ fromImport_ (pure dot_, ["b"]) (pure "d")
        , line_ $ call_ (var_ "print") [p_ $ var_ "c"]
        , line_ $ call_ (var_ "print") [p_ $ var_ "c" /> "x"]
        ]
      )
    ]
