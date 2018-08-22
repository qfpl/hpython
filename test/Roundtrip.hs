{-# language OverloadedStrings #-}
{-# language DataKinds #-}
module Roundtrip (roundtripTests) where

import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Data.Validate (Validate(..), validate)
import Hedgehog
  ( (===), Group(..), Property, annotateShow, failure, property
  , withTests, withShrinks
  )
import System.FilePath ((</>))

import qualified Data.Text.IO as StrictText
import qualified Data.Text as Strict

import Language.Python.Internal.Lexer (SrcInfo)
import Language.Python.Internal.Render (showModule)
import Language.Python.Parse (parseModule)
import Language.Python.Validate.Indentation
  (Indentation, runValidateIndentation, validateModuleIndentation)
import Language.Python.Validate.Indentation.Error (IndentationError)
import Language.Python.Validate.Syntax
  (runValidateSyntax, validateModuleSyntax, initialSyntaxContext)
import Language.Python.Validate.Syntax.Error (SyntaxError)

roundtripTests :: Group
roundtripTests =
  Group "Roundtrip tests" $
  (\name -> (fromString name, withTests 1 . withShrinks 1 $ doRoundtrip name)) <$>
  [ "typeann.py"
  , "dictcomp.py"
  , "imaginary.py"
  , "weird.py"
  , "weird2.py"
  , "django.py"
  , "django2.py"
  , "test.py"
  , "ansible.py"
  , "comments.py"
  , "pypy.py"
  , "pypy2.py"
  , "sqlalchemy.py"
  , "numpy.py"
  , "numpy2.py"
  , "mypy.py"
  , "mypy2.py"
  ]

doRoundtrip :: FilePath -> Property
doRoundtrip name =
  property $ do
    file <- liftIO . StrictText.readFile $ "test/files" </> name
    py <- validate (\e -> annotateShow e *> failure) pure $ parseModule "test" file
    case runValidateIndentation $ validateModuleIndentation py of
      Failure errs -> annotateShow (errs :: [IndentationError '[] SrcInfo]) *> failure
      Success res ->
        case runValidateSyntax initialSyntaxContext [] (validateModuleSyntax res) of
          Failure errs' -> annotateShow (errs' :: [SyntaxError '[Indentation] SrcInfo]) *> failure
          Success _ -> Strict.lines (showModule py) === Strict.lines file
