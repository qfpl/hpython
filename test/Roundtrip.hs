{-# language OverloadedStrings #-}
{-# language DataKinds #-}
module Roundtrip (roundtripTests) where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty)
import Data.String (fromString)
import Data.Text (Text)
import Data.Validation (Validation(..))
import Hedgehog
  ( (===), Group(..), Property, PropertyT, annotateShow, failure, property
  , withTests, withShrinks
  )
import System.FilePath ((</>))

import qualified Data.Text.IO as StrictText

import Language.Python.Internal.Lexer (SrcInfo)
import Language.Python.Render (showModule)
import Language.Python.Parse (parseModule)
import Language.Python.Validate
  ( IndentationError, SyntaxError
  , runValidateIndentation, validateModuleIndentation, runValidateSyntax
  , validateModuleSyntax
  )

import Helpers (shouldBeParseSuccess)

roundtripTests :: Group
roundtripTests =
  Group "Roundtrip tests" $
  (\name -> (fromString name, withTests 1 . withShrinks 0 $ doRoundtripFile name)) <$>
  [ "decorators.py"
  , "string.py"
  , "set.py"
  , "regex.py"
  , "asyncstatements.py"
  , "typeann.py"
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
  , "requests.py"
  , "requests2.py"
  , "joblib.py"
  , "joblib2.py"
  , "pandas.py"
  , "pandas2.py"
  ]

doRoundtripFile :: FilePath -> Property
doRoundtripFile name =
  property $ do
    file <- liftIO . StrictText.readFile $ "test/files" </> name
    doRoundtrip file

doRoundtrip :: Text -> PropertyT IO ()
doRoundtrip file = do
  py <- shouldBeParseSuccess parseModule file
  case runValidateIndentation $ validateModuleIndentation py of
    Failure errs -> do
      annotateShow (errs :: NonEmpty (IndentationError SrcInfo))
      failure
    Success res ->
      case runValidateSyntax (validateModuleSyntax res) of
        Failure errs' -> do
          annotateShow (errs' :: NonEmpty (SyntaxError SrcInfo))
          failure
        Success _ -> do
          annotateShow py
          showModule py === file
