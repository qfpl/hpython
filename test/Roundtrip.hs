{-# language OverloadedStrings #-}
{-# language DataKinds #-}
module Roundtrip (roundtripTests) where

import Data.Foldable (traverse_)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Data.Validate (Validate(..))
import Hedgehog
  ( (===), Group(..), Property, annotateShow, failure, property
  , withTests, withShrinks, success
  )
import System.FilePath ((</>))
import Text.Trifecta (Caret)

import Language.Python.Internal.Parse (module_)
import Language.Python.Internal.Render (showModule)
import Language.Python.Validate.Indentation
  (Indentation, runValidateIndentation, validateModuleIndentation)
import Language.Python.Validate.Indentation.Error (IndentationError)
import Language.Python.Validate.Syntax
  (validateModuleSyntax, runValidateSyntax, initialSyntaxContext)
import Language.Python.Validate.Syntax.Error (SyntaxError)

import Helpers (doToPython)

roundtripTests :: Group
roundtripTests =
  Group "Roundtrip tests" $
  (\name -> (fromString name, withTests 1 . withShrinks 1 $ doRoundtrip name)) <$>
  [ "weird.py"
  , "weird2.py"
  , "django.py"
  , "test.py"
  , "ansible.py"
  , "comments.py"
  , "pypy.py"
  , "pypy2.py"
  ]

doRoundtrip :: String -> Property
doRoundtrip name =
  property $ do
    file <- liftIO . readFile $ "test/files" </> name
    py <- doToPython module_ file
    case runValidateIndentation $ validateModuleIndentation py of
      Failure errs -> annotateShow (errs :: [IndentationError '[] Caret]) *> failure
      Success res ->
        case runValidateSyntax initialSyntaxContext [] (validateModuleSyntax res) of
          Failure errs' -> annotateShow (errs' :: [SyntaxError '[Indentation] Caret]) *> failure
          Success _ ->
            -- lines (showModule py) === lines file
            traverse_ (\(a, b) -> if a == b then success else a === b) $ zip (lines $ showModule py) (lines file)
