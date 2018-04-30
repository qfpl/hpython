{-# language OverloadedStrings #-}
{-# language DataKinds #-}
module Roundtrip (roundtripTests) where

import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Data.Validate (Validate(..))
import Hedgehog
  ((===), Group(..), Property, annotateShow, failure, property, withTests)
import System.FilePath ((</>))
import Text.Trifecta (Span, parseString)
import qualified Text.Trifecta as Trifecta (Result(..))

import Language.Python.Internal.Parse (module_)
import Language.Python.Internal.Render (renderModule)
import Language.Python.Validate.Indentation (Indentation, validateModuleIndentation)
import Language.Python.Validate.Indentation.Error (IndentationError)
import Language.Python.Validate.Syntax
  (validateModuleSyntax, runValidateSyntax, initialSyntaxContext)
import Language.Python.Validate.Syntax.Error (SyntaxError)

roundtripTests :: Group
roundtripTests =
  Group "Roundtrip tests" $
  (\name -> (fromString name, withTests 1 $ doRoundtrip name)) <$>
  [ "django.py"
  , "test.py"
  , "weird.py"
  ]

doRoundtrip :: String -> Property
doRoundtrip name =
  property $ do
    file <- liftIO . readFile $ "test/files" </> name
    case parseString module_ mempty file of
      Trifecta.Failure err -> do
        annotateShow err
        failure
      Trifecta.Success a ->
        case validateModuleIndentation a of
          Failure errs -> annotateShow (errs :: [IndentationError '[] Span]) *> failure
          Success res ->
            case runValidateSyntax initialSyntaxContext [] (validateModuleSyntax res) of
              Failure errs' -> annotateShow (errs' :: [SyntaxError '[Indentation] Span]) *> failure
              Success _ -> renderModule a === file
