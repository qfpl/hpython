{-# language OverloadedStrings #-}
module Roundtrip (roundtripTests) where

import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Hedgehog
  ((===), Group(..), Property, annotateShow, failure, property, withTests)
import System.FilePath ((</>))
import Text.Trifecta (parseString, Result(..))

import Language.Python.Internal.Parse (module_)
import Language.Python.Internal.Render (renderModule)

roundtripTests :: Group
roundtripTests =
  Group "Roundtrip tests" $
  (\name -> (fromString name, withTests 1 $ doRoundtrip name)) <$>
  ["django.py"]

doRoundtrip :: String -> Property
doRoundtrip name =
  property $ do
    file <- liftIO . readFile $ "test/files" </> name
    case parseString module_ mempty file of
      Failure err -> do
        annotateShow err
        failure
      Success a -> renderModule a === file
