{-# language OverloadedStrings #-}
module Optics (opticsTests) where

import Hedgehog

import Control.Lens.Plated (transformOn)
import Control.Lens.Setter ((.~))
import Control.Monad.IO.Class (liftIO)
import Data.Validation (validation)
import qualified Data.Text.IO as Text

import Language.Python.Parse (parseModule)
import Language.Python.Internal.Render (showModule)
import Language.Python.Internal.Syntax (Whitespace(..), _Statements)
import Language.Python.Internal.Optics (_Indent)

opticsTests :: Group
opticsTests =
  Group "Optics Tests"
  [ ("Optics test 1", test_optics_1)
  , ("Optics test 2", test_optics_2)
  ]

test_optics_1 :: Property
test_optics_1 =
  withTests 1 . property $ do
    str <- liftIO $ Text.readFile "test/files/indent_optics_in.py"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    -- annotateShow $! tree

    str' <- liftIO $ Text.readFile "test/files/indent_optics_out.py"
    showModule
      (transformOn _Statements (_Indent .~ [Space, Space, Space, Space]) tree) === str'

test_optics_2 :: Property
test_optics_2 =
  withTests 1 . property $ do
    str <- liftIO $ Text.readFile "test/files/indent_optics_in2.py"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    -- annotateShow $! tree

    str' <- liftIO $ Text.readFile "test/files/indent_optics_out2.py"
    showModule
      (transformOn _Statements (_Indent .~ [Space, Space, Space, Space]) tree) === str'
