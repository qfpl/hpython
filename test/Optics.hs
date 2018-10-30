{-# language OverloadedStrings, TemplateHaskell #-}
module Optics (opticsTests) where

import Hedgehog

import Control.Lens.Plated (transformOn)
import Control.Lens.Setter ((.~))
import Control.Monad.IO.Class (liftIO)
import Data.Validation (validation)
import qualified Data.Text.IO as Text

import Language.Python.Parse (parseModule)
import Language.Python.Render (showModule)
import Language.Python.Syntax.Statement (_Statements)
import Language.Python.Syntax.Whitespace (Whitespace (..))
import Language.Python.Optics (_Indent)

opticsTests :: Group
opticsTests = $$discover

prop_optics_1 :: Property
prop_optics_1 =
  withTests 1 . property $ do
    str <- liftIO $ Text.readFile "test/files/indent_optics_in.py"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    -- annotateShow $! tree

    str' <- liftIO $ Text.readFile "test/files/indent_optics_out.py"
    showModule
      (transformOn _Statements (_Indent .~ [Space, Space, Space, Space]) tree) === str'

prop_optics_2 :: Property
prop_optics_2 =
  withTests 1 . property $ do
    str <- liftIO $ Text.readFile "test/files/indent_optics_in2.py"

    tree <- validation (\e -> annotateShow e *> failure) pure $ parseModule "test" str
    -- annotateShow $! tree

    str' <- liftIO $ Text.readFile "test/files/indent_optics_out2.py"
    showModule
      (transformOn _Statements (_Indent .~ [Space, Space, Space, Space]) tree) === str'
