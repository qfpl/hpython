{-# language TypeApplications #-}
module Recase (recase) where

import Control.Lens.Setter (over)
import Control.Lens.Plated (transform)
import Data.Char (toUpper)

import qualified Data.Text.IO as Text

import Language.Python.Optics.Idents
import Language.Python35.Parse
import Language.Python35.Render (showModule)
import Language.Python35.Syntax (identValue)

snakeToCamel :: String -> String
snakeToCamel =
  transform $
  \s -> case s of
    '_' : c : cs -> toUpper c : cs
    _ -> s

recase :: IO ()
recase = do
  m <- readModule @(ParseError SrcInfo) "example/snake_cased.py"
  case m of
    Failure err -> error $ show err
    Success a -> do
      let fixed = over (_Idents.identValue) snakeToCamel a
      Text.putStrLn $ showModule fixed
