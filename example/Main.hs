{-# language DataKinds #-}
module Main where

import Control.Lens

import Programs
import FixMutableDefaultArguments
import OptimizeTailRecursion
import Indentation
import Recase
import Validation

import Language.Python35.Render (showModule)
import Language.Python35.Syntax.Statement (_Statements)

import qualified Data.Text.IO as StrictText

section :: IO a -> IO ()
section a = do
  putStrLn "**********"
  _ <- a
  putStrLn "\n**********\n"

main :: IO ()
main = do
  section $ do
    putStrLn "Before\n"
    StrictText.putStrLn $ showModule everything

  section $ do
    putStrLn "Spaced\n"
    StrictText.putStrLn .
      showModule $
      everything & _Statements %~ indentSpaces 2

  section $ do
    putStrLn "Tabbed\n"
    StrictText.putStrLn .
      showModule $
      everything & _Statements %~ indentTabs

  section $ do
    putStrLn "Refactored\n"
    StrictText.putStrLn .
      showModule .
      rewriteOn _Statements fixMutableDefaultArguments .
      rewriteOn _Statements optimizeTailRecursion $
      everything

  section $ do
    putStrLn "Validated\n"
  doValidating

  section $ do
    putStrLn "Recased\n"
  recase
