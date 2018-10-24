{-# language DataKinds #-}
module Main where

import Control.Lens

import Programs
import FixMutableDefaultArguments
import OptimizeTailRecursion
import Indentation

import Language.Python.Render (showModule)
import Language.Python.Internal.Syntax (_Statements)

import qualified Data.Text.IO as StrictText

section a = do
  putStrLn "**********"
  a
  putStrLn "\n**********\n"

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
