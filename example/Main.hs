{-# language DataKinds #-}
module Main where

import Control.Lens

import Programs
import FixMutableDefaultArguments
import OptimizeTailRecursion
import Indentation

import Language.Python.Internal.Render
import Language.Python.Internal.Syntax

section a = do
  putStrLn "**********"
  a
  putStrLn "\n**********\n"

main = do
  section $ do
    putStrLn "Before\n"
    putStrLn $ renderModule everything

  section $ do
    putStrLn "Spaced\n"
    putStrLn .
      renderModule $
      everything & _Statements %~ indentSpaces 2

  section $ do
    putStrLn "Tabbed\n"
    putStrLn .
      renderModule $
      everything & _Statements %~ indentTabs

  section $ do
    putStrLn "Refactored\n"
    putStrLn .
      renderModule .
      rewriteOn _Statements fixMutableDefaultArguments .
      rewriteOn _Statements optimizeTailRecursion $
      everything
