{-# options_ghc -fno-warn-unused-do-bind #-}
{-# language DataKinds, TypeOperators, FlexibleContexts #-}
{-# language OverloadedStrings #-}
module Main where

import DSL
import LexerParser
import Optics
import Parser
import Roundtrip
import Scope
import Syntax

import Control.Monad (when)
import System.Exit

import Hedgehog

main :: IO ()
main = do
  results <- traverse checkParallel groups
  when (not (and results))
    exitFailure
  where
    groups = [lexerParserTests, dslTests, parserTests, opticsTests, scopeTests, syntaxTests, roundtripTests]
