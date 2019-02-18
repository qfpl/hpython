{-# options_ghc -fno-warn-unused-do-bind #-}
{-# language DataKinds, TypeOperators, FlexibleContexts #-}
{-# language OverloadedStrings #-}
module Main where

import DSL
import Imports
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
  results1 <- traverse checkSequential groups1
  results2 <- traverse checkParallel groups2
  when (not (and results1 && and results2))
    exitFailure
  where
    groups1 =
      [ importsTests
      ]

    groups2 =
      [ lexerParserTests
      , dslTests
      , parserTests
      , opticsTests
      , scopeTests
      , syntaxTests
      , roundtripTests
      ]
