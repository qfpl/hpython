{-# language TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, DataKinds, KindSignatures #-}

{-|
Module      : Language.Python.Validate.Indentation.Error
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Indentation.Error where

import Control.Lens.TH

import Language.Python.Syntax.Whitespace


data IndentationError (v :: [*]) a
  = WrongIndent Indent Indent a
  | TabError a
  | ExpectedGreaterThan [Indent] (Indents a)
  | ExpectedEqualTo [Indent] (Indents a)
  | EmptyContinuedLine a
  deriving (Eq, Show)

makeClassyPrisms ''IndentationError
