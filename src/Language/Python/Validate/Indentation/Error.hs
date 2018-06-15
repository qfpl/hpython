{-# language TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, DataKinds, KindSignatures #-}
module Language.Python.Validate.Indentation.Error where

import Language.Python.Internal.Syntax

import Control.Lens.TH

data IndentationError (v :: [*]) a
  = WrongIndent Indent Indent a
  | TabError a
  | ExpectedGreaterThan [Indent] (Indents a)
  | ExpectedDedent a
  | ExpectedEqualTo [Indent] (Indents a)
  deriving (Eq, Show)

makeClassyPrisms ''IndentationError
