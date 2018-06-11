{-# language TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, DataKinds, KindSignatures #-}
module Language.Python.Validate.Indentation.Error where

import Language.Python.Internal.Syntax

import Control.Lens.TH

data IndentationError (v :: [*]) a
  = WrongIndent Indent Indent a
  | ExpectedIndent a
  deriving (Eq, Show)

makeClassyPrisms ''IndentationError
