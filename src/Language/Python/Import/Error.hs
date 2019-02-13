{-# language DataKinds, KindSignatures #-}
{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
module Language.Python.Import.Error where

import Control.Lens.TH (makeClassyPrisms)
import Data.List.NonEmpty (NonEmpty)

import Language.Python.Parse (ParseError)
import Language.Python.Syntax.ModuleNames (ModuleName)
import Language.Python.Validate (ValidationError)

data ImportError a
  = ImportNotFound (ModuleName '[] a)
  | ImportParseErrors (NonEmpty (ParseError a))
  | ImportValidationErrors (NonEmpty (ValidationError a))
  deriving (Eq, Show)
makeClassyPrisms ''ImportError