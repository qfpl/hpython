{-# language LambdaCase #-}
{-# language TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, DataKinds, KindSignatures #-}

{-|
Module      : Language.Python.Validate.Indentation.Error
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Indentation.Error
  ( IndentationError(..)
  , AsTabError(..)
  , AsIndentationError(..)
  )
where

import Control.Lens.Prism (Prism', prism')

import Language.Python.Internal.Lexer (AsTabError(..))
import Language.Python.Syntax.Whitespace


data IndentationError a
  = IndentationTabError a
  | ExpectedGreaterThan [Indent] (Indents a)
  | ExpectedEqualTo [Indent] (Indents a)
  | EmptyContinuedLine a
  deriving (Eq, Show)

class AsTabError s a => AsIndentationError s a | s -> a where
  _IndentationError :: Prism' s (IndentationError a)

  _ExpectedGreaterThan :: Prism' s ([Indent], Indents a)
  _ExpectedGreaterThan = _IndentationError._ExpectedGreaterThan

  _ExpectedEqualTo :: Prism' s ([Indent], Indents a)
  _ExpectedEqualTo = _IndentationError._ExpectedEqualTo

  _EmptyContinuedLine :: Prism' s a
  _EmptyContinuedLine = _IndentationError._EmptyContinuedLine

instance AsTabError (IndentationError a) a where
  _TabError =
    prism'
      IndentationTabError
      (\case
          IndentationTabError a -> Just a
          _ -> Nothing)

instance AsIndentationError (IndentationError a) a where
  _IndentationError = id

  _ExpectedGreaterThan =
    prism'
      (uncurry ExpectedGreaterThan)
      (\case
          ExpectedGreaterThan a b -> Just (a, b)
          _ -> Nothing)
  _ExpectedEqualTo =
    prism'
      (uncurry ExpectedEqualTo)
      (\case
          ExpectedEqualTo a b -> Just (a, b)
          _ -> Nothing)
  _EmptyContinuedLine =
    prism'
      EmptyContinuedLine
      (\case
          EmptyContinuedLine a -> Just a
          _ -> Nothing)
