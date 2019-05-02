{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
module Language.Python.Validate.Error
  ( module Language.Python.Validate.Indentation.Error
  , module Language.Python.Validate.Scope.Error
  , module Language.Python.Validate.Syntax.Error
  , ValidationError(..)
  )
where

import Control.Lens.Prism (prism')
import Language.Python.Validate.Indentation.Error
import Language.Python.Validate.Scope.Error
import Language.Python.Validate.Syntax.Error

data ValidationError a
  = IndentationError (IndentationError a)
  | ScopeError (ScopeError a)
  | SyntaxError (SyntaxError a)
  deriving (Eq, Show)

instance AsTabError (ValidationError a) a where
  _TabError = _IndentationError._TabError

instance AsIndentationError (ValidationError a) a where
  _IndentationError =
    prism'
      IndentationError
      (\case; IndentationError a -> Just a; _ -> Nothing)

instance AsScopeError (ValidationError a) a where
  _ScopeError =
    prism'
      ScopeError
      (\case; ScopeError a -> Just a; _ -> Nothing)

instance AsSyntaxError (ValidationError a) a where
  _SyntaxError =
    prism'
      SyntaxError
      (\case; SyntaxError a -> Just a; _ -> Nothing)