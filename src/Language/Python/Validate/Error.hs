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

data ValidationError v a
  = IndentationError (IndentationError a)
  | ScopeError (ScopeError v a)
  | SyntaxError (SyntaxError v a)
  deriving (Eq, Show)

instance AsTabError (ValidationError v a) a where
  _TabError = _IndentationError._TabError

instance AsIndentationError (ValidationError v a) a where
  _IndentationError =
    prism'
      IndentationError
      (\case; IndentationError a -> Just a; _ -> Nothing)

instance AsScopeError (ValidationError v a) v a where
  _ScopeError =
    prism'
      ScopeError
      (\case; ScopeError a -> Just a; _ -> Nothing)

instance AsSyntaxError (ValidationError v a) v a where
  _SyntaxError =
    prism'
      SyntaxError
      (\case; SyntaxError a -> Just a; _ -> Nothing)