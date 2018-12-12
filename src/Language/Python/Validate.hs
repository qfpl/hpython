{-# language DataKinds, TypeOperators #-}
{-# language FlexibleContexts #-}
{-|
Module      : Language.Python.Validate
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate
  ( module Data.Validation
  , module Language.Python.Validate.Error
  , module Language.Python.Validate.Indentation
  , module Language.Python.Validate.Scope
  , module Language.Python.Validate.Syntax
  , validateModuleAll
  , validateStatementAll
  , validateExprAll
  , validateAll
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Validation

import Language.Python.Syntax
import Language.Python.Validate.Error
import Language.Python.Validate.Indentation
import Language.Python.Validate.Scope
import Language.Python.Validate.Syntax

validateModuleAll
  :: ( AsIndentationError e a
     , AsSyntaxError e a
     , AsScopeError e a
     )
  => Module '[] a -- ^ 'Module' to validate
  -> Validation (NonEmpty e) (Module '[Scope, Syntax, Indentation] a)
validateModuleAll =
  validateAll validateModuleIndentation validateModuleSyntax validateModuleScope

validateStatementAll
  :: ( AsIndentationError e a
     , AsSyntaxError e a
     , AsScopeError e a
     )
  => Statement '[] a -- ^ 'Statement' to validate
  -> Validation (NonEmpty e) (Statement '[Scope, Syntax, Indentation] a)
validateStatementAll =
  validateAll validateStatementIndentation validateStatementSyntax validateStatementScope

validateExprAll
  :: ( AsIndentationError e a
     , AsSyntaxError e a
     , AsScopeError e a
     )
  => Expr '[] a -- ^ 'Expr' to validate
  -> Validation (NonEmpty e) (Expr '[Scope, Syntax, Indentation] a)
validateExprAll =
  validateAll validateExprIndentation validateExprSyntax validateExprScope

-- | Validate a datatype for indentation, syntax, and scope correctness
--
-- e.g.
--
-- @
-- 'validateModuleAll' =
--   'validateAll'
--     'validateModuleIndentation'
--     'validateModuleSyntax'
--     'validateModuleScope'
-- @
validateAll
  :: ( AsIndentationError e a
     , AsSyntaxError e a
     , AsScopeError e a
     )
  => (s '[] a -> ValidateIndentation e (s '[Indentation] a)) -- ^ Indentation validator
  -> (s '[Indentation] a -> ValidateSyntax e (s '[Syntax, Indentation] a)) -- ^ Syntax validator
  -> (s '[Syntax, Indentation] a -> ValidateScope a e (s '[Scope, Syntax, Indentation] a)) -- ^ Scope validator
  -> s '[] a
  -> Validation (NonEmpty e) (s '[Scope, Syntax, Indentation] a)
validateAll vi vsyn vsco m =
  runValidateIndentation (vi m) `bindValidation` \m' ->
  runValidateSyntax (vsyn m') `bindValidation` \m'' ->
  runValidateScope (vsco m'')
