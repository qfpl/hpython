{-# language MultiParamTypeClasses, TemplateHaskell, FunctionalDependencies,
    FlexibleInstances #-}

{-|
Module      : Language.Python.Validate.Syntax.Error
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Syntax.Error where

import Control.Lens.TH
import Language.Python.Syntax.Expr (Expr)
import Language.Python.Syntax.Ident (Ident)

data SyntaxError a
  = PositionalAfterKeywordArg a (Expr a)
  | PositionalAfterKeywordUnpacking a (Expr a)
  | CannotAssignTo a (Expr a)
  | CannotDelete a (Expr a)
  | CannotAugAssignTo a (Expr a)
  | NoBindingNonlocal (Ident a)
  | PositionalAfterKeywordParam a String
  | UnexpectedDoubleStarParam a String
  | DuplicateArgument a String
  | UnexpectedNewline a
  | UnexpectedComment a
  | IdentifierReservedWord a String
  | EmptyIdentifier a
  | BadCharacter a String
  | BreakOutsideLoop a
  | ContinueOutsideLoop a
  | ReturnOutsideFunction a
  | NonlocalOutsideFunction a
  | ParametersNonlocal a [String]
  | Can'tJoinStringAndBytes a
  | YieldOutsideGenerator a
  | MalformedDecorator a
  | InvalidDictUnpacking a
  | InvalidSetUnpacking a
  | TypedParamInLambda a
  | AsyncWithOutsideCoroutine a
  | AsyncForOutsideCoroutine a
  | YieldFromInsideCoroutine a
  | YieldInsideCoroutine a
  | AwaitOutsideCoroutine a
  | AwaitInsideComprehension a
  | NullByte a
  | NonAsciiInBytes a Char
  | DefaultExceptMustBeLast a
  | WildcardImportInDefinition a
  | NoKeywordsAfterEmptyStarArg a
  | ManyStarredTargets a
  | ManyStarredParams a
  | ContinueInsideFinally a
  | ParameterMarkedGlobal a String
  deriving (Eq, Show)

makeClassyPrisms ''SyntaxError
