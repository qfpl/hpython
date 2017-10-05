{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Python.IR.SyntaxChecker where

import Papa
import Data.Text (Text)
import Data.Validation
import qualified Data.DList as D

import Language.Python.AST.Identifier
import Language.Python.AST.IndentedLines

data InvalidLHS
  = LHSIf
  | LHSOperator
  | LHSAwaitExpr
  | LHSTrailer
  | LHSArgument
  | LHSFor
  | LHSIfOrFor
  | LHSSubscript
  | LHSYieldExpr
  | LHSLiteral
  | LHSEllipsis
  | LHSNone
  | LHSTrue
  | LHSFalse
  | LHSSingleStarExpr
  | LHSLambda
  | LHSDictOrSet
  deriving (Eq, Show, Ord)

data SyntaxError a
  = CannotAssignTo InvalidLHS a
  | AwaitNotInAsyncFunction a
  | YieldNotInFunction a
  | YieldInAsyncFunction a
  | IdentifierIsKeyword Text a
  -- ^ *a for a in a
  | UnpackingInComprehension a
  -- ^ (*a)
  | UnpackingInParens a
  | DuplicateArguments [Identifier a] a
  | NonlocalAtModuleLevel a
  | AsyncNotInAsyncFunction a
  | ReturnOutsideFunction a
  | BreakOutsideLoop a
  | ContinueOutsideLoop a
  | IndentationError (IndentationError a)
  deriving (Eq, Show, Ord)

newtype SyntaxChecker ann a
  = SyntaxChecker
  { runSyntaxChecker :: AccValidation (D.DList (SyntaxError ann)) a
  } deriving
  ( Functor
  , Applicative
  )

instance Alt (SyntaxChecker ann) where
  SyntaxChecker a <!> SyntaxChecker b = SyntaxChecker $ a <!> b

runChecker :: SyntaxChecker ann a -> Either [SyntaxError ann] a
runChecker c =
  case runSyntaxChecker c of
    AccFailure es -> Left $ D.toList es
    AccSuccess a -> Right a

syntaxError :: SyntaxError ann -> SyntaxChecker ann a
syntaxError = SyntaxChecker . AccFailure . D.singleton

liftError
  :: (b -> SyntaxError ann)
  -> SyntaxChecker ann (Either b a)
  -> SyntaxChecker ann a
liftError f err =
  SyntaxChecker $ case runSyntaxChecker err of
    AccSuccess (Left e) -> AccFailure $ D.singleton (f e)
    AccSuccess (Right a) -> AccSuccess a
    AccFailure es -> AccFailure es
