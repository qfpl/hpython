{-# language LambdaCase #-}
module Language.Python.Module.IR.Checker where

import Papa

import Data.Functor.Sum

import Language.Python.IR.ExprConfig
import Language.Python.IR.StatementConfig
import Language.Python.IR.SyntaxChecker
import Language.Python.Statement.IR.Checker

import qualified Language.Python.Module.AST as Safe
import qualified Language.Python.Module.IR as IR

checkModule :: Ord a => IR.Module a -> SyntaxChecker a (Safe.Module a)
checkModule (IR.Module s a) =
  Safe.Module <$>
  traverseOf
    (_Wrapped.traverse)
    (\case
        InL x -> pure $ InL x
        InR x ->
          InR <$>
          checkStatement
          (ExprConfig SNotAssignable STopLevel)
          (StatementConfig SNotInLoop)
          x)
    s <*>
  pure a
