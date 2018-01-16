{-# language DataKinds #-}
{-# language RankNTypes #-}
module Test.Language.Python.Gen.ArgumentList where

import Papa

import Data.Functor.Sum

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.IR.ExprConfig
import Language.Python.AST.ArgumentList
import Language.Python.AST.IsArgList hiding (Argument)
import Language.Python.AST.Symbols

import Test.Language.Python.Gen.Combinators

genArgument
  :: ( MonadGen m
     , HasName name
     )
  => ExprConfig 'NotAssignable dctxt
  -> m (name ())
  -> ( forall as' ws' dctxt'
     . ExprConfig as' dctxt'
    -> m ws'
    -> m (expr ws' as' dctxt' ())
     )
  -> m (Argument name expr dctxt ())
genArgument cfg _genName _genExpr =
  Gen.choice
    [ ArgumentPositional <$>
      _genExpr cfg genAnyWhitespaceChar <*>
      pure ()
    , ArgumentKeyword <$>
      _genName <*>
      genBetweenAnyWhitespace (pure Equals) <*>
      _genExpr cfg genAnyWhitespaceChar <*>
      pure ()
    , ArgumentStar <$>
      genAnyWhitespaceAfter (pure Asterisk) <*>
      _genExpr cfg genAnyWhitespaceChar <*>
      pure ()
    , ArgumentDoublestar <$>
      genAnyWhitespaceAfter (pure DoubleAsterisk) <*>
      _genExpr cfg genAnyWhitespaceChar <*>
      pure ()
    ]

genArgumentList
  :: ( MonadGen m
     , HasName name
     )
  => ExprConfig as dctxt
  -> m (name ())
  -> ( forall as' ws' dctxt'
     . ExprConfig as' dctxt'
    -> m ws'
    -> m (expr ws' as' dctxt' ())
     )
  -> m (ArgumentList name expr 'NotAssignable dctxt ())
genArgumentList cfg _genName _genExpr =
  Gen.just . fmap (^? _Right) $
  mkArgumentList <$>
  genArgument (cfg & atomType .~ SNotAssignable) _genName _genExpr <*>
  genListF
    (genBeforeF
      (genBetweenAnyWhitespace $ pure Comma)
      (genArgument (cfg & atomType .~ SNotAssignable) _genName _genExpr)) <*>
  Gen.list (Range.linear 0 10) genAnyWhitespaceChar <*>
  Gen.maybe (genAnyWhitespaceAfter $ pure Comma) <*>
  pure ()
