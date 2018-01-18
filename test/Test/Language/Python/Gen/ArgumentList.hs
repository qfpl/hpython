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
  -> m (val ())
  -> m (name ())
  -> ( forall as' ws' dctxt'
     . ExprConfig as' dctxt'
    -> m ws'
    -> m (expr ws' as' dctxt' ())
     )
  -> m (Argument val name expr dctxt ())
genArgument cfg _genVal _genName _genExpr =
  Gen.choice
    [ ArgumentPositional <$>
      _genVal <*>
      pure ()
    , ArgumentKeyword <$>
      _genName <*>
      genBetweenAnyWhitespace (pure Equals) <*>
      _genExpr cfg genAnyWhitespaceChar <*>
      pure ()
    , ArgumentStar <$>
      genAnyWhitespaceAfter (pure Asterisk) <*>
      _genVal <*>
      pure ()
    , ArgumentDoublestar <$>
      genAnyWhitespaceAfter (pure DoubleAsterisk) <*>
      _genVal <*>
      pure ()
    ]

genArgumentList
  :: ( MonadGen m
     , HasName name
     )
  => ExprConfig as dctxt
  -> m (val ())
  -> m (name ())
  -> ( forall as' ws' dctxt'
     . ExprConfig as' dctxt'
    -> m ws'
    -> m (expr ws' as' dctxt' ())
     )
  -> m (ArgumentList val name expr 'NotAssignable dctxt ())
genArgumentList cfg _genVal _genName _genExpr =
  Gen.just . fmap (^? _Right) $
  mkArgumentList <$>
  genArgument (cfg & atomType .~ SNotAssignable) _genVal _genName _genExpr <*>
  genListF
    (genBeforeF
      (genBetweenAnyWhitespace $ pure Comma)
      (genArgument (cfg & atomType .~ SNotAssignable) _genVal _genName _genExpr)) <*>
  Gen.list (Range.linear 0 10) genAnyWhitespaceChar <*>
  Gen.maybe (genAnyWhitespaceAfter $ pure Comma) <*>
  pure ()
