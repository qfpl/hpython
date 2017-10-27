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
import Language.Python.AST.Symbols

import Test.Language.Python.Gen.Combinators

genArgumentList
  :: MonadGen m
  => ExprConfig as dctxt
  -> m (name ())
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> m (expr as' dctxt' ()))
  -> m (ArgumentList name expr 'NotAssignable dctxt ())
genArgumentList cfg _genName _genExpr =
  Gen.choice
    [ Gen.just $
      fmap (review _ArgumentListAll) $
      (,,,,) <$>
      genPositionalArguments cfg _genExpr <*>
      genMaybeF
        (genBeforeF (genBetweenWhitespace $ pure Comma)
         (genStarredAndKeywords cfg _genName _genExpr)) <*>
      genMaybeF
        (genBeforeF
           (genBetweenWhitespace $ pure Comma)
           (genKeywordsArguments cfg _genName _genExpr)) <*>
      Gen.maybe (genBetweenWhitespace $ pure Comma) <*>
      pure ()
    , Gen.just $
      fmap (review _ArgumentListUnpacking) $
      (,,,) <$>
      genStarredAndKeywords cfg _genName _genExpr <*>
      genMaybeF
        (genBeforeF
           (genBetweenWhitespace $ pure Comma)
           (genKeywordsArguments cfg _genName _genExpr)) <*>
      Gen.maybe (genBetweenWhitespace $ pure Comma) <*>
      pure ()
    , Gen.just $
      fmap (review _ArgumentListKeywords) $
      (,,) <$>
      genKeywordsArguments cfg _genName _genExpr <*>
      Gen.maybe (genBetweenWhitespace $ pure Comma) <*>
      pure ()
    ]

genPositionalArguments
  :: MonadGen m
  => ExprConfig as dctxt
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> m (expr as' dctxt' ()))
  -> m (PositionalArguments expr 'NotAssignable dctxt ())
genPositionalArguments cfg _genExpr = do
  n <- Size <$> Gen.int (Range.linear 1 20)
  PositionalArguments <$>
    Gen.resize n
      (genBeforeF
        (Gen.maybe . genBetweenWhitespace $ pure Asterisk)
        (_genExpr $ cfg & atomType .~ SNotAssignable)) <*>
    genListF
      (Gen.resize n $
       genBeforeF
         (genBetweenWhitespace $ pure Comma)
         (genBeforeF
           (Gen.maybe . genBetweenWhitespace $ pure Asterisk)
           (_genExpr $ cfg & atomType .~ SNotAssignable))) <*>
    pure ()

genStarredAndKeywords
  :: MonadGen m
  => ExprConfig as dctxt
  -> m (name ())
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> m (expr as' dctxt' ()))
  -> m (StarredAndKeywords name expr 'NotAssignable dctxt ())
genStarredAndKeywords cfg _genName _genExpr = do
  n <- Size <$> Gen.int (Range.linear 1 20)
  StarredAndKeywords <$>
    starOrKeyword n <*>
    genListF
      (genBeforeF (genBetweenWhitespace $ pure Comma) (starOrKeyword n)) <*>
    pure ()
  where
    starOrKeyword n =
      Gen.choice
        [ InL <$>
          Gen.resize n
            (genBeforeF
              (genBetweenWhitespace $ pure Asterisk)
              (_genExpr $ cfg & atomType .~ SNotAssignable))
        , InR <$> Gen.resize n (genKeywordItem cfg _genName _genExpr)
        ]

genKeywordsArguments
  :: MonadGen m
  => ExprConfig as dctxt
  -> m (name ())
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> m (expr as' dctxt' ()))
  -> m (KeywordsArguments name expr 'NotAssignable dctxt ())
genKeywordsArguments cfg _genName _genExpr = do
  n <- Size <$> Gen.int (Range.linear 1 20)
  KeywordsArguments <$>
    keywordOrDoublestar n <*>
    genListF
      (genBeforeF
        (genBetweenWhitespace $ pure Comma)
        (keywordOrDoublestar n)) <*>
    pure ()
  where
    keywordOrDoublestar n =
      Gen.choice
        [ InL <$> genKeywordItem cfg _genName _genExpr
        , Gen.resize n $
          InR <$>
          genBeforeF
            (genBetweenWhitespace $ pure DoubleAsterisk)
            (_genExpr $ cfg & atomType .~ SNotAssignable)
        ]

genKeywordItem
  :: MonadGen m
  => ExprConfig as dctxt
  -> m (name ())
  -> ( forall as' dctxt'
     . ExprConfig as' dctxt'
    -> m (expr as' dctxt' ()))
  -> m (KeywordItem name expr 'NotAssignable dctxt ())
genKeywordItem cfg _genName _genExpr =
  KeywordItem <$>
  genWhitespaceAfterF _genName <*>
  genWhitespaceBeforeF (_genExpr $ cfg & atomType .~ SNotAssignable) <*>
  pure ()
