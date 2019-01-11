{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
module DSL (dslTests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Lens (Lens, view, set)
import Control.Monad (when)
import Language.Python.DSL

import Generators.General

dslTests :: Group
dslTests = $$discover

lens_setGet
  :: (Show s, Show a, Eq a)
  => Lens s s a a
  -> Gen s
  -> Gen a
  -> PropertyT IO ()
lens_setGet l ms mb = do
  s <- forAll ms
  b <- forAll mb
  view l (set l b s) === b

lens_getSet
  :: (Show s, Eq s)
  => Lens s s a a
  -> Gen s
  -> PropertyT IO ()
lens_getSet l ms = do
  s <- forAll ms
  set l (view l s) s === s

lens_setSet
  :: (Show s, Eq s, Show b)
  => Lens s s a b
  -> Gen s
  -> Gen b
  -> PropertyT IO ()
lens_setSet l ms mb = do
  s <- forAll ms
  b <- forAll mb
  b' <- forAll mb
  set l b' (set l b s) === set l b' s

parameters_lens
  :: (Eq (Raw s), Show (Raw s), ParametersSyntax s)
  => Gen (Raw s)
  -> Gen (Raw (Param Expr))
  -> PropertyT IO ()
parameters_lens ms mp = do
  lens_setGet parameters_ ms (Gen.list (Range.constant 0 10) mp)
  lens_getSet parameters_ ms
  lens_setSet parameters_ ms (Gen.list (Range.constant 0 10) mp)

body_lens
  :: (Eq (Raw s), Show (Raw s), BodySyntax s)
  => Gen (Raw s)
  -> Gen (Raw Line)
  -> PropertyT IO ()
body_lens ms ml = do
  lens_setGet body_ ms (Gen.list (Range.constant 0 10) ml)
  lens_getSet body_ ms
  lens_setSet body_ ms (Gen.list (Range.constant 0 10) ml)

{-

These tests are 'existence proofs' that the following 'optics' break the
Lens laws. They also provide minimal counterexamples. If you want to see
it for yourself, then change this flag to True and re-run the tests

-}

runFailingTests :: Bool
runFailingTests = False

prop_fundef_body_lens :: Property
prop_fundef_body_lens =
  property $
  when runFailingTests $
  body_lens
    @Fundef
    genFundef
    (Gen.choice [pure blank_, line_ <$> genStatement])

prop_fundef_parameters_lens :: Property
prop_fundef_parameters_lens =
  property $
  when runFailingTests $
  parameters_lens genFundef (genParam genExpr)
