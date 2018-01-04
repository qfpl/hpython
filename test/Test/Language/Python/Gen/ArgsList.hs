{-# language DataKinds #-}
module Test.Language.Python.Gen.ArgsList where

import Data.Functor.Compose

import Papa hiding (Sum)
import Data.Functor.Sum
import Hedgehog
import Language.Python.AST.ArgsList
import Language.Python.AST.IsArgList
import Language.Python.AST.Symbols
import Language.Python.IR.ExprConfig

import Test.Language.Python.Gen.Combinators

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genArgsListArg
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (name ())
  -> m (f ())
  -> m (ArgsListArg ws name f ())
genArgsListArg _ ws genName gen =
  ArgsListArg <$>
  genName <*>
  genMaybeF
    (genBeforeF
      (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Equals)
      gen) <*>
  pure ()

genArgsListStarPart
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (name ())
  -> m (f ())
  -> m (ArgsListStarPart ws name f ())
genArgsListStarPart cfg ws genName gen = do
  n <- Size <$> Gen.integral_ (Range.linear 0 20)
  Gen.recursive
    Gen.choice
    [ pure $ ArgsListStarPartEmpty ()
    , ArgsListStarPart <$>
      genBeforeF
        (genAfter (Gen.list (Range.linear 0 10) ws) $ pure Asterisk)
        genName <*>
      pure (Compose []) <*>
      genMaybeF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma)
          (genArgsListDoublestarArg cfg ws genName)) <*>
      pure ()
    ]
    [ ArgsListStarPart <$>
      genBeforeF
        (genAfter (Gen.list (Range.linear 0 10) ws) $ pure Asterisk)
        genName <*>
      genListF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma)
          (Gen.resize n $ genArgsListArg cfg ws genName gen)) <*>
      genMaybeF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma)
          (genArgsListDoublestarArg cfg ws genName)) <*>
      pure ()
    ]

genArgsListDoublestarArg
  :: MonadGen m
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (name ())
  -> m (ArgsListDoublestarArg ws name test ())
genArgsListDoublestarArg _ ws genName =
  ArgsListDoublestarArg <$>
  genBetween'F (Gen.list (Range.linear 0 10) ws) genName <*>
  pure ()

genArgsList
  :: ( MonadGen m
     , HasName name
     )
  => ExprConfig 'NotAssignable ctxt
  -> m ws
  -> m (name ())
  -> m (f ())
  -> m (ArgsList ws name f ())
genArgsList cfg ws genName gen = do
  n <- Size <$> Gen.integral_ (Range.linear 0 20)
  Gen.recursive
    Gen.choice
    [ Gen.just $
      fmap (review _ArgsListArgsKwargs) $
      (,) <$>
      (InR <$> genArgsListDoublestarArg cfg ws genName) <*>
      pure ()
    ]
    [ Gen.just $
      fmap (review _ArgsListAll) $
      (,,,) <$>
      Gen.resize n (genArgsListArg cfg ws genName gen) <*>
      genListF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma)
          (Gen.resize n $ genArgsListArg cfg ws genName gen)) <*>
      genMaybeF
        (genBeforeF
          (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma)
          (genMaybeF genStarOrDouble)) <*>
      pure ()
    , Gen.just $
      fmap (review _ArgsListArgsKwargs) $
      (,) <$>
      genStarOrDouble <*>
      pure ()
    ]
  where
    genStarOrDouble =
      Gen.choice
        [ Gen.small $ InL <$> genArgsListStarPart cfg ws genName gen
        , InR <$> genArgsListDoublestarArg cfg ws genName
        ]
