{-# language DataKinds #-}
module Test.Language.Python.Gen.ArgList where

import Papa hiding (Sum)
import Data.Functor.Sum
import Hedgehog
import Language.Python.AST.ArgsList
import Language.Python.AST.Symbols
import Language.Python.IR.SyntaxConfig

import Test.Language.Python.Gen.Combinators

import qualified Hedgehog.Gen as Gen

genArgsListArg
  :: MonadGen m
  => SyntaxConfig 'NotAssignable ctxt
  -> m (name ())
  -> m (f ())
  -> m (ArgsListArg name f ())
genArgsListArg _ genName gen =
  ArgsListArg <$>
  genName <*>
  genMaybeF
    (genBeforeF
       (genBetweenWhitespace $ pure Equals)
       gen) <*>
  pure ()

genArgsListStarPart
  :: MonadGen m
  => SyntaxConfig 'NotAssignable ctxt
  -> m (name ())
  -> m (f ())
  -> m (ArgsListStarPart name f ())
genArgsListStarPart cfg genName gen =
  Gen.choice
    [ pure $ ArgsListStarPartEmpty ()
    , ArgsListStarPart <$>
      genBeforeF
        (genBetweenWhitespace $ pure Asterisk)
        genName <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure Comma)
          (Gen.small $ genArgsListArg cfg genName gen)) <*>
      genMaybeF
        (genBeforeF
          (genBetweenWhitespace $ pure Comma)
          (genArgsListDoublestarArg cfg genName)) <*>
      pure ()
    ]

genArgsListDoublestarArg
  :: MonadGen m
  => SyntaxConfig 'NotAssignable ctxt
  -> m (name ())
  -> m (ArgsListDoublestarArg name test ())
genArgsListDoublestarArg _ genName =
  ArgsListDoublestarArg <$>
  genBetweenWhitespaceF genName <*>
  pure ()

genArgsList
  :: ( HasName name
     , MonadGen m
     )
  => SyntaxConfig 'NotAssignable ctxt
  -> m (name ())
  -> m (f ())
  -> m (ArgsList name f ())
genArgsList cfg genName gen =
  Gen.choice
    [ Gen.small . Gen.just $
      fmap (review _ArgsListAll) $
      (,,,) <$>
      genArgsListArg cfg genName gen <*>
      genListF
        (genBeforeF
          (genBetweenWhitespace $ pure Comma)
          (genArgsListArg cfg genName gen)) <*>
      genMaybeF
        (genBeforeF
          (genBetweenWhitespace $ pure Comma)
          (genMaybeF genStarOrDouble)) <*>
      pure ()
    , Gen.small . Gen.just $
      fmap (review _ArgsListArgsKwargs) $
      (,) <$>
      genStarOrDouble <*>
      pure ()
    ]
  where
    genStarOrDouble =
      Gen.choice
        [ Gen.small $ InL <$> genArgsListStarPart cfg genName gen
        , InR <$> genArgsListDoublestarArg cfg genName
        ]
