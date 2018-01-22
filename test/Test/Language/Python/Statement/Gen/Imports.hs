module Test.Language.Python.Statement.Gen.Imports where

import Papa

import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Between 
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Python.AST.Keywords
import Language.Python.AST.Symbols
import Language.Python.Statement.AST.Imports

import Test.Language.Python.Gen.Combinators
import Test.Language.Python.Gen.DottedName
import Test.Language.Python.Gen.Identifier

genImportStatement
  :: MonadGen m
  => m (ImportStatement ())
genImportStatement =
  Gen.choice
    [ ImportStatementName <$> genImportName <*> pure ()
    , ImportStatementFrom <$> genImportFrom <*> pure ()
    ]

genDottedAsName
  :: MonadGen m
  => m (DottedAsName ())
genDottedAsName =
  DottedAsName <$>
  genDottedName <*>
  genMaybeF (genBeforeF (genBetweenWhitespace1 $ pure KAs) genIdentifier) <*>
  pure ()

genDottedAsNames
  :: MonadGen m
  => m (DottedAsNames ())
genDottedAsNames =
  DottedAsNames <$>
  genDottedAsName <*>
  genListF
    (genBeforeF (genBetweenWhitespace $ pure Comma) genDottedAsName) <*>
  pure ()

genImportAsName
  :: MonadGen m
  => m ws
  -> m (ImportAsName ws ())
genImportAsName ws =
  ImportAsName <$>
  genIdentifier <*>
  genMaybeF
    (genBeforeF
      (genBetween'1 ws $ pure KAs)
      genIdentifier) <*>
  pure ()

genImportAsNames
  :: MonadGen m
  => m ws
  -> m (ImportAsNames ws ())
genImportAsNames ws =
  ImportAsNames <$>
  genImportAsName ws <*>
  genListF
    (genBeforeF
      (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma)
      (genImportAsName ws)) <*>
  Gen.maybe (genBetween' (Gen.list (Range.linear 0 10) ws) $ pure Comma) <*>
  pure ()

genImportName
  :: MonadGen m
  => m (ImportName ())
genImportName =
  ImportName <$>
  genWhitespaceBefore1F genDottedAsNames <*>
  pure ()

genImportFrom
  :: MonadGen m
  => m (ImportFrom ())
genImportFrom =
  ImportFrom <$>
  Gen.choice
    [ fmap InL .
      genWhitespaceAfter1F $
        Gen.choice
        [ InL <$> genWhitespaceBefore1F genDottedName
        , InR <$>
          genBeforeF
            (Gen.nonEmpty
              (Range.linear 1 10)
              (genBetweenWhitespace $ Gen.element [Left Dot, Right Ellipsis]))
            genDottedName
        ]
    , InR <$>
      genBetweenWhitespaceF
      (Const <$>
       Gen.nonEmpty
         (Range.linear 1 10)
         (Gen.element [Left Dot, Right Ellipsis]))
    ] <*>
  Gen.choice
    [ fmap InL .
      genWhitespaceBeforeF $
        Gen.choice
        [ pure . InL $ Const Asterisk
        , fmap (InR . Compose) $
          Between LeftParen <$>
          genBetween'F
            (Gen.list (Range.linear 0 10) genAnyWhitespaceChar)
            (genImportAsNames genAnyWhitespaceChar) <*>
          pure RightParen
        ]
    , InR <$> genWhitespaceBefore1F (genImportAsNames genWhitespaceChar)
    ] <*>
  pure ()
