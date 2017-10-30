{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
module Language.Python.Parser.Indentation
  ( IndentationParsing(..)
  , IndentationParserT
  , runIndentationParserT
  , indented
  , absolute
  )
  where

import Papa
import Control.Monad.Reader
import Data.Functor.Compose
import Data.Separated.Before
import GHC.Natural
import Text.Trifecta
import Text.Parser.LookAhead

import Language.Python.AST.Symbols
import Language.Python.Parser.Symbols

class (LookAheadParsing m, CharParsing m, MonadPlus m) => IndentationParsing m where
  indentedF :: m (f a) -> m (Compose (Before (NonEmpty IndentationChar)) f a)
  absoluteF :: m (f a) -> m (Compose (Before (NonEmpty IndentationChar)) f a)

indented
  :: IndentationParsing m
  => m a
  -> m (Before (NonEmpty IndentationChar) a)
indented m = fmap getConst . getCompose <$> indentedF (Const <$> m)

absolute
  :: IndentationParsing m
  => m a
  -> m (Before (NonEmpty IndentationChar) a)
absolute m = fmap getConst . getCompose <$> absoluteF (Const <$> m)

newtype IndentationParserT m a
  = IndentationParserT
  { runIndentationParserT' :: ReaderT (NonEmpty Natural) m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadPlus, Alternative)

runIndentationParserT :: IndentationParserT m a -> NonEmpty Natural -> m a
runIndentationParserT m = runReaderT (runIndentationParserT' m)

deriving instance (MonadPlus m, Parsing m) => Parsing (IndentationParserT m)
deriving instance (MonadPlus m, CharParsing m) => CharParsing (IndentationParserT m)
deriving instance (MonadPlus m, TokenParsing m) => TokenParsing (IndentationParserT m)
deriving instance (MonadPlus m, LookAheadParsing m) => LookAheadParsing (IndentationParserT m)
deriving instance DeltaParsing m => DeltaParsing (IndentationParserT m)

indentLevel :: NonEmpty IndentationChar -> Natural
indentLevel (i :| is) = go 0 (i:is)
  where
    go level [] = level
    go level (c:cs) =
      case c of
        IndentSpace -> go (level+1) cs
        IndentTab -> go (level + 8 - (level `mod` 8)) cs
        IndentContinued _ _ -> level

instance (LookAheadParsing m, CharParsing m, MonadPlus m) => IndentationParsing (IndentationParserT m) where
  indentedF m = IndentationParserT $ do
    (previousIndentLevel :| _) <- ask
    someIndent <-
      (lookAhead $ optional formFeed *> some1 indentationChar) <?>
      ("indent to indentation level greater than " <> show previousIndentLevel)
    let currentIndentLevel = indentLevel someIndent
    case compare currentIndentLevel previousIndentLevel of
      LT ->
        unexpected $
        "dedent to indentation level " <> show currentIndentLevel
      EQ ->
        unexpected $
        "indentation level " <> show currentIndentLevel
      GT ->
        local (pure currentIndentLevel <>) $
        Compose . Before someIndent <$> runIndentationParserT' m

  absoluteF m = IndentationParserT $ do
    (previousIndentLevel :| _) <- ask
    someIndent <-
      (optional formFeed *> some1 indentationChar) <?>
      ("indentation level " <> show previousIndentLevel)
    let currentIndentLevel = indentLevel someIndent
    case compare currentIndentLevel previousIndentLevel of
      LT ->
        unexpected $
        "dedent to indentation level " <> show currentIndentLevel
      GT ->
        unexpected $
        "indent to indentation level " <> show currentIndentLevel
      EQ -> Compose . Before someIndent <$> runIndentationParserT' m
