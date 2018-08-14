module Helpers where

import Control.Monad ((<=<))
import Data.Text (Text)
import Data.Validate (Validate(..))

import Hedgehog

import Language.Python.Internal.Lexer
  (SrcInfo, LogicalLine, IndentedLine, Nested, tokenize, logicalLines, indentation, nested
  , initialSrcInfo)
import Language.Python.Internal.Parse (Parser, runParser)
import Language.Python.Internal.Token (PyToken)
import Language.Python.Validate.Syntax.Error (SyntaxError)

doTokenize :: Monad m => Text -> PropertyT m [PyToken SrcInfo]
doTokenize str = do
  let res = tokenize str
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doIndentation :: (Show a, Monad m) => [LogicalLine a] -> PropertyT m [IndentedLine a]
doIndentation lls = do
  let res = indentation lls
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doNested :: Monad m => [IndentedLine a] -> PropertyT m (Nested a)
doNested ils = do
  let res = nested ils
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doParse :: (Show ann, Monad m) => ann -> Parser ann a -> Nested ann -> PropertyT m a
doParse initial pa input = do
  let res = runParser initial pa input
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doParse' :: Monad m => Parser SrcInfo a -> Nested SrcInfo -> PropertyT m a
doParse' = doParse $ initialSrcInfo "test"

doFromIR
  :: (Show ann, Monad m, Show a)
  => (a -> Validate [SyntaxError v ann] b)
  -> a
  -> PropertyT m b
doFromIR f a =
  case f a of
    Failure errs -> do
      annotateShow a
      annotateShow errs
      failure
    Success a -> pure a

doToPython
  :: (Show ann, Monad m, Show a)
  => Parser SrcInfo a
  -> (a -> Validate [SyntaxError v ann] b)
  -> Text
  -> PropertyT m b
doToPython pa f =
  doFromIR f <=<
  doParse (initialSrcInfo "test") pa <=<
  doNested <=<
  doIndentation <=<
  pure . logicalLines <=<
  doTokenize
