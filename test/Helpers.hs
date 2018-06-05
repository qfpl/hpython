module Helpers where

import Control.Monad ((<=<))
import qualified Text.Trifecta as Trifecta

import Hedgehog

import Language.Python.Internal.Lexer
  (PyToken, LogicalLine, IndentedLine, Nested, tokenize, logicalLines, indentation, nested)
import Language.Python.Internal.Parse (Parser, runParser)

doTokenize :: Monad m => String -> PropertyT m [PyToken Trifecta.Caret]
doTokenize str = do
  let res = tokenize str
  case res of
    Trifecta.Failure err -> do
      annotateShow err
      failure
    Trifecta.Success a -> pure a

doIndentation :: Monad m => [LogicalLine a] -> PropertyT m [IndentedLine a]
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

doParse :: (Show ann, Monad m) => Parser ann a -> Nested ann -> PropertyT m a
doParse pa input = do
  let res = runParser pa input
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a

doToPython :: Monad m => Parser Trifecta.Caret a -> String -> PropertyT m a
doToPython pa =
  doParse pa <=<
  doNested <=<
  doIndentation <=<
  pure . logicalLines <=<
  doTokenize
