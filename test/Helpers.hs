module Helpers where

import Hedgehog

import Language.Python.Internal.Lexer
  (SrcInfo, nested, initialSrcInfo, IndentedLine, Nested
  )
import Language.Python.Internal.Parse (Parser, runParser)

doNested :: (Show a, Monad m)=> [IndentedLine a] -> PropertyT m (Nested a)
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
