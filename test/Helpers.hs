module Helpers where

import Data.Semigroup (Semigroup)
import Data.Text (Text)
import Hedgehog

import Language.Python.Internal.Lexer
  (SrcInfo, insertTabs, tokenize
  )
import Language.Python.Internal.Token (PyToken)
import Language.Python.Internal.Parse (Parser, runParser)

doTokenize :: Monad m => Text -> PropertyT m [PyToken SrcInfo]
doTokenize input =
  case tokenize "test" input of
    Left err -> annotateShow err *> failure
    Right a -> pure a

doTabs
  :: (Semigroup ann, Show ann, Monad m)
  => ann
  -> [PyToken ann]
  -> PropertyT m [PyToken ann]
doTabs ann input =
  case insertTabs ann input of
    Left err -> annotateShow err *> failure
    Right a -> pure a

doParse :: Monad m => Parser a -> [PyToken SrcInfo] -> PropertyT m a
doParse pa input = do
  let res = runParser "test" pa input
  case res of
    Left err -> do
      annotateShow err
      failure
    Right a -> pure a
