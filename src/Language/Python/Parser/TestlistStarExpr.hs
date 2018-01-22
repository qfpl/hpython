{-# language RankNTypes #-}
module Language.Python.Parser.TestlistStarExpr where

import Papa
import Data.Functor.Sum
import Text.Parser.LookAhead
import Text.Trifecta hiding (Unspaced, comma)

import Language.Python.IR.TestlistStarExpr
import Language.Python.Parser.Combinators
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols

import Text.Parser.Unspaced

testlistStarExpr
  :: ( LookAheadParsing m
     , DeltaParsing m
     , Functor (test ws)
     , Functor (starExpr ws)
     )
  => Unspaced m ws
  -> (forall ws'. Unspaced m ws' -> Unspaced m (test ws' SrcInfo))
  -> (forall ws'. Unspaced m ws' -> Unspaced m (starExpr ws' SrcInfo))
  -> Unspaced m (TestlistStarExpr ws test starExpr SrcInfo)
testlistStarExpr ws test starExpr =
  annotated $
  TestlistStarExpr <$>
  testOrStar <*>
  manyF
    (beforeF
      (try $
       between' (many ws) comma <*
       notFollowedBy (void newlineChar <|> void (char '=') <|> void (char ';') <|> void (char '#')))
      testOrStar) <*>
  optional (try $ between' (many ws) comma)
  where
    testOrStar = (InL <$> test ws) <|> (InR <$> starExpr ws)
