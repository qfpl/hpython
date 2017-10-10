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
     , Functor test
     , Functor starExpr
     )
  => Unspaced m (test SrcInfo)
  -> Unspaced m (starExpr SrcInfo)
  -> Unspaced m (TestlistStarExpr test starExpr SrcInfo)
testlistStarExpr test starExpr=
  annotated $
  TestlistStarExpr <$>
  testOrStar <*>
  manyF (try $ beforeF (betweenWhitespace comma) testOrStar) <*>
  optional (try $ betweenWhitespace comma)
  where
    testOrStar = (InL <$> try test) <|> (InR <$> starExpr)
