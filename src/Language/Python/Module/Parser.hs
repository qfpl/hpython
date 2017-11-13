module Language.Python.Module.Parser where

import Papa
import Data.Functor.Sum
import Text.Parser.LookAhead
import Text.Trifecta hiding (Unspaced)

import Text.Parser.Unspaced

import Language.Python.Module.IR
import Language.Python.Parser.Combinators
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols
import Language.Python.Statement.Parser

module' :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Module SrcInfo)
module' =
  annotated $
  Module <$>
  manyF ((InL . Const <$> newlineChar) <|> (InR <$> statement))
