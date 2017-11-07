module Language.Python.Parser.Comment where

import Papa hiding (noneOf)
import Data.Text (pack)
import Text.Trifecta hiding (Unspaced(..))

import Language.Python.AST.Comment
import Language.Python.Parser.SrcInfo

import Text.Parser.Unspaced

comment :: DeltaParsing m => Unspaced m (Comment SrcInfo)
comment =
  annotated $
  Comment <$>
  fmap pack 
  (char '#' *>
   many (noneOf "\r\n"))
