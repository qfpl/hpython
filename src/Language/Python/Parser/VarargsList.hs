module Language.Python.Parser.VarargsList where

import Papa
import Data.Functor.Sum
import Text.Parser.LookAhead
import Text.Trifecta hiding (comma)

import Language.Python.AST.VarargsList
import Language.Python.Parser.Combinators
import Language.Python.Parser.Identifier
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols

varargsListArg
  :: ( DeltaParsing m
     , LookAheadParsing m
     , Functor f
     )
  => m (f SrcInfo)
  -> m (VarargsListArg f SrcInfo)
varargsListArg p =
  annotated $
  VarargsListArg <$>
  identifier <*>
  optionalF (beforeF (betweenWhitespace equals) p)

varargsListStarPart
  :: ( DeltaParsing m
     , LookAheadParsing m
     , Functor f
     )
  => m (f SrcInfo)
  -> m (VarargsListStarPart f SrcInfo)
varargsListStarPart p =
  annotated $
  VarargsListStarPart <$>
  beforeF (betweenWhitespace asterisk) (optionalF identifier) <*>
  manyF (beforeF (betweenWhitespace comma) (varargsListArg p)) <*>
  optionalF (beforeF (betweenWhitespace comma) varargsListDoublestarArg)

varargsListDoublestarArg
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => m (VarargsListDoublestarArg SrcInfo)
varargsListDoublestarArg =
  annotated $
  VarargsListDoublestarArg <$>
  (doubleAsterisk *>
   betweenWhitespaceF identifier)

varargsList
  :: ( DeltaParsing m
     , LookAheadParsing m
     , Functor f
     )
  => m (f SrcInfo)
  -> m (VarargsList f SrcInfo)
varargsList p = try varargsListAll <|> varargsListArgsKwargs
  where
    varargsListAll =
      annotated $
      VarargsListAll <$>
      varargsListArg p <*>
      manyF (beforeF (betweenWhitespace comma) (varargsListArg p)) <*>
      optionalF (beforeF (betweenWhitespace comma) $ optionalF starOrDouble)

    varargsListArgsKwargs =
      annotated $
      VarargsListArgsKwargs <$>
      starOrDouble

    starOrDouble = 
      (InL <$> try (varargsListStarPart p)) <|>
      (InR <$> varargsListDoublestarArg)
