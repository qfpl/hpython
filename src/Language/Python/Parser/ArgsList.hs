module Language.Python.Parser.ArgsList where

import Papa
import Data.Functor.Sum
import Text.Parser.LookAhead
import Text.Trifecta hiding (Unspaced(..), comma)

import Language.Python.AST.ArgsList hiding (ArgsList)
import Language.Python.IR.ArgsList
import Language.Python.Parser.Combinators
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols

import Text.Parser.Unspaced

argsListArg
  :: ( DeltaParsing m
     , LookAheadParsing m
     , Functor f
     , Functor name
     )
  => Unspaced m (f SrcInfo)
  -> Unspaced m (name SrcInfo)
  -> Unspaced m (ArgsListArg name f SrcInfo)
argsListArg p pname =
  annotated $
  ArgsListArg <$>
  pname <*>
  optionalF (beforeF (betweenWhitespace equals) p)

argsListStarPart
  :: ( DeltaParsing m
     , LookAheadParsing m
     , Functor f
     , Functor name
     )
  => Unspaced m (f SrcInfo)
  -> Unspaced m (name SrcInfo)
  -> Unspaced m (ArgsListStarPart name f SrcInfo)
argsListStarPart p pname =
  annotated $
  try argsListStarPartSome <|>
  pure ArgsListStarPartEmpty
  where
    argsListStarPartSome =
      ArgsListStarPart <$>
      beforeF (betweenWhitespace asterisk) pname <*>
      manyF (beforeF (betweenWhitespace comma) (argsListArg p pname)) <*>
      optionalF
        (beforeF
          (betweenWhitespace comma)
          (argsListDoublestarArg pname))

argsListDoublestarArg
  :: ( DeltaParsing m
     , LookAheadParsing m
     , Functor name
     )
  => Unspaced m (name SrcInfo)
  -> Unspaced m (ArgsListDoublestarArg name test SrcInfo)
argsListDoublestarArg pname =
  annotated $
  ArgsListDoublestarArg <$>
  (doubleAsterisk *>
   betweenWhitespaceF pname)

argsList
  :: ( DeltaParsing m
     , LookAheadParsing m
     , Functor f
     , Functor name
     )
  => Unspaced m (f SrcInfo)
  -> Unspaced m (name SrcInfo)
  -> Unspaced m (ArgsList name f SrcInfo)
argsList p pname = try argsListAll <|> argsListArgsKwargs
  where
    argsListAll =
      annotated $
      ArgsListAll <$>
      argsListArg p pname <*>
      manyF (beforeF (betweenWhitespace comma) (argsListArg p pname)) <*>
      optionalF (beforeF (betweenWhitespace comma) $ optionalF starOrDouble)

    argsListArgsKwargs =
      annotated $
      ArgsListArgsKwargs <$>
      starOrDouble

    starOrDouble = 
      (InL <$> try (argsListStarPart p pname)) <|>
      (InR <$> argsListDoublestarArg pname)
