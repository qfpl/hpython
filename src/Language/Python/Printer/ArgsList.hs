{-# language RankNTypes #-}
module Language.Python.Printer.ArgsList where

import Papa
import Text.PrettyPrint hiding ((<>), comma, colon)

import Language.Python.AST.ArgsList

argsListArg
  :: (forall x. name x -> Doc)
  -> (forall x. f x -> Doc)
  -> ArgsListArg name f a
  -> Doc
argsListArg renderName f (ArgsListArg l r _) =
  renderName l <>
  foldMapF (beforeF (betweenWhitespace' . const $ char '=') f) r

argsListStarPart
  :: (forall x. name x -> Doc)
  -> (forall x. f x -> Doc)
  -> ArgsListStarPart name f a
  -> Doc
argsListStarPart renderName f e =
  case e of
    ArgsListStarPartEmpty _ -> mempty
    ArgsListStarPart h t r _ ->
      beforeF (betweenWhitespace' . const $ text "*") renderName h <>
      foldMapF (beforeF (betweenWhitespace' comma) $ argsListArg f) t <>
      foldMapF (beforeF (betweenWhitespace' comma) argsListDoublestarArg) r

argsListDoublestarArg
  :: (forall x. name x -> Doc)
  -> ArgsListDoublestarArg name test a
  -> Doc
argsListDoublestarArg renderName (ArgsListDoublestarArg a _) =
  text "**" <>
  betweenWhitespace'F renderName a

argsList
  :: (forall x. name x -> Doc)
  -> (forall x. f x -> Doc)
  -> ArgsList name f a
  -> Doc
argsList renderName f e =
  Just e &
    (outside _ArgsListAll .~
       (\(h, t, r, _) ->
         argsListArg f h <>
         foldMapF (beforeF (betweenWhitespace' comma) $ argsListArg f) t <>
         foldMapF
           (beforeF
             (betweenWhitespace' comma)
             (foldMapF $ starOrDouble f)) r) $
     outside _ArgsListArgsKwargs .~
       (\(a, _) -> starOrDouble f a) $
     error "incomplete pattern")
  where
    starOrDouble
      :: (forall x. f x -> Doc)
      -> Sum (ArgsListStarPart f) (ArgsListDoublestarArg f) a
      -> Doc
    starOrDouble f' =
      sumElim
        (argsListStarPart f')
        argsListDoublestarArg
