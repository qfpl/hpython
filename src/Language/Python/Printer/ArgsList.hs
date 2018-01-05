module Language.Python.Printer.ArgsList where

import Prelude (error)
import Papa hiding (Sum)
import Data.Functor.Sum
import Text.PrettyPrint hiding ((<>), comma, colon)

import Language.Python.AST.IsArgList
import Language.Python.AST.ArgsList
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols

argsListArg
  :: (ws -> Doc)
  -> (name a -> Doc)
  -> (f a -> Doc)
  -> ArgsListArg ws name f a
  -> Doc
argsListArg ws renderName f (ArgsListArg l r _) =
  renderName l <>
  foldMapF
    (beforeF (between' (foldMap ws) . const $ char '=') f)
    r

argsListStarPart
  :: (ws -> Doc)
  -> (name a -> Doc)
  -> (f a -> Doc)
  -> ArgsListStarPart ws name f a
  -> Doc
argsListStarPart ws renderName f e =
  case e of
    ArgsListStarPartEmpty _ -> mempty
    ArgsListStarPart h t r _ ->
      beforeF
        (after (foldMap ws) . const $ text "*")
        renderName
        h <>
      foldMapF
        (beforeF
          (between' (foldMap ws) comma)
          (argsListArg ws renderName f)) t <>
      foldMapF
        (beforeF
          (between' (foldMap ws) comma)
          (argsListDoublestarArg ws renderName)) r

argsListDoublestarArg
  :: (ws -> Doc)
  -> (name a -> Doc)
  -> ArgsListDoublestarArg ws name test a
  -> Doc
argsListDoublestarArg ws renderName (ArgsListDoublestarArg a _) =
  beforeF (after (foldMap ws) doubleAsterisk) renderName a

argsList
  :: HasName name
  => (ws -> Doc)
  -> (name a -> Doc)
  -> (f a -> Doc)
  -> ArgsList ws name f a
  -> Doc
argsList ws renderName f e =
  Just e &
    (outside _ArgsListAll .~
       (\(h, t, r, _) ->
         argsListArg ws renderName f h <>
         foldMapF
           (beforeF
             (between' (foldMap ws) comma)
             (argsListArg ws renderName f)) t <>
         foldMapF
           (beforeF
             (between' (foldMap ws) comma)
             (foldMapF $ starOrDouble ws renderName f)) r) $
     outside _ArgsListArgsKwargs .~
       (\(a, _) -> starOrDouble ws renderName f a) $
     error "incomplete pattern")
  where
    starOrDouble
      :: (ws -> Doc)
      -> (name a -> Doc)
      -> (f a -> Doc)
      -> Sum (ArgsListStarPart ws name f) (ArgsListDoublestarArg ws name f) a
      -> Doc
    starOrDouble ws' renderName' f' =
      sumElim
        (argsListStarPart ws' renderName' f')
        (argsListDoublestarArg ws' renderName')
