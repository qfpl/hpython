module Language.Python.Printer.IndentedLines where

import Papa
import Data.Functor.Compose
import Data.Separated.Before
import Text.PrettyPrint hiding ((<>))

import Language.Python.AST.IndentedLines
import Language.Python.Printer.Combinators
import Language.Python.Printer.Symbols

indentedLines
  :: (comment a -> [Doc -> Doc])
  -> (f a -> [Doc -> Doc])
  -> IndentedLines comment f a
  -> [Doc -> Doc]
indentedLines c f i =
  foldMapOf
    (_Wrapped.folded)
    (sumElim
      c
      (\(Compose (Before a b)) ->
         let a' = foldMap indentationChar a
         in (. (<> a')) <$> f b))
    (getIndentedLines i)
