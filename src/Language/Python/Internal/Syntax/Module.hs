{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Python.Internal.Syntax.Module where

import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.Statement
import Language.Python.Internal.Syntax.Whitespace

data Module v a
  = ModuleEmpty
  | ModuleBlankFinal a [Whitespace] (Maybe (Comment a))
  | ModuleBlank a [Whitespace] (Maybe (Comment a)) Newline (Module v a)
  | ModuleStatement (Statement v a) (Module v a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasStatements Module where
  _Statements f = go
    where
      go ModuleEmpty = pure ModuleEmpty
      go (ModuleBlankFinal a b c) = pure $ ModuleBlankFinal a b c
      go (ModuleBlank a b c d e) = ModuleBlank a b c d <$> go e
      go (ModuleStatement a b) = ModuleStatement <$> f a <*> go b
