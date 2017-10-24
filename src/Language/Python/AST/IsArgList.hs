{-# language TypeFamilies #-}
module Language.Python.AST.IsArgList where

import Papa

class IsArgList l where
  data KeywordArgument l
  data PositionalArgument l

  arguments :: l -> [Either (KeywordArgument l) (PositionalArgument l)]

data ArgumentError l
  = KeywordBeforePositional (KeywordArgument l)
  | DuplicateArguments

keywordBeforePositional :: IsArgList l => l -> Either (ArgumentError l) l
keywordBeforePositional l = go Nothing $ arguments l
  where
    go _ [] = Right l
    go keyword (a:as) =
      case a of
        Left k -> go (Just k) as
        Right _
          | Just k <- keyword -> Left $ KeywordBeforePositional k
          | otherwise -> go keyword as
