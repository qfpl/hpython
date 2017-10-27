{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
module Language.Python.AST.IsArgList where

import Papa

import Data.Functor.Classes
import qualified Data.Set as S

class IsArgList l where
  type Name l
  data KeywordArgument l
  data DoublestarArgument l
  data PositionalArgument l

  keywordName :: KeywordArgument l -> Name l
  arguments :: l -> [Argument l]

data Argument l
  = KeywordArgument (KeywordArgument l)
  | DoublestarArgument (DoublestarArgument l)
  | PositionalArgument (PositionalArgument l)

data ArgumentError l
  = KeywordBeforePositional
  | DuplicateArguments

keywordBeforePositional
  :: IsArgList l
  => [Argument l]
  -> Either (ArgumentError l) [Argument l]
keywordBeforePositional l = go False l
  where
    go _ [] = Right l
    go keyword (a:as) =
      case a of
        KeywordArgument _ -> go True as
        DoublestarArgument _ -> go True as
        PositionalArgument _
          | keyword -> Left KeywordBeforePositional
          | otherwise -> go keyword as

duplicateArguments
  :: ( IsArgList l
     , Ord (Name l)
     )
  => [Argument l]
  -> Either (ArgumentError l) [Argument l]
duplicateArguments l = go S.empty l
  where
    go _ [] = Right l
    go seen (a:as) =
      case a of
        KeywordArgument k ->
          let name = keywordName k
          in
            if name `S.member` seen
            then Left DuplicateArguments
            else go (S.insert name seen) as
        _ -> go seen as

validateArgList
  :: ( IsArgList l
     , Ord (Name l)
     )
  => l
  -> Either (ArgumentError l) l
validateArgList l =
  (keywordBeforePositional (arguments l) >>=
   duplicateArguments)
  $> l
