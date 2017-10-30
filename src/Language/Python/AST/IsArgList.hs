{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
module Language.Python.AST.IsArgList where

import Papa
import Data.Text (Text)

import qualified Data.Set as S

class HasName s where
  name :: Lens' (s a) Text

class IsArgList l where
  data KeywordArgument l
  data DoublestarArgument l
  data PositionalArgument l

  argumentName :: Argument l -> Maybe Text
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
  :: IsArgList l
  => [Text]
  -> Either (ArgumentError l) [Text]
duplicateArguments l
  | length l /= length (S.fromList l)
  = Left DuplicateArguments
  | otherwise = Right l

validateArgList
  :: IsArgList l
  => l
  -> Either (ArgumentError l) l
validateArgList l =
  let
    l' = arguments l
  in
    keywordBeforePositional l' *>
    duplicateArguments (catMaybes $ fmap argumentName l') $>
    l
