{-# language MultiParamTypeClasses #-}
module Language.Python.Internal.Syntax.Keyword where

import Control.Lens.Lens (lens)
import Data.List.NonEmpty (NonEmpty(..))
import Language.Python.Internal.Syntax.Token
import Language.Python.Internal.Syntax.Whitespace

import qualified Data.List.NonEmpty as NonEmpty

data Keyword = Keyword (NonEmpty Char) [Whitespace]
  deriving (Eq, Show)

keyword :: Keyword -> String
keyword (Keyword a _) = NonEmpty.toList a

instance Token Keyword Keyword where
  unvalidate = id
  whitespaceAfter =
    lens
      (\(Keyword _ ws) -> ws)
      (\(Keyword a _) ws -> Keyword a ws)
  startChar (Keyword a _) = NonEmpty.head a
  endChar (Keyword a _) = NonEmpty.last a
