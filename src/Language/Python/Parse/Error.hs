{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
module Language.Python.Parse.Error
  ( ParseError(..)
    -- * Classy Prisms
  , AsLexicalError(..), AsTabError(..), AsIRError(..), AsParseError(..)
    -- * Megaparsec re-exports
  , ErrorItem(..)
  , SourcePos(..)
  )
where

import Control.Lens.Prism (prism')
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)
import Text.Megaparsec.Error (ErrorItem(..))
import Text.Megaparsec.Pos (SourcePos(..))

import Language.Python.Internal.Lexer (AsLexicalError(..), AsTabError(..))
import Language.Python.Internal.Parse (AsParseError(..))
import Language.Python.Internal.Syntax.IR (AsIRError(..))
import Language.Python.Internal.Token (PyToken)

data ParseError a
  = LexicalError
      (NonEmpty SourcePos)
      (Maybe (ErrorItem Char))
      (Set (ErrorItem Char))
  | ParseError
      (NonEmpty SourcePos)
      (Maybe (ErrorItem (PyToken a)))
      (Set (ErrorItem (PyToken a)))
  | TabError a
  | IncorrectDedent a
  | ExpectedDedent a
  | InvalidUnpacking a
  deriving (Eq, Show)

instance AsLexicalError (ParseError a) Char where
  _LexicalError =
    prism'
      (\(a, b, c) -> LexicalError a b c)
      (\case
          LexicalError a b c -> Just (a, b ,c)
          _ -> Nothing)

instance AsTabError (ParseError a) a where
  _TabError =
    prism'
      TabError
      (\case
          TabError a -> Just a
          _ -> Nothing)
  _IncorrectDedent =
    prism'
      IncorrectDedent
      (\case
          IncorrectDedent a -> Just a
          _ -> Nothing)

instance AsParseError (ParseError a) (PyToken a) Void where
  _ParseError =
    prism'
      (\(a, b, c) -> ParseError a b c)
      (\case
          ParseError a b c -> Just (a, b ,c)
          _ -> Nothing)

instance AsIRError (ParseError a) a where
  _InvalidUnpacking =
    prism'
      InvalidUnpacking
      (\case
          InvalidUnpacking a -> Just a
          _ -> Nothing)
