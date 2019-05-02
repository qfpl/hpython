{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
module Language.Python.Parse.Error
  ( ParseError(..)
    -- * Classy Prisms
  , AsLexicalError(..), AsTabError(..), AsIncorrectDedent(..)
  , AsIRError(..), AsParseError(..)
    -- * Megaparsec re-exports
  , ErrorItem(..)
  , SourcePos(..)
  )
where

import Control.Lens.Prism (prism')
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty)
import Text.Megaparsec.Error (ErrorItem(..))
import Text.Megaparsec.Pos (SourcePos(..))

import Language.Python.Internal.Lexer
  (AsLexicalError(..), AsTabError(..), AsIncorrectDedent(..))
import Language.Python.Internal.Parse (AsParseError(..))
import Language.Python.Internal.Syntax.IR (AsIRError(..))
import Language.Python.Token (PyToken)

data ParseError a
  -- | An error occured during tokenization (this is a re-packed megaparsec error)
  = LexicalError
      (NonEmpty SourcePos)
      (Maybe (ErrorItem Char))
      (Set (ErrorItem Char))
  -- | An error occured during parsing (this is a re-packed megaparsec error)
  | ParseError
      (NonEmpty SourcePos)
      (Maybe (ErrorItem (PyToken a)))
      (Set (ErrorItem (PyToken a)))
  -- | Tabs and spaces were used inconsistently
  | TabError a
  -- | The dedent at the end of a block doesn't match and preceding indents
  --
  -- e.g.
  --
  -- @
  -- def a():
  --     if b:
  --         pass
  --     else:
  --         pass
  --   pass
  -- @
  --
  -- The final line will cause an 'IncorrectDedent' error
  | IncorrectDedent a
  -- | Unpacking ( @*value@ ) was used in an invalid position
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

instance AsIncorrectDedent (ParseError a) a where
  _IncorrectDedent =
    prism'
      IncorrectDedent
      (\case
          IncorrectDedent a -> Just a
          _ -> Nothing)

instance AsParseError (ParseError a) (PyToken a) where
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
