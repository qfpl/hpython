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
-- import Data.Set (Set)
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Error (ErrorItem(..))
import Text.Megaparsec.Pos (SourcePos(..))

import Language.Python.Internal.Lexer
  (AsLexicalError(..), AsTabError(..), AsIncorrectDedent(..))
import Language.Python.Internal.Parse (AsParseError(..), PyTokens())
import Language.Python.Internal.Syntax.IR (AsIRError(..))
-- import Language.Python.Internal.Token (PyToken)

data ParseError a
  -- | An error occured during tokenization (this is a re-packed megaparsec error)
  = LexicalError
      (Megaparsec.ParseErrorBundle Text.Text Void)
  -- | An error occured during parsing (this is a re-packed megaparsec error)
  | ParseError
      (Megaparsec.ParseErrorBundle PyTokens Void)
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



instance AsLexicalError (ParseError a) Text.Text where
  _LexicalError =
    prism'
      LexicalError
      (\case
          LexicalError bundle -> Just bundle
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

instance AsParseError (ParseError a) PyTokens where
  _ParseError =
    prism'
      ParseError
      (\case
          ParseError bundle -> Just bundle
          _ -> Nothing)

instance AsIRError (ParseError a) a where
  _InvalidUnpacking =
    prism'
      InvalidUnpacking
      (\case
          InvalidUnpacking a -> Just a
          _ -> Nothing)
