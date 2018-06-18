{-# language MultiParamTypeClasses #-}
module Language.Python.Internal.Syntax.Keyword where

import Control.Lens.Lens (lens)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Language.Python.Internal.Render (RenderOutput, singleton)
import Language.Python.Internal.Syntax.Token
import Language.Python.Internal.Syntax.Whitespace
import Language.Python.Internal.Token (PyToken(..))

import qualified Data.List.NonEmpty as NonEmpty

data Keyword = Keyword (NonEmpty Char) [Whitespace]
  deriving (Eq, Show)

keyword :: Keyword -> RenderOutput
keyword (Keyword a _) =
  case toList a of
    "if" -> singleton (TkIf ())
    "else" -> singleton (TkElse ())
    "while" -> singleton (TkWhile ())
    "def" -> singleton (TkDef ())
    "return" -> singleton (TkReturn ())
    "pass" -> singleton (TkPass ())
    "break" -> singleton (TkBreak ())
    "continue" -> singleton (TkContinue ())
    "True" -> singleton (TkTrue ())
    "False" -> singleton (TkFalse ())
    "None" -> singleton (TkNone ())
    "or" -> singleton (TkOr ())
    "and" -> singleton (TkAnd ())
    "is" -> singleton (TkIs ())
    "not" -> singleton (TkNot ())
    "global" -> singleton (TkGlobal ())
    "nonlocal" -> singleton (TkNonlocal ())
    "defl" -> singleton (TkDel ())
    "import" -> singleton (TkImport ())
    "from" -> singleton (TkFrom ())
    "as" -> singleton (TkAs ())
    "raise" -> singleton (TkRaise ())
    "try" -> singleton (TkTry ())
    "except" -> singleton (TkExcept ())
    "finally" -> singleton (TkFinally ())
    "class" -> singleton (TkClass ())
    "for" -> singleton (TkFor ())
    "in" -> singleton (TkIn ())
    str -> singleton (TkIdent str ())

instance Token Keyword Keyword where
  unvalidate = id
  whitespaceAfter =
    lens
      (\(Keyword _ ws) -> ws)
      (\(Keyword a _) ws -> Keyword a ws)
  startChar (Keyword a _) = NonEmpty.head a
  endChar (Keyword a _) = NonEmpty.last a
