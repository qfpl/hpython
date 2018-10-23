{-|
Module      : Language.Python.Internal.Syntax
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Syntax
  ( reservedWords
  , idStyle
  , reserved
  , module Language.Python.Internal.Syntax.AugAssign
  , module Language.Python.Internal.Syntax.BinOp
  , module Language.Python.Internal.Syntax.CommaSep
  , module Language.Python.Internal.Syntax.Comment
  , module Language.Python.Internal.Syntax.Expr
  , module Language.Python.Internal.Syntax.Ident
  , module Language.Python.Internal.Syntax.Import
  , module Language.Python.Internal.Syntax.Module
  , module Language.Python.Internal.Syntax.ModuleNames
  , module Language.Python.Internal.Syntax.Numbers
  , module Language.Python.Internal.Syntax.Statement
  , module Language.Python.Internal.Syntax.Strings
  , module Language.Python.Internal.Syntax.UnOp
  , module Language.Python.Internal.Syntax.Whitespace
  )
where

import Control.Applicative ((<|>))
import GHC.Exts (fromList)
import Text.Parser.Char (CharParsing, char, letter, digit)
import Text.Parser.Token (TokenParsing, IdentifierStyle(..), Unspaced(..), reserve)
import Text.Parser.Token.Highlight (Highlight(..))

import Language.Python.Internal.Syntax.AugAssign
import Language.Python.Internal.Syntax.BinOp
import Language.Python.Internal.Syntax.CommaSep
import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.Expr
import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Import
import Language.Python.Internal.Syntax.Module
import Language.Python.Internal.Syntax.ModuleNames
import Language.Python.Internal.Syntax.Numbers
import Language.Python.Internal.Syntax.Statement
import Language.Python.Internal.Syntax.Strings
import Language.Python.Internal.Syntax.UnOp
import Language.Python.Internal.Syntax.Whitespace

reservedWords :: [String]
reservedWords =
  [ "False"
  , "class"
  , "finally"
  , "is"
  , "return"
  , "None"
  , "continue"
  , "for"
  , "lambda"
  , "try"
  , "True"
  , "def"
  , "from"
  , "nonlocal"
  , "while"
  , "and"
  , "del"
  , "global"
  , "not"
  , "with"
  , "as"
  , "elif"
  , "if"
  , "or"
  , "yield"
  , "assert"
  , "else"
  , "import"
  , "pass"
  , "break"
  , "except"
  , "in"
  , "raise"
  ]

idStyle :: CharParsing m => IdentifierStyle (Unspaced m)
idStyle =
  IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = letter <|> char '_'
  , _styleLetter = letter <|> char '_' <|> digit
  , _styleReserved = fromList reservedWords
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved s = runUnspaced $ reserve idStyle s
