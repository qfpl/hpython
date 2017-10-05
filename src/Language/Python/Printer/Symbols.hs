module Language.Python.Printer.Symbols where

import Papa hiding (Plus, Space)
import Text.PrettyPrint hiding ((<>), comma, colon)

import Language.Python.AST.Symbols

leftParen :: LeftParen -> Doc
leftParen _ = char '('

rightParen :: RightParen -> Doc
rightParen _ = char ')'

plus :: Plus -> Doc
plus _ = char '+'

minus :: Minus -> Doc
minus _ = char '-'

ampersand :: Ampersand -> Doc
ampersand _ = char '&'

doubleLT :: DoubleLT -> Doc
doubleLT _ = text "<<"

doubleGT :: DoubleGT -> Doc
doubleGT _ = text ">>"

comma :: Comma -> Doc
comma _ = char ','

caret :: Caret -> Doc
caret _ = char '^'

pipe :: Pipe -> Doc
pipe _ = char '|'

colon :: Colon -> Doc
colon _ = char ':'

newlineChar :: NewlineChar -> Doc
newlineChar n =
  case n of
    CR -> char '\r'
    LF -> char '\n'
    CRLF -> text "\r\n"

whitespaceChar :: WhitespaceChar -> Doc
whitespaceChar w =
  case w of
    Space -> char ' '
    Tab -> char '\t'
    Continued nl -> char '\\' <> newlineChar nl

asterisk :: Asterisk -> Doc
asterisk _ = char '*'

doubleAsterisk :: DoubleAsterisk -> Doc
doubleAsterisk _ = text "**"

semicolon :: Semicolon -> Doc
semicolon _ = text ";"
