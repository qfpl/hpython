{-# language DeriveFunctor #-}
{-# language TemplateHaskell #-}
module Language.Python.Internal.Token where

import Data.Deriving (deriveEq1)

import Language.Python.Internal.Syntax.Numbers (IntLiteral(..), FloatLiteral(..))
import Language.Python.Internal.Syntax.Strings
  (StringPrefix(..), BytesPrefix(..), QuoteType(..), StringType(..), PyChar(..))
import Language.Python.Internal.Syntax.Whitespace (Newline(..))

data PyToken a
  = TkIf a
  | TkElse a
  | TkElif a
  | TkWhile a
  | TkAssert a
  | TkDef a
  | TkReturn a
  | TkPass a
  | TkBreak a
  | TkContinue a
  | TkTrue a
  | TkFalse a
  | TkNone a
  | TkOr a
  | TkAnd a
  | TkIs a
  | TkNot a
  | TkGlobal a
  | TkNonlocal a
  | TkDel a
  | TkLambda a
  | TkImport a
  | TkFrom a
  | TkAs a
  | TkRaise a
  | TkTry a
  | TkExcept a
  | TkFinally a
  | TkClass a
  | TkWith a
  | TkFor a
  | TkIn a
  | TkYield a
  | TkInt (IntLiteral a)
  | TkFloat (FloatLiteral a)
  | TkIdent String a
  | TkString (Maybe StringPrefix) QuoteType StringType [PyChar] a
  | TkBytes BytesPrefix QuoteType StringType [PyChar] a
  | TkSpace a
  | TkTab a
  | TkNewline Newline a
  | TkLeftBracket a
  | TkRightBracket a
  | TkLeftParen a
  | TkRightParen a
  | TkLeftBrace a
  | TkRightBrace a
  | TkLt a
  | TkLte a
  | TkEq a
  | TkDoubleEq a
  | TkBangEq a
  | TkGt a
  | TkGte a
  | TkContinued Newline a
  | TkColon a
  | TkSemicolon a
  | TkComma a
  | TkDot a
  | TkPlus a
  | TkMinus a
  | TkTilde a
  | TkComment String a
  | TkStar a
  | TkDoubleStar a
  | TkSlash a
  | TkDoubleSlash a
  | TkPercent a
  | TkShiftLeft a
  | TkShiftRight a
  | TkPlusEq a
  | TkMinusEq a
  | TkStarEq a
  | TkAtEq a
  | TkAt a
  | TkSlashEq a
  | TkPercentEq a
  | TkAmpersandEq a
  | TkPipeEq a
  | TkCaretEq a
  | TkShiftLeftEq a
  | TkShiftRightEq a
  | TkDoubleStarEq a
  | TkDoubleSlashEq a
  | TkPipe a
  | TkCaret a
  | TkAmpersand a
  deriving (Eq, Show, Functor)
deriveEq1 ''PyToken

pyTokenAnn :: PyToken a -> a
pyTokenAnn tk =
  case tk of
    TkPipe a -> a
    TkCaret a -> a
    TkAmpersand a -> a
    TkDef a -> a
    TkReturn a -> a
    TkPass a -> a
    TkBreak a -> a
    TkContinue a -> a
    TkTrue a -> a
    TkFalse a -> a
    TkNone a -> a
    TkOr a -> a
    TkAnd a -> a
    TkIs a -> a
    TkNot a -> a
    TkGlobal a -> a
    TkNonlocal a -> a
    TkDel a -> a
    TkLambda a -> a
    TkImport a -> a
    TkFrom a -> a
    TkAs a -> a
    TkRaise a -> a
    TkTry a -> a
    TkExcept a -> a
    TkFinally a -> a
    TkClass a -> a
    TkWith a -> a
    TkFor a -> a
    TkIn a -> a
    TkYield a -> a
    TkPlus a -> a
    TkMinus a -> a
    TkTilde a -> a
    TkIf a -> a
    TkElse a -> a
    TkElif a -> a
    TkWhile a -> a
    TkAssert a -> a
    TkInt a -> _intLiteralAnn a
    TkFloat a -> _floatLiteralAnn a
    TkIdent _ a -> a
    TkString _ _ _ _ a -> a
    TkBytes _ _ _ _ a -> a
    TkSpace a -> a
    TkTab a -> a
    TkNewline _ a -> a
    TkLeftBracket a -> a
    TkRightBracket a -> a
    TkLeftParen a -> a
    TkRightParen a -> a
    TkLeftBrace a -> a
    TkRightBrace a -> a
    TkLt a -> a
    TkLte a -> a
    TkEq a -> a
    TkDoubleEq a -> a
    TkBangEq a -> a
    TkGt a -> a
    TkGte a -> a
    TkContinued _ a -> a
    TkColon a -> a
    TkSemicolon a -> a
    TkComma a -> a
    TkDot a -> a
    TkComment _ a -> a
    TkStar a -> a
    TkDoubleStar a -> a
    TkSlash a -> a
    TkDoubleSlash a -> a
    TkPercent a -> a
    TkShiftLeft a -> a
    TkShiftRight a -> a
    TkPlusEq a -> a
    TkMinusEq a -> a
    TkStarEq a -> a
    TkAtEq a -> a
    TkAt a -> a
    TkSlashEq a -> a
    TkPercentEq a -> a
    TkAmpersandEq a -> a
    TkPipeEq a -> a
    TkCaretEq a -> a
    TkShiftLeftEq a -> a
    TkShiftRightEq a -> a
    TkDoubleStarEq a -> a
    TkDoubleSlashEq a -> a
