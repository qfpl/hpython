{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}

{-|
Module      : Language.Python.Internal.Render
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Render
  ( -- * Common Functions
    showModule, showStatement, showExpr
    -- * Rendering
  , RenderOutput, showRenderOutput, singleton
  , renderModule, renderStatement, renderExpr
    -- * Miscellany
  , showQuoteType, showStringPrefix, showBytesPrefix, showToken, showTokens
  , expandIndents, whitespaceTokens, commentTokens
  , parens, braces, brackets
  , renderWhitespace, renderCommaSep, renderCommaSep1, renderCommaSep1'
  , renderIdent, renderComment, renderModuleName, renderDot, renderRelativeModuleName
  , renderImportAs, renderImportTargets, renderSimpleStatement, renderCompoundStatement
  , renderBlock, renderIndent, renderIndents, renderExceptAs, renderArg, renderParam
  , renderParams, renderCompFor, renderCompIf, renderComprehension, renderBinOp, renderUnOp
  , renderSubscriptItem, renderPyChars, escapeChars, intToHex
  )
where

import Control.Lens.Cons (_init, _last)
import Control.Lens.Fold ((^..), folded, traverseOf_)
import Control.Lens.Getter ((^.))
import Control.Lens.Review ((#))
import Control.Lens.Setter ((.~))
import Control.Monad.Writer.Strict (Writer, execWriter, writer)
import Control.Monad.Reader (ReaderT, runReaderT, local, ask)
import Data.Bifoldable (bitraverse_, bitraverse_)
import Data.Char (ord)
import Data.Digit.Char (charHeXaDeCiMaL, charOctal)
import Data.Digit.Hexadecimal.MixedCase (HeXDigit(..))
import Data.DList (DList)
import Data.Function ((&))
import Data.Foldable (toList, traverse_)
import Data.Maybe (isNothing)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)

import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import Data.VFix
import Data.VIdentity
import Language.Python.Internal.Render.Correction
import Language.Python.Syntax.AugAssign
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comment
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Import
import Language.Python.Syntax.Module
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace
import Language.Python.Token (PyToken(..))

-- | A 'RenderOutput' is an intermediate form used during rendering
-- with efficient concatenation
newtype RenderOutput a
  = RenderOutput
  { unRenderOutput
    :: ReaderT
         -- Is the thing we're rendering followed by an optional
         -- newline? (as opposed to mandatory newline)
         --
         -- This is because the AST may be missing critical newlines
         -- and we supply them during rendering
         Bool
         (Writer (DList (PyToken ())))
         a
  } deriving (Functor, Applicative, Monad)

-- | Treats the input as a terminating statement (does not cause additional newlines to
-- be inserted)
final :: RenderOutput a -> RenderOutput a
final = id

-- | Treats the input as a non-terminating statement (causes additional newlines to be
-- inserted)
notFinal :: RenderOutput a -> RenderOutput a
notFinal (RenderOutput a) = RenderOutput $ local (const False) a

-- | Are we inside a terminating or non-terminating context?
isFinal :: RenderOutput Bool
isFinal = RenderOutput ask

-- | Render a single token as a 'RenderOutput'
singleton :: PyToken () -> RenderOutput ()
singleton a = RenderOutput $ writer ((), DList.singleton a)

-- | Run a 'RenderOutput' to produce a final 'Text'.
--
-- These 'Text's should then not be appended any more. All appending should
-- be done during the 'RenderOutput' phase.
showRenderOutput :: RenderOutput a -> Text
showRenderOutput =
  Lazy.toStrict .
  Builder.toLazyText .
  foldMap (Builder.fromText . showToken) .
  correctSpaces showToken .
  correctNewlines .
  DList.toList .
  execWriter .
  flip runReaderT True .
  unRenderOutput

renderComment :: Comment a -> RenderOutput ()
renderComment = traverse_ singleton . commentTokens

commentTokens :: Comment a -> [PyToken ()]
commentTokens c = [TkComment $ () <$ c]

showComment :: Comment a -> Text
showComment (MkComment _ s) = Text.pack $ "#" <> s

between :: RenderOutput l -> RenderOutput r -> RenderOutput a -> RenderOutput a
between l r m = l *> m <* r

parens :: RenderOutput a -> RenderOutput a
parens = between (singleton $ TkLeftParen ()) (singleton $ TkRightParen ())

brackets :: RenderOutput a -> RenderOutput a
brackets = between (singleton $ TkLeftBracket ()) (singleton $ TkRightBracket ())

braces :: RenderOutput a -> RenderOutput a
braces = between (singleton $ TkLeftBrace ()) (singleton $ TkRightBrace ())

-- | Parenthesise a term, but put its trailing whitespace *outside* the parens
parensDistTWS
  :: HasTrailingWhitespace s
  => (s -> RenderOutput ())
  -> s -> RenderOutput ()
parensDistTWS f a = do
  parens $ f (a & trailingWhitespace .~ [])
  traverse_ renderWhitespace (a ^. trailingWhitespace)

parensTuple :: Expr v a -> RenderOutput ()
parensTuple e =
  case vout e of
    Tuple{} -> parensDistTWS renderExpr e
    _ -> renderExpr e

parensGenerator :: Expr v a -> RenderOutput ()
parensGenerator e =
  case vout e of
    Generator{} -> parensDistTWS renderExpr e
    _ -> renderExpr e

parensTupleGenerator :: Expr v a -> RenderOutput ()
parensTupleGenerator e =
  case vout e of
    Tuple{} -> parensDistTWS renderExpr e
    Generator{} -> parensDistTWS renderExpr e
    _ -> renderExpr e

escapeChars :: [(Char, Char)]
escapeChars =
  [ ('\\', '\\')
  , ('"', '"')
  , ('\a', 'a')
  , ('\b', 'b')
  , ('\f', 'f')
  , ('\n', 'n')
  , ('\r', 'r')
  , ('\t', 't')
  , ('\v', 'v')
  ]

intToHex :: Int -> Text
intToHex n = Text.pack $ go n []
  where
    go 0 = (++"0")
    go 1 = (++"1")
    go 2 = (++"2")
    go 3 = (++"3")
    go 4 = (++"4")
    go 5 = (++"5")
    go 6 = (++"6")
    go 7 = (++"7")
    go 8 = (++"8")
    go 9 = (++"9")
    go 10 = (++"A")
    go 11 = (++"B")
    go 12 = (++"C")
    go 13 = (++"D")
    go 14 = (++"E")
    go 15 = (++"F")
    go b = let (q, r) = quotRem b 16 in go r . go q

intToHexH :: Int -> [HeXDigit]
intToHexH n = go n []
  where
    go 0 = (++[HeXDigit0])
    go 1 = (++[HeXDigit1])
    go 2 = (++[HeXDigit2])
    go 3 = (++[HeXDigit3])
    go 4 = (++[HeXDigit4])
    go 5 = (++[HeXDigit5])
    go 6 = (++[HeXDigit6])
    go 7 = (++[HeXDigit7])
    go 8 = (++[HeXDigit8])
    go 9 = (++[HeXDigit9])
    go 10 = (++[HeXDigitA])
    go 11 = (++[HeXDigitB])
    go 12 = (++[HeXDigitC])
    go 13 = (++[HeXDigitD])
    go 14 = (++[HeXDigitE])
    go 15 = (++[HeXDigitF])
    go b = let (q, r) = quotRem b 16 in go r . go q

renderPyCharsWithCorrection
  :: (QuoteType -> StringType -> [PyChar] -> [PyChar])
  -> QuoteType
  -> StringType
  -> [PyChar] -> Text
renderPyCharsWithCorrection c qt st = Text.pack . go . c qt st
  where
    go s =
      case s of
        [] -> ""
        Char_newline : cs -> "\\newline" <> go cs
        Char_octal1 a : cs ->
          "\\" <>
          [charOctal # a] <>
          go cs
        Char_octal2 a b : cs ->
          "\\" <>
          [charOctal # a, charOctal # b] <>
          go cs
        Char_octal3 a b cc : cs ->
          "\\" <>
          [charOctal # a, charOctal # b, charOctal # cc] <>
          go cs
        Char_hex a b : cs ->
          "\\x" <> [charHeXaDeCiMaL # a, charHeXaDeCiMaL # b] <> go cs
        Char_uni16 a b cc d : cs ->
          "\\u" <>
          [ charHeXaDeCiMaL # a
          , charHeXaDeCiMaL # b
          , charHeXaDeCiMaL # cc
          , charHeXaDeCiMaL # d
          ] <>
          go cs
        Char_uni32 a b cc d e f g h : cs ->
          "\\u" <>
          [ charHeXaDeCiMaL # a
          , charHeXaDeCiMaL # b
          , charHeXaDeCiMaL # cc
          , charHeXaDeCiMaL # d
          , charHeXaDeCiMaL # e
          , charHeXaDeCiMaL # f
          , charHeXaDeCiMaL # g
          , charHeXaDeCiMaL # h
          ] <>
          go cs
        Char_esc_bslash : cs -> '\\' : '\\' : go cs
        Char_esc_singlequote : cs -> '\\' : '\'' : go cs
        Char_esc_doublequote : cs -> '\\' : '"' : go cs
        Char_esc_a : cs -> '\\' : 'a' : go cs
        Char_esc_b : cs -> '\\' : 'b' : go cs
        Char_esc_f : cs -> '\\' : 'f' : go cs
        Char_esc_n : cs -> '\\' : 'n' : go cs
        Char_esc_r : cs -> '\\' : 'r' : go cs
        Char_esc_t : cs -> '\\' : 't' : go cs
        Char_esc_v : cs -> '\\' : 'v' : go cs
        Char_lit cc : cs ->
          case st of
            LongString -> cc : go cs
            ShortString ->
              case cc of
                '\r' -> go $ Char_esc_r : cs
                '\n' -> go $ Char_esc_n : cs
                _ -> cc : go cs

renderPyChars :: QuoteType -> StringType -> [PyChar] -> Text
renderPyChars =
  renderPyCharsWithCorrection $
  \qt st ->
    case st of
      LongString ->
        correctBackslashes . correctBackslashEscapes .
        correctInitialFinalQuotesLong qt
      ShortString ->
        correctBackslashes . correctBackslashEscapes .
        correctQuotes qt

renderRawPyChars :: QuoteType -> StringType -> [PyChar] -> Text
renderRawPyChars =
  renderPyCharsWithCorrection $
  \qt st ->
    case st of
      LongString ->
        correctInitialFinalQuotesLongRaw qt .
        correctBackslashEscapesRaw .
        correctBackslashesRaw
      ShortString ->
        correctBackslashEscapesRaw . correctBackslashesRaw .
        correctQuotesRaw qt

renderPyCharsBytesWithCorrection
  :: (QuoteType -> StringType -> [PyChar] -> [PyChar])
  -> QuoteType
  -> StringType
  -> [PyChar] -> Text
renderPyCharsBytesWithCorrection c qt st = Text.pack . go . c qt st
  where
    go s =
      case s of
        [] -> ""
        Char_newline : cs -> "\\newline" <> go cs
        Char_octal1 a  : cs ->
          "\\" <>
          [charOctal # a] <>
          go cs
        Char_octal2 a b : cs ->
          "\\" <>
          [charOctal # a, charOctal # b] <>
          go cs
        Char_octal3 a b cc : cs ->
          "\\" <>
          [charOctal # a, charOctal # b, charOctal # cc] <>
          go cs
        Char_hex a b : cs ->
          "\\x" <> [charHeXaDeCiMaL # a, charHeXaDeCiMaL # b] <> go cs
        Char_uni16 a b cc d : cs ->
          "\\u" <>
          [ charHeXaDeCiMaL # a
          , charHeXaDeCiMaL # b
          , charHeXaDeCiMaL # cc
          , charHeXaDeCiMaL # d
          ] <>
          go cs
        Char_uni32 a b cc d e f g h : cs ->
          "\\u" <>
          [ charHeXaDeCiMaL # a
          , charHeXaDeCiMaL # b
          , charHeXaDeCiMaL # cc
          , charHeXaDeCiMaL # d
          , charHeXaDeCiMaL # e
          , charHeXaDeCiMaL # f
          , charHeXaDeCiMaL # g
          , charHeXaDeCiMaL # h
          ] <>
          go cs
        Char_esc_bslash : cs -> '\\' : '\\' : go cs
        Char_esc_singlequote : cs -> '\\' : '\'' : go cs
        Char_esc_doublequote : cs -> '\\' : '"' : go cs
        Char_esc_a : cs -> '\\' : 'a' : go cs
        Char_esc_b : cs -> '\\' : 'b' : go cs
        Char_esc_f : cs -> '\\' : 'f' : go cs
        Char_esc_n : cs -> '\\' : 'n' : go cs
        Char_esc_r : cs -> '\\' : 'r' : go cs
        Char_esc_t : cs -> '\\' : 't' : go cs
        Char_esc_v : cs -> '\\' : 'v' : go cs
        Char_lit ch : cs
          | o <- ord ch, o > 127 ->
            let
              h = intToHexH o
            in
            case replicate (8 - length h) HeXDigit0 <> h of
              [a, b, cc, d, e, f, g, hh] -> go $ Char_uni32 a b cc d e f g hh : cs
              _ -> error $ "character " <> show ch <> " out of unicode range"
          | otherwise ->
              case st of
                LongString -> ch : go cs
                ShortString ->
                  case ch of
                    '\r' -> go $ Char_esc_r : cs
                    '\n' -> go $ Char_esc_n : cs
                    _ -> ch : go cs

renderPyCharsBytes :: QuoteType -> StringType -> [PyChar] -> Text
renderPyCharsBytes =
  renderPyCharsBytesWithCorrection $
  \qt st ->
  case st of
    LongString ->
      correctBackslashes . correctBackslashEscapes . correctInitialFinalQuotesLong qt
    ShortString ->
      correctBackslashes . correctBackslashEscapes . correctQuotes qt

renderRawPyCharsBytes :: QuoteType -> StringType -> [PyChar] -> Text
renderRawPyCharsBytes =
  renderPyCharsBytesWithCorrection $
  \qt st ->
    case st of
      LongString ->
        correctInitialFinalQuotesLongRaw qt .
        correctBackslashEscapesRaw .
        correctBackslashesRaw
      ShortString ->
        correctBackslashEscapesRaw . correctBackslashesRaw .
        correctQuotesRaw qt

showTokens :: [PyToken a] -> Text
showTokens =
  Lazy.toStrict .
  Builder.toLazyText .
  foldMap (Builder.fromText . showToken . (() <$)) .
  (expandIndents =<<)

expandIndents :: PyToken a -> [PyToken ()]
expandIndents (TkIndent _ i) =
  (i ^.. indentsValue.folded.indentWhitespaces.folded) >>=
  whitespaceTokens 
expandIndents (TkLevel _ i) =
  (i ^.. indentsValue.folded.indentWhitespaces.folded) >>=
  whitespaceTokens
expandIndents TkDedent{} = []
expandIndents a = pure $ () <$ a

showToken :: PyToken a -> Text
showToken t =
  case t of
    TkIndent{} -> error "trying to show indent token"
    TkLevel{} -> error "trying to show level token"
    TkDedent{} -> error "trying to show dedent token"
    TkIf{} -> "if"
    TkElse{} -> "else"
    TkElif{} -> "elif"
    TkWhile{} -> "while"
    TkAssert{} -> "assert"
    TkDef{} -> "def"
    TkReturn{} -> "return"
    TkPass{} -> "pass"
    TkBreak{} -> "break"
    TkContinue{} -> "continue"
    TkTrue{} -> "True"
    TkFalse{} -> "False"
    TkNone{} -> "None"
    TkEllipsis{} -> "..."
    TkOr{} -> "or"
    TkAnd{} -> "and"
    TkIs{} -> "is"
    TkNot{} -> "not"
    TkGlobal{} -> "global"
    TkNonlocal{} -> "nonlocal"
    TkDel{} -> "del"
    TkLambda{} -> "lambda"
    TkImport{} -> "import"
    TkFrom{} -> "from"
    TkAs{} -> "as"
    TkRaise{} -> "raise"
    TkTry{} -> "try"
    TkExcept{} -> "except"
    TkFinally{} -> "finally"
    TkClass{} -> "class"
    TkRightArrow{} -> "->"
    TkWith{} -> "with"
    TkFor{} -> "for"
    TkIn{} -> "in"
    TkYield{} -> "yield"
    TkInt i -> Text.pack $ showIntLiteral i
    TkFloat i -> Text.pack $ showFloatLiteral i
    TkImag i -> Text.pack $ showImagLiteral i
    TkIdent s _ -> Text.pack s
    TkString sp st qt s _ ->
      let
        theQuote =
          Text.pack $
          (case st of; LongString -> replicate 3; ShortString -> pure) (showQuoteType qt)
      in
        foldMap showStringPrefix sp <>
        theQuote <>
        renderPyChars qt st s <>
        theQuote
    TkBytes sp st qt s _ ->
      let
        theQuote =
          Text.pack $
          (case st of; LongString -> replicate 3; ShortString -> pure) (showQuoteType qt)
      in
        showBytesPrefix sp <>
        theQuote <>
        renderPyCharsBytes qt st s <>
        theQuote
    TkRawString sp st qt s _ ->
      let
        theQuote =
          case st of
            LongString -> Text.pack . replicate 3 $ showQuoteType qt
            ShortString -> Text.singleton $ showQuoteType qt
      in
        showRawStringPrefix sp <>
        theQuote <>
        renderRawPyChars qt st s <>
        theQuote
    TkRawBytes sp st qt s _ ->
      let
        theQuote =
          case st of
            LongString -> Text.pack . replicate 3 $ showQuoteType qt
            ShortString -> Text.singleton $ showQuoteType qt
      in
        showRawBytesPrefix sp <>
        theQuote <>
        renderRawPyCharsBytes qt st s <>
        theQuote
    TkSpace{} -> " "
    TkTab{} -> "\t"
    TkNewline nl _ ->
      case nl of
        CR -> "\r"
        LF -> "\n"
        CRLF -> "\r\n"
    TkLeftBracket{} -> "["
    TkRightBracket{} -> "]"
    TkLeftParen{} -> "("
    TkRightParen{} -> ")"
    TkLeftBrace{} -> "{"
    TkRightBrace{} -> "}"
    TkLt{} -> "<"
    TkLte{} -> "<="
    TkEq{} -> "="
    TkDoubleEq{}-> "=="
    TkBangEq{}-> "!="
    TkGt{} -> ">"
    TkGte{} -> ">="
    TkContinued nl _ ->
      "\\" <>
      case nl of
        CR -> "\r"
        LF -> "\n"
        CRLF -> "\r\n"
    TkColon{} -> ":"
    TkSemicolon{} -> ";"
    TkComma{} -> ","
    TkDot{} -> "."
    TkPlus{} -> "+"
    TkMinus{} -> "-"
    TkTilde{} -> "~"
    TkComment c -> showComment c
    TkStar{} -> "*"
    TkDoubleStar{} -> "**"
    TkSlash{} -> "/"
    TkDoubleSlash{} -> "//"
    TkPercent{} -> "%"
    TkShiftLeft{} -> "<<"
    TkShiftRight{} -> ">>"
    TkPlusEq{} -> "+="
    TkMinusEq{} -> "-="
    TkStarEq{} -> "*="
    TkAtEq{} -> "@="
    TkAt{} -> "@"
    TkSlashEq{} -> "/="
    TkPercentEq{} -> "%="
    TkAmpersandEq{} -> "&="
    TkPipeEq{} -> "|="
    TkCaretEq{} -> "^="
    TkAmpersand{} -> "&"
    TkPipe{} -> "|"
    TkCaret{} -> "^"
    TkShiftLeftEq{} -> "<<="
    TkShiftRightEq{} -> ">>="
    TkDoubleStarEq{} -> "**="
    TkDoubleSlashEq{} -> "//="

whitespaceTokens :: Whitespace -> [PyToken ()]
whitespaceTokens Space = [TkSpace ()]
whitespaceTokens Tab = [TkTab ()]
whitespaceTokens (Continued nl ws) = TkContinued nl () : (ws >>= whitespaceTokens)
whitespaceTokens (Newline nl) = [TkNewline nl ()]
whitespaceTokens (Comment cmt) = commentTokens cmt

renderWhitespace :: Whitespace -> RenderOutput ()
renderWhitespace = traverse_ singleton . whitespaceTokens

renderNewline :: Newline -> PyToken ()
renderNewline nl = TkNewline nl ()

renderComma :: Comma -> RenderOutput ()
renderComma (MkComma ws) = do
  singleton $ TkComma ()
  traverse_ renderWhitespace ws

renderAt :: At -> RenderOutput ()
renderAt (MkAt ws) = do
  singleton $ TkAt ()
  traverse_ renderWhitespace ws

renderCommaSep :: (a -> RenderOutput ()) -> CommaSep a -> RenderOutput ()
renderCommaSep _ CommaSepNone = pure ()
renderCommaSep f (CommaSepOne a) = f a
renderCommaSep f (CommaSepMany a c cs) = do
  f a
  renderComma c
  renderCommaSep f cs

renderCommaSep1 :: (a -> RenderOutput ()) -> CommaSep1 a -> RenderOutput ()
renderCommaSep1 f (CommaSepOne1 a) = f a
renderCommaSep1 f (CommaSepMany1 a comma c) = do
  f a
  renderComma comma
  renderCommaSep1 f c

renderCommaSep1' :: (a -> RenderOutput ()) -> CommaSep1' a -> RenderOutput ()
renderCommaSep1' f (CommaSepOne1' a b) = do
  f a
  traverse_
    renderComma
    b
renderCommaSep1' f (CommaSepMany1' a comma c) = do
  f a
  renderComma comma
  renderCommaSep1' f c

renderIdent :: Ident v a -> RenderOutput ()
renderIdent (MkIdent _ a b) = do
  singleton $ TkIdent a ()
  traverse_ renderWhitespace b

parensTernaryLambda :: (Expr v a -> RenderOutput ()) -> Expr v a -> RenderOutput ()
parensTernaryLambda f expr =
  case vout expr of
    Ternary{} -> parensDistTWS renderExpr expr
    Lambda{} -> parensDistTWS renderExpr expr
    _ -> f expr

renderCompFor :: CompFor Expr v a -> RenderOutput ()
renderCompFor (CompFor _ ws1 ex1 ws2 ex2) = do
  singleton $ TkFor ()
  traverse_ renderWhitespace ws1
  (case vout ex1 of
     Not{} -> parensDistTWS renderExpr ex1
     _ -> parensGenerator ex1)
  singleton $ TkIn ()
  traverse_ renderWhitespace ws2
  parensTernaryLambda parensTupleGenerator ex2

renderCompIf :: CompIf Expr v a -> RenderOutput ()
renderCompIf (CompIf _ ws ex) = do
  singleton $ TkIf ()
  traverse_ renderWhitespace ws
  parensTernaryLambda parensTupleGenerator ex

renderComprehension
  :: (e Expr v a -> RenderOutput ())
  -> Comprehension e Expr v a
  -> RenderOutput ()
renderComprehension f (Comprehension _ expr cf cs) = do
  f expr
  renderCompFor cf
  traverse_ (bitraverse_ renderCompFor renderCompIf) cs

renderDictItem :: DictItem Expr v a -> RenderOutput ()
renderDictItem (DictItem _ a b c) = do
  parensTupleGenerator a
  renderColon b
  parensTupleGenerator c
renderDictItem (DictUnpack _ a b) = do
  singleton $ TkDoubleStar ()
  traverse_ renderWhitespace a
  case vout b of
    Binary _ _ BoolAnd{} _ -> parensDistTWS renderExpr b
    Binary _ _ BoolOr{} _ -> parensDistTWS renderExpr b
    Binary _ _ op _ | isComparison op -> parensDistTWS renderExpr b
    Not{} -> parensDistTWS renderExpr b
    _ -> parensTernaryLambda parensTupleGenerator b

renderStringLiteral :: StringLiteral a -> RenderOutput ()
renderStringLiteral (StringLiteral _ a b c d e) = do
  singleton $ TkString a b c d ()
  traverse_ renderWhitespace e
renderStringLiteral (BytesLiteral _ a b c d e) = do
  singleton $ TkBytes a b c d ()
  traverse_ renderWhitespace e
renderStringLiteral (RawStringLiteral _ a b c d e) = do
  singleton $ TkRawString a b c d ()
  traverse_ renderWhitespace e
renderStringLiteral (RawBytesLiteral _ a b c d e) = do
  singleton $ TkRawBytes a b c d ()
  traverse_ renderWhitespace e

renderSubscriptItem :: SubscriptItem Expr v a -> RenderOutput ()
renderSubscriptItem (SubscriptExpr a) =
  case vout a of
    Await{} -> parensDistTWS renderExpr a
    _ -> parensTupleGenerator a
renderSubscriptItem (SubscriptSlice a b c d) = do
  traverse_ parensTupleGenerator a
  renderColon b
  traverse_ parensTupleGenerator c
  traverse_
    (bitraverse_
      renderColon
      (traverse_ parensTupleGenerator))
    d

renderYield :: (Expr v a -> RenderOutput ()) -> Expr v a -> RenderOutput ()
renderYield re expr =
  case vout expr of
    Yield _ a b -> do
      singleton $ TkYield ()
      traverse_ renderWhitespace a
      renderCommaSep parensTupleGenerator b
    YieldFrom _ a b c -> do
      singleton $ TkYield ()
      traverse_ renderWhitespace a
      singleton $ TkFrom ()
      traverse_ renderWhitespace b
      parensTupleGenerator c
    _ -> re expr

renderUnpackTarget :: Expr v a -> RenderOutput ()
renderUnpackTarget e =
  case vout e of
    Binary _ _ BoolAnd{} _ -> parensDistTWS renderExpr e
    Binary _ _ BoolOr{} _ -> parensDistTWS renderExpr e
    Binary _ _ op _ | isComparison op -> parensDistTWS renderExpr e
    Not{} -> parensDistTWS renderExpr e
    _ -> parensTernaryLambda parensTupleGenerator e

renderNestedParens
  :: RenderOutput ()
  -> [([Whitespace], [Whitespace])]
  -> RenderOutput ()
renderNestedParens =
  foldr
    (\(ws1, ws2) y -> do
        singleton $ TkLeftParen ()
        traverse_ renderWhitespace ws1
        y
        singleton $ TkRightParen ()
        traverse_ renderWhitespace ws2)

renderTupleItems
  :: CommaSep1' (TupleItem Expr v a)
  -> RenderOutput ()
renderTupleItems (CommaSepOne1' a Nothing) =
  case a of
    TupleItem _ b -> parensTupleGenerator b
    TupleUnpack _ b c d ->
      renderNestedParens
        (do
            singleton $ TkStar ()
            traverse_ renderWhitespace c
            renderUnpackTarget d)
        b
renderTupleItems (CommaSepOne1' a (Just comma)) = do
  (case a of
     TupleItem _ b -> parensTupleGenerator b
     TupleUnpack _ [] b c ->
       parens $ do
         singleton $ TkStar ()
         traverse_ renderWhitespace b
         renderUnpackTarget c
     TupleUnpack _ b c d ->
       renderNestedParens
         (do
             singleton $ TkStar ()
             traverse_ renderWhitespace c
             renderUnpackTarget d)
         b)
  renderComma comma
renderTupleItems (CommaSepMany1' a comma rest) = do
  (case a of
    TupleItem _ b -> parensTupleGenerator b
    TupleUnpack _ [] b c ->
      parens $ do
        singleton $ TkStar ()
        traverse_ renderWhitespace b
        renderUnpackTarget c
    TupleUnpack _ b c d ->
      renderNestedParens
        (do
            singleton $ TkStar ()
            traverse_ renderWhitespace c
            renderUnpackTarget d)
        b)
  renderComma comma
  renderTupleItems rest

renderSetItem :: SetItem Expr v a -> RenderOutput ()
renderSetItem a =
  case a of
    SetItem _ b -> parensTupleGenerator b
    SetUnpack _ b c d ->
      renderNestedParens
        (do
            singleton $ TkStar ()
            traverse_ renderWhitespace c
            renderUnpackTarget d)
        b

renderSetItems :: CommaSep1' (SetItem Expr v a) -> RenderOutput ()
renderSetItems (CommaSepOne1' a Nothing) =
  case a of
    SetItem _ b -> parensTupleGenerator b
    SetUnpack _ b c d ->
      renderNestedParens
        (do
            singleton $ TkStar ()
            traverse_ renderWhitespace c
            renderUnpackTarget d)
        b
renderSetItems (CommaSepOne1' a (Just comma)) = do
  (case a of
     SetItem _ b -> parensTupleGenerator b
     SetUnpack _ [] b c -> do
       singleton $ TkStar ()
       traverse_ renderWhitespace b
       renderUnpackTarget c
     SetUnpack _ b c d ->
       renderNestedParens
         (do
             singleton $ TkStar ()
             traverse_ renderWhitespace c
             renderUnpackTarget d)
         b)
  renderComma comma
renderSetItems (CommaSepMany1' a comma rest) = do
  (case a of
    SetItem _ b -> parensTupleGenerator b
    SetUnpack _ [] b c -> do
      singleton $ TkStar ()
      traverse_ renderWhitespace b
      renderUnpackTarget c
    SetUnpack _ b c d ->
      renderNestedParens
        (do
            singleton $ TkStar ()
            traverse_ renderWhitespace c
            renderUnpackTarget d)
        b)
  renderComma comma
  renderSetItems rest

renderListItems :: CommaSep1' (ListItem Expr v a) -> RenderOutput ()
renderListItems (CommaSepOne1' a Nothing) =
  case a of
    ListItem _ b -> parensTupleGenerator b
    ListUnpack _ b c d ->
      renderNestedParens
        (do
            singleton $ TkStar ()
            traverse_ renderWhitespace c
            renderUnpackTarget d)
        b
renderListItems (CommaSepOne1' a (Just comma)) = do
  (case a of
     ListItem _ b -> parensTupleGenerator b
     ListUnpack _ [] b c -> do
       singleton $ TkStar ()
       traverse_ renderWhitespace b
       renderUnpackTarget c
     ListUnpack _ b c d ->
       renderNestedParens
         (do
             singleton $ TkStar ()
             traverse_ renderWhitespace c
             renderUnpackTarget d)
         b)
  renderComma comma
renderListItems (CommaSepMany1' a comma rest) = do
  (case a of
    ListItem _ b -> parensTupleGenerator b
    ListUnpack _ [] b c -> do
      singleton $ TkStar ()
      traverse_ renderWhitespace b
      renderUnpackTarget c
    ListUnpack _ b c d ->
      renderNestedParens
        (do
            singleton $ TkStar ()
            traverse_ renderWhitespace c
            renderUnpackTarget d)
        b)
  renderComma comma
  renderListItems rest

renderExpr :: Expr v a -> RenderOutput ()
renderExpr expr =
  case vout expr of
    Unit _ a b -> do
      singleton $ TkLeftParen ()
      traverse_ renderWhitespace a
      singleton $ TkRightParen ()
      traverse_ renderWhitespace b
    Lambda _ a b c d -> do
      singleton $ TkLambda ()
      traverse_ renderWhitespace a
      renderParams b
      renderColon c
      parensTupleGenerator d
    Yield{} -> parensDistTWS (renderYield parensTupleGenerator) expr
    YieldFrom{} -> parensDistTWS (renderYield parensTupleGenerator) expr
    Ternary _ a b c d e -> do
      (case vout a of
        Generator{} -> parensDistTWS renderExpr a
        _ -> parensTupleGenerator a)
      singleton $ TkIf ()
      traverse_ renderWhitespace b
      parensTernaryLambda parensTupleGenerator c
      singleton $ TkElse ()
      traverse_ renderWhitespace d
      parensTupleGenerator e
    Subscript _ a b c d -> do
      (case vout a of
        Binary{} -> parensDistTWS renderExpr a
        Unary{} -> parensDistTWS renderExpr a
        Not{} -> parensDistTWS renderExpr a
        Ternary{} -> parensDistTWS renderExpr a
        Lambda{} -> parensDistTWS renderExpr a
        Await{} -> parensDistTWS renderExpr a
        _ -> parensTupleGenerator a)
      brackets $ do
        traverse_ renderWhitespace b
        renderCommaSep1' renderSubscriptItem c
      traverse_ renderWhitespace d
    Not _ ws e -> do
      singleton $ TkNot ()
      traverse_ renderWhitespace ws
      case vout e of
        Binary _ _ BoolAnd{} _ -> parensDistTWS renderExpr e
        Binary _ _ BoolOr{} _ -> parensDistTWS renderExpr e
        Ternary{} -> parensDistTWS renderExpr e
        Lambda{} -> parensDistTWS renderExpr e
        _ -> parensTupleGenerator e
    Parens _ ws1 e ws2 -> do
      parens $ do
        traverse_ renderWhitespace ws1
        renderYield renderExpr e
      traverse_ renderWhitespace ws2
    Bool _ b ws -> do
      singleton $ if b then TkTrue () else TkFalse ()
      traverse_ renderWhitespace ws
    Unary _ op e -> do
      renderUnOp op
      case vout e of
        Binary _ _ Exp{} _ -> parensTupleGenerator e
        Binary{} -> parensDistTWS renderExpr e
        Deref _ (VIn Int{}) _ _ -> parensDistTWS renderExpr e
        Not{} -> parensDistTWS renderExpr e
        Ternary{} -> parensDistTWS renderExpr e
        Lambda{} -> parensDistTWS renderExpr e
        _ -> parensTupleGenerator e
    String _ vs ->
      traverse_ renderStringLiteral $ correctAdjacentStrings vs
    Int _ n ws -> do
      singleton $ TkInt (() <$ n)
      traverse_ renderWhitespace ws
    Float _ n ws -> do
      singleton $ TkFloat (() <$ n)
      traverse_ renderWhitespace ws
    Imag _ n ws -> do
      singleton $ TkImag (() <$ n)
      traverse_ renderWhitespace ws
    Ident _ name -> renderIdent name
    List _ ws1 exprs ws2 -> do
      brackets $ do
        traverse_ renderWhitespace ws1
        traverse_ renderListItems exprs
      traverse_ renderWhitespace ws2
    ListComp _ ws1 comp ws2 -> do
      brackets $ do
        traverse_ renderWhitespace ws1
        renderComprehension
          (\(VIdentity e) -> case vout e of
              Yield{} -> parensDistTWS renderExpr e
              YieldFrom{} -> parensDistTWS renderExpr e
              _ -> parensTupleGenerator e)
          comp
      traverse_ renderWhitespace ws2
    Call _ e ws args ws2 -> do
      (case vout e of
        Unary{} -> parensDistTWS renderExpr e
        Binary{} -> parensDistTWS renderExpr e
        Tuple{} -> parensDistTWS renderExpr e
        Not{} -> parensDistTWS renderExpr e
        Ternary{} -> parensDistTWS renderExpr e
        Lambda{} -> parensDistTWS renderExpr e
        _ -> parensGenerator e)
      parens $ do
        traverse_ renderWhitespace ws
        traverse_ renderArgs args
      traverse_ renderWhitespace ws2
    Deref _ e ws name -> do
      (case vout e of
        Int{} -> parensDistTWS renderExpr e
        Binary{} -> parensDistTWS renderExpr e
        Tuple{} -> parensDistTWS renderExpr e
        Not{} -> parensDistTWS renderExpr e
        Unary{} -> parensDistTWS renderExpr e
        Ternary{} -> parensDistTWS renderExpr e
        Lambda{} -> parensDistTWS renderExpr e
        Await{} -> parensDistTWS renderExpr e
        _ -> parensGenerator e)
      singleton $ TkDot ()
      traverse_ renderWhitespace ws
      renderIdent name
    None _ ws -> do
      singleton $ TkNone ()
      traverse_ renderWhitespace ws
    Ellipsis _ ws -> do
      singleton $ TkEllipsis ()
      traverse_ renderWhitespace ws
    Binary _ e1 op e2 -> do
      if shouldGroupLeft op e1
        then parensDistTWS renderExpr e1
        else parensTernaryLambda parensGenerator e1

      renderBinOp op

      if shouldGroupRight op e2
        then parensDistTWS renderExpr e2
        else parensTernaryLambda parensGenerator e2
    Tuple _ a ws c ->
      renderTupleItems $
      case c of
        Nothing -> CommaSepOne1' a (Just ws)
        Just c' -> CommaSepMany1' a ws c'
    DictComp _ ws1 comp ws2 -> do
      braces $ do
        traverse_ renderWhitespace ws1
        renderComprehension renderDictItem comp
      traverse_ renderWhitespace ws2
    Dict _ a b c -> do
      braces $ do
        traverse_ renderWhitespace a
        traverse_ (renderCommaSep1' renderDictItem) b
      traverse_ renderWhitespace c
    SetComp _ ws1 comp ws2 -> do
      braces $ do
        traverse_ renderWhitespace ws1
        renderComprehension renderSetItem comp
      traverse_ renderWhitespace ws2
    Set _ a b c -> do
      braces $ do
        traverse_ renderWhitespace a
        renderSetItems b
      traverse_ renderWhitespace c
    Generator _ a ->
      renderComprehension
        (\(VIdentity e) -> case vout e of
            Yield{} -> parensDistTWS renderExpr e
            YieldFrom{} -> parensDistTWS renderExpr e
            _ -> parensTupleGenerator e)
        a
    Await _ ws e -> do
      singleton $ TkIdent "await" ()
      traverse_ renderWhitespace ws
      (case vout e of
        Unary{} -> parensDistTWS renderExpr e
        Binary{} -> parensDistTWS renderExpr e
        Tuple{} -> parensDistTWS renderExpr e
        Not{} -> parensDistTWS renderExpr e
        Ternary{} -> parensDistTWS renderExpr e
        Lambda{} -> parensDistTWS renderExpr e
        Await{} -> parensDistTWS renderExpr e
        _ -> parensGenerator e)

renderModuleName :: ModuleName v a -> RenderOutput ()
renderModuleName (ModuleNameOne _ s) = renderIdent s
renderModuleName (ModuleNameMany _ n dot rest) = do
  renderIdent n
  renderDot dot
  renderModuleName rest

renderDot :: Dot -> RenderOutput ()
renderDot (MkDot ws) = do
  singleton $ TkDot ()
  traverse_ renderWhitespace ws

renderRelativeModuleName :: RelativeModuleName v a -> RenderOutput ()
renderRelativeModuleName (RelativeWithName _ ds mn) = do
  traverse_ renderDot ds
  renderModuleName mn
renderRelativeModuleName (Relative _ ds) =
  traverse_ renderDot ds

renderImportAs :: (e v a -> RenderOutput ()) -> ImportAs e v a -> RenderOutput ()
renderImportAs f (ImportAs _ ea m) = do
  f ea
  traverse_
    (\(a, b) -> do
        singleton $ TkAs ()
        traverse_ renderWhitespace a
        renderIdent b)
    m

renderImportTargets :: ImportTargets v a -> RenderOutput ()
renderImportTargets (ImportAll _ ws) = do
  singleton $ TkStar ()
  traverse_ renderWhitespace ws
renderImportTargets (ImportSome _ ts) =
  renderCommaSep1 (renderImportAs renderIdent) ts
renderImportTargets (ImportSomeParens _ ws1 ts ws2) = do
  parens $ do
    traverse_ renderWhitespace ws1
    renderCommaSep1' (renderImportAs renderIdent) ts
  traverse_ renderWhitespace ws2

renderAugAssign :: AugAssign a -> RenderOutput ()
renderAugAssign aa = do
  singleton $ case _augAssignType aa of
    PlusEq -> TkPlusEq ()
    MinusEq -> TkMinusEq ()
    StarEq -> TkStarEq ()
    AtEq -> TkAtEq ()
    SlashEq -> TkSlashEq ()
    PercentEq -> TkPercentEq ()
    AmpersandEq -> TkAmpersandEq ()
    PipeEq -> TkPipeEq ()
    CaretEq -> TkCaretEq ()
    ShiftLeftEq -> TkShiftLeftEq ()
    ShiftRightEq -> TkShiftRightEq ()
    DoubleStarEq -> TkDoubleStarEq ()
    DoubleSlashEq -> TkDoubleSlashEq ()
  traverse_ renderWhitespace (_augAssignWhitespace aa)

renderSimpleStatement :: SimpleStatement v a -> RenderOutput ()
renderSimpleStatement (Assert _ b c d) = do
  singleton $ TkAssert ()
  traverse_ renderWhitespace b
  parensTupleGenerator c
  traverse_
    (\(x, y) -> do
        renderComma x
        parensTupleGenerator y)
    d
renderSimpleStatement (Raise _ ws x) = do
  singleton $ TkRaise ()
  traverse_ renderWhitespace ws
  traverse_
    (\(b, c) -> do
       parensTupleGenerator b
       traverse_
         (\(d, e) -> do
            singleton $ TkFrom ()
            traverse_ renderWhitespace d
            parensTupleGenerator e)
         c)
    x
renderSimpleStatement (Return _ ws expr) = do
  singleton $ TkReturn ()
  traverse_ renderWhitespace ws
  traverse_ parensGenerator expr
renderSimpleStatement (Expr _ expr) = renderYield parensGenerator expr
renderSimpleStatement (Assign _ lvalue rvalues) = do
  renderExpr lvalue
  traverse_
    (\(ws2, rvalue) -> do
       renderEquals ws2
       renderYield parensGenerator rvalue)
    rvalues
renderSimpleStatement (AugAssign _ lvalue as rvalue) = do
  renderExpr lvalue
  renderAugAssign as
  parensTupleGenerator rvalue
renderSimpleStatement (Pass _ ws) = do
  singleton $ TkPass ()
  traverse_ renderWhitespace ws
renderSimpleStatement (Continue _ ws) = do
  singleton $ TkContinue ()
  traverse_ renderWhitespace ws
renderSimpleStatement (Break _ ws) = do
  singleton $ TkBreak ()
  traverse_ renderWhitespace ws
renderSimpleStatement (Global _ ws ids) = do
  singleton $ TkGlobal ()
  traverse_ renderWhitespace ws
  renderCommaSep1 renderIdent ids
renderSimpleStatement (Nonlocal _ ws ids) = do
  singleton $ TkNonlocal ()
  traverse_ renderWhitespace ws
  renderCommaSep1 renderIdent ids
renderSimpleStatement (Del _ ws vals) = do
  singleton $ TkDel ()
  traverse_ renderWhitespace ws
  renderCommaSep1'
    (\a -> case vout a of
        Binary{} -> parensDistTWS renderExpr a
        Not{} -> parensDistTWS renderExpr a
        Ternary{} -> parensDistTWS renderExpr a
        Lambda{} -> parensDistTWS renderExpr a
        _ -> parensTupleGenerator a)
    vals
renderSimpleStatement (Import _ ws ns) = do
  singleton $ TkImport ()
  traverse_ renderWhitespace ws
  renderCommaSep1 (renderImportAs renderModuleName) ns
renderSimpleStatement (From _ ws1 name ws3 ns) = do
  singleton $ TkFrom ()
  traverse_ renderWhitespace ws1
  renderRelativeModuleName name
  singleton $ TkImport ()
  traverse_ renderWhitespace ws3
  renderImportTargets ns

renderBlank :: Blank a -> RenderOutput ()
renderBlank (Blank _ a b) = do
  traverse_ renderWhitespace a
  traverse_ renderComment b

renderBlock :: Block v a -> RenderOutput ()
renderBlock (Block a b c) = do
  traverse_ (bitraverse_ renderBlank (singleton . renderNewline)) a
  (if null c then final else notFinal) $ renderStatement b
  traverseOf_
    (_init.traverse)
    (bitraverse_
      (bitraverse_ renderBlank (singleton . renderNewline))
      (notFinal . renderStatement))
    c
  traverseOf_
    _last
    (bitraverse_
      (bitraverse_ renderBlank (singleton . renderNewline))
      (final . renderStatement))
    c

renderSemicolon :: Semicolon a -> RenderOutput ()
renderSemicolon (MkSemicolon _ ws) = do
  singleton $ TkSemicolon ()
  traverse_ renderWhitespace ws

renderEquals :: Equals -> RenderOutput ()
renderEquals (MkEquals ws) = do
  singleton $ TkEq ()
  traverse_ renderWhitespace ws

renderColon :: Colon -> RenderOutput ()
renderColon (MkColon ws) = do
  singleton $ TkColon ()
  traverse_ renderWhitespace ws

renderSuite
  :: Suite v a
  -> RenderOutput ()
renderSuite (SuiteMany _ a b c d) = do
  renderColon a
  traverse_ renderComment b
  singleton (renderNewline c)
  renderBlock d
renderSuite (SuiteOne _ a b) = do
  renderColon a
  fin <- isFinal
  renderSmallStatement $ correctTrailingNewline fin b

renderDecorator :: Decorator v a -> RenderOutput ()
renderDecorator (Decorator _ a b c d e f) = do
  renderIndents a
  renderAt b
  renderExpr c
  traverse_ renderComment d
  singleton (renderNewline e)
  traverse_ (bitraverse_ renderBlank (singleton . renderNewline)) f

renderCompoundStatement :: CompoundStatement v a -> RenderOutput ()
renderCompoundStatement (Fundef _ decos idnt asyncWs ws1 name ws2 params ws3 mty s) = do
  traverse_ renderDecorator decos
  renderIndents idnt
  traverse_
    (\ws -> do
        singleton $ TkIdent "async" ()
        traverse_ renderWhitespace ws)
    asyncWs
  singleton (TkDef ())
  traverse_ renderWhitespace ws1
  renderIdent name
  parens $ do
    traverse_ renderWhitespace ws2
    renderParams params
  traverse_ renderWhitespace ws3
  traverse_
    (\(ws, ty) -> do
        singleton $ TkRightArrow ()
        traverse_ renderWhitespace ws
        parensTupleGenerator ty)
    mty
  final $ renderSuite s
renderCompoundStatement (If _ idnt ws1 expr s elifs body') = do
  renderIndents idnt
  singleton $ TkIf ()
  traverse_ renderWhitespace ws1
  parensTupleGenerator expr
  notFinal $ renderSuite s
  traverseOf_
    (_init.traverse)
    (\(i, ws4, ex, t) -> do
        renderIndents i
        singleton $ TkElif ()
        traverse_ renderWhitespace ws4
        parensTupleGenerator ex
        notFinal $ renderSuite t)
    elifs
  traverseOf_
    _last
    (\(i, ws4, ex, t) -> do
        renderIndents i
        singleton $ TkElif ()
        traverse_ renderWhitespace ws4
        parensTupleGenerator ex
        (if isNothing body' then final else notFinal) $ renderSuite t)
    elifs
  traverse_
    (\(i, ws4, t) -> do
        renderIndents i
        singleton $ TkElse ()
        traverse_ renderWhitespace ws4
        final $ renderSuite t)
    body'
renderCompoundStatement (While _ idnt ws1 expr s els) = do
  renderIndents idnt
  singleton $ TkWhile ()
  traverse_ renderWhitespace ws1
  parensTupleGenerator expr
  (if isNothing els then final else notFinal) $ renderSuite s
  traverse_
    (\(i, ws4, t) -> do
        renderIndents i
        singleton $ TkElse ()
        traverse_ renderWhitespace ws4
        final $ renderSuite t)
    els
renderCompoundStatement (TryExcept _ idnt a s e f g) = do
  renderIndents idnt
  singleton $ TkTry ()
  traverse_ renderWhitespace a
  notFinal $ renderSuite s
  traverse_
    (\(i, ws1, eas, t) -> do
       renderIndents i
       singleton $ TkExcept ()
       traverse_ renderWhitespace ws1
       traverse_ renderExceptAs eas
       notFinal $ renderSuite t)
    (NonEmpty.init e)
  (case NonEmpty.last e of
     (i, ws1, eas, t) -> do
       renderIndents i
       singleton $ TkExcept ()
       traverse_ renderWhitespace ws1
       traverse_ renderExceptAs eas
       (if isNothing f && isNothing g then final else notFinal) $ renderSuite t)
  traverse_
    (\(i, ws1, t) -> do
       renderIndents i
       singleton $ TkElse ()
       traverse_ renderWhitespace ws1
       (if isNothing g then final else notFinal) $ renderSuite t)
    f
  traverse_
    (\(i, ws1, t) -> do
       renderIndents i
       singleton $ TkFinally ()
       traverse_ renderWhitespace ws1
       final $ renderSuite t)
    g
renderCompoundStatement (TryFinally _ idnt a s idnt2 e s') = do
  renderIndents idnt
  singleton $ TkTry ()
  traverse_ renderWhitespace a
  notFinal $ renderSuite s
  renderIndents idnt2
  singleton $ TkFinally ()
  traverse_ renderWhitespace e
  final $ renderSuite s'
renderCompoundStatement (For _ idnt asyncWs a b c d s h) = do
  renderIndents idnt
  traverse_
    (\ws -> do
        singleton $ TkIdent "async" ()
        traverse_ renderWhitespace ws)
    asyncWs
  singleton $ TkFor ()
  traverse_ renderWhitespace a
  parensGenerator b
  singleton $ TkIn ()
  traverse_ renderWhitespace c
  renderCommaSep1' parensTupleGenerator d
  (if isNothing h then final else notFinal) $ renderSuite s
  traverse_
    (\(i, x, t) -> do
        renderIndents i
        singleton $ TkElse ()
        traverse_ renderWhitespace x
        final $ renderSuite t)
    h
renderCompoundStatement (ClassDef _ decos idnt a b c s) = do
  traverse_ renderDecorator decos
  renderIndents idnt
  singleton $ TkClass ()
  traverse_ renderWhitespace a
  renderIdent b
  traverse_
    (\(x, y, z) -> do
      parens $ do
        traverse_ renderWhitespace x
        traverse_ renderArgs y
      traverse_ renderWhitespace z)
    c
  final $ renderSuite s
renderCompoundStatement (With _ idnt asyncWs a b s) = do
  renderIndents idnt
  traverse_
    (\ws -> do
        singleton $ TkIdent "async" ()
        traverse_ renderWhitespace ws)
    asyncWs
  singleton $ TkWith ()
  traverse_ renderWhitespace a
  renderCommaSep1 renderWithItem b
  final $ renderSuite s

renderWithItem :: WithItem v a -> RenderOutput ()
renderWithItem (WithItem _ a b) = do
  parensTupleGenerator a
  traverse_
    (\(c, d) -> do
       singleton $ TkAs ()
       traverse_ renderWhitespace c
       parensTupleGenerator d)
    b

renderIndent :: Indent -> RenderOutput ()
renderIndent (MkIndent ws) = traverse_ renderWhitespace $ toList ws

renderSmallStatement :: SmallStatement v a -> RenderOutput ()
renderSmallStatement (MkSmallStatement s ss sc cmt nl) = do
  renderSimpleStatement s
  traverse_
    (\(b, c) -> do
       renderSemicolon b
       renderSimpleStatement c)
    ss
  traverse_ renderSemicolon sc
  traverse_ renderComment cmt
  traverse_ (singleton . renderNewline) nl

renderStatement :: Statement v a -> RenderOutput ()
renderStatement (CompoundStatement c) = renderCompoundStatement c
renderStatement (SmallStatement idnts s) = do
  renderIndents idnts
  fin <- isFinal
  renderSmallStatement $ correctTrailingNewline fin s

renderExceptAs :: ExceptAs v a -> RenderOutput ()
renderExceptAs (ExceptAs _ e f) = do
  parensTupleGenerator e
  traverse_
    (\(a, b) -> do
        singleton $ TkAs ()
        traverse_ renderWhitespace a
        renderIdent b)
    f

renderArgs :: CommaSep1' (Arg Expr v a) -> RenderOutput ()
renderArgs (CommaSepOne1' a Nothing) = renderArg parensTuple a
renderArgs e = renderCommaSep1' (renderArg parensTupleGenerator) e

renderArg :: (Expr v a -> RenderOutput ()) -> Arg Expr v a -> RenderOutput ()
renderArg re (PositionalArg _ expr) = re expr
renderArg _ (KeywordArg _ name ws2 expr) = do
  renderIdent name
  singleton $ TkEq ()
  traverse_ renderWhitespace ws2
  parensTupleGenerator expr
renderArg _ (StarArg _ ws expr) = do
  singleton $ TkStar ()
  traverse_ renderWhitespace ws
  parensTupleGenerator expr
renderArg _ (DoubleStarArg _ ws expr) = do
  singleton $ TkDoubleStar ()
  traverse_ renderWhitespace ws
  parensTupleGenerator expr

renderParams :: CommaSep (Param Expr v a) -> RenderOutput ()
renderParams = renderCommaSep renderParam . correctParams

renderParam :: Param Expr v a -> RenderOutput ()
renderParam (PositionalParam _ name mty) = do
  renderIdent name
  traverse_
    (\(c, ty) -> do
        renderColon c
        parensTupleGenerator ty)
    mty
renderParam (StarParam _ ws name mty) = do
  singleton $ TkStar ()
  traverse_ renderWhitespace ws
  renderIdent name
  traverse_
    (\(c, ty) -> do
        renderColon c
        parensTupleGenerator ty)
    mty
renderParam (UnnamedStarParam _ ws) = do
  singleton $ TkStar ()
  traverse_ renderWhitespace ws
renderParam (DoubleStarParam _ ws name mty) = do
  singleton $ TkDoubleStar ()
  traverse_ renderWhitespace ws
  renderIdent name
  traverse_
    (\(c, ty) -> do
        renderColon c
        parensTupleGenerator ty)
    mty
renderParam (KeywordParam _ name mty ws2 expr) = do
  renderIdent name
  traverse_
    (\(c, ty) -> do
        renderColon c
        parensTupleGenerator ty)
    mty
  singleton $ TkEq ()
  traverse_ renderWhitespace ws2
  parensTupleGenerator expr

renderUnOp :: UnOp a -> RenderOutput ()
renderUnOp (Negate _ ws) = do
  singleton $ TkMinus ()
  traverse_ renderWhitespace ws
renderUnOp (Positive _ ws) = do
  singleton $ TkPlus ()
  traverse_ renderWhitespace ws
renderUnOp (Complement _ ws) = do
  singleton $ TkTilde ()
  traverse_ renderWhitespace ws

renderBinOp :: BinOp a -> RenderOutput ()
renderBinOp (Is _ ws) = do
  singleton $ TkIs ()
  traverse_ renderWhitespace ws
renderBinOp (IsNot _ ws1 ws2) = do
  singleton $ TkIs ()
  traverse_ renderWhitespace ws1
  singleton $ TkNot ()
  traverse_ renderWhitespace ws2
renderBinOp (In _ ws) = do
  singleton $ TkIn ()
  traverse_ renderWhitespace ws
renderBinOp (NotIn _ ws1 ws2) = do
  singleton $ TkNot ()
  traverse_ renderWhitespace ws1
  singleton $ TkIn ()
  traverse_ renderWhitespace ws2
renderBinOp (Plus _ ws) = do
  singleton $ TkPlus ()
  traverse_ renderWhitespace ws
renderBinOp (Minus _ ws) = do
  singleton $ TkMinus ()
  traverse_ renderWhitespace ws
renderBinOp (Multiply _ ws) = do
  singleton $ TkStar ()
  traverse_ renderWhitespace ws
renderBinOp (At _ ws) = do
  singleton $ TkAt ()
  traverse_ renderWhitespace ws
renderBinOp (Divide _ ws) = do
  singleton $ TkSlash ()
  traverse_ renderWhitespace ws
renderBinOp (FloorDivide _ ws) = do
  singleton $ TkDoubleSlash ()
  traverse_ renderWhitespace ws
renderBinOp (Exp _ ws) = do
  singleton $ TkDoubleStar ()
  traverse_ renderWhitespace ws
renderBinOp (BoolAnd _ ws) = do
  singleton $ TkAnd ()
  traverse_ renderWhitespace ws
renderBinOp (BoolOr _ ws) = do
  singleton $ TkOr ()
  traverse_ renderWhitespace ws
renderBinOp (Eq _ ws) = do
  singleton $ TkDoubleEq ()
  traverse_ renderWhitespace ws
renderBinOp (Lt _ ws) = do
  singleton $ TkLt ()
  traverse_ renderWhitespace ws
renderBinOp (LtEq _ ws) = do
  singleton $ TkLte ()
  traverse_ renderWhitespace ws
renderBinOp (Gt _ ws) = do
  singleton $ TkGt ()
  traverse_ renderWhitespace ws
renderBinOp (GtEq _ ws) = do
  singleton $ TkGte ()
  traverse_ renderWhitespace ws
renderBinOp (NotEq _ ws) = do
  singleton $ TkBangEq ()
  traverse_ renderWhitespace ws
renderBinOp (Percent _ ws) = do
  singleton $ TkPercent ()
  traverse_ renderWhitespace ws
renderBinOp (BitOr _ ws) = do
  singleton $ TkPipe ()
  traverse_ renderWhitespace ws
renderBinOp (BitXor _ ws) = do
  singleton $ TkCaret ()
  traverse_ renderWhitespace ws
renderBinOp (BitAnd _ ws) = do
  singleton $ TkAmpersand ()
  traverse_ renderWhitespace ws
renderBinOp (ShiftLeft _ ws) = do
  singleton $ TkShiftLeft ()
  traverse_ renderWhitespace ws
renderBinOp (ShiftRight _ ws) = do
  singleton $ TkShiftRight ()
  traverse_ renderWhitespace ws

renderIndents :: Indents a -> RenderOutput ()
renderIndents (Indents is _) = traverse_ renderIndent is

renderModule :: Module v a -> RenderOutput ()
renderModule ModuleEmpty = pure ()
renderModule (ModuleBlankFinal a) = renderBlank a
renderModule (ModuleBlank a b c) = do
  renderBlank a
  singleton $ renderNewline b
  renderModule c
renderModule (ModuleStatement a b) = do
  renderStatement a
  renderModule b

-- | Render an entire Python module to 'Text'
showModule :: Module v a -> Text
showModule = showRenderOutput . renderModule

-- | Render a single Python statement to 'Text'
showStatement :: Statement v a -> Text
showStatement = showRenderOutput . renderStatement

-- | Render a single Python expression to 'Text'
showExpr :: Expr v a -> Text
showExpr = showRenderOutput . parensGenerator
