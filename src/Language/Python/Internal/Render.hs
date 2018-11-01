{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}

{-|
Module      : Language.Python.Internal.Render
Copyright   : (C) CSIRO 2017-2018
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
  , parens, braces, brackets
  , renderWhitespace, renderCommaSep, renderCommaSep1, renderCommaSep1'
  , renderIdent, renderComment, renderModuleName, renderDot, renderRelativeModuleName
  , renderImportAs, renderImportTargets, renderSmallStatement, renderCompoundStatement
  , renderBlock, renderIndent, renderIndents, renderExceptAs, renderArg, renderParam
  , renderParams, renderCompFor, renderCompIf, renderComprehension, renderBinOp, renderUnOp
  , renderSubscript, renderPyChars, escapeChars, intToHex
  )
where

import Control.Lens.Cons (_init, _last)
import Control.Lens.Fold (traverseOf_)
import Control.Lens.Review ((#))
import Control.Monad.Writer.Strict (Writer, execWriter, writer)
import Control.Monad.Reader (ReaderT, runReaderT, local, ask)
import Data.Bifoldable (bitraverse_, bitraverse_)
import Data.Char (ord)
import Data.Digit.Char (charHeXaDeCiMaL, charOctal)
import Data.Digit.Hexadecimal.MixedCase (HeXDigit(..))
import Data.DList (DList)
import Data.Foldable (toList, traverse_)
import Data.Maybe (isNothing)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)

import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import Language.Python.Internal.Syntax.AugAssign
import Language.Python.Internal.Syntax.Comment
import Language.Python.Internal.Syntax.Ident
import Language.Python.Internal.Syntax.Import
import Language.Python.Internal.Syntax.ModuleNames
import Language.Python.Internal.Syntax.Numbers
import Language.Python.Internal.Syntax.Operator.Binary
import Language.Python.Internal.Syntax.Operator.Unary
import Language.Python.Internal.Syntax.Strings
import Language.Python.Internal.Render.Correction
import Language.Python.Internal.Token (PyToken(..))
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Statement
import Language.Python.Syntax.Module
import Language.Python.Syntax.Whitespace

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
renderComment c = singleton $ TkComment (() <$ c)

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

parensTuple :: Expr v a -> RenderOutput ()
parensTuple e =
  case e of
    Tuple{} -> parens $ renderExpr e
    _ -> renderExpr e

parensGenerator :: Expr v a -> RenderOutput ()
parensGenerator e =
  case e of
    Generator{} -> parens $ renderExpr e
    _ -> renderExpr e

parensTupleGenerator :: Expr v a -> RenderOutput ()
parensTupleGenerator e =
  case e of
    Tuple{} -> parens $ renderExpr e
    Generator{} -> parens $ renderExpr e
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

renderPyCharsBytes :: QuoteType -> StringType -> [PyChar] -> Text
renderPyCharsBytes qt st =
  case st of
    LongString ->
      Text.pack . go . correctBackslashes . correctInitialFinalQuotes qt
    ShortString ->
      Text.pack . go . correctBackslashes . correctQuotes qt
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
        Char_octal3 a b c : cs ->
          "\\" <>
          [charOctal # a, charOctal # b, charOctal # c] <>
          go cs
        Char_hex a b : cs ->
          "\\x" <> [charHeXaDeCiMaL # a, charHeXaDeCiMaL # b] <> go cs
        Char_uni16 a b c d : cs ->
          "\\u" <>
          [ charHeXaDeCiMaL # a
          , charHeXaDeCiMaL # b
          , charHeXaDeCiMaL # c
          , charHeXaDeCiMaL # d
          ] <>
          go cs
        Char_uni32 a b c d e f g h : cs ->
          "\\u" <>
          [ charHeXaDeCiMaL # a
          , charHeXaDeCiMaL # b
          , charHeXaDeCiMaL # c
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
        Char_lit c : cs
          | o <- ord c, o > 127 ->
            let
              h = intToHexH o
            in
            case replicate (8 - length h) HeXDigit0 <> h of
              [a, b, c, d, e, f, g, h] -> go $ Char_uni32 a b c d e f g h : cs
              _ -> error $ "character " <> show c <> " out of unicode range"
          | otherwise ->
              case st of
                LongString -> c : go cs
                ShortString ->
                  case c of
                    '\r' -> go $ Char_esc_r : cs
                    '\n' -> go $ Char_esc_n : cs
                    '\'' | SingleQuote <- qt -> go $ Char_esc_singlequote : cs
                    '\"' | DoubleQuote <- qt -> go $ Char_esc_singlequote : cs
                    _ -> c : go cs

showTokens :: [PyToken a] -> Text
showTokens =
  Lazy.toStrict .
  Builder.toLazyText .
  foldMap (Builder.fromText . showToken . (() <$))

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
    TkInt i -> showIntLiteral i
    TkFloat i -> showFloatLiteral i
    TkImag i -> showImagLiteral i
    TkIdent s _ -> Text.pack s
    TkString sp st qt s _ ->
      let
        quote =
          Text.pack $
          (case st of; LongString -> replicate 3; ShortString -> pure) (showQuoteType qt)
      in
        foldMap showStringPrefix sp <>
        quote <>
        renderPyChars qt st s <>
        quote
    TkBytes sp st qt s _ ->
      let
        quote =
          Text.pack $
          (case st of; LongString -> replicate 3; ShortString -> pure) (showQuoteType qt)
      in
        showBytesPrefix sp <>
        quote <>
        renderPyCharsBytes qt st s <>
        quote
    TkRawString sp st qt s _ ->
      let
        quote =
          case st of
            LongString -> Text.pack . replicate 3 $ showQuoteType qt
            ShortString -> Text.singleton $ showQuoteType qt
      in
        showRawStringPrefix sp <>
        quote <>
        renderPyChars qt st s <>
        quote
    TkRawBytes sp st qt s _ ->
      let
        quote =
          case st of
            LongString -> Text.pack . replicate 3 $ showQuoteType qt
            ShortString -> Text.singleton $ showQuoteType qt
      in
        showRawBytesPrefix sp <>
        quote <>
        renderPyCharsBytes qt st s <>
        quote
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

renderPyChars :: QuoteType -> StringType -> [PyChar] -> Text
renderPyChars qt st =
  case st of
    LongString ->
      Text.pack . go . correctBackslashes . correctInitialFinalQuotes qt
    ShortString ->
      Text.pack . go . correctBackslashes . correctQuotes qt
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
        Char_octal3 a b c : cs ->
          "\\" <>
          [charOctal # a, charOctal # b, charOctal # c] <>
          go cs
        Char_hex a b : cs ->
          "\\x" <> [charHeXaDeCiMaL # a, charHeXaDeCiMaL # b] <> go cs
        Char_uni16 a b c d : cs ->
          "\\u" <>
          [ charHeXaDeCiMaL # a
          , charHeXaDeCiMaL # b
          , charHeXaDeCiMaL # c
          , charHeXaDeCiMaL # d
          ] <>
          go cs
        Char_uni32 a b c d e f g h : cs ->
          "\\u" <>
          [ charHeXaDeCiMaL # a
          , charHeXaDeCiMaL # b
          , charHeXaDeCiMaL # c
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
        Char_lit c : cs ->
          case st of
            LongString -> c : go cs
            ShortString ->
              case c of
                '\r' -> go $ Char_esc_r : cs
                '\n' -> go $ Char_esc_n : cs
                '\'' | SingleQuote <- qt -> go $ Char_esc_singlequote : cs
                '\"' | DoubleQuote <- qt -> go $ Char_esc_singlequote : cs
                _ -> c : go cs

renderWhitespace :: Whitespace -> RenderOutput ()
renderWhitespace Space = singleton $ TkSpace ()
renderWhitespace Tab = singleton $ TkTab ()
renderWhitespace (Continued nl ws) = do
  singleton $ TkContinued nl ()
  traverse_ renderWhitespace ws
renderWhitespace (Newline nl) = singleton $ TkNewline nl ()
renderWhitespace (Comment cmt) = renderComment cmt

renderNewline :: Newline -> PyToken ()
renderNewline nl = TkNewline nl ()

renderCommaSep :: (a -> RenderOutput ()) -> CommaSep a -> RenderOutput ()
renderCommaSep _ CommaSepNone = pure ()
renderCommaSep f (CommaSepOne a) = f a
renderCommaSep f (CommaSepMany a ws2 c) = do
  f a
  singleton $ TkComma ()
  traverse_ renderWhitespace ws2
  renderCommaSep f c

renderCommaSep1 :: (a -> RenderOutput ()) -> CommaSep1 a -> RenderOutput ()
renderCommaSep1 f (CommaSepOne1 a) = f a
renderCommaSep1 f (CommaSepMany1 a ws2 c) = do
  f a
  singleton $ TkComma ()
  traverse_ renderWhitespace ws2
  renderCommaSep1 f c

renderCommaSep1' :: (a -> RenderOutput ()) -> CommaSep1' a -> RenderOutput ()
renderCommaSep1' f (CommaSepOne1' a b) = do
  f a
  traverse_
    (\x -> do
        singleton $ TkComma ()
        traverse renderWhitespace x)
    b
renderCommaSep1' f (CommaSepMany1' a ws2 c) = do
  f a
  singleton (TkComma ())
  traverse_ renderWhitespace ws2
  renderCommaSep1' f c

renderIdent :: Ident v a -> RenderOutput ()
renderIdent (MkIdent _ a b) = do
  singleton $ TkIdent a ()
  traverse_ renderWhitespace b

parensTernaryLambda :: (Expr v a -> RenderOutput ()) -> Expr v a -> RenderOutput ()
parensTernaryLambda _ e@Ternary{} = parens $ renderExpr e
parensTernaryLambda _ e@Lambda{} = parens $ renderExpr e
parensTernaryLambda f e = f e

renderCompFor :: CompFor v a -> RenderOutput ()
renderCompFor (CompFor _ ws1 ex1 ws2 ex2) = do
  singleton $ TkFor ()
  traverse_ renderWhitespace ws1
  (case ex1 of
     Not{} -> parens $ renderExpr ex1
     _ -> parensGenerator ex1)
  singleton $ TkIn ()
  traverse_ renderWhitespace ws2
  parensTernaryLambda parensTupleGenerator ex2

renderCompIf :: CompIf v a -> RenderOutput ()
renderCompIf (CompIf _ ws ex) = do
  singleton $ TkIf ()
  traverse_ renderWhitespace ws
  parensTernaryLambda parensTupleGenerator ex

renderComprehension
  :: (e v a -> RenderOutput ())
  -> Comprehension e v a
  -> RenderOutput ()
renderComprehension f (Comprehension _ expr cf cs) = do
  f expr
  renderCompFor cf
  traverse_ (bitraverse_ renderCompFor renderCompIf) cs

renderDictItem :: DictItem v a -> RenderOutput ()
renderDictItem (DictItem _ a b c) = do
  parensTupleGenerator a
  singleton $ TkColon ()
  traverse_ renderWhitespace b
  parensTupleGenerator c
renderDictItem (DictUnpack _ a b) = do
  singleton $ TkDoubleStar ()
  traverse_ renderWhitespace a
  case b of
    BinOp _ _ BoolAnd{} _ -> parens $ renderExpr b
    BinOp _ _ BoolOr{} _ -> parens $ renderExpr b
    BinOp _ _ op _ | isComparison op -> parens $ renderExpr b
    Not{} -> parens $ renderExpr b
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

renderSubscript :: Subscript v a -> RenderOutput ()
renderSubscript (SubscriptExpr a) =
  case a of
    Await{} -> parens $ renderExpr a
    _ -> parensTupleGenerator a
renderSubscript (SubscriptSlice a b c d) = do
  traverse_ parensTupleGenerator a
  singleton $ TkColon ()
  traverse_ renderWhitespace b
  traverse_ parensTupleGenerator c
  traverse_
    (bitraverse_
      (\ws -> do
          singleton $ TkColon ()
          traverse_ renderWhitespace ws)
      (traverse_ parensTupleGenerator))
    d

renderYield :: (Expr v a -> RenderOutput ()) -> Expr v a -> RenderOutput ()
renderYield re (Yield _ a b) = do
  singleton $ TkYield ()
  traverse_ renderWhitespace a
  traverse_
    (\x -> case x of
       Generator{} -> parens $ renderExpr x
       _ -> re x)
    b
renderYield re (YieldFrom _ a b c) = do
  singleton $ TkYield ()
  traverse_ renderWhitespace a
  singleton $ TkFrom ()
  traverse_ renderWhitespace b
  case c of
    Generator{} -> parens $ renderExpr c
    Tuple{} -> parens $ renderExpr c
    _ -> re c
renderYield re e = re e

renderUnpackTarget :: Expr v a -> RenderOutput ()
renderUnpackTarget e =
  case e of
    BinOp _ _ BoolAnd{} _ -> parens $ renderExpr e
    BinOp _ _ BoolOr{} _ -> parens $ renderExpr e
    BinOp _ _ op _ | isComparison op -> parens $ renderExpr e
    Not{} -> parens $ renderExpr e
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
  :: CommaSep1' (TupleItem v a)
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
renderTupleItems (CommaSepOne1' a (Just ws)) = do
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
  singleton $ TkComma ()
  traverse_ renderWhitespace ws
renderTupleItems (CommaSepMany1' a ws rest) = do
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
  singleton $ TkComma ()
  traverse_ renderWhitespace ws
  renderTupleItems rest

renderSetItem :: SetItem v a -> RenderOutput ()
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

renderSetItems :: CommaSep1' (SetItem v a) -> RenderOutput ()
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
renderSetItems (CommaSepOne1' a (Just ws)) = do
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
  singleton $ TkComma ()
  traverse_ renderWhitespace ws
renderSetItems (CommaSepMany1' a ws rest) = do
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
  singleton $ TkComma ()
  traverse_ renderWhitespace ws
  renderSetItems rest

renderListItems :: CommaSep1' (ListItem v a) -> RenderOutput ()
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
renderListItems (CommaSepOne1' a (Just ws)) = do
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
  singleton (TkComma ())
  traverse_ renderWhitespace ws
renderListItems (CommaSepMany1' a ws rest) = do
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
  singleton (TkComma ())
  traverse_ renderWhitespace ws
  renderListItems rest

renderExpr :: Expr v a -> RenderOutput ()
renderExpr (Unit _ a b) = do
  singleton $ TkLeftParen ()
  traverse_ renderWhitespace a
  singleton $ TkRightParen ()
  traverse_ renderWhitespace b
renderExpr (Lambda _ a b c d) = do
  singleton $ TkLambda ()
  traverse_ renderWhitespace a
  renderParams b
  singleton $ TkColon ()
  traverse_ renderWhitespace c
  parensTupleGenerator d
renderExpr e@Yield{} = parens $ renderYield parensTupleGenerator e
renderExpr e@YieldFrom{} = parens $ renderYield parensTupleGenerator e
renderExpr (Ternary _ a b c d e) = do
  (case a of
     Generator{} -> parens $ renderExpr a
     _ -> parensTupleGenerator a)
  singleton $ TkIf ()
  traverse_ renderWhitespace b
  parensTernaryLambda parensTupleGenerator c
  singleton $ TkElse ()
  traverse_ renderWhitespace d
  parensTupleGenerator e
renderExpr (Subscript _ a b c d) = do
  (case a of
     BinOp{} -> parens $ renderExpr a
     UnOp{} -> parens $ renderExpr a
     Not{} -> parens $ renderExpr a
     Ternary{} -> parens $ renderExpr a
     Lambda{} -> parens $ renderExpr a
     _ -> parensTupleGenerator a)
  brackets $ do
    traverse_ renderWhitespace b
    renderCommaSep1' renderSubscript c
  traverse_ renderWhitespace d
renderExpr (Not _ ws e) = do
  singleton $ TkNot ()
  traverse_ renderWhitespace ws
  case e of
    BinOp _ _ BoolAnd{} _ -> parens $ renderExpr e
    BinOp _ _ BoolOr{} _ -> parens $ renderExpr e
    Ternary{} -> parens $ renderExpr e
    Lambda{} -> parens $ renderExpr e
    _ -> parensTupleGenerator e
renderExpr (Parens _ ws1 e ws2) = do
  parens $ do
    traverse_ renderWhitespace ws1
    renderYield renderExpr e
  traverse_ renderWhitespace ws2
renderExpr (Bool _ b ws) = do
  singleton $ if b then TkTrue () else TkFalse ()
  traverse_ renderWhitespace ws
renderExpr (UnOp _ op expr) = do
  renderUnOp op
  case expr of
    BinOp _ _ Exp{} _ -> parensTupleGenerator expr
    BinOp{} -> parens $ renderExpr expr
    Deref _ Int{} _ _ -> parens $ renderExpr expr
    Not{} -> parens $ renderExpr expr
    Ternary{} -> parens $ renderExpr expr
    Lambda{} -> parens $ renderExpr expr
    _ -> parensTupleGenerator expr
renderExpr (String _ vs) =
  traverse_ renderStringLiteral $ correctAdjacentStrings vs
renderExpr (Int _ n ws) = do
  singleton $ TkInt (() <$ n)
  traverse_ renderWhitespace ws
renderExpr (Float _ n ws) = do
  singleton $ TkFloat (() <$ n)
  traverse_ renderWhitespace ws
renderExpr (Imag _ n ws) = do
  singleton $ TkImag (() <$ n)
  traverse_ renderWhitespace ws
renderExpr (Ident name) = renderIdent name
renderExpr (List _ ws1 exprs ws2) = do
  brackets $ do
    traverse_ renderWhitespace ws1
    traverse_ renderListItems exprs
  traverse_ renderWhitespace ws2
renderExpr (ListComp _ ws1 comp ws2) = do
  brackets $ do
    traverse_ renderWhitespace ws1
    renderComprehension
      (\e -> case e of
          Yield{} -> parens $ renderExpr e
          YieldFrom{} -> parens $ renderExpr e
          _ -> parensTupleGenerator e)
      comp
  traverse_ renderWhitespace ws2
renderExpr (Call _ expr ws args ws2) = do
  (case expr of
     UnOp{} -> parens $ renderExpr expr
     BinOp{} -> parens $ renderExpr expr
     Tuple{} -> parens $ renderExpr expr
     Not{} -> parens $ renderExpr expr
     Ternary{} -> parens $ renderExpr expr
     Lambda{} -> parens $ renderExpr expr
     _ -> parensGenerator expr)
  parens $ do
    traverse_ renderWhitespace ws
    traverse_ renderArgs args
  traverse_ renderWhitespace ws2
renderExpr (Deref _ expr ws name) = do
  (case expr of
     Int{} -> parens $ renderExpr expr
     BinOp{} -> parens $ renderExpr expr
     Tuple{} -> parens $ renderExpr expr
     Not{} -> parens $ renderExpr expr
     UnOp{} -> parens $ renderExpr expr
     Ternary{} -> parens $ renderExpr expr
     Lambda{} -> parens $ renderExpr expr
     Await{} -> parens $ renderExpr expr
     _ -> parensGenerator expr)
  singleton $ TkDot ()
  traverse_ renderWhitespace ws
  renderIdent name
renderExpr (None _ ws) = do
  singleton $ TkNone ()
  traverse_ renderWhitespace ws
renderExpr (Ellipsis _ ws) = do
  singleton $ TkEllipsis ()
  traverse_ renderWhitespace ws
renderExpr (BinOp _ e1 op e2) = do
  (if shouldGroupLeft op e1 then parens else id)
    (parensTernaryLambda parensGenerator e1)
  renderBinOp op
  (if shouldGroupRight op e2 then parens else id)
    (parensTernaryLambda parensGenerator e2)
renderExpr (Tuple _ a ws c) =
  renderTupleItems $
  case c of
    Nothing -> CommaSepOne1' a (Just ws)
    Just c' -> CommaSepMany1' a ws c'
renderExpr (DictComp _ ws1 comp ws2) = do
  braces $ do
    traverse_ renderWhitespace ws1
    renderComprehension renderDictItem comp
  traverse_ renderWhitespace ws2
renderExpr (Dict _ a b c) = do
  braces $ do
    traverse_ renderWhitespace a
    traverse_ (renderCommaSep1' renderDictItem) b
  traverse_ renderWhitespace c
renderExpr (SetComp _ ws1 comp ws2) = do
  braces $ do
    traverse_ renderWhitespace ws1
    renderComprehension renderSetItem comp
  traverse_ renderWhitespace ws2
renderExpr (Set _ a b c) = do
  braces $ do
    traverse_ renderWhitespace a
    renderSetItems b
  traverse_ renderWhitespace c
renderExpr (Generator _ a) =
  renderComprehension
    (\e -> case e of
        Yield{} -> parens $ renderExpr e
        YieldFrom{} -> parens $ renderExpr e
        _ -> parensTupleGenerator e)
    a
renderExpr (Await _ ws expr) = do
  singleton $ TkIdent "await" ()
  traverse_ renderWhitespace ws
  (case expr of
     UnOp{} -> parens $ renderExpr expr
     BinOp{} -> parens $ renderExpr expr
     Tuple{} -> parens $ renderExpr expr
     Not{} -> parens $ renderExpr expr
     Ternary{} -> parens $ renderExpr expr
     Lambda{} -> parens $ renderExpr expr
     Await{} -> parens $ renderExpr expr
     _ -> parensGenerator expr)

renderModuleName :: ModuleName v a -> RenderOutput ()
renderModuleName (ModuleNameOne _ s) = renderIdent s
renderModuleName (ModuleNameMany _ n dot rest) = do
  renderIdent n
  renderDot dot
  renderModuleName rest

renderDot :: Dot -> RenderOutput ()
renderDot (Dot ws) = do
  singleton $ TkDot ()
  traverse_ renderWhitespace ws

renderRelativeModuleName :: RelativeModuleName v a -> RenderOutput ()
renderRelativeModuleName (RelativeWithName ds mn) = do
  traverse_ renderDot ds
  renderModuleName mn
renderRelativeModuleName (Relative ds) =
  traverse_ renderDot ds

renderImportAs :: (e a -> RenderOutput ()) -> ImportAs e v a -> RenderOutput ()
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

renderSmallStatement :: SmallStatement v a -> RenderOutput ()
renderSmallStatement (Assert _ b c d) = do
  singleton $ TkAssert ()
  traverse_ renderWhitespace b
  parensTupleGenerator c
  traverse_
    (\(a, b) -> do
        singleton $ TkComma ()
        traverse_ renderWhitespace a
        parensTupleGenerator b)
    d
renderSmallStatement (Raise _ ws x) = do
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
renderSmallStatement (Return _ ws expr) = do
  singleton $ TkReturn ()
  traverse_ renderWhitespace ws
  traverse_ parensGenerator expr
renderSmallStatement (Expr _ expr) = renderYield parensGenerator expr
renderSmallStatement (Assign _ lvalue rvalues) = do
  renderExpr lvalue
  traverse_
    (\(ws2, rvalue) -> do
       singleton $ TkEq ()
       traverse_ renderWhitespace ws2
       renderYield parensGenerator rvalue)
    rvalues
renderSmallStatement (AugAssign _ lvalue as rvalue) = do
  renderExpr lvalue
  renderAugAssign as
  parensTupleGenerator rvalue
renderSmallStatement (Pass _ ws) = do
  singleton $ TkPass ()
  traverse_ renderWhitespace ws
renderSmallStatement (Continue _ ws) = do
  singleton $ TkContinue ()
  traverse_ renderWhitespace ws
renderSmallStatement (Break _ ws) = do
  singleton $ TkBreak ()
  traverse_ renderWhitespace ws
renderSmallStatement (Global _ ws ids) = do
  singleton $ TkGlobal ()
  traverse_ renderWhitespace ws
  renderCommaSep1 renderIdent ids
renderSmallStatement (Nonlocal _ ws ids) = do
  singleton $ TkNonlocal ()
  traverse_ renderWhitespace ws
  renderCommaSep1 renderIdent ids
renderSmallStatement (Del _ ws vals) = do
  singleton $ TkDel ()
  traverse_ renderWhitespace ws
  renderCommaSep1'
    (\a -> case a of
        BinOp{} -> parens $ renderExpr a
        Not{} -> parens $ renderExpr a
        Ternary{} -> parens $ renderExpr a
        Lambda{} -> parens $ renderExpr a
        _ -> parensTupleGenerator a)
    vals
renderSmallStatement (Import _ ws ns) = do
  singleton $ TkImport ()
  traverse_ renderWhitespace ws
  renderCommaSep1 (renderImportAs renderModuleName) ns
renderSmallStatement (From _ ws1 name ws3 ns) = do
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

renderSuite
  :: Suite v a
  -> RenderOutput ()
renderSuite (SuiteMany _ a b c d) = do
  singleton $ TkColon ()
  traverse_ renderWhitespace a
  traverse_ renderComment b
  singleton (renderNewline c)
  renderBlock d
renderSuite (SuiteOne _ a b) = do
  singleton $ TkColon ()
  traverse_ renderWhitespace a
  fin <- isFinal
  renderSimpleStatement $ correctTrailingNewline fin b

renderDecorator :: Decorator v a -> RenderOutput ()
renderDecorator (Decorator _ a b c d e f) = do
  renderIndents a
  singleton $ TkAt ()
  traverse_ renderWhitespace b
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
    (\(idnt, ws4, ex, s) -> do
        renderIndents idnt
        singleton $ TkElif ()
        traverse_ renderWhitespace ws4
        parensTupleGenerator ex
        notFinal $ renderSuite s)
    elifs
  traverseOf_
    _last
    (\(idnt, ws4, ex, s) -> do
        renderIndents idnt
        singleton $ TkElif ()
        traverse_ renderWhitespace ws4
        parensTupleGenerator ex
        (if isNothing body' then final else notFinal) $ renderSuite s)
    elifs
  traverse_
    (\(idnt, ws4, s) -> do
        renderIndents idnt
        singleton $ TkElse ()
        traverse_ renderWhitespace ws4
        final $ renderSuite s)
    body'
renderCompoundStatement (While _ idnt ws1 expr s els) = do
  renderIndents idnt
  singleton $ TkWhile ()
  traverse_ renderWhitespace ws1
  parensTupleGenerator expr
  (if isNothing els then final else notFinal) $ renderSuite s
  traverse_
    (\(idnt, ws4, s) -> do
        renderIndents idnt
        singleton $ TkElse ()
        traverse_ renderWhitespace ws4
        final $ renderSuite s)
    els
renderCompoundStatement (TryExcept _ idnt a s e f g) = do
  renderIndents idnt
  singleton $ TkTry ()
  traverse_ renderWhitespace a
  notFinal $ renderSuite s
  traverse_
    (\(idnt, ws1, eas, s) -> do
       renderIndents idnt
       singleton $ TkExcept ()
       traverse_ renderWhitespace ws1
       traverse_ renderExceptAs eas
       notFinal $ renderSuite s)
    (NonEmpty.init e)
  (case NonEmpty.last e of
     (idnt, ws1, eas, s) -> do
       renderIndents idnt
       singleton $ TkExcept ()
       traverse_ renderWhitespace ws1
       traverse_ renderExceptAs eas
       (if isNothing f && isNothing g then final else notFinal) $ renderSuite s)
  traverse_
    (\(idnt, ws1, s) -> do
       renderIndents idnt
       singleton $ TkElse ()
       traverse_ renderWhitespace ws1
       (if isNothing g then final else notFinal) $ renderSuite s)
    f
  traverse_
    (\(idnt, ws1, s) -> do
       renderIndents idnt
       singleton $ TkFinally ()
       traverse_ renderWhitespace ws1
       final $ renderSuite s)
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
    (\(idnt, x, s) -> do
        renderIndents idnt
        singleton $ TkElse ()
        traverse_ renderWhitespace x
        final $ renderSuite s)
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

renderSimpleStatement :: SimpleStatement v a -> RenderOutput ()
renderSimpleStatement (MkSimpleStatement s ss sc cmt nl) = do
  renderSmallStatement s
  traverse_
    (\(b, c) -> do
       singleton $ TkSemicolon ()
       traverse_ renderWhitespace b
       renderSmallStatement c)
    ss
  traverse_
    (\b -> do
        singleton $ TkSemicolon ()
        traverse_ renderWhitespace b)
    sc
  traverse_ renderComment cmt
  traverse_ (singleton . renderNewline) nl

renderStatement :: Statement v a -> RenderOutput ()
renderStatement (CompoundStatement c) = renderCompoundStatement c
renderStatement (SimpleStatement idnts s) = do
  renderIndents idnts
  fin <- isFinal
  renderSimpleStatement $ correctTrailingNewline fin s

renderExceptAs :: ExceptAs v a -> RenderOutput ()
renderExceptAs (ExceptAs _ e f) = do
  parensTupleGenerator e
  traverse_
    (\(a, b) -> do
        singleton $ TkAs ()
        traverse_ renderWhitespace a
        renderIdent b)
    f

renderArgs :: CommaSep1' (Arg v a) -> RenderOutput ()
renderArgs (CommaSepOne1' a Nothing) = renderArg parensTuple a
renderArgs e = renderCommaSep1' (renderArg parensTupleGenerator) e

renderArg :: (Expr v a -> RenderOutput ()) -> Arg v a -> RenderOutput ()
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

renderParams :: CommaSep (Param v a) -> RenderOutput ()
renderParams = go False
  where
    go :: Bool -> CommaSep (Param v a) -> RenderOutput ()
    go _ CommaSepNone = pure ()
    go _ (CommaSepOne a) = renderParam a
    go sawStar (CommaSepMany a ws2 b) = do
      let
        sawStar' =
          case a of
            StarParam{} -> True;
            DoubleStarParam{} -> True
            _ -> sawStar
      renderParam a
      (case b of
          CommaSepNone | sawStar' -> pure ()
          _ -> do
            singleton (TkComma ())
            traverse_ renderWhitespace ws2)
      go sawStar' b

renderParam :: Param v a -> RenderOutput ()
renderParam (PositionalParam _ name mty) = do
  renderIdent name
  traverse_
    (\(ws, ty) -> do
        singleton $ TkColon ()
        traverse_ renderWhitespace ws
        parensTupleGenerator ty)
    mty
renderParam (StarParam _ ws name mty) = do
  singleton $ TkStar ()
  traverse_ renderWhitespace ws
  traverse_ renderIdent name
  traverse_
    (\(ws, ty) -> do
        singleton $ TkColon ()
        traverse_ renderWhitespace ws
        parensTupleGenerator ty)
    mty
renderParam (DoubleStarParam _ ws name mty) = do
  singleton $ TkDoubleStar ()
  traverse_ renderWhitespace ws
  renderIdent name
  traverse_
    (\(ws, ty) -> do
        singleton $ TkColon ()
        traverse_ renderWhitespace ws
        parensTupleGenerator ty)
    mty
renderParam (KeywordParam _ name mty ws2 expr) = do
  renderIdent name
  traverse_
    (\(ws, ty) -> do
        singleton $ TkColon ()
        traverse_ renderWhitespace ws
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
renderBinOp (Equals _ ws) = do
  singleton $ TkDoubleEq ()
  traverse_ renderWhitespace ws
renderBinOp (Lt _ ws) = do
  singleton $ TkLt ()
  traverse_ renderWhitespace ws
renderBinOp (LtEquals _ ws) = do
  singleton $ TkLte ()
  traverse_ renderWhitespace ws
renderBinOp (Gt _ ws) = do
  singleton $ TkGt ()
  traverse_ renderWhitespace ws
renderBinOp (GtEquals _ ws) = do
  singleton $ TkGte ()
  traverse_ renderWhitespace ws
renderBinOp (NotEquals _ ws) = do
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
