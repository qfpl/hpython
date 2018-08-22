{-# language GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
module Language.Python.Internal.Render
  ( -- * Common Functions
    showModule, showStatement, showExpr
    -- * Rendering
  , RenderOutput, showRenderOutput, singleton, cons
  , renderModule, renderStatement, renderExpr
    -- * Miscellany
  , showQuoteType, showStringPrefix, showBytesPrefix, showToken
  , bracket, renderWhitespace, renderCommaSep, renderCommaSep1, renderCommaSep1'
  , renderIdent, renderComment, renderModuleName, renderDot, renderRelativeModuleName
  , renderImportAs, renderImportTargets, renderSmallStatement, renderCompoundStatement
  , renderBlock, renderIndent, renderIndents, renderExceptAs, renderArg, renderParam
  , renderCompFor, renderCompIf, renderComprehension, renderBinOp, renderUnOp
  , renderSubscript, renderPyChars
  )
where

import Control.Lens.Getter (view)
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.Plated (transform)
import Control.Lens.Review ((#))
import Data.Bifoldable (bifoldMap)
import Data.Digit.Char (charHeXaDeCiMaL, charOctal, charBinary, charDecimal)
import Data.DList (DList)
import Data.Foldable (toList)
import Data.Maybe (maybe)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.These (These(..))

import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import Language.Python.Internal.Syntax
import Language.Python.Internal.Token (PyToken(..))

newtype RenderOutput
  = RenderOutput
  { unRenderOutput :: DList (PyToken ())
  } deriving (Eq, Show, Semigroup, Monoid)

singleton :: PyToken () -> RenderOutput
singleton a = RenderOutput $ DList.singleton a

cons :: PyToken () -> RenderOutput -> RenderOutput
cons a (RenderOutput b) = RenderOutput $ DList.cons a b
infixr 5 `cons`

showRenderOutput :: RenderOutput -> Text
showRenderOutput =
  Lazy.toStrict .
  Builder.toLazyText .
  foldMap (Builder.fromText . showToken) .
  correctSpaces .
  correctNewlines .
  DList.toList .
  unRenderOutput
  where
    correctSpaces =
      transform $
      \case
        a : b : rest
          | isIdentifierChar (Text.last $ showToken a)
          , isIdentifierChar (Text.head $ showToken b)
          -> a : TkSpace () : b : rest
        a@(TkString _ qt _ _ _) : b@(TkString _ qt' _ _ _) : rest
          | qt == qt' -> a : TkSpace () : b : rest
        a@(TkFloat (FloatLiteralFull _ _ Nothing)) : b : rest
          | isIdentifierChar (Text.head $ showToken b) -> a : TkSpace () : b : rest
        a -> a

    correctNewlines =
      transform $
      \case
        TkNewline (CR cmt) () : TkNewline (LF cmt') () : rest ->
          TkNewline (CRLF cmt) () : TkNewline (LF cmt') () : rest
        TkContinued (CR cmt) () : TkNewline (LF cmt') () : rest ->
          TkContinued (CRLF cmt) () : TkNewline (LF cmt') () : rest
        a -> a

showStringPrefix :: StringPrefix -> Text
showStringPrefix sp =
  case sp of
    Prefix_u -> "u"
    Prefix_U -> "U"

showRawStringPrefix :: RawStringPrefix -> Text
showRawStringPrefix sp =
  case sp of
    Prefix_r -> "r"
    Prefix_R -> "R"

showBytesPrefix :: BytesPrefix -> Text
showBytesPrefix sp =
  case sp of
    Prefix_b -> "b"
    Prefix_B -> "B"

showRawBytesPrefix :: RawBytesPrefix -> Text
showRawBytesPrefix sp =
  case sp of
    Prefix_br -> "br"
    Prefix_Br -> "Br"
    Prefix_bR -> "bR"
    Prefix_BR -> "BR"
    Prefix_rb -> "rb"
    Prefix_rB -> "rB"
    Prefix_Rb -> "Rb"
    Prefix_RB -> "RB"

showQuoteType :: QuoteType -> Char
showQuoteType qt =
  case qt of
    DoubleQuote -> '\"'
    SingleQuote -> '\''

showToken :: PyToken a -> Text
showToken t =
  case t of
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
    TkWith{} -> "with"
    TkFor{} -> "for"
    TkIn{} -> "in"
    TkYield{} -> "yield"
    TkInt i -> renderIntLiteral i
    TkFloat i -> renderFloatLiteral i
    TkImag i -> renderImagLiteral i
    TkIdent s _ -> Text.pack s
    TkString sp qt st s _ ->
      let
        quote =
          Text.pack $
          (case st of; LongString -> replicate 3; ShortString -> pure) (showQuoteType qt)
      in
        foldMap showStringPrefix sp <>
        quote <>
        renderPyChars qt st s <>
        quote
    TkBytes sp qt st s _ ->
      let
        quote =
          Text.pack $
          (case st of; LongString -> replicate 3; ShortString -> pure) (showQuoteType qt)
      in
        showBytesPrefix sp <>
        quote <>
        renderPyChars qt st s <>
        quote
    TkRawString sp qt st s _ ->
      let
        quote =
          Text.pack $
          (case st of; LongString -> replicate 3; ShortString -> pure) (showQuoteType qt)
      in
        showRawStringPrefix sp <>
        quote <>
        renderRawPyChars qt st (_RawString # s) <>
        quote
    TkRawBytes sp qt st s _ ->
      let
        quote =
          Text.pack $
          (case st of; LongString -> replicate 3; ShortString -> pure) (showQuoteType qt)
      in
        showRawBytesPrefix sp <>
        quote <>
        renderRawPyChars qt st (_RawString # s) <>
        quote
    TkSpace{} -> " "
    TkTab{} -> "\t"
    TkNewline nl _ ->
      case nl of
        CR cmt -> foldMap (("#" <>) . renderComment) cmt <> "\r"
        LF cmt -> foldMap (("#" <>) . renderComment) cmt <> "\n"
        CRLF cmt -> foldMap (("#" <>) . renderComment) cmt <> "\r\n"
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
        CR cmt -> foldMap (("#" <>) . renderComment) cmt <> "\r"
        LF cmt -> foldMap (("#" <>) . renderComment) cmt <> "\n"
        CRLF cmt -> foldMap (("#" <>) . renderComment) cmt <> "\r\n"
    TkColon{} -> ":"
    TkSemicolon{} -> ";"
    TkComma{} -> ","
    TkDot{} -> "."
    TkPlus{} -> "+"
    TkMinus{} -> "-"
    TkTilde{} -> "~"
    TkComment s _ -> "#" <> Text.pack s
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

bracket :: RenderOutput -> RenderOutput
bracket a = TkLeftParen () `cons` a <> singleton (TkRightParen ())

bracketTuple :: Expr v a -> RenderOutput
bracketTuple e =
  case e of
    Tuple{} -> bracket $ renderExpr e
    _ -> renderExpr e

bracketGenerator :: Expr v a -> RenderOutput
bracketGenerator e =
  case e of
    Generator{} -> bracket $ renderExpr e
    _ -> renderExpr e

bracketTupleGenerator :: Expr v a -> RenderOutput
bracketTupleGenerator e =
  case e of
    Tuple{} -> bracket $ renderExpr e
    Generator{} -> bracket $ renderExpr e
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

renderRawPyChars :: QuoteType -> StringType -> [Char] -> Text
renderRawPyChars qt st = Text.pack . go
  where
    endSingleQuotesShort a =
      transform
        (\x -> case x of
           '\\' : '\'' : as -> x
           c : '\'' : as -> c : '\\' : '\'' : as
           _ -> x)
        (case a of
           '\'' : cs -> '\\' : '\'' : cs
           _ -> a)

    endSingleQuotesLong a =
      transform
        (\x -> case x of
           '\'' : '\'' : '\'' : as -> "\\\'\\\'\\\'" ++ as
           ['\\', '\''] -> ['\\', '\'']
           [c, '\''] -> [c, '\\', '\'']
           _ -> x)
        (case a of
           '\'' : cs -> '\\' : '\'' : cs
           _ -> a)

    endDoubleQuotesShort a =
      transform
        (\x -> case x of
           '\\' : '\"' : as -> x
           c : '\"' : as -> c : '\\' : '\"' : as
           _ -> x)
        (case a of
           '\"' : cs -> '\\' : '\"' : cs
           _ -> a)

    endDoubleQuotesLong a =
      transform
        (\x -> case x of
           '\"' : '\"' : '\"' : as -> "\\\"\\\"\\\"" ++ as
           ['\\', '\"']  -> x
           [c, '\"'] -> [c, '\\', '\"']
           _ -> x)
        (case a of
           '\"' : cs -> '\\' : '\"' : cs
           _ -> a)

    go s =
      case (qt, st) of
        (SingleQuote, ShortString) -> endSingleQuotesShort s
        (SingleQuote, LongString) -> endSingleQuotesLong s
        (DoubleQuote, ShortString) -> endDoubleQuotesShort s
        (DoubleQuote, LongString) -> endDoubleQuotesLong s

renderPyChars :: QuoteType -> StringType -> [PyChar] -> Text
renderPyChars qt st = Text.pack . go
  where
    endSingleQuotesShort =
      snd .
      foldr
        (\a (bl, b) ->
           case a of
             Char_lit '\'' -> (bl, '\\' : '\'' : b)
             Char_lit '\\' ->
               ( bl
               , if bl
                 then '\\' : (case b of; '\\' : _ -> '\\' : b; _ -> b)
                 else '\\' : '\\' : b
               )
             Char_lit c -> (True, c : b)
             _ -> (True, go [a] <> b))
        (False, [])

    endSingleQuotesLong =
      snd .
      foldr
        (\a (bl, b) ->
           case a of
             Char_lit '\'' -> (bl, if bl then '\'' : b else '\\' : '\'' : b)
             Char_lit '\\' ->
               ( bl
               , if bl
                 then '\\' : (case b of; '\\' : _ -> '\\' : b; _ -> b)
                 else '\\' : '\\' : b
               )
             Char_lit c -> (True, c : b)
             _ -> (True, go [a] <> b))
        (False, [])

    endDoubleQuotesShort =
      snd .
      foldr
        (\a (bl, b) ->
           case a of
             Char_lit '\"' -> (bl, '\\' : '\"' : b)
             Char_lit '\\' ->
               ( bl
               , if bl
                 then '\\' : (case b of; '\\' : _ -> '\\' : b; _ -> b)
                 else '\\' : '\\' : b)
             Char_lit c -> (True, c : b)
             _ -> (True, go [a] <> b))
        (False, [])

    endDoubleQuotesLong =
      snd .
      foldr
        (\a (bl, b) ->
           case a of
             Char_lit '\"' -> (bl, if bl then '\"' : b else '\\' : '\"' : b)
             Char_lit '\\' ->
               ( bl
               , if bl
                 then '\\' : (case b of; '\\' : _ -> '\\' : b; _ -> b)
                 else '\\' : '\\' : b)
             Char_lit c -> (True, c : b)
             _ -> (True, go [a] <> b))
        (False, [])

    escapeTripleDoubleQuotes (Char_lit '"' : Char_lit '"' : Char_lit '"' : cs) =
      Char_esc_doublequote : Char_esc_doublequote : Char_esc_doublequote : cs
    escapeTripleDoubleQuotes cs = cs

    escapeTripleSingleQuotes (Char_lit '\'' : Char_lit '\'' : Char_lit '\'' : cs) =
      Char_esc_singlequote : Char_esc_singlequote : Char_esc_singlequote : cs
    escapeTripleSingleQuotes cs = cs

    go s =
      case s of
        [] -> ""
        Char_newline : cs -> "\\newline" <> go cs
        Char_octal a b : cs ->
          "\\o" <>
          [charOctal # a, charOctal # b] <>
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
          case (qt, st) of
            (SingleQuote, ShortString) ->
              case c of
                '\'' -> '\\' : '\'' : go cs
                _ -> endSingleQuotesShort s
            (SingleQuote, LongString) ->
              case c of
                '\'' -> '\\' : '\'' : go cs
                _ -> endSingleQuotesLong (transform escapeTripleSingleQuotes s)
            (DoubleQuote, ShortString) ->
              case c of
                '"' -> '\\' : '"' : go cs
                _ -> endDoubleQuotesShort s
            (DoubleQuote, LongString) ->
              case c of
                '"' -> '\\' : '"' : go cs
                _ -> endDoubleQuotesLong (transform escapeTripleDoubleQuotes s)

renderWhitespace :: Whitespace -> RenderOutput
renderWhitespace Space = singleton $ TkSpace ()
renderWhitespace Tab = singleton $ TkTab ()
renderWhitespace (Continued nl ws) = TkContinued nl () `cons` foldMap renderWhitespace ws
renderWhitespace (Newline nl) = singleton $ TkNewline nl ()

renderNewline :: Newline -> PyToken ()
renderNewline nl = TkNewline nl ()

renderCommaSep :: (a -> RenderOutput) -> CommaSep a -> RenderOutput
renderCommaSep _ CommaSepNone = mempty
renderCommaSep f (CommaSepOne a) = f a
renderCommaSep f (CommaSepMany a ws2 c) =
  f a <>
  singleton (TkComma ()) <>
  foldMap renderWhitespace ws2 <>
  renderCommaSep f c

renderCommaSep1 :: (a -> RenderOutput) -> CommaSep1 a -> RenderOutput
renderCommaSep1 f (CommaSepOne1 a) = f a
renderCommaSep1 f (CommaSepMany1 a ws2 c) =
  f a <>
  singleton (TkComma ()) <>
  foldMap renderWhitespace ws2 <>
  renderCommaSep1 f c

renderCommaSep1' :: (a -> RenderOutput) -> CommaSep1' a -> RenderOutput
renderCommaSep1' f (CommaSepOne1' a b) =
  f a <>
  foldMap (\x -> TkComma () `cons` foldMap renderWhitespace x) b
renderCommaSep1' f (CommaSepMany1' a ws2 c) =
  f a <>
  singleton (TkComma ()) <>
  foldMap renderWhitespace ws2 <>
  renderCommaSep1' f c

renderIdent :: Ident v a -> RenderOutput
renderIdent (MkIdent _ a b) = TkIdent a () `cons` foldMap renderWhitespace b

renderComment :: Comment -> Text
renderComment (Comment s) = Text.pack s

bracketTernaryLambda :: (Expr v a -> RenderOutput) -> Expr v a -> RenderOutput
bracketTernaryLambda _ e@Ternary{} = bracket $ renderExpr e
bracketTernaryLambda _ e@Lambda{} = bracket $ renderExpr e
bracketTernaryLambda f e = f e

renderCompFor :: CompFor v a -> RenderOutput
renderCompFor (CompFor _ ws1 ex1 ws2 ex2) =
  TkFor () `cons`
  foldMap renderWhitespace ws1 <>
  bracketGenerator ex1 <>
  singleton (TkIn ()) <>
  foldMap renderWhitespace ws2 <>
  bracketTernaryLambda bracketTupleGenerator ex2

renderCompIf :: CompIf v a -> RenderOutput
renderCompIf (CompIf _ ws ex) =
  TkIf () `cons`
  foldMap renderWhitespace ws <>
  bracketTernaryLambda bracketTupleGenerator ex

renderComprehension :: (e v a -> RenderOutput) -> Comprehension e v a -> RenderOutput
renderComprehension f (Comprehension _ expr cf cs) =
  f expr <>
  renderCompFor cf <>
  foldMap (bifoldMap renderCompFor renderCompIf) cs

renderDictItem :: DictItem v a -> RenderOutput
renderDictItem (DictItem _ a b c) =
  bracketTupleGenerator a <>
  singleton (TkColon ()) <>
  foldMap renderWhitespace b <>
  bracketTupleGenerator c
renderDictItem (DictUnpack _ a b) =
  TkDoubleStar () `cons`
  foldMap renderWhitespace a <>
  case b of
    BinOp _ _ BoolAnd{} _ -> bracket $ renderExpr b
    BinOp _ _ BoolOr{} _ -> bracket $ renderExpr b
    BinOp _ _ op _ | isComparison op -> bracket $ renderExpr b
    Not{} -> bracket $ renderExpr b
    _ -> bracketTernaryLambda bracketTupleGenerator b

renderIntLiteral :: IntLiteral a -> Text
renderIntLiteral (IntLiteralDec _ n) =
  Text.pack $
  (charDecimal #) <$> NonEmpty.toList n
renderIntLiteral (IntLiteralBin _ b n) =
  Text.pack $
  '0' : (if b then 'B' else 'b') : fmap (charBinary #) (NonEmpty.toList n)
renderIntLiteral (IntLiteralOct _ b n) =
  Text.pack $
  '0' : (if b then 'O' else 'o') : fmap (charOctal #) (NonEmpty.toList n)
renderIntLiteral (IntLiteralHex _ b n) =
  Text.pack $
  '0' : (if b then 'X' else 'x') : fmap (charHeXaDeCiMaL #) (NonEmpty.toList n)

renderFloatExponent :: FloatExponent -> Text
renderFloatExponent (FloatExponent e s ds) =
  Text.pack $
  (if e then 'E' else 'e') :
  foldMap (\case; Pos -> "+"; Neg -> "-") s <>
  fmap (charDecimal #) (NonEmpty.toList ds)

renderFloatLiteral :: FloatLiteral a -> Text
renderFloatLiteral (FloatLiteralFull _ a b) =
  Text.pack (fmap (charDecimal #) (NonEmpty.toList a) <> ".") <>
  foldMap
    (\case
       This x -> Text.pack $ fmap (charDecimal #) (NonEmpty.toList x)
       That x -> renderFloatExponent x
       These x y ->
         Text.pack (fmap (charDecimal #) (NonEmpty.toList x)) <>
         renderFloatExponent y)
    b
renderFloatLiteral (FloatLiteralPoint _ a b) =
  Text.pack ('.' : fmap (charDecimal #) (NonEmpty.toList a)) <>
  foldMap renderFloatExponent b
renderFloatLiteral (FloatLiteralWhole _ a b) =
  Text.pack (fmap (charDecimal #) (NonEmpty.toList a)) <>
  renderFloatExponent b

renderImagLiteral :: ImagLiteral a -> Text
renderImagLiteral (ImagLiteralInt _ ds b) =
  Text.pack $ fmap (charDecimal #) (NonEmpty.toList ds) ++ [if b then 'J' else 'j']
renderImagLiteral (ImagLiteralFloat _ f b) =
  renderFloatLiteral f <> Text.singleton (if b then 'J' else 'j')

renderStringLiteral :: StringLiteral a -> RenderOutput
renderStringLiteral (StringLiteral _ a b c d e) =
  TkString a b c d () `cons`
  foldMap renderWhitespace e
renderStringLiteral (BytesLiteral _ a b c d e) =
  TkBytes a b c d () `cons`
  foldMap renderWhitespace e
renderStringLiteral (RawStringLiteral _ a b c d e) =
  TkRawString a b c d () `cons`
  foldMap renderWhitespace e
renderStringLiteral (RawBytesLiteral _ a b c d e) =
  TkRawBytes a b c d () `cons`
  foldMap renderWhitespace e

renderSubscript :: Subscript v a -> RenderOutput
renderSubscript (SubscriptExpr a) = bracketTupleGenerator a
renderSubscript (SubscriptSlice a b c d) =
  foldMap bracketTupleGenerator a <>
  singleton (TkColon ()) <>
  foldMap renderWhitespace b <>
  foldMap bracketTupleGenerator c <>
  foldMap
    (bifoldMap
      (cons (TkColon ()) . foldMap renderWhitespace)
      (foldMap bracketTupleGenerator))
    d

renderYield :: (Expr v a -> RenderOutput) -> Expr v a -> RenderOutput
renderYield re (Yield _ a b) =
  singleton (TkYield ()) <>
  foldMap renderWhitespace a <>
  foldMap re b
renderYield re (YieldFrom _ a b c) =
  singleton (TkYield ()) <>
  foldMap renderWhitespace a <>
  singleton (TkFrom ()) <>
  foldMap renderWhitespace b <>
  re c
renderYield re e = re e

renderUnpackTarget :: Expr v a -> RenderOutput
renderUnpackTarget e =
  case e of
    BinOp _ _ BoolAnd{} _ -> bracket $ renderExpr e
    BinOp _ _ BoolOr{} _ -> bracket $ renderExpr e
    BinOp _ _ op _ | isComparison op -> bracket $ renderExpr e
    Not{} -> bracket $ renderExpr e
    _ -> bracketTernaryLambda bracketTupleGenerator e

renderNestedParens :: RenderOutput -> [([Whitespace], [Whitespace])] -> RenderOutput
renderNestedParens =
  foldr
    (\(ws1, ws2) y ->
        TkLeftParen () `cons`
        foldMap renderWhitespace ws1 <>
        y <>
        singleton (TkRightParen ()) <>
        foldMap renderWhitespace ws2)

renderTupleItems :: CommaSep1' (TupleItem v a) -> RenderOutput
renderTupleItems (CommaSepOne1' a Nothing) =
  case a of
    TupleItem _ b -> bracketTupleGenerator b
    TupleUnpack _ b c d ->
      renderNestedParens
        (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
        b
renderTupleItems (CommaSepOne1' a (Just ws)) =
  (case a of
     TupleItem _ b -> bracketTupleGenerator b
     TupleUnpack _ [] b c ->
       bracket $ TkStar () `cons` foldMap renderWhitespace b <> renderUnpackTarget c
     TupleUnpack _ b c d ->
       renderNestedParens
         (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
         b) <>
  singleton (TkComma ()) <> foldMap renderWhitespace ws
renderTupleItems (CommaSepMany1' a ws rest) =
  (case a of
    TupleItem _ b -> bracketTupleGenerator b
    TupleUnpack _ [] b c ->
      bracket $ TkStar () `cons` foldMap renderWhitespace b <> renderUnpackTarget c
    TupleUnpack _ b c d ->
      renderNestedParens
        (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
        b) <>
  singleton (TkComma ()) <> foldMap renderWhitespace ws <>
  renderTupleItems rest

renderSetItem :: SetItem v a -> RenderOutput
renderSetItem a =
  case a of
    SetItem _ b -> bracketTupleGenerator b
    SetUnpack _ b c d ->
      renderNestedParens
        (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
        b

renderSetItems :: CommaSep1' (SetItem v a) -> RenderOutput
renderSetItems (CommaSepOne1' a Nothing) =
  case a of
    SetItem _ b -> bracketTupleGenerator b
    SetUnpack _ b c d ->
      renderNestedParens
        (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
        b
renderSetItems (CommaSepOne1' a (Just ws)) =
  (case a of
     SetItem _ b -> bracketTupleGenerator b
     SetUnpack _ [] b c ->
       TkStar () `cons` foldMap renderWhitespace b <> renderUnpackTarget c
     SetUnpack _ b c d ->
       renderNestedParens
         (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
         b) <>
  singleton (TkComma ()) <> foldMap renderWhitespace ws
renderSetItems (CommaSepMany1' a ws rest) =
  (case a of
    SetItem _ b -> bracketTupleGenerator b
    SetUnpack _ [] b c ->
      TkStar () `cons` foldMap renderWhitespace b <> renderUnpackTarget c
    SetUnpack _ b c d ->
      renderNestedParens
        (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
        b) <>
  singleton (TkComma ()) <> foldMap renderWhitespace ws <>
  renderSetItems rest

renderListItems :: CommaSep1' (ListItem v a) -> RenderOutput
renderListItems (CommaSepOne1' a Nothing) =
  case a of
    ListItem _ b -> bracketTupleGenerator b
    ListUnpack _ b c d ->
      renderNestedParens
        (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
        b
renderListItems (CommaSepOne1' a (Just ws)) =
  (case a of
     ListItem _ b -> bracketTupleGenerator b
     ListUnpack _ [] b c ->
       TkStar () `cons` foldMap renderWhitespace b <> renderUnpackTarget c
     ListUnpack _ b c d ->
       renderNestedParens
         (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
         b) <>
  singleton (TkComma ()) <> foldMap renderWhitespace ws
renderListItems (CommaSepMany1' a ws rest) =
  (case a of
    ListItem _ b -> bracketTupleGenerator b
    ListUnpack _ [] b c ->
      TkStar () `cons` foldMap renderWhitespace b <> renderUnpackTarget c
    ListUnpack _ b c d ->
      renderNestedParens
        (TkStar () `cons` foldMap renderWhitespace c <> renderUnpackTarget d)
        b) <>
  singleton (TkComma ()) <> foldMap renderWhitespace ws <>
  renderListItems rest

renderExpr :: Expr v a -> RenderOutput
renderExpr (Unit _ a b) =
  TkLeftParen () `cons`
  foldMap renderWhitespace a <>
  singleton (TkRightParen ()) <>
  foldMap renderWhitespace b
renderExpr (Lambda _ a b c d) =
  TkLambda () `cons`
  foldMap renderWhitespace a <>
  renderCommaSep renderParam b <>
  singleton (TkColon ()) <> foldMap renderWhitespace c <>
  bracketTupleGenerator d
renderExpr e@Yield{} = bracket $ renderYield bracketTupleGenerator e
renderExpr e@YieldFrom{} = bracket $ renderYield bracketTupleGenerator e
renderExpr (Ternary _ a b c d e) =
  (case a of
     Generator{} -> bracket $ renderExpr a
     _ -> bracketTupleGenerator a) <>
  singleton (TkIf ()) <> foldMap renderWhitespace b <>
  (case c of
     _ -> bracketTernaryLambda bracketTupleGenerator c) <>
  singleton (TkElse ()) <> foldMap renderWhitespace d <>
  (case e of
     _ -> bracketTupleGenerator e)
renderExpr (Subscript _ a b c d) =
  (case a of
     BinOp{} -> bracket $ renderExpr a
     Not{} -> bracket $ renderExpr a
     Ternary{} -> bracket $ renderExpr a
     Lambda{} -> bracket $ renderExpr a
     _ -> bracketTupleGenerator a) <>
  singleton (TkLeftBracket ()) <>
  foldMap renderWhitespace b <>
  renderCommaSep1' renderSubscript c <>
  singleton (TkRightBracket ()) <>
  foldMap renderWhitespace d
renderExpr (Not _ ws e) =
  TkNot () `cons`
  foldMap renderWhitespace ws <>
  case e of
    BinOp _ _ BoolAnd{} _ -> bracket $ renderExpr e
    BinOp _ _ BoolOr{} _ -> bracket $ renderExpr e
    Ternary{} -> bracket $ renderExpr e
    Lambda{} -> bracket $ renderExpr e
    _ -> bracketTupleGenerator e
renderExpr (Parens _ ws1 e ws2) =
  bracket (foldMap renderWhitespace ws1 <> renderYield renderExpr e) <>
  foldMap renderWhitespace ws2
renderExpr (Bool _ b ws) =
  (if b then TkTrue () else TkFalse ()) `cons`
  foldMap renderWhitespace ws
renderExpr (UnOp _ op expr) =
  renderUnOp op <>
  case expr of
    BinOp _ _ Exp{} _ -> bracketTupleGenerator expr
    BinOp{} -> bracket $ renderExpr expr
    Deref _ Int{} _ _ -> bracket $ renderExpr expr
    Not{} -> bracket $ renderExpr expr
    Ternary{} -> bracket $ renderExpr expr
    Lambda{} -> bracket $ renderExpr expr
    _ -> bracketTupleGenerator expr
renderExpr (String _ vs) = foldMap renderStringLiteral vs
renderExpr (Int a n ws) = TkInt (() <$ n) `cons` foldMap renderWhitespace ws
renderExpr (Float a n ws) = TkFloat (() <$ n) `cons` foldMap renderWhitespace ws
renderExpr (Imag a n ws) = TkImag (() <$ n) `cons` foldMap renderWhitespace ws
renderExpr (Ident _ name) = renderIdent name
renderExpr (List _ ws1 exprs ws2) =
  TkLeftBracket () `cons`
  foldMap renderWhitespace ws1 <>
  foldMap renderListItems exprs <>
  singleton (TkRightBracket ()) <> foldMap renderWhitespace ws2
renderExpr (ListComp _ ws1 comp ws2) =
  TkLeftBracket () `cons`
  foldMap renderWhitespace ws1 <>
  renderComprehension bracketTupleGenerator comp <>
  singleton (TkRightBracket ()) <> foldMap renderWhitespace ws2
renderExpr (Call _ expr ws args ws2) =
  (case expr of
     UnOp{} -> bracket $ renderExpr expr
     BinOp{} -> bracket $ renderExpr expr
     Tuple{} -> bracket $ renderExpr expr
     Not{} -> bracket $ renderExpr expr
     Ternary{} -> bracket $ renderExpr expr
     Lambda{} -> bracket $ renderExpr expr
     _ -> bracketGenerator expr) <>
  bracket (foldMap renderWhitespace ws <> foldMap renderArgs args) <>
  foldMap renderWhitespace ws2
renderExpr (Deref _ expr ws name) =
  (case expr of
     Int{} -> bracket $ renderExpr expr
     BinOp{} -> bracket $ renderExpr expr
     Tuple{} -> bracket $ renderExpr expr
     Not{} -> bracket $ renderExpr expr
     UnOp{} -> bracket $ renderExpr expr
     Ternary{} -> bracket $ renderExpr expr
     Lambda{} -> bracket $ renderExpr expr
     _ -> bracketGenerator expr) <>
  singleton (TkDot ()) <>
  foldMap renderWhitespace ws <>
  renderIdent name
renderExpr (None _ ws) = TkNone () `cons` foldMap renderWhitespace ws
renderExpr (Ellipsis _ ws) = TkEllipsis () `cons` foldMap renderWhitespace ws
renderExpr (BinOp _ e1 op e2) =
  (if shouldBracketLeft op e1 then bracket else id) (bracketTernaryLambda bracketGenerator e1) <>
  renderBinOp op <>
  (if shouldBracketRight op e2 then bracket else id) (bracketTernaryLambda bracketGenerator e2)
renderExpr (Tuple _ a ws c) =
  renderTupleItems $
  case c of
    Nothing -> CommaSepOne1' a (Just ws)
    Just c' -> CommaSepMany1' a ws c'
renderExpr (DictComp _ ws1 comp ws2) =
  TkLeftBrace () `cons`
  foldMap renderWhitespace ws1 <>
  renderComprehension renderDictItem comp <>
  singleton (TkRightBrace ()) <> foldMap renderWhitespace ws2
renderExpr (Dict _ a b c) =
  TkLeftBrace () `cons`
  foldMap renderWhitespace a <>
  foldMap (renderCommaSep1' renderDictItem) b <>
  singleton (TkRightBrace ()) <>
  foldMap renderWhitespace c
renderExpr (SetComp _ ws1 comp ws2) =
  TkLeftBrace () `cons`
  foldMap renderWhitespace ws1 <>
  renderComprehension renderSetItem comp <>
  singleton (TkRightBrace ()) <> foldMap renderWhitespace ws2
renderExpr (Set _ a b c) =
  TkLeftBrace () `cons`
  foldMap renderWhitespace a <>
  renderSetItems b <>
  singleton (TkRightBrace ()) <>
  foldMap renderWhitespace c
renderExpr (Generator _ a) = renderComprehension bracketTupleGenerator a

renderModuleName :: ModuleName v a -> RenderOutput
renderModuleName (ModuleNameOne _ s) = renderIdent s
renderModuleName (ModuleNameMany _ n ws2 rest) =
  renderIdent n <> singleton (TkDot ()) <> foldMap renderWhitespace ws2 <>
  renderModuleName rest

renderDot :: Dot -> RenderOutput
renderDot (Dot ws) = TkDot () `cons` foldMap renderWhitespace ws

renderRelativeModuleName :: RelativeModuleName v a -> RenderOutput
renderRelativeModuleName (RelativeWithName ds mn) =
  foldMap renderDot ds <> renderModuleName mn
renderRelativeModuleName (Relative ds) =
  foldMap renderDot ds

renderImportAs :: (e a -> RenderOutput) -> ImportAs e v a -> RenderOutput
renderImportAs f (ImportAs _ ea m) =
  f ea <>
  foldMap (\(a, b) -> TkAs () `cons` foldMap renderWhitespace a <> renderIdent b) m

renderImportTargets :: ImportTargets v a -> RenderOutput
renderImportTargets (ImportAll _ ws) = TkStar () `cons` foldMap renderWhitespace ws
renderImportTargets (ImportSome _ ts) =
  renderCommaSep1 (renderImportAs renderIdent) ts
renderImportTargets (ImportSomeParens _ ws1 ts ws2) =
  bracket
    (foldMap renderWhitespace ws1 <> renderCommaSep1' (renderImportAs renderIdent) ts) <>
  foldMap renderWhitespace ws2

renderAugAssign :: AugAssign a -> RenderOutput
renderAugAssign aa =
  (case aa of
     PlusEq{} -> TkPlusEq ()
     MinusEq{} -> TkMinusEq ()
     StarEq{} -> TkStarEq ()
     AtEq{} -> TkAtEq ()
     SlashEq{} -> TkSlashEq ()
     PercentEq{} -> TkPercentEq ()
     AmpersandEq{} -> TkAmpersandEq ()
     PipeEq{} -> TkPipeEq ()
     CaretEq{} -> TkCaretEq ()
     ShiftLeftEq{} -> TkShiftLeftEq ()
     ShiftRightEq{} -> TkShiftRightEq ()
     DoubleStarEq{} -> TkDoubleStarEq ()
     DoubleSlashEq{} -> TkDoubleSlashEq ()) `cons`
  foldMap renderWhitespace (_augAssignWhitespace aa)

renderSmallStatement :: SmallStatement v a -> RenderOutput
renderSmallStatement (Assert a b c d) =
  TkAssert () `cons`
  foldMap renderWhitespace b <>
  bracketTupleGenerator c <>
  foldMap
    (\(a, b) -> TkComma () `cons` foldMap renderWhitespace a <> bracketTupleGenerator b)
    d
renderSmallStatement (Raise _ ws x) =
  TkRaise () `cons` foldMap renderWhitespace ws <>
  foldMap
    (\(b, c) ->
       bracketTupleGenerator b <>
       foldMap
         (\(d, e) ->
            TkFrom () `cons` foldMap renderWhitespace d <>
            bracketTupleGenerator e)
         c)
    x
renderSmallStatement (Return _ ws expr) =
  TkReturn () `cons` foldMap renderWhitespace ws <> foldMap bracketGenerator expr
renderSmallStatement (Expr _ expr) = renderYield bracketGenerator expr
renderSmallStatement (Assign _ lvalue rvalues) =
  renderExpr lvalue <>
  foldMap
    (\(ws2, rvalue) ->
       TkEq () `cons`
       foldMap renderWhitespace ws2 <>
       renderYield bracketGenerator rvalue)
    rvalues
renderSmallStatement (AugAssign _ lvalue as rvalue) =
  renderExpr lvalue <> renderAugAssign as <> bracketTupleGenerator rvalue
renderSmallStatement (Pass _ ws) = TkPass () `cons` foldMap renderWhitespace ws
renderSmallStatement (Continue _ ws) = TkContinue () `cons` foldMap renderWhitespace ws
renderSmallStatement (Break _ ws) = TkBreak () `cons` foldMap renderWhitespace ws
renderSmallStatement (Global _ ws ids) =
  TkGlobal () `cons` foldMap renderWhitespace ws <> renderCommaSep1 renderIdent ids
renderSmallStatement (Nonlocal _ ws ids) =
  TkNonlocal () `cons` foldMap renderWhitespace ws <> renderCommaSep1 renderIdent ids
renderSmallStatement (Del _ ws vals) =
  TkDel () `cons`
  foldMap renderWhitespace ws <>
  renderCommaSep1'
    (\a -> case a of
        BinOp{} -> bracket $ renderExpr a
        Not{} -> bracket $ renderExpr a
        Ternary{} -> bracket $ renderExpr a
        Lambda{} -> bracket $ renderExpr a
        _ -> bracketTupleGenerator a)
    vals
renderSmallStatement (Import _ ws ns) =
  TkImport () `cons` foldMap renderWhitespace ws <>
  renderCommaSep1 (renderImportAs renderModuleName) ns
renderSmallStatement (From _ ws1 name ws3 ns) =
  TkFrom () `cons` foldMap renderWhitespace ws1 <>
  renderRelativeModuleName name <>
  singleton (TkImport ()) <> foldMap renderWhitespace ws3 <>
  renderImportTargets ns

renderBlock :: Block v a -> RenderOutput
renderBlock =
  foldMap
    (either
       (\(x, z) ->
          foldMap renderWhitespace x <>
          singleton (renderNewline z))
        renderStatement) .
  view _Wrapped

renderSuite :: Suite v a -> RenderOutput
renderSuite (SuiteMany _ a c d) =
  TkColon () `cons`
  foldMap renderWhitespace a <>
  singleton (renderNewline c) <>
  renderBlock d
renderSuite (SuiteOne _ a c d) =
  TkColon () `cons`
  foldMap renderWhitespace a <>
  renderSmallStatement c <>
  singleton (renderNewline d)

renderDecorator :: Decorator v a -> RenderOutput
renderDecorator (Decorator _ a b c d) =
  renderIndents a <>
  singleton (TkAt ()) <>
  foldMap renderWhitespace b <>
  renderExpr c <>
  singleton (renderNewline d)

renderCompoundStatement :: CompoundStatement v a -> RenderOutput
renderCompoundStatement (Fundef _ decos idnt ws1 name ws2 params ws3 s) =
  foldMap renderDecorator decos <>
  renderIndents idnt <>
  singleton (TkDef ()) <> foldMap renderWhitespace ws1 <> renderIdent name <>
  bracket (foldMap renderWhitespace ws2 <> renderCommaSep renderParam params) <>
  foldMap renderWhitespace ws3 <> renderSuite s
renderCompoundStatement (If idnt _ ws1 expr s elifs body') =
  renderIndents idnt <>
  singleton (TkIf ()) <> foldMap renderWhitespace ws1 <>
  bracketTupleGenerator expr <>
  renderSuite s <>
  foldMap
    (\(idnt, ws4, ex, s) ->
        renderIndents idnt <>
        singleton (TkElif ()) <> foldMap renderWhitespace ws4 <>
        bracketTupleGenerator ex <>
        renderSuite s)
    elifs <>
  foldMap
    (\(idnt, ws4, s) ->
        renderIndents idnt <>
        singleton (TkElse ()) <> foldMap renderWhitespace ws4 <>
        renderSuite s)
    body'
renderCompoundStatement (While idnt _ ws1 expr s) =
  renderIndents idnt <>
  singleton (TkWhile ()) <> foldMap renderWhitespace ws1 <> bracketTupleGenerator expr <>
  renderSuite s
renderCompoundStatement (TryExcept idnt _ a s e f g) =
  renderIndents idnt <>
  singleton (TkTry ()) <> foldMap renderWhitespace a <>
  renderSuite s <>
  foldMap
    (\(idnt, ws1, eas, s) ->
       renderIndents idnt <>
       singleton (TkExcept ()) <> foldMap renderWhitespace ws1 <>
       renderExceptAs eas <>
       renderSuite s)
    e <>
  foldMap
    (\(idnt, ws1, s) ->
       renderIndents idnt <>
       singleton (TkElse ()) <> foldMap renderWhitespace ws1 <>
       renderSuite s)
    f <>
  foldMap
    (\(idnt, ws1, s) ->
       renderIndents idnt <>
       singleton (TkFinally ()) <> foldMap renderWhitespace ws1 <>
       renderSuite s)
    g
renderCompoundStatement (TryFinally idnt _ a s idnt2 e s') =
  renderIndents idnt <>
  singleton (TkTry ()) <> foldMap renderWhitespace a <>
  renderSuite s <>
  renderIndents idnt2 <>
  singleton (TkFinally ()) <> foldMap renderWhitespace e <>
  renderSuite s'
renderCompoundStatement (For idnt _ a b c d s h) =
  renderIndents idnt <>
  singleton (TkFor ()) <> foldMap renderWhitespace a <> bracketGenerator b <>
  singleton (TkIn ()) <> foldMap renderWhitespace c <> bracketGenerator d <>
  renderSuite s <>
  foldMap
    (\(idnt, x, s) ->
        renderIndents idnt <>
        singleton (TkElse ()) <> foldMap renderWhitespace x <>
        renderSuite s)
    h
renderCompoundStatement (ClassDef _ decos idnt a b c s) =
  foldMap renderDecorator decos <>
  renderIndents idnt <>
  singleton (TkClass ()) <> foldMap renderWhitespace a <>
  renderIdent b <>
  foldMap
    (\(x, y, z) ->
      bracket
        (foldMap renderWhitespace x <>
         foldMap renderArgs y) <>
      foldMap renderWhitespace z)
    c <>
  renderSuite s
renderCompoundStatement (With idnt _ a b s) =
  renderIndents idnt <>
  singleton (TkWith ()) <> foldMap renderWhitespace a <>
  renderCommaSep1 renderWithItem b <>
  renderSuite s

renderWithItem :: WithItem v a -> RenderOutput
renderWithItem (WithItem _ a b) =
  bracketTupleGenerator a <>
  foldMap
    (\(c, d) -> 
       singleton (TkAs ()) <>
       foldMap renderWhitespace c <>
       bracketTupleGenerator d)
    b

renderIndent :: Indent -> RenderOutput
renderIndent (MkIndent ws) = foldMap renderWhitespace $ toList ws

renderStatement :: Statement v a -> RenderOutput
renderStatement (CompoundStatement c) = renderCompoundStatement c
renderStatement (SmallStatements idnts s ss sc nl) =
  renderIndents idnts <>
  renderSmallStatement s <>
  foldMap
    (\(b, c) ->
       TkSemicolon () `cons`
       foldMap renderWhitespace b <>
       renderSmallStatement c)
    ss <>
  foldMap
    (\b -> TkSemicolon () `cons` foldMap renderWhitespace b)
    sc <>
  foldMap (singleton . renderNewline) nl

renderExceptAs :: ExceptAs v a -> RenderOutput
renderExceptAs (ExceptAs _ e f) =
  bracketTupleGenerator e <>
  foldMap (\(a, b) -> TkAs () `cons` foldMap renderWhitespace a <> renderIdent b) f

renderArgs :: CommaSep1' (Arg v a) -> RenderOutput
renderArgs (CommaSepOne1' a Nothing) = renderArg bracketTuple a
renderArgs e = renderCommaSep1' (renderArg bracketTupleGenerator) e

renderArg :: (Expr v a -> RenderOutput) -> Arg v a -> RenderOutput
renderArg re (PositionalArg _ expr) = re expr
renderArg re (KeywordArg _ name ws2 expr) =
  renderIdent name <> singleton (TkEq ()) <>
  foldMap renderWhitespace ws2 <>
  bracketTupleGenerator expr
renderArg re (StarArg _ ws expr) =
  TkStar () `cons`
  foldMap renderWhitespace ws <>
  bracketTupleGenerator expr
renderArg re (DoubleStarArg _ ws expr) =
  TkDoubleStar () `cons`
  foldMap renderWhitespace ws <>
  bracketTupleGenerator expr

renderParam :: Param v a -> RenderOutput
renderParam (PositionalParam _ name mty) =
  renderIdent name <>
  foldMap
    (\(ws, ty) -> TkColon () `cons` foldMap renderWhitespace ws <> bracketTupleGenerator ty)
    mty
renderParam (StarParam _ ws name mty) =
  TkStar () `cons`
  foldMap renderWhitespace ws <>
  renderIdent name <>
  foldMap
    (\(ws, ty) -> TkColon () `cons` foldMap renderWhitespace ws <> bracketTupleGenerator ty)
    mty
renderParam (DoubleStarParam _ ws name mty) =
  TkDoubleStar () `cons`
  foldMap renderWhitespace ws <>
  renderIdent name <>
  foldMap
    (\(ws, ty) -> TkColon () `cons` foldMap renderWhitespace ws <> bracketTupleGenerator ty)
    mty
renderParam (KeywordParam _ name mty ws2 expr) =
  renderIdent name <>
  foldMap
    (\(ws, ty) -> TkColon () `cons` foldMap renderWhitespace ws <> bracketTupleGenerator ty)
    mty <>
  singleton (TkEq ()) <>
  foldMap renderWhitespace ws2 <> bracketTupleGenerator expr

renderUnOp :: UnOp a -> RenderOutput
renderUnOp (Negate _ ws) = singleton (TkMinus ()) <> foldMap renderWhitespace ws
renderUnOp (Positive _ ws) = singleton (TkPlus ()) <> foldMap renderWhitespace ws
renderUnOp (Complement _ ws) = singleton (TkTilde ()) <> foldMap renderWhitespace ws

renderBinOp :: BinOp a -> RenderOutput
renderBinOp (Is _ ws) = TkIs () `cons` foldMap renderWhitespace ws
renderBinOp (IsNot _ ws1 ws2) =
  TkIs () `cons`
  foldMap renderWhitespace ws1 <>
  singleton (TkNot ()) <>
  foldMap renderWhitespace ws2
renderBinOp (In _ ws) = TkIn () `cons` foldMap renderWhitespace ws
renderBinOp (NotIn _ ws1 ws2) =
  TkNot () `cons`
  foldMap renderWhitespace ws1 <>
  singleton (TkIn ()) <>
  foldMap renderWhitespace ws2
renderBinOp (Plus _ ws) = TkPlus () `cons` foldMap renderWhitespace ws
renderBinOp (Minus _ ws) = TkMinus () `cons` foldMap renderWhitespace ws
renderBinOp (Multiply _ ws) = TkStar () `cons` foldMap renderWhitespace ws
renderBinOp (Divide _ ws) = TkSlash () `cons` foldMap renderWhitespace ws
renderBinOp (FloorDivide _ ws) =
  TkDoubleSlash () `cons` foldMap renderWhitespace ws
renderBinOp (Exp _ ws) = TkDoubleStar () `cons` foldMap renderWhitespace ws
renderBinOp (BoolAnd _ ws) = TkAnd () `cons` foldMap renderWhitespace ws
renderBinOp (BoolOr _ ws) = TkOr () `cons` foldMap renderWhitespace ws
renderBinOp (Equals _ ws) = TkDoubleEq () `cons` foldMap renderWhitespace ws
renderBinOp (Lt _ ws) = TkLt () `cons` foldMap renderWhitespace ws
renderBinOp (LtEquals _ ws) = TkLte () `cons` foldMap renderWhitespace ws
renderBinOp (Gt _ ws) = TkGt () `cons` foldMap renderWhitespace ws
renderBinOp (GtEquals _ ws) = TkGte () `cons` foldMap renderWhitespace ws
renderBinOp (NotEquals _ ws) = TkBangEq () `cons` foldMap renderWhitespace ws
renderBinOp (Percent _ ws) = TkPercent () `cons` foldMap renderWhitespace ws
renderBinOp (BitOr _ ws) = TkPipe () `cons` foldMap renderWhitespace ws
renderBinOp (BitXor _ ws) = TkCaret () `cons` foldMap renderWhitespace ws
renderBinOp (BitAnd _ ws) = TkAmpersand () `cons` foldMap renderWhitespace ws
renderBinOp (ShiftLeft _ ws) = TkShiftLeft () `cons` foldMap renderWhitespace ws
renderBinOp (ShiftRight _ ws) = TkShiftRight () `cons` foldMap renderWhitespace ws

renderIndents :: Indents a -> RenderOutput
renderIndents (Indents is _) = foldMap renderIndent is

renderModule :: Module v a -> RenderOutput
renderModule (Module ms) =
  foldMap
    (either
       (\(a, b, c) ->
          renderIndents a <>
          maybe mempty (\a -> singleton $ TkComment (Text.unpack $ renderComment a) ()) b <>
          maybe mempty (singleton . renderNewline) c)
       renderStatement)
    ms

showModule :: Module v a -> Text
showModule = showRenderOutput . renderModule

showStatement :: Statement v a -> Text
showStatement = showRenderOutput . renderStatement

showExpr :: Expr v a -> Text
showExpr = showRenderOutput . bracketGenerator
