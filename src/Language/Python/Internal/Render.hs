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
  , showQuoteType, showStringPrefix, showBytesPrefix, showToken, showTokens
  , bracket, renderWhitespace, renderCommaSep, renderCommaSep1, renderCommaSep1'
  , renderIdent, renderComment, renderModuleName, renderDot, renderRelativeModuleName
  , renderImportAs, renderImportTargets, renderSmallStatement, renderCompoundStatement
  , renderBlock, renderIndent, renderIndents, renderExceptAs, renderArg, renderParam
  , renderParams, renderCompFor, renderCompIf, renderComprehension, renderBinOp, renderUnOp
  , renderSubscript, renderPyChars, escapeChars, intToHex
  )
where

import Control.Lens.Plated (transform)
import Control.Lens.Review ((#))
import Data.Bifoldable (bifoldMap)
import Data.Char (ord)
import Data.Digit.Char (charHeXaDeCiMaL, charOctal, charBinary, charDecimal)
import Data.Digit.Hexadecimal.MixedCase (HeXDigit(..))
import Data.DList (DList)
import Data.Foldable (toList)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.These (These(..))

import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import Language.Python.Internal.Syntax
import Language.Python.Internal.Render.Correction
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

showTokens :: [PyToken a] -> Text
showTokens =
  Lazy.toStrict .
  Builder.toLazyText .
  foldMap (Builder.fromText . showToken . (() <$))

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

renderComment :: Comment -> RenderOutput
renderComment c = singleton $ TkComment c ()

showComment :: Comment -> Text
showComment (Comment s) = Text.pack $ "#" <> s

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
    TkInt i -> renderIntLiteral i
    TkFloat i -> renderFloatLiteral i
    TkImag i -> renderImagLiteral i
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
        renderRawPyChars qt st s <>
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
        renderRawPyCharsBytes qt st s <>
        quote
    TkSpace{} -> " "
    TkTab{} -> "\t"
    TkNewline nl _ ->
      case nl of
        CR cmt -> foldMap showComment cmt <> "\r"
        LF cmt -> foldMap showComment cmt <> "\n"
        CRLF cmt -> foldMap showComment cmt <> "\r\n"
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
        CR cmt -> foldMap showComment cmt <> "\r"
        LF cmt -> foldMap showComment cmt <> "\n"
        CRLF cmt -> foldMap showComment cmt <> "\r\n"
    TkColon{} -> ":"
    TkSemicolon{} -> ";"
    TkComma{} -> ","
    TkDot{} -> "."
    TkPlus{} -> "+"
    TkMinus{} -> "-"
    TkTilde{} -> "~"
    TkComment c _ -> showComment c
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

renderRawPyChars :: QuoteType -> StringType -> [PyChar] -> Text
renderRawPyChars = renderPyChars

renderRawPyCharsBytes :: QuoteType -> StringType -> [PyChar] -> Text
renderRawPyCharsBytes = renderPyCharsBytes

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
      Text.pack . go . correctInitialFinalBackslashes . correctInitialFinalQuotes qt
    ShortString ->
      Text.pack . go . correctInitialFinalBackslashes . correctQuotes qt
  where
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

renderPyChars :: QuoteType -> StringType -> [PyChar] -> Text
renderPyChars qt st =
  case st of
    LongString ->
      Text.pack . go . correctInitialFinalBackslashes . correctInitialFinalQuotes qt
    ShortString ->
      Text.pack . go . correctInitialFinalBackslashes . correctQuotes qt
  where
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
          case st of
            LongString -> c : go cs
            ShortString ->
              case c of
                '\r' -> go $ Char_esc_r : cs
                '\n' -> go $ Char_esc_n : cs
                '\'' | SingleQuote <- qt -> go $ Char_esc_singlequote : cs
                '\"' | DoubleQuote <- qt -> go $ Char_esc_singlequote : cs
                _ -> c : go cs

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

bracketTernaryLambda :: (Expr v a -> RenderOutput) -> Expr v a -> RenderOutput
bracketTernaryLambda _ e@Ternary{} = bracket $ renderExpr e
bracketTernaryLambda _ e@Lambda{} = bracket $ renderExpr e
bracketTernaryLambda f e = f e

renderCompFor :: CompFor v a -> RenderOutput
renderCompFor (CompFor _ ws1 ex1 ws2 ex2) =
  TkFor () `cons`
  foldMap renderWhitespace ws1 <>
  (case ex1 of
     Not{} -> bracket $ renderExpr ex1
     _ -> bracketGenerator ex1) <>
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
renderSubscript (SubscriptExpr a) =
  case a of
    Await{} -> bracket $ renderExpr a
    _ -> bracketTupleGenerator a
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
  foldMap
    (\x -> case x of
       Generator{} -> bracket $ renderExpr x
       _ -> re x)
    b
renderYield re (YieldFrom _ a b c) =
  singleton (TkYield ()) <>
  foldMap renderWhitespace a <>
  singleton (TkFrom ()) <>
  foldMap renderWhitespace b <>
  case c of
    Generator{} -> bracket $ renderExpr c
    _ -> re c
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
  renderParams b <>
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
     UnOp{} -> bracket $ renderExpr a
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
renderExpr (String _ vs) =
  foldMap renderStringLiteral $ correctAdjacentStrings vs
renderExpr (Int _ n ws) = TkInt (() <$ n) `cons` foldMap renderWhitespace ws
renderExpr (Float _ n ws) = TkFloat (() <$ n) `cons` foldMap renderWhitespace ws
renderExpr (Imag _ n ws) = TkImag (() <$ n) `cons` foldMap renderWhitespace ws
renderExpr (Ident name) = renderIdent name
renderExpr (List _ ws1 exprs ws2) =
  TkLeftBracket () `cons`
  foldMap renderWhitespace ws1 <>
  foldMap renderListItems exprs <>
  singleton (TkRightBracket ()) <> foldMap renderWhitespace ws2
renderExpr (ListComp _ ws1 comp ws2) =
  TkLeftBracket () `cons`
  foldMap renderWhitespace ws1 <>
  renderComprehension
    (\e -> case e of
        Yield{} -> bracket $ renderExpr e
        YieldFrom{} -> bracket $ renderExpr e
        _ -> bracketTupleGenerator e)
    comp <>
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
renderExpr (Generator _ a) =
  renderComprehension
    (\e -> case e of
        Yield{} -> bracket $ renderExpr e
        YieldFrom{} -> bracket $ renderExpr e
        _ -> bracketTupleGenerator e)
    a
renderExpr (Await _ ws expr) =
  TkIdent "await" () `cons`
  foldMap renderWhitespace ws <>
  (case expr of
     UnOp{} -> bracket $ renderExpr expr
     BinOp{} -> bracket $ renderExpr expr
     Tuple{} -> bracket $ renderExpr expr
     Not{} -> bracket $ renderExpr expr
     Ternary{} -> bracket $ renderExpr expr
     Lambda{} -> bracket $ renderExpr expr
     _ -> bracketGenerator expr)

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
renderSmallStatement (Assert _ b c d) =
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
renderBlock (Block a b c) =
  foldMap
    (\(_, x, z) ->
        foldMap renderWhitespace x <>
        singleton (renderNewline z))
    a <>
  renderStatement b <>
  foldMap
    (either
       (\(_, x, z) ->
          foldMap renderWhitespace x <>
          singleton (renderNewline z))
        renderStatement)
    c

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
renderCompoundStatement (Fundef _ decos idnt asyncWs ws1 name ws2 params ws3 mty s) =
  foldMap renderDecorator decos <>
  renderIndents idnt <>
  foldMap (\ws -> TkIdent "async" () `cons` foldMap renderWhitespace ws) asyncWs <>
  singleton (TkDef ()) <> foldMap renderWhitespace ws1 <> renderIdent name <>
  bracket (foldMap renderWhitespace ws2 <> renderParams params) <>
  foldMap renderWhitespace ws3 <>
  foldMap
    (\(ws, ty) -> TkRightArrow () `cons` foldMap renderWhitespace ws <> bracketTupleGenerator ty)
    mty <>
  renderSuite s
renderCompoundStatement (If _ idnt ws1 expr s elifs body') =
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
renderCompoundStatement (While _ idnt ws1 expr s) =
  renderIndents idnt <>
  singleton (TkWhile ()) <> foldMap renderWhitespace ws1 <> bracketTupleGenerator expr <>
  renderSuite s
renderCompoundStatement (TryExcept _ idnt a s e f g) =
  renderIndents idnt <>
  singleton (TkTry ()) <> foldMap renderWhitespace a <>
  renderSuite s <>
  foldMap
    (\(idnt, ws1, eas, s) ->
       renderIndents idnt <>
       singleton (TkExcept ()) <> foldMap renderWhitespace ws1 <>
       foldMap renderExceptAs eas <>
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
renderCompoundStatement (TryFinally _ idnt a s idnt2 e s') =
  renderIndents idnt <>
  singleton (TkTry ()) <> foldMap renderWhitespace a <>
  renderSuite s <>
  renderIndents idnt2 <>
  singleton (TkFinally ()) <> foldMap renderWhitespace e <>
  renderSuite s'
renderCompoundStatement (For _ idnt asyncWs a b c d s h) =
  renderIndents idnt <>
  foldMap (\ws -> TkIdent "async" () `cons` foldMap renderWhitespace ws) asyncWs <>
  singleton (TkFor ()) <> foldMap renderWhitespace a <> bracketGenerator b <>
  singleton (TkIn ()) <>
  foldMap renderWhitespace c <>
  renderCommaSep1' bracketTupleGenerator d <>
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
renderCompoundStatement (With _ idnt asyncWs a b s) =
  renderIndents idnt <>
  foldMap (\ws -> TkIdent "async" () `cons` foldMap renderWhitespace ws) asyncWs <>
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
renderArg _ (KeywordArg _ name ws2 expr) =
  renderIdent name <> singleton (TkEq ()) <>
  foldMap renderWhitespace ws2 <>
  bracketTupleGenerator expr
renderArg _ (StarArg _ ws expr) =
  TkStar () `cons`
  foldMap renderWhitespace ws <>
  bracketTupleGenerator expr
renderArg _ (DoubleStarArg _ ws expr) =
  TkDoubleStar () `cons`
  foldMap renderWhitespace ws <>
  bracketTupleGenerator expr

renderParams :: CommaSep (Param v a) -> RenderOutput
renderParams = go False
  where
    go :: Bool -> CommaSep (Param v a) -> RenderOutput
    go _ CommaSepNone = mempty
    go _ (CommaSepOne a) = renderParam a
    go sawStar (CommaSepMany a ws2 b) =
      let
        sawStar' =
          case a of
            StarParam{} -> True;
            DoubleStarParam{} -> True
            _ -> sawStar
      in
        renderParam a <>
        (case b of
           CommaSepNone | sawStar' -> mempty
           _ ->
             singleton (TkComma ()) <>
             foldMap renderWhitespace ws2) <>
      go sawStar' b

renderParam :: Param v a -> RenderOutput
renderParam (PositionalParam _ name mty) =
  renderIdent name <>
  foldMap
    (\(ws, ty) -> TkColon () `cons` foldMap renderWhitespace ws <> bracketTupleGenerator ty)
    mty
renderParam (StarParam _ ws name mty) =
  TkStar () `cons`
  foldMap renderWhitespace ws <>
  foldMap renderIdent name <>
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
  foldMap renderWhitespace ws2 <>
  bracketTupleGenerator expr

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
renderBinOp (At _ ws) = TkAt () `cons` foldMap renderWhitespace ws
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
renderModule ModuleEmpty = mempty
renderModule (ModuleBlankFinal _ a b) =
  foldMap renderWhitespace a <>
  foldMap renderComment b
renderModule (ModuleBlank _ a b c) =
  foldMap renderWhitespace a <>
  singleton (renderNewline b) <>
  renderModule c
renderModule (ModuleStatement a b) =
  renderStatement a <>
  renderModule b

showModule :: Module v a -> Text
showModule = showRenderOutput . renderModule

showStatement :: Statement v a -> Text
showStatement = showRenderOutput . renderStatement

showExpr :: Expr v a -> Text
showExpr = showRenderOutput . bracketGenerator
