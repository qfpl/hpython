{-# language GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
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
  , renderCompFor, renderCompIf, renderComprehension, renderBinOp, renderSubscript
  , renderPyChars
  )
where

import Control.Lens.Getter (view)
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.Plated (transform)
import Control.Lens.Review ((#))
import Data.Bifoldable (bifoldMap)
import Data.Digit.Char (charHeXaDeCiMaL, charOctal)
import Data.Foldable (toList)
import Data.Maybe (maybe)
import Data.Semigroup (Semigroup(..))

import Language.Python.Internal.Syntax
import Language.Python.Internal.Token (PyToken(..))

newtype RenderOutput
  = RenderOutput
  { unRenderOutput :: [PyToken ()]
  } deriving (Eq, Show, Semigroup, Monoid)

singleton :: PyToken () -> RenderOutput
singleton a = RenderOutput [a]

cons :: PyToken () -> RenderOutput -> RenderOutput
cons a (RenderOutput b) = RenderOutput $ a : b
infixr 5 `cons`

showRenderOutput :: RenderOutput -> String
showRenderOutput =
  foldMap showToken .
  correctSpaces .
  correctNewlines .
  unRenderOutput
  where
    correctSpaces =
      transform $
      \case
        a : b : rest
          | isIdentifierChar (last $ showToken a)
          , isIdentifierChar (head $ showToken b)
          -> a : TkSpace () : b : rest
        a@(TkString _ qt _ _ _) : b@(TkString _ qt' _ _ _) : rest
          | qt == qt' -> a : TkSpace () : b : rest
        a -> a

    correctNewlines =
      transform $
      \case
        TkNewline CR () : TkNewline LF () : rest ->
          TkNewline CRLF () : TkNewline LF () : rest
        TkContinued CR () : TkNewline LF () : rest ->
          TkContinued CRLF () : TkNewline LF () : rest
        a -> a

showStringPrefix :: StringPrefix -> String
showStringPrefix sp =
  case sp of
    Prefix_r -> "r"
    Prefix_R -> "R"
    Prefix_u -> "u"
    Prefix_U -> "U"

showBytesPrefix :: BytesPrefix -> String
showBytesPrefix sp =
  case sp of
    Prefix_b -> "b"
    Prefix_B -> "B"
    Prefix_br -> "br"
    Prefix_Br -> "Br"
    Prefix_bR -> "bR"
    Prefix_BR -> "BR"
    Prefix_rb -> "rb"
    Prefix_rB -> "rB"
    Prefix_Rb -> "Rb"
    Prefix_RB -> "RB"

showQuoteType :: QuoteType -> String
showQuoteType qt =
  case qt of
    DoubleQuote -> "\""
    SingleQuote -> "\'"

showToken :: PyToken a -> String
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
    TkOr{} -> "or"
    TkAnd{} -> "and"
    TkIs{} -> "is"
    TkNot{} -> "not"
    TkGlobal{} -> "global"
    TkNonlocal{} -> "nonlocal"
    TkDel{} -> "del"
    TkImport{} -> "import"
    TkFrom{} -> "from"
    TkAs{} -> "as"
    TkRaise{} -> "raise"
    TkTry{} -> "try"
    TkExcept{} -> "except"
    TkFinally{} -> "finally"
    TkClass{} -> "class"
    TkFor{} -> "for"
    TkIn{} -> "in"
    TkYield{} -> "yield"
    TkInt i _ -> show i
    TkFloat i i' _ -> show i <> foldMap (("." <>) . show) i'
    TkIdent s _ -> s
    TkString sp qt st s _ ->
      let
        quote =
          showQuoteType qt >>= (case st of; LongString -> replicate 3; ShortString -> pure)
      in
        foldMap showStringPrefix sp <>
        quote <>
        renderPyChars qt st s <>
        quote
    TkBytes sp qt st s _ ->
      let
        quote =
          showQuoteType qt >>= (case st of; LongString -> replicate 3; ShortString -> pure)
      in
        showBytesPrefix sp <>
        quote <>
        renderPyChars qt st s <>
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
    TkComment s _ -> "#" <> s
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
    TkSlashEq{} -> "/="
    TkPercentEq{} -> "%="
    TkAmphersandEq{} -> "&="
    TkPipeEq{} -> "|="
    TkCaretEq{} -> "^="
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

intToHex :: Int -> String
intToHex n = go n []
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

renderPyChars :: QuoteType -> StringType -> [PyChar] -> String
renderPyChars qt st = go
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
                _ -> c : endSingleQuotesShort cs
            (SingleQuote, LongString) ->
              case c of
                '\'' -> '\\' : '\'' : go cs
                _ -> c : endSingleQuotesLong (transform escapeTripleSingleQuotes cs)
            (DoubleQuote, ShortString) ->
              case c of
                '"' -> '\\' : '"' : go cs
                _ -> c : endDoubleQuotesShort cs
            (DoubleQuote, LongString) ->
              case c of
                '"' -> '\\' : '"' : go cs
                _ -> c : endDoubleQuotesLong (transform escapeTripleDoubleQuotes cs)

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

renderComment :: Comment -> PyToken ()
renderComment (Comment s) = TkComment s ()

bracketTernary :: (Expr v a -> RenderOutput) -> Expr v a -> RenderOutput
bracketTernary _ e@Ternary{} = bracket $ renderExpr e
bracketTernary f e = f e

renderCompFor :: CompFor v a -> RenderOutput
renderCompFor (CompFor _ ws1 ex1 ws2 ex2) =
  TkFor () `cons`
  foldMap renderWhitespace ws1 <>
  bracketGenerator ex1 <>
  singleton (TkIn ()) <>
  foldMap renderWhitespace ws2 <>
  bracketTernary bracketTupleGenerator ex2

renderCompIf :: CompIf v a -> RenderOutput
renderCompIf (CompIf _ ws ex) =
  TkIf () `cons`
  foldMap renderWhitespace ws <>
  bracketTernary bracketTupleGenerator ex

renderComprehension :: Comprehension v a -> RenderOutput
renderComprehension (Comprehension _ expr cf cs) =
  bracketTupleGenerator expr <>
  renderCompFor cf <>
  foldMap (bifoldMap renderCompFor renderCompIf) cs

renderDictItem :: DictItem v a -> RenderOutput
renderDictItem (DictItem _ a b c) =
  bracketTupleGenerator a <>
  singleton (TkColon ()) <>
  foldMap renderWhitespace b <>
  bracketTupleGenerator c

renderStringLiteral :: StringLiteral a -> RenderOutput
renderStringLiteral (StringLiteral _ a b c d e) =
  TkString a b c d () `cons`
  foldMap renderWhitespace e
renderStringLiteral (BytesLiteral _ a b c d e) =
  TkBytes a b c d () `cons`
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

renderExpr :: Expr v a -> RenderOutput
renderExpr e@Yield{} = bracket $ renderYield renderExpr e
renderExpr e@YieldFrom{} = bracket $ renderYield renderExpr e
renderExpr (Ternary _ a b c d e) =
  (case a of
     Generator{} -> bracket $ renderExpr a
     _ -> bracketTupleGenerator a) <>
  singleton (TkIf ()) <> foldMap renderWhitespace b <>
  bracketTernary bracketTupleGenerator c <>
  singleton (TkElse ()) <> foldMap renderWhitespace d <>
  bracketTupleGenerator e
renderExpr (Subscript _ a b c d) =
  (case a of
     BinOp{} -> bracket $ renderExpr a
     Not{} -> bracket $ renderExpr a
     Ternary{} -> bracket $ renderExpr a
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
    _ -> bracketTupleGenerator e
renderExpr (Parens _ ws1 e ws2) =
  bracket (foldMap renderWhitespace ws1 <> renderYield renderExpr e) <>
  foldMap renderWhitespace ws2
renderExpr (Bool _ b ws) =
  (if b then TkTrue () else TkFalse ()) `cons`
  foldMap renderWhitespace ws
renderExpr (Negate _ ws expr) =
  TkMinus () `cons`
  foldMap renderWhitespace ws <>
  case expr of
    BinOp _ _ Exp{} _ -> bracketTupleGenerator expr
    BinOp{} -> bracket $ renderExpr expr
    Deref _ Int{} _ _ -> bracket $ renderExpr expr
    Not{} -> bracket $ renderExpr expr
    Ternary{} -> bracket $ renderExpr expr
    _ -> bracketTupleGenerator expr
renderExpr (String _ vs) = foldMap renderStringLiteral vs
renderExpr (Int _ n ws) = TkInt n () `cons` foldMap renderWhitespace ws
renderExpr (Ident _ name) = renderIdent name
renderExpr (List _ ws1 exprs ws2) =
  TkLeftBracket () `cons`
  foldMap renderWhitespace ws1 <>
  foldMap
    (renderCommaSep1' bracketTupleGenerator)
    exprs <>
  singleton (TkRightBracket ()) <> foldMap renderWhitespace ws2
renderExpr (ListComp _ ws1 comp ws2) =
  TkLeftBracket () `cons`
  foldMap renderWhitespace ws1 <>
  renderComprehension comp <>
  singleton (TkRightBracket ()) <> foldMap renderWhitespace ws2
renderExpr (Call _ expr ws args ws2) =
  (case expr of
     Int _ n _ | n < 0 -> bracket $ renderExpr expr
     BinOp{} -> bracket $ renderExpr expr
     Tuple{} -> bracket $ renderExpr expr
     Not{} -> bracket $ renderExpr expr
     Ternary{} -> bracket $ renderExpr expr
     _ -> bracketGenerator expr) <>
  bracket (foldMap renderWhitespace ws <> foldMap renderArgs args) <>
  foldMap renderWhitespace ws2
renderExpr (Deref _ expr ws name) =
  (case expr of
     Int{} -> bracket $ renderExpr expr
     BinOp{} -> bracket $ renderExpr expr
     Tuple{} -> bracket $ renderExpr expr
     Not{} -> bracket $ renderExpr expr
     Negate{} -> bracket $ renderExpr expr
     Ternary{} -> bracket $ renderExpr expr
     _ -> bracketGenerator expr) <>
  singleton (TkDot ()) <>
  foldMap renderWhitespace ws <>
  renderIdent name
renderExpr (None _ ws) = TkNone () `cons` foldMap renderWhitespace ws
renderExpr (BinOp _ e1 op e2) =
  (if shouldBracketLeft op e1 then bracket else id) (bracketTernary bracketGenerator e1) <>
  renderBinOp op <>
  (if shouldBracketRight op e2 then bracket else id) (bracketTernary bracketGenerator e2)
renderExpr (Tuple _ a ws c) =
  bracketTupleGenerator a <> singleton (TkComma ()) <> foldMap renderWhitespace ws <>
  foldMap
    (renderCommaSep1' bracketTupleGenerator)
    c
renderExpr (Dict _ a b c) =
  TkLeftBrace () `cons`
  foldMap renderWhitespace a <>
  foldMap (renderCommaSep1' renderDictItem) b <>
  singleton (TkRightBrace ()) <>
  foldMap renderWhitespace c
renderExpr (Set _ a b c) =
  TkLeftBrace () `cons`
  foldMap renderWhitespace a <>
  renderCommaSep1' bracketTupleGenerator b <>
  singleton (TkRightBrace ()) <>
  foldMap renderWhitespace c
renderExpr (Generator _ a) = renderComprehension a

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
     AmphersandEq{} -> TkAmphersandEq ()
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
renderSmallStatement (Assign _ lvalue ws2 rvalue) =
  renderExpr lvalue <> singleton (TkEq ()) <>
  foldMap renderWhitespace ws2 <> renderYield bracketGenerator rvalue
renderSmallStatement (AugAssign _ lvalue as rvalue) =
  renderExpr lvalue <> renderAugAssign as <> bracketGenerator rvalue
renderSmallStatement (Pass _) = singleton $ TkPass ()
renderSmallStatement (Continue _) = singleton $ TkContinue ()
renderSmallStatement (Break _) = singleton $ TkBreak ()
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
       (\(x, y, z) ->
          foldMap renderWhitespace x <>
          maybe mempty (singleton . renderComment) y
          <> singleton (renderNewline z))
        renderStatement) .
  view _Wrapped

renderSuite :: Suite v a -> RenderOutput
renderSuite (Suite _ a b c d) =
  TkColon () `cons`
  foldMap renderWhitespace a <>
  foldMap (singleton . renderComment) b <>
  singleton (renderNewline c) <>
  renderBlock d

renderCompoundStatement :: CompoundStatement v a -> RenderOutput
renderCompoundStatement (Fundef idnt _ ws1 name ws2 params ws3 s) =
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
renderCompoundStatement (ClassDef idnt _ a b c s) =
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
renderParam (PositionalParam _ name) =
  renderIdent name
renderParam (StarParam _ ws name) =
  TkStar () `cons` foldMap renderWhitespace ws <> renderIdent name
renderParam (DoubleStarParam _ ws name) =
  TkDoubleStar () `cons` foldMap renderWhitespace ws <> renderIdent name
renderParam (KeywordParam _ name ws2 expr) =
  renderIdent name <> singleton (TkEq ()) <>
  foldMap renderWhitespace ws2 <> bracketTupleGenerator expr

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

renderIndents :: Indents a -> RenderOutput
renderIndents (Indents is _) = foldMap renderIndent is

renderModule :: Module v a -> RenderOutput
renderModule (Module ms) =
  foldMap
    (either
       (\(a, b, c) ->
          renderIndents a <>
          maybe mempty (singleton . renderComment) b <>
          maybe mempty (singleton . renderNewline) c)
       renderStatement)
    ms

showModule :: Module v a -> String
showModule = showRenderOutput . renderModule

showStatement :: Statement v a -> String
showStatement = showRenderOutput . renderStatement

showExpr :: Expr v a -> String
showExpr = showRenderOutput . bracketGenerator
