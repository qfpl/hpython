{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
module Language.Python.Expr.Printer where

import Papa hiding (Plus, Product, Sum, Space, zero, o, argument)

import Data.Functor.Product
import Data.Functor.Sum
import Text.PrettyPrint hiding ((<>), comma, colon)

import qualified Data.Text as T

import Language.Python.AST.Comment
import Language.Python.AST.Symbols
import Language.Python.Expr.AST
import Language.Python.Expr.AST.BytesLiteral
import Language.Python.Expr.AST.BytesPrefix
import Language.Python.Expr.AST.CompOperator
import Language.Python.Expr.AST.EscapeSeq
import Language.Python.Expr.AST.FactorOperator
import Language.Python.Expr.AST.Float
import Language.Python.Expr.AST.Digits
import Language.Python.Expr.AST.Imag
import Language.Python.Expr.AST.Integer
import Language.Python.Expr.AST.LongBytes
import Language.Python.Expr.AST.LongBytesChar
import Language.Python.Expr.AST.LongString
import Language.Python.Expr.AST.LongStringChar
import Language.Python.Expr.AST.ShortBytes
import Language.Python.Expr.AST.ShortBytesChar
import Language.Python.Expr.AST.ShortString
import Language.Python.Expr.AST.ShortStringChar
import Language.Python.Expr.AST.StringLiteral
import Language.Python.Expr.AST.StringPrefix
import Language.Python.Expr.AST.TermOperator
import Language.Python.Expr.AST.StringContent (stringContent)
import Language.Python.Printer.ArgsList
import Language.Python.Printer.ArgumentList
import Language.Python.Printer.Combinators
import Language.Python.Printer.Identifier
import Language.Python.Printer.Keywords
import Language.Python.Printer.Symbols
import Language.Python.Printer.TestlistStarExpr

stringPrefix :: StringPrefix -> Doc
stringPrefix sp =
  case sp of
    StringPrefix_r -> char 'r'
    StringPrefix_u -> char 'u'
    StringPrefix_R -> char 'R'
    StringPrefix_U -> char 'U'

shortStringCharDouble :: ShortStringChar DoubleQuote -> Doc
shortStringCharDouble s = char (_ShortStringCharDouble # s)

shortStringCharSingle :: ShortStringChar SingleQuote -> Doc
shortStringCharSingle s = char (_ShortStringCharSingle # s)

escape :: EscapeSeq -> Doc
escape = text . review _Escape

shortString :: ShortString a -> Doc
shortString s =
  case s of
    ShortStringSingle val _ ->
      quotes $
      foldMapOf
        stringContent
        (either escape shortStringCharSingle)
        val
    ShortStringDouble val _ ->
      doubleQuotes $
      foldMapOf
        stringContent
        (either escape shortStringCharDouble)
        val

longStringChar :: LongStringChar -> Doc
longStringChar s = char (_LongStringChar # s)

longStringCharFinalSingle :: LongStringCharFinal SingleQuote -> Doc
longStringCharFinalSingle s = char (_LongStringCharFinalSingle # s)

longStringCharFinalDouble :: LongStringCharFinal DoubleQuote -> Doc
longStringCharFinalDouble s = char (_LongStringCharFinalDouble # s)

longString :: LongString a -> Doc
longString s =
  case s of
    LongStringSingleEmpty _ -> tripled quotes mempty
    LongStringDoubleEmpty _ -> tripled doubleQuotes mempty
    LongStringSingle cs _ ->
      tripled quotes $
      foldMapOf
        stringContent
        (either escape longStringChar)
        cs
    LongStringDouble cs _ ->
      tripled doubleQuotes $
      foldMapOf
        stringContent
        (either escape longStringChar)
        cs

bytesPrefix :: BytesPrefix -> Doc
bytesPrefix b =
  case b of
    BytesPrefix_b -> char 'b'
    BytesPrefix_B -> char 'B'
    BytesPrefix_br -> text "br"
    BytesPrefix_Br -> text "Br"
    BytesPrefix_bR -> text "bR"
    BytesPrefix_BR -> text "BR"
    BytesPrefix_rb -> text "rb"
    BytesPrefix_rB -> text "rB"
    BytesPrefix_Rb -> text "Rb"
    BytesPrefix_RB -> text "RB"

shortBytesCharDouble :: ShortBytesChar DoubleQuote -> Doc
shortBytesCharDouble s = char (_ShortBytesCharDouble # s)

shortBytesCharSingle :: ShortBytesChar SingleQuote -> Doc
shortBytesCharSingle s = char (_ShortBytesCharSingle # s)

shortBytes :: ShortBytes a -> Doc
shortBytes s =
  case s of
    ShortBytesSingle val _ ->
      quotes $
      foldMapOf
        stringContent
        (either escape shortBytesCharSingle)
        val
    ShortBytesDouble val _ ->
      doubleQuotes $
      foldMapOf
        stringContent
        (either escape shortBytesCharDouble)
        val

longBytesChar :: LongBytesChar -> Doc
longBytesChar s = char (_LongBytesChar # s)

longBytesCharFinalSingle :: LongBytesCharFinal SingleQuote -> Doc
longBytesCharFinalSingle s = char (_LongBytesCharFinalSingle # s)

longBytesCharFinalDouble :: LongBytesCharFinal DoubleQuote -> Doc
longBytesCharFinalDouble s = char (_LongBytesCharFinalDouble # s)

longBytes :: LongBytes a -> Doc
longBytes s =
  case s of
    LongBytesSingleEmpty _ -> tripled quotes mempty
    LongBytesDoubleEmpty _ -> tripled doubleQuotes mempty
    LongBytesSingle cs _ ->
      tripled quotes $
      foldMapOf
        stringContent
        (either escape longBytesChar)
        cs
    LongBytesDouble cs _ ->
      tripled doubleQuotes $
      foldMapOf
        stringContent
        (either escape longBytesChar)
        cs

digit :: Digit -> Doc
digit = text . printDigit

nonZeroDigit :: NonZeroDigit -> Doc
nonZeroDigit = text . printNonZeroDigit

octDigit :: OctDigit -> Doc
octDigit = text . printOctDigit

hexDigit :: HexDigit -> Doc
hexDigit = text . printHexDigit

binDigit :: BinDigit -> Doc
binDigit = text . printBinDigit

zero :: Zero -> Doc
zero _ = char '0'

integer' :: Integer' a -> Doc
integer' i =
  case i of
    IntegerDecimal val _ ->
      either
        (\(a, b) -> nonZeroDigit a <> foldMap digit b)
        (foldMap zero)
        val
    IntegerOct val _ ->
      char '0' <>
      before
        (either (const $ char 'o') (const $ char 'O'))
        (foldMap octDigit)
        val
    IntegerHex val _ ->
      char '0' <>
      before
        (either (const $ char 'x') (const $ char 'X'))
        (foldMap hexDigit)
        val
    IntegerBin val _ ->
      char '0' <>
      before
        (either (const $ char 'b') (const $ char 'B'))
        (foldMap binDigit)
        val

float' :: Float' a -> Doc
float' f =
  case f of
    FloatNoDecimal b e _ ->
      foldMap digit b <>
      before eE (foldMap digit) e
    FloatDecimalNoBase f' e _ ->
      char '.' <> foldMap digit f' <> ex e
    FloatDecimalBase b f' e _ ->
      foldMap digit b <> char '.' <> foldMap digit f' <> ex e
  where
    eE = either (const $ char 'e') (const $ char 'E')
    ex = foldMap (before eE $ foldMap digit)

imag :: Imag a -> Doc
imag (Imag val _) =
  afterF
    (either (const $ char 'j') (const $ char 'J'))
    (sumElim float' (foldMap digit . getConst))
    val

stringLiteral :: StringLiteral a -> Doc
stringLiteral (StringLiteral val _) =
  beforeF
    (foldMap stringPrefix)
    (sumElim shortString longString)
  val

bytesLiteral :: BytesLiteral a -> Doc
bytesLiteral (BytesLiteral prefix val _) =
  bytesPrefix prefix <>
  sumElim shortBytes longBytes val

ifThenElse :: Ord a => (ws -> Doc) -> IfThenElse ws atomType ctxt a -> Doc
ifThenElse ws (IfThenElse i v1 e v2) =
  after (foldMap ws) (const $ text "if") i <>
  orTest ws v1 <>
  between' (foldMap ws) (const $ text "else") e <>
  test ws v2

compOperator :: (ws -> Doc) -> CompOperator ws -> Doc
compOperator ws o =
  case o of
    CompLT b a ->
      foldMap ws b <>
      char '<' <>
      foldMap ws a
    CompGT b a ->
      foldMap ws b <>
      char '>' <>
      foldMap ws a
    CompEq b a ->
      foldMap ws b <>
      text "==" <>
      foldMap ws a
    CompNEq b a ->
      foldMap ws b <>
      text "!=" <>
      foldMap ws a
    CompLEq b a ->
      foldMap ws b <>
      text "<=" <>
      foldMap ws a
    CompGEq b a ->
      foldMap ws b <>
      text ">=" <>
      foldMap ws a
    CompIs b a ->
      foldMap ws b <>
      text "is" <>
      foldMap ws a
    CompIsNot b m a ->
      foldMap ws b <>
      text "is" <>
      foldMap ws m <>
      text "not" <>
      foldMap ws a
    CompIn b a ->
      foldMap ws b <>
      text "in" <>
      foldMap ws a
    CompNotIn b m a ->
      foldMap ws b <>
      text "not" <>
      foldMap ws m <>
      text "in" <>
      foldMap ws a

tupleElim :: Semigroup r => (a -> r) -> (b -> r) -> (a, b) -> r
tupleElim f g (a, b) = f a <> g b

compIter :: Ord a => (ws -> Doc) -> CompIter ws atomType ctxt a -> Doc
compIter ws (CompIter val _) = sumElim (compFor ws) (compIf ws) val

lambdefNocond :: Ord a => (ws -> Doc) -> LambdefNocond ws atomType ctxt a -> Doc
lambdefNocond ws (LambdefNocond a e _) =
  text "lambda" <>
  foldMapF
    (betweenF
      (foldMap ws)
      (foldMap ws)
      (argsList ws identifier $ test ws))
    a <>
  char ':' <>
  beforeF (foldMap ws) (testNocond ws) e

testNocond :: Ord a => (ws -> Doc) -> TestNocond ws atomType ctxt a -> Doc
testNocond ws (TestNocond val _) = sumElim (orTest ws) (lambdefNocond ws) val

compIf :: Ord a => (ws -> Doc) -> CompIf ws atomType ctxt a -> Doc
compIf ws (CompIf kw e i _) =
  between' (foldMap ws) (const $ text "if") kw <>
  testNocond ws e <>
  foldMapF (beforeF (foldMap ws) (compIter ws)) i

exprList :: Ord a => (ws -> Doc) -> ExprList ws atomType ctxt a -> Doc
exprList ws e =
  case e of
    ExprListSingleStarredNoComma s _ -> starExpr ws s
    ExprListSingleStarredComma s c _ -> starExpr ws s <> before (foldMap ws) comma c
    ExprListSingle v c _ -> expr ws v <> foldMap (before (foldMap ws) comma) c
    ExprListMany h t c _ ->
      exprOrStar h <>
      foldMapF (beforeF (between' (foldMap ws) comma) exprOrStar) t <>
      foldMap (before (foldMap ws) comma) c
  where
    exprOrStar = sumElim (expr ws) (starExpr ws)

compFor :: Ord a => (ws -> Doc) -> CompFor ws atomType ctxt a -> Doc
compFor ws (CompFor t e i _) =
  beforeF
    (between' (foldMap ws) . const $ text "for")
    (afterF (foldMap ws) $ testlistStarExpr ws expr starExpr)
    t <>
  text "in" <>
  beforeF (foldMap ws) (orTest ws) e <>
  foldMapF (beforeF (foldMap ws) (compIter ws)) i

prodElim
  :: Semigroup r
  => (f a -> r)
  -> (g a -> r)
  -> Product f g a -> r
prodElim f g (Pair a b) = f a <> g b

expr :: Ord a => (ws -> Doc) -> Expr ws atomType ctxt a -> Doc
expr ws (ExprOne v _) = xorExpr ws v
expr ws (ExprMany l r _) =
  xorExpr ws l <>
  foldMapF (beforeF (between' (foldMap ws) pipe) (xorExpr ws)) r

comparison :: Ord a => (ws -> Doc) -> Comparison ws atomType ctxt a -> Doc
comparison ws (ComparisonOne v _) = expr ws v
comparison ws (ComparisonMany l r _) =
  expr ws l <>
  foldMapF (beforeF (compOperator ws) (expr ws)) r

dictItem :: Ord a => (ws -> Doc) -> DictItem ws atomType ctxt a -> Doc
dictItem ws (DictItem k c v _) =
  test ws k <>
  between' (foldMap ws) colon c <>
  test ws v

dictUnpacking :: Ord a => (ws -> Doc) -> DictUnpacking ws atomType ctxt a -> Doc
dictUnpacking ws (DictUnpacking v _) =
  beforeF (between' (foldMap ws) doubleAsterisk) (expr ws) v

dictOrSetMaker :: forall a ws atomType ctxt. Ord a => (ws -> Doc) -> DictOrSetMaker ws atomType ctxt a -> Doc
dictOrSetMaker ws e =
  case e of
    DictOrSetMakerDictComp h t _ ->
      dictItem ws h <>
      compFor ws t
    DictOrSetMakerDictUnpack h t c _ ->
      itemOrUnpacking h <>
      foldMapF (beforeF (between' (foldMap ws) comma) itemOrUnpacking) t <>
      foldMap (between' (foldMap ws) comma) c
    DictOrSetMakerSetComp h t _ ->
      test ws h <>
      compFor ws t
    DictOrSetMakerSetUnpack h t c _ ->
      testOrStar h <>
      foldMapF (beforeF (between' (foldMap ws) comma) testOrStar) t <>
      foldMap (between' (foldMap ws) comma) c
  where
    itemOrUnpacking :: Ord a => Sum (DictItem ws atomType ctxt) (DictUnpacking ws atomType ctxt) a -> Doc
    itemOrUnpacking = sumElim (dictItem ws) (dictUnpacking ws)

    testOrStar :: Ord a => Sum (Test ws atomType ctxt) (StarExpr ws atomType ctxt) a -> Doc
    testOrStar = sumElim (test ws) (starExpr ws)

starExpr :: Ord a => (ws -> Doc) -> StarExpr ws atomType ctxt a -> Doc
starExpr ws (StarExpr val _) =
  char '*' <>
  beforeF (foldMap ws) (expr ws) val

listTestlistComp :: Ord a => (ws -> Doc) -> ListTestlistComp ws atomType ctxt a -> Doc
listTestlistComp ws t =
  case t of
    ListTestlistCompFor h t' _ ->
      test ws h <>
      compFor ws t'
    ListTestlistCompList h t' c _ ->
      test ws h <>
      foldMapF (beforeF (between' (foldMap ws) comma) testOrStar) t' <>
      foldMap (before (foldMap ws) comma) c
    ListTestlistCompStarred h t' c _ ->
      starExpr ws h <>
      foldMapF (beforeF (between' (foldMap ws) comma) testOrStar) t' <>
      foldMap (before (foldMap ws) comma) c
  where
    testOrStar = sumElim (test ws) (starExpr ws)

tupleTestlistComp :: Ord a => (ws -> Doc) -> TupleTestlistComp ws atomType ctxt a -> Doc
tupleTestlistComp ws t =
  case t of
    TupleTestlistCompFor h t' _ ->
      test ws h <>
      compFor ws t'
    TupleTestlistCompList h t' c _ ->
      test ws h <>
      foldMapF (beforeF (between' (foldMap ws) comma) testOrStar) t' <>
      foldMap (before (foldMap ws) comma) c
    TupleTestlistCompStarredOne h c _ ->
      starExpr ws h <>
      before (foldMap ws) comma c
    TupleTestlistCompStarredMany h t' c _ ->
      starExpr ws h <>
      foldMapF (beforeF (between' (foldMap ws) comma) testOrStar) t' <>
      foldMap (before (foldMap ws) comma) c
  where
    testOrStar = sumElim (test ws) (starExpr ws)

testList :: Ord a => (ws -> Doc) -> TestList ws atomType ctxt a -> Doc
testList ws (TestList h t c _) =
  test ws h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (between' (foldMap ws) comma) (test ws))
    t <>
  foldMap (before (foldMap ws) comma) c

yieldArg :: Ord a => (ws -> Doc) -> YieldArg ws atomType ctxt a -> Doc
yieldArg ws y =
  case y of
    YieldArgFrom val _ -> text "from" <> beforeF (foldMap ws) (test ws) val
    YieldArgList val _ -> testList ws val

yieldExpr :: Ord a => (ws -> Doc) -> YieldExpr ws ctxt a -> Doc
yieldExpr ws (YieldExpr val _) =
  text "yield" <>
  foldMapF (beforeF (foldMap ws) (yieldArg ws)) val

atom :: Ord a => (ws -> Doc) -> Atom ws atomType ctxt a -> Doc
atom ws a =
  case a of
    AtomNoInt val _ -> atomNoInt ws val
    AtomInteger val _ -> integer' val

atomNoInt :: Ord a => (ws -> Doc) -> AtomNoInt ws atomType ctxt a -> Doc
atomNoInt ws a =
  case a of
    AtomParenYield val _ ->
      parens $ between'F (foldMap anyWhitespaceChar) (yieldExpr anyWhitespaceChar) val
    AtomParenNoYield val _ ->
      parens $ between'F (foldMap anyWhitespaceChar) (foldMapF (tupleTestlistComp anyWhitespaceChar)) val
    AtomBracket val _ ->
      brackets $
      between'F (foldMap anyWhitespaceChar) (foldMapF (listTestlistComp anyWhitespaceChar)) val
    AtomCurly val _ ->
      braces $
      between'F (foldMap anyWhitespaceChar) (foldMapF (dictOrSetMaker anyWhitespaceChar)) val
    AtomIdentifier val _ -> identifier val
    AtomFloat val _ -> float' val
    AtomString h t _ ->
      sumElim stringLiteral bytesLiteral h <>
      foldMapF (beforeF (foldMap ws) $ sumElim stringLiteral bytesLiteral) t
    AtomEllipsis _ -> text "..."
    AtomNone _ -> text "None"
    AtomTrue _ -> text "True"
    AtomFalse _ -> text "False"
    AtomImag v _ -> beforeF (foldMap ws) imag v

sliceOp :: Ord a => (ws -> Doc) -> SliceOp ws atomType ctxt a -> Doc
sliceOp ws (SliceOp val _) = char ':' <> foldMapF (beforeF (foldMap ws) (test ws)) val

subscript :: Ord a => (ws -> Doc) -> Subscript ws atomType ctxt a -> Doc
subscript ws s =
  case s of
    SubscriptTest val _ -> test ws val
    SubscriptSlice l c r o _ ->
      afterF (foldMap ws) (foldMapF $ test ws) l <>
      after (foldMap ws) (const $ text ":") c <>
      foldMapF (afterF (foldMap ws) (test ws)) r <>
      foldMapF (afterF (foldMap ws) (sliceOp ws)) o

subscriptList :: Ord a => (ws -> Doc) -> SubscriptList ws atomType ctxt a -> Doc
subscriptList ws (SubscriptList h t c _) =
  subscript ws h <>
  foldMapF (beforeF (between' (foldMap ws) comma) (subscript ws)) t <>
  foldMap (before (foldMap ws) comma) c

trailer :: Ord a => (ws -> Doc) -> Trailer ws atomType ctxt a -> Doc
trailer ws t =
  case t of
    TrailerCall val _ ->
      parens $
      beforeF
        (foldMap anyWhitespaceChar)
        (foldMapF $ argumentList identifier test) val
    TrailerSubscript val _ ->
      brackets $ between'F (foldMap anyWhitespaceChar) (subscriptList anyWhitespaceChar) val
    TrailerAccess val _ -> char '.' <> beforeF (foldMap ws) identifier val

atomExprTrailers :: Ord a => (ws -> Doc) -> AtomExprTrailers ws atomType ctxt a -> Doc
atomExprTrailers ws (AtomExprTrailersBase v t _) =
  atomNoInt ws v <>
  beforeF (foldMap ws) (trailer ws) t
atomExprTrailers ws (AtomExprTrailersMany v t _) =
  atomExprTrailers ws v <>
  beforeF (foldMap ws) (trailer ws) t

atomExpr :: Ord a => (ws -> Doc) -> AtomExpr ws atomType ctxt a -> Doc
atomExpr ws (AtomExprSingle v _) =
  atom ws v
atomExpr ws (AtomExprTrailers v _) =
  atomExprTrailers ws v
atomExpr ws (AtomExprAwaitSingle a b _) =
  after (foldMap ws) kAwait a <>
  atom ws b
atomExpr ws (AtomExprAwaitTrailers a b _) =
  after (foldMap ws) kAwait a <>
  atomExprTrailers ws b

power :: Ord a => (ws -> Doc) -> Power ws atomType ctxt a -> Doc
power ws (PowerOne v _) = atomExpr ws v
power ws (PowerMany l r _) =
  atomExpr ws l <>
  beforeF (between' (foldMap ws) doubleAsterisk) (factor ws) r

factorOp :: FactorOperator -> Doc
factorOp f =
  case f of
    FactorNeg -> char '-'
    FactorPos -> char '+'
    FactorInv -> char '~'

factor :: Ord a => (ws -> Doc) -> Factor ws atomType ctxt a -> Doc
factor ws f =
  case f of
    FactorNone val _ -> power ws val
    FactorOne compOp val _ -> after (foldMap ws) factorOp compOp <> factor ws val

termOp :: TermOperator -> Doc
termOp t =
  case t of
    TermMult -> char '*'
    TermAt -> char '@'
    TermFloorDiv -> text "//"
    TermDiv -> char '/'
    TermMod -> char '%'

term :: Ord a => (ws -> Doc) -> Term ws atomType ctxt a -> Doc
term ws (TermOne v _) = factor ws v
term ws (TermMany l r _) =
  factor ws l <>
  foldMapF (beforeF (between' (foldMap ws) termOp) (factor ws)) r

arithExpr :: Ord a => (ws -> Doc) -> ArithExpr ws atomType ctxt a -> Doc
arithExpr ws (ArithExprOne v _) = term ws v
arithExpr ws (ArithExprMany l r _) =
  term ws l <>
  foldMapF (beforeF (between' (foldMap ws) (either plus minus)) (term ws)) r

shiftExpr :: Ord a => (ws -> Doc) -> ShiftExpr ws atomType ctxt a -> Doc
shiftExpr ws (ShiftExprOne v _) = arithExpr ws v
shiftExpr ws (ShiftExprMany l r _) =
  arithExpr ws l <>
  foldMapF
    (beforeF
      (between' (foldMap ws) (either doubleLT doubleGT))
      (arithExpr ws))
    r

andExpr :: Ord a => (ws -> Doc) -> AndExpr ws atomType ctxt a -> Doc
andExpr ws (AndExprOne v _) = shiftExpr ws v
andExpr ws (AndExprMany l r _) =
  shiftExpr ws l <>
  foldMapF (beforeF (between' (foldMap ws) ampersand) (shiftExpr ws)) r

xorExpr :: Ord a => (ws -> Doc) -> XorExpr ws atomType ctxt a -> Doc
xorExpr ws (XorExprOne v _) = andExpr ws v
xorExpr ws (XorExprMany l r _) =
  andExpr ws l <>
  foldMapF (beforeF (between' (foldMap ws) caret) (andExpr ws)) r

notTest :: Ord a => (ws -> Doc) -> NotTest ws atomType ctxt a -> Doc
notTest ws n =
  case n of
    NotTestMany val _ -> beforeF (after (foldMap ws) kNot) (notTest ws) val
    NotTestOne val _ -> comparison ws val

andTest :: Ord a => (ws -> Doc) -> AndTest ws atomType ctxt a -> Doc
andTest ws (AndTestOne v _) = notTest ws v
andTest ws (AndTestMany l r _) =
  notTest ws l <>
  foldMapF
    (beforeF
      (between' (foldMap ws) kAnd)
      (notTest ws))
    r

orTest :: Ord a => (ws -> Doc) -> OrTest ws atomType ctxt a -> Doc
orTest ws (OrTestOne v _) = andTest ws v
orTest ws (OrTestMany l r _) =
  andTest ws l <>
  foldMapF (beforeF (between' (foldMap ws) kOr) (andTest ws)) r

test :: Ord a => (ws -> Doc) -> Test ws atomType ctxt a -> Doc
test ws t =
  case t of
    TestCondNoIf v _ -> orTest ws v
    TestCondIf h t' _ ->
      orTest ws h <>
      beforeF (foldMap ws) (ifThenElse ws) t'
    TestLambdef a _ -> lambdef ws a

lambdef :: Ord a => (ws -> Doc) -> Lambdef ws atomType ctxt a -> Doc
lambdef ws (Lambdef a b _) =
  text "lambda" <>
  foldMapF
    (beforeF (foldMap ws) $ argsList ws identifier (test ws))
    a <>
  beforeF (between' (foldMap ws) colon) (test ws) b

comment :: Comment a -> Doc
comment (Comment val _) = char '#' <> text (T.unpack val)
