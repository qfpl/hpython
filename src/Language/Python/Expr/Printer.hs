{-# language GADTs #-}
{-# language RankNTypes #-}
module Language.Python.Expr.Printer where

import Papa hiding (Plus, Product, Sum, Space, zero, o, argument)

import Data.Functor.Product
import Text.PrettyPrint hiding ((<>), comma, colon)

import qualified Data.Text as T

import Language.Python.AST.Comment
import Language.Python.AST.Identifier
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
import Language.Python.Printer.Combinators
import Language.Python.Printer.Identifier
import Language.Python.Printer.Keywords
import Language.Python.Printer.Symbols

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
      ex e
    FloatDecimalNoBase f' e _ ->
      char '.' <> foldMap digit f' <> ex e 
    FloatDecimalBase b f' e _ ->
      foldMap digit b <> char '.' <> foldMap digit f' <> ex e
  where
    ex =
      foldMap
        (before
          (either (const $ char 'e') (const $ char 'E')) $
          foldMap digit)

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

ifThenElse :: IfThenElse atomType ctxt a -> Doc
ifThenElse (IfThenElse i v1 e v2) =
  betweenWhitespace' (const $ text "if") i <>
  orTest v1 <>
  betweenWhitespace' (const $ text "else") e <>
  test v2

compOperator :: CompOperator -> Doc
compOperator o =
  case o of
    CompLT b a ->
      foldMap whitespaceChar b <>
      char '<' <>
      foldMap whitespaceChar a
    CompGT b a ->
      foldMap whitespaceChar b <>
      char '>' <>
      foldMap whitespaceChar a
    CompEq b a ->
      foldMap whitespaceChar b <>
      text "==" <>
      foldMap whitespaceChar a
    CompNEq b a ->
      foldMap whitespaceChar b <>
      text "!=" <>
      foldMap whitespaceChar a
    CompLEq b a ->
      foldMap whitespaceChar b <>
      text "<=" <>
      foldMap whitespaceChar a
    CompGEq b a ->
      foldMap whitespaceChar b <>
      text ">=" <>
      foldMap whitespaceChar a
    CompIs b a ->
      foldMap whitespaceChar b <>
      text "is" <>
      foldMap whitespaceChar a
    CompIsNot b m a ->
      foldMap whitespaceChar b <>
      text "is" <>
      foldMap whitespaceChar m <>
      text "not" <>
      foldMap whitespaceChar a
    CompIn b a ->
      foldMap whitespaceChar b <>
      text "in" <>
      foldMap whitespaceChar a
    CompNotIn b m a ->
      foldMap whitespaceChar b <>
      text "not" <>
      foldMap whitespaceChar m <>
      text "in" <>
      foldMap whitespaceChar a

tupleElim :: Semigroup r => (a -> r) -> (b -> r) -> (a, b) -> r
tupleElim f g (a, b) = f a <> g b

compIter :: CompIter atomType ctxt a -> Doc
compIter (CompIter val _) = sumElim compFor compIf val

lambdefNocond :: LambdefNocond atomType ctxt a -> Doc
lambdefNocond  (LambdefNocond a e _) =
  text "lambda" <>
  foldMapF
    (betweenF
      (foldMap whitespaceChar)
      (foldMap whitespaceChar)
      (argsList identifier test))
    a <>
  char ':' <>
  whitespaceBeforeF testNocond e

testNocond :: TestNocond atomType ctxt a -> Doc
testNocond (TestNocond val _) = sumElim orTest lambdefNocond val

compIf :: CompIf atomType ctxt a -> Doc
compIf (CompIf kw e i _) =
  betweenWhitespace' (const $ text "if") kw <>
  testNocond e <>
  foldMapF (whitespaceBeforeF compIter) i

exprList :: ExprList atomType ctxt a -> Doc
exprList e =
  case e of
    ExprListSingleStarredNoComma s _ -> starExpr s
    ExprListSingleStarredComma s c _ -> starExpr s <> whitespaceBefore comma c
    ExprListSingle v c _ -> expr v <> foldMap (whitespaceBefore comma) c
    ExprListMany h t c _ ->
      exprOrStar h <>
      foldMapF (beforeF (betweenWhitespace' comma) exprOrStar) t <>
      foldMap (whitespaceBefore comma) c
  where
    exprOrStar = sumElim expr starExpr

compFor :: CompFor atomType ctxt a -> Doc
compFor (CompFor t e i _) =
  beforeF
    (betweenWhitespace' . const $ text "for")
    (whitespaceAfterF exprList)
    t <>
  text "in" <>
  whitespaceBeforeF orTest e <>
  foldMapF (whitespaceBeforeF compIter) i

prodElim
  :: Semigroup r
  => (forall x. f x -> r)
  -> (forall x. g x -> r)
  -> Product f g a -> r
prodElim f g (Pair a b) = f a <> g b

expr :: Expr atomType ctxt a -> Doc
expr (ExprOne v _) = xorExpr v
expr (ExprMany l r _) =
  xorExpr l <>
  foldMapF (beforeF (betweenWhitespace' pipe) xorExpr) r

comparison :: Comparison atomType ctxt a -> Doc
comparison (ComparisonOne v _) = expr v
comparison (ComparisonMany l r _) =
  expr l <>
  foldMapF (beforeF compOperator expr) r

dictItem :: DictItem atomType ctxt a -> Doc
dictItem (DictItem k c v _) =
  test k <>
  betweenWhitespace' colon c <>
  test v

dictUnpacking :: DictUnpacking atomType ctxt a -> Doc
dictUnpacking (DictUnpacking v _) =
  beforeF (betweenWhitespace' doubleAsterisk) expr v

dictOrSetMaker :: DictOrSetMaker atomType ctxt a -> Doc
dictOrSetMaker e =
  case e of
    DictOrSetMakerDictComp h t _ ->
      dictItem h <>
      compFor t
    DictOrSetMakerDictUnpack h t c _ ->
      itemOrUnpacking h <>
      foldMapF (beforeF (betweenWhitespace' comma) itemOrUnpacking) t <>
      foldMap (betweenWhitespace' comma) c
    DictOrSetMakerSetComp h t _ ->
      test h <>
      compFor t
    DictOrSetMakerSetUnpack h t c _ ->
      testOrStar h <>
      foldMapF (beforeF (betweenWhitespace' comma) testOrStar) t <>
      foldMap (betweenWhitespace' comma) c
  where
    itemOrUnpacking = sumElim dictItem dictUnpacking
    testOrStar = sumElim test starExpr

starExpr :: StarExpr atomType ctxt a -> Doc
starExpr (StarExpr val _) =
  char '*' <>
  whitespaceBeforeF expr val

listTestlistComp :: ListTestlistComp atomType ctxt a -> Doc
listTestlistComp t =
  case t of
    ListTestlistCompFor h t' _ ->
      test h <>
      compFor t'
    ListTestlistCompList h t' c _ ->
      test h <>
      foldMapF (beforeF (betweenWhitespace' comma) testOrStar) t' <>
      foldMap (whitespaceBefore comma) c
    ListTestlistCompStarred h t' c _ ->
      starExpr h <>
      foldMapF (beforeF (betweenWhitespace' comma) testOrStar) t' <>
      foldMap (whitespaceBefore comma) c
  where
    testOrStar = sumElim test starExpr
    
tupleTestlistComp :: TupleTestlistComp atomType ctxt a -> Doc
tupleTestlistComp t =
  case t of
    TupleTestlistCompFor h t' _ ->
      test h <>
      compFor t'
    TupleTestlistCompList h t' c _ ->
      test h <>
      foldMapF (beforeF (betweenWhitespace' comma) testOrStar) t' <>
      foldMap (whitespaceBefore comma) c
    TupleTestlistCompStarredOne h c _ ->
      starExpr h <>
      whitespaceBefore comma c
    TupleTestlistCompStarredMany h t' c _ ->
      starExpr h <>
      foldMapF (beforeF (betweenWhitespace' comma) testOrStar) t' <>
      foldMap (whitespaceBefore comma) c
  where
    testOrStar = sumElim test starExpr

testList :: TestList atomType ctxt a -> Doc
testList (TestList h t c _) =
  test h <>
  beforeF (betweenWhitespace' comma) test t <>
  foldMap (whitespaceBefore comma) c

yieldArg :: YieldArg atomType ctxt a -> Doc
yieldArg y =
  case y of
    YieldArgFrom val _ -> text "from" <> whitespaceBeforeF test val
    YieldArgList val _ -> testList val

yieldExpr :: YieldExpr ctxt a -> Doc
yieldExpr (YieldExpr val _) =
  text "yield" <>
  foldMapF (whitespaceBeforeF yieldArg) val

atom :: Atom atomType ctxt a -> Doc
atom a =
  case a of
    AtomParenYield val _ ->
      parens $ betweenWhitespace'F yieldExpr val
    AtomParenNoYield val _ ->
      parens $ betweenWhitespace'F (foldMapF tupleTestlistComp) val
    AtomBracket val _ ->
      brackets $
      betweenWhitespace'F (foldMapF listTestlistComp) val
    AtomCurly val _ ->
      braces $
      betweenWhitespace'F (foldMapF dictOrSetMaker) val
    AtomIdentifier val _ -> identifier val
    AtomInteger val _ -> integer' val
    AtomFloat val _ -> float' val
    AtomString h t _ ->
      sumElim stringLiteral bytesLiteral h <>
      foldMapF (whitespaceBeforeF $ sumElim stringLiteral bytesLiteral) t
    AtomEllipsis _ -> text "..."
    AtomNone _ -> text "None"
    AtomTrue _ -> text "True"
    AtomFalse _ -> text "False"
    AtomImag v _ -> whitespaceBeforeF imag v

argument :: Argument atomType ctxt a -> Doc
argument a =
  case a of
    ArgumentExpr e _ -> test e
    ArgumentFor l e f r _ ->
      whitespaceAfter leftParen l <>
      test e <>
      compFor f <>
      whitespaceBefore rightParen r
    ArgumentDefault l r _ ->
      whitespaceAfterF test l <>
      char '=' <>
      whitespaceBeforeF test r
    ArgumentUnpack s val _ ->
      either asterisk doubleAsterisk s <>
      whitespaceBeforeF test val

argList :: ArgList atomType ctxt a -> Doc
argList e =
  case e of
    ArgListMany h t c _ ->
      argument h <>
      foldMapF (beforeF (betweenWhitespace' comma) argument) t <>
      foldMap (whitespaceBefore comma) c
    ArgListSingleFor e f c _ ->
      test e <>
      compFor f <>
      foldMap (whitespaceBefore comma) c

sliceOp :: SliceOp atomType ctxt a -> Doc
sliceOp (SliceOp val _) = char ':' <> foldMapF (whitespaceBeforeF test) val

subscript :: Subscript atomType ctxt a -> Doc
subscript s =
  case s of
    SubscriptTest val _ -> test val
    SubscriptSlice l c r o _ ->
      whitespaceAfterF (foldMapF test) l <>
      whitespaceAfter (const $ text ":") c <>
      foldMapF (whitespaceAfterF test) r <>
      foldMapF (whitespaceAfterF sliceOp) o

subscriptList :: SubscriptList atomType ctxt a -> Doc
subscriptList (SubscriptList h t c _) =
  subscript h <>
  foldMapF (beforeF (betweenWhitespace' comma) subscript) t <>
  foldMap (whitespaceBefore comma) c

trailer :: Trailer atomType ctxt a -> Doc
trailer t =
  case t of
    TrailerCall val _ -> parens $ betweenWhitespace'F (foldMapF argList) val
    TrailerSubscript val _ ->
      brackets $ betweenWhitespace'F subscriptList val
    TrailerAccess val _ -> char '.' <> whitespaceBeforeF identifier val

atomExpr :: AtomExpr atomType ctxt a -> Doc
atomExpr (AtomExprAwait await a trailers _) =
  whitespaceAfter kAwait await <>
  atom a <>
  foldMapF (whitespaceBeforeF trailer) trailers
atomExpr (AtomExprNoAwait a trailers _) =
  atom a <>
  foldMapF (whitespaceBeforeF trailer) trailers

power :: Power atomType ctxt a -> Doc
power (PowerOne v _) = atomExpr v
power (PowerMany l r _) =
  atomExpr l <>
  beforeF (betweenWhitespace' doubleAsterisk) factor r

factorOp :: FactorOperator -> Doc
factorOp f =
  case f of
    FactorNeg -> char '-'
    FactorPos -> char '+'
    FactorInv -> char '~'

factor :: Factor atomType ctxt a -> Doc
factor f =
  case f of
    FactorNone val _ -> power val
    FactorOne compOp val _ -> whitespaceAfter factorOp compOp <> factor val

termOp :: TermOperator -> Doc
termOp t =
  case t of
    TermMult -> char '*'
    TermAt -> char '@'
    TermFloorDiv -> text "//"
    TermDiv -> char '/'
    TermMod -> char '%'

term :: Term atomType ctxt a -> Doc
term (TermOne v _) = factor v
term (TermMany l r _) =
  factor l <>
  foldMapF (beforeF (betweenWhitespace' termOp) factor) r

arithExpr :: ArithExpr atomType ctxt a -> Doc
arithExpr (ArithExprOne v _) = term v
arithExpr (ArithExprMany l r _) =
  term l <>
  foldMapF (beforeF (betweenWhitespace' (either plus minus)) term) r
  
shiftExpr :: ShiftExpr atomType ctxt a -> Doc
shiftExpr (ShiftExprOne v _) = arithExpr v
shiftExpr (ShiftExprMany l r _) =
  arithExpr l <>
  foldMapF
    (beforeF
      (betweenWhitespace' (either doubleLT doubleGT))
      arithExpr)
    r
  
andExpr :: AndExpr atomType ctxt a -> Doc
andExpr (AndExprOne v _) = shiftExpr v
andExpr (AndExprMany l r _) =
  shiftExpr l <>
  foldMapF (beforeF (betweenWhitespace' ampersand) shiftExpr) r

xorExpr :: XorExpr atomType ctxt a -> Doc
xorExpr (XorExprOne v _) = andExpr v
xorExpr (XorExprMany l r _) =
  andExpr l <>
  foldMapF (beforeF (betweenWhitespace' caret) andExpr) r

notTest :: NotTest atomType ctxt a -> Doc
notTest n =
  case n of
    NotTestMany val _ -> beforeF (whitespaceAfter kNot) notTest val
    NotTestOne val _ -> comparison val
      
andTest :: AndTest atomType ctxt a -> Doc
andTest (AndTestOne v _) = notTest v
andTest (AndTestMany l r _) =
  notTest l <>
  foldMapF
    (beforeF
      (betweenWhitespace' kAnd)
      andTest)
    r

orTest :: OrTest atomType ctxt a -> Doc
orTest (OrTestOne v _) = andTest v
orTest (OrTestMany l r _) =
  andTest l <>
  foldMapF (beforeF (betweenWhitespace' kOr) andTest) r

test :: Test atomType ctxt a -> Doc
test t =
  case t of
    TestCondNoIf v _ -> orTest v
    TestCondIf h t' _ ->
      orTest h <>
      whitespaceBeforeF ifThenElse t'
    TestLambdef a _ -> lambdef a
    
lambdef :: Lambdef atomType ctxt a -> Doc
lambdef (Lambdef a b _) =
  text "lambda" <>
  foldMapF (whitespaceBeforeF $ argsList identifier test) a <>
  beforeF (betweenWhitespace' colon) test b

comment :: Comment a -> Doc
comment (Comment val _) = char '#' <> text (T.unpack val)
