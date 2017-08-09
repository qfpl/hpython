{-# language RankNTypes #-}
module Language.Python.Printer where

import Prelude (error)

import Papa hiding (Plus, Product, Sum, Space, zero, o, argument)

import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import qualified Data.Text as T

import Text.PrettyPrint hiding ((<>), comma, colon)

import Data.Separated.After (After(..))
import Data.Separated.Before (Before(..))
import Data.Separated.Between (Between(..), Between'(..))
import Language.Python.AST
import Language.Python.AST.BytesEscapeSeq
import Language.Python.AST.Keywords
import Language.Python.AST.ShortBytesChar
import Language.Python.AST.ShortStringChar
import Language.Python.AST.LongBytesChar
import Language.Python.AST.LongStringChar
import Language.Python.AST.Symbols

identifier :: Identifier a -> Doc
identifier i = i ^. identifier_value . to T.unpack . to text

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

kAwait :: KAwait -> Doc
kAwait _ = text "await"

kOr :: KOr -> Doc
kOr _ = text "or"

kAnd :: KAnd -> Doc
kAnd _ = text "and"

kNot :: KNot -> Doc
kNot _ = text "not"

foldMapF :: (Foldable f, Monoid r) => (g a -> r) -> Compose f g a -> r
foldMapF f = foldMap f . getCompose

before :: Semigroup r => (s -> r) -> (a -> r) -> Before s a -> r
before f g (Before s a) = f s <> g a

beforeF
  :: Semigroup r
  => (s -> r)
  -> (forall x. f x -> r)
  -> Compose (Before s) f a
  -> r
beforeF f g = before f g . getCompose

after :: Semigroup r => (s -> r) -> (a -> r) -> After s a -> r
after f g (After s a) = g a <> f s

afterF
  :: Semigroup r
  => (s -> r)
  -> (forall x. f x -> r)
  -> Compose (After s) f a
  -> r
afterF f g = after f g . getCompose

between
  :: Semigroup r
  => (s -> r)
  -> (t -> r)
  -> (a -> r)
  -> Between s t a
  -> r
between f g h (Between s a t) = f s <> h a <> g t

betweenF
  :: Semigroup r
  => (s -> r)
  -> (t -> r)
  -> (forall x. f x -> r)
  -> Compose (Between s t) f a
  -> r
betweenF f g h = between f g h . getCompose

between'
  :: Semigroup r
  => (s -> r)
  -> (a -> r)
  -> Between' s a
  -> r
between' f g (Between' (Between s a t)) = f s <> g a <> f t


whitespaceAfterF
  :: Foldable g
  => (forall x. f x -> Doc)
  -> Compose (After (g WhitespaceChar)) f a
  -> Doc
whitespaceAfterF f = after (foldMap whitespaceChar) f . getCompose

whitespaceAfter
  :: Foldable g
  => (a -> Doc) -> After (g WhitespaceChar) a -> Doc
whitespaceAfter = after (foldMap whitespaceChar)

whitespaceBefore
  :: Foldable g
  => (a -> Doc)
  -> Before (g WhitespaceChar) a -> Doc
whitespaceBefore = before (foldMap whitespaceChar)

whitespaceBeforeF
  :: Foldable g
  => (forall x. f x -> Doc)
  -> Compose (Before (g WhitespaceChar)) f a
  -> Doc
whitespaceBeforeF f = before (foldMap whitespaceChar) f . getCompose

betweenWhitespace'F
  :: Foldable g
  => (forall x. f x -> Doc)
  -> Compose (Between' (g WhitespaceChar)) f a
  -> Doc
betweenWhitespace'F f = between' (foldMap whitespaceChar) f . getCompose

betweenWhitespace'
  :: Foldable f
  => (a -> Doc) -> Between' (f WhitespaceChar) a -> Doc
betweenWhitespace' = between' (foldMap whitespaceChar)

stringPrefix :: StringPrefix -> Doc
stringPrefix sp =
  case sp of
    StringPrefix_r -> char 'r'
    StringPrefix_u -> char 'u'
    StringPrefix_R -> char 'R'
    StringPrefix_U -> char 'U'

stringEscapeSeq :: StringEscapeSeq -> Doc
stringEscapeSeq (StringEscapeSeq s) = char '\\' <> char s

shortStringCharDouble :: ShortStringChar DoubleQuote -> Doc
shortStringCharDouble s = char (_ShortStringCharDouble # s)

shortStringCharSingle :: ShortStringChar SingleQuote -> Doc
shortStringCharSingle s = char (_ShortStringCharSingle # s)

tripled :: (Doc -> Doc) -> Doc -> Doc
tripled f = f . f . f

shortString :: ShortString a -> Doc
shortString s =
  case s of
    ShortStringSingle val _ ->
      quotes $ foldMap (either shortStringCharSingle stringEscapeSeq) val
    ShortStringDouble val _ ->
      doubleQuotes $ foldMap (either shortStringCharDouble stringEscapeSeq) val
      
longStringChar :: LongStringChar -> Doc
longStringChar s = char (_LongStringChar # s)
      
longString :: LongString a -> Doc
longString s =
  case s of
    LongStringSingle val _ ->
      tripled quotes $ foldMap (either longStringChar stringEscapeSeq) val
    LongStringDouble val _ ->
      tripled doubleQuotes $
      foldMap (either longStringChar stringEscapeSeq) val

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

bytesEscapeSeq :: BytesEscapeSeq -> Doc
bytesEscapeSeq b = char '\\' <> char (_BytesEscapeSeq # b)

shortBytes :: ShortBytes a -> Doc
shortBytes s =
  case s of
    ShortBytesSingle val _ ->
      quotes $ foldMap (either shortBytesCharSingle bytesEscapeSeq) val
    ShortBytesDouble val _ ->
      doubleQuotes $ foldMap (either shortBytesCharDouble bytesEscapeSeq) val
      
longBytesChar :: LongBytesChar -> Doc
longBytesChar s = char (_LongBytesChar # s)
      
longBytes :: LongBytes a -> Doc
longBytes s =
  case s of
    LongBytesSingle val _ ->
      tripled quotes $ foldMap (either longBytesChar bytesEscapeSeq) val
    LongBytesDouble val _ ->
      tripled doubleQuotes $
      foldMap (either longBytesChar bytesEscapeSeq) val

digit :: Digit -> Doc
digit d =
  case d of
    Digit_0 -> char '0'
    Digit_1 -> char '1'
    Digit_2 -> char '2'
    Digit_3 -> char '3'
    Digit_4 -> char '4'
    Digit_5 -> char '5'
    Digit_6 -> char '6'
    Digit_7 -> char '7'
    Digit_8 -> char '8'
    Digit_9 -> char '9'

nonZeroDigit :: NonZeroDigit -> Doc
nonZeroDigit d =
  case d of
    NonZeroDigit_1 -> char '1'
    NonZeroDigit_2 -> char '2'
    NonZeroDigit_3 -> char '3'
    NonZeroDigit_4 -> char '4'
    NonZeroDigit_5 -> char '5'
    NonZeroDigit_6 -> char '6'
    NonZeroDigit_7 -> char '7'
    NonZeroDigit_8 -> char '8'
    NonZeroDigit_9 -> char '9'

octDigit :: OctDigit -> Doc
octDigit d =
  case d of
    OctDigit_0 -> char '0'
    OctDigit_1 -> char '1'
    OctDigit_2 -> char '2'
    OctDigit_3 -> char '3'
    OctDigit_4 -> char '4'
    OctDigit_5 -> char '5'
    OctDigit_6 -> char '6'
    OctDigit_7 -> char '7'

hexDigit :: HexDigit -> Doc
hexDigit d =
  case d of
    HexDigit_0 -> char '0'
    HexDigit_1 -> char '1'
    HexDigit_2 -> char '2'
    HexDigit_3 -> char '3'
    HexDigit_4 -> char '4'
    HexDigit_5 -> char '5'
    HexDigit_6 -> char '6'
    HexDigit_7 -> char '7'
    HexDigit_8 -> char '8'
    HexDigit_9 -> char '9'
    HexDigit_a -> char 'a'
    HexDigit_A -> char 'A'
    HexDigit_b -> char 'b'
    HexDigit_B -> char 'B'
    HexDigit_c -> char 'c'
    HexDigit_C -> char 'C'
    HexDigit_d -> char 'd'
    HexDigit_D -> char 'D'
    HexDigit_e -> char 'e'
    HexDigit_E -> char 'E'
    HexDigit_f -> char 'f'
    HexDigit_F -> char 'F'

binDigit :: BinDigit -> Doc
binDigit d =
  case d of
    BinDigit_0 -> char '0'
    BinDigit_1 -> char '1'

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
      integer' b <>
      ex e
    FloatDecimalNoBase f e _ ->
      char '.' <> integer' f <> ex e 
    FloatDecimalBase b f e _ ->
      integer' b <> char '.' <> foldMapF integer' f <> ex e
  where
    ex =
      foldMapF
        (beforeF
          (either (const $ char 'e') (const $ char 'E'))
          integer')

sumElim :: (forall x. f x -> r) -> (forall x. g x -> r) -> Sum f g a -> r
sumElim f _ (InL a) = f a
sumElim _ g (InR a) = g a

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

literal :: Literal a -> Doc
literal l =
  case l of
    LiteralString h t _ ->
      sumElim stringLiteral bytesLiteral h <>
      foldMapF
        (whitespaceBeforeF (sumElim stringLiteral bytesLiteral))
        t
    LiteralInteger val _ -> integer' val
    LiteralFloat val _ -> float' val
    LiteralImag val _ -> imag val

ifThenElse :: IfThenElse a -> Doc
ifThenElse (IfThenElse i e) =
  text "if" <>
  betweenWhitespace'F orTest i <>
  text "else" <>
  whitespaceBeforeF test e

compOperator :: CompOperator -> Doc
compOperator o =
  case o of
    CompLT -> char '<'
    CompGT -> char '>'
    CompEq -> text "=="
    CompNEq -> text "!="
    CompLEq -> text "<="
    CompGEq -> text ">="
    CompIs -> text "is"
    CompIsNot s -> text "is" <> foldMap whitespaceChar s <> text "not"
    CompIn -> text "in"
    CompNotIn s -> text "not" <> foldMap whitespaceChar s <> text "in"

tupleElim :: Semigroup r => (a -> r) -> (b -> r) -> (a, b) -> r
tupleElim f g (a, b) = f a <> g b

asterisk :: Asterisk -> Doc
asterisk _ = char '*'

doubleAsterisk :: DoubleAsterisk -> Doc
doubleAsterisk _ = text "**"


compIter :: CompIter a -> Doc
compIter (CompIter val _) = sumElim compFor compIf val

varargsList :: VarargsList a -> Doc
varargsList = error "varargsList not implemented" 

lambdefNocond :: LambdefNocond a -> Doc
lambdefNocond  (LambdefNocond a e _) =
  text "lambda" <>
  foldMapF
    (betweenF
      (foldMap whitespaceChar)
      (foldMap whitespaceChar)
      varargsList)
    a <>
  char ':' <>
  whitespaceBeforeF testNocond e

testNocond :: TestNocond a -> Doc
testNocond (TestNocond val _) = sumElim orTest lambdefNocond val

compIf :: CompIf a -> Doc
compIf (CompIf e i _) =
  text "if" <>
  whitespaceBeforeF testNocond e <>
  foldMapF (whitespaceBeforeF compIter) i

exprList :: ExprList a -> Doc
exprList (ExprList h t _) =
  exprOrStar h <>
  foldMapF (beforeF (betweenWhitespace' comma) exprOrStar) t
  where
    exprOrStar = sumElim expr starExpr

compFor :: CompFor a -> Doc
compFor (CompFor t e i _) =
  text "for" <>
  betweenWhitespace'F exprList t <>
  text "in" <>
  whitespaceBeforeF orTest e <>
  foldMapF (whitespaceBeforeF compIter) i

prodElim
  :: Semigroup r
  => (forall x. f x -> r)
  -> (forall x. g x -> r)
  -> Product f g a -> r
prodElim f g (Pair a b) = f a <> g b

expr :: Expr a -> Doc
expr (Expr l r _) =
  xorExpr l <>
  foldMapF (beforeF (betweenWhitespace' pipe) xorExpr) r

comparison :: Comparison a -> Doc
comparison (Comparison l r _) =
  expr l <>
  foldMapF (beforeF (betweenWhitespace' compOperator) expr) r

dictOrSetMaker :: DictOrSetMaker a -> Doc
dictOrSetMaker _ = error "dictOrSetMaker not implemented"

starExpr :: StarExpr a -> Doc
starExpr (StarExpr val _) =
  char '*' <>
  whitespaceBeforeF expr val

testlistComp :: TestlistComp a -> Doc
testlistComp t =
  case t of
    TestlistCompFor h t _ ->
      testOrStar h <>
      whitespaceBeforeF compFor t
    TestlistCompList h t c _ ->
      testOrStar h <>
      foldMapF (beforeF (betweenWhitespace' comma) testOrStar) t <>
      foldMap (whitespaceBefore comma) c
  where
    testOrStar = sumElim test starExpr

testList :: TestList a -> Doc
testList (TestList h t c _) =
  test h <>
  beforeF (betweenWhitespace' comma) test t <>
  foldMap (whitespaceBefore comma) c

yieldArg :: YieldArg a -> Doc
yieldArg y =
  case y of
    YieldArgFrom val _ -> text "from" <> whitespaceBeforeF test val
    YieldArgList val _ -> testList val

yieldExpr :: YieldExpr a -> Doc
yieldExpr (YieldExpr val _) =
  text "yield" <>
  foldMapF (whitespaceBeforeF yieldArg) val

atom :: Atom a -> Doc
atom a =
  case a of
    AtomParen val _ ->
      parens $ betweenWhitespace'F (sumElim yieldExpr testlistComp) val
    AtomBracket val _ -> brackets $ betweenWhitespace'F testlistComp val
    AtomCurly val _ -> braces $ betweenWhitespace'F dictOrSetMaker val
    AtomIdentifier val _ -> identifier val
    AtomInteger val _ -> integer' val
    AtomFloat val _ -> float' val
    AtomString val _ -> foldMapF (sumElim stringLiteral bytesLiteral) val
    AtomEllipsis _ -> text "..."
    AtomNone _ -> text "None"
    AtomTrue _ -> text "True"
    AtomFalse _ -> text "False"

argument :: Argument a -> Doc
argument a =
  case a of
    ArgumentFor e f _ ->
      test e <>
      foldMapF (whitespaceBeforeF compFor) f
    ArgumentDefault l r _ ->
      whitespaceAfterF test l <>
      char '=' <>
      whitespaceBeforeF test r
    ArgumentUnpack s val _ ->
      either asterisk doubleAsterisk s <>
      whitespaceBeforeF test val

argList :: ArgList a -> Doc
argList (ArgList h t c _) =
  argument h <>
  foldMapF (beforeF (betweenWhitespace' comma) argument) t <>
  foldMap (whitespaceBefore comma) c

sliceOp :: SliceOp a -> Doc
sliceOp (SliceOp val _) = char ':' <> foldMapF (whitespaceBeforeF test) val

subscript :: Subscript a -> Doc
subscript s =
  case s of
    SubscriptTest val _ -> test val
    SubscriptSlice l r o _ ->
      foldMapF (whitespaceAfterF test) l <>
      char ':' <>
      foldMapF (whitespaceBeforeF test) r <>
      foldMapF (whitespaceBeforeF sliceOp) o

subscriptList :: SubscriptList a -> Doc
subscriptList (SubscriptList h t c _) =
  subscript h <>
  foldMapF (beforeF (betweenWhitespace' comma) subscript) t <>
  foldMap (whitespaceBefore comma) c

trailer :: Trailer a -> Doc
trailer t =
  case t of
    TrailerCall val _ -> parens $ foldMapF (betweenWhitespace'F argList) val
    TrailerSubscript val _ ->
      brackets $ foldMapF (betweenWhitespace'F subscriptList) val
    TrailerAccess val _ -> char '.' <> whitespaceBeforeF identifier val

atomExpr :: AtomExpr a -> Doc
atomExpr (AtomExpr await a trailers _) =
  foldMapF (whitespaceAfter kAwait) await <>
  whitespaceAfterF atom a <>
  foldMapF (whitespaceBeforeF trailer) trailers

power :: Power a -> Doc
power (Power l r _) =
  atomExpr l <>
  foldMapF (beforeF (whitespaceAfter doubleAsterisk) factor) r

factorOp :: FactorOp -> Doc
factorOp f =
  case f of
    FactorNeg -> char '-'
    FactorPos -> char '+'
    FactorInv -> char '~'

factor :: Factor a -> Doc
factor f =
  case f of
    FactorNone val _ -> power val
    FactorSome val _ -> beforeF (whitespaceAfter factorOp) factor val

termOp :: TermOp -> Doc
termOp t =
  case t of
    TermMult -> char '*'
    TermAt -> char '@'
    TermFloorDiv -> text "//"
    TermDiv -> char '/'
    TermMod -> char '%'

term :: Term a -> Doc
term (Term l r _) =
  factor l <>
  foldMapF (beforeF (betweenWhitespace' termOp) factor) r

arithExpr :: ArithExpr a -> Doc
arithExpr (ArithExpr l r _) =
  term l <>
  foldMapF (beforeF (betweenWhitespace' (either plus minus)) term) r
  
shiftExpr :: ShiftExpr a -> Doc
shiftExpr (ShiftExpr l r _) =
  arithExpr l <>
  foldMapF
    (beforeF
      (betweenWhitespace' (either doubleLT doubleGT))
      arithExpr)
    r
  
andExpr :: AndExpr a -> Doc
andExpr (AndExpr l r _) =
  shiftExpr l <>
  foldMapF (beforeF (betweenWhitespace' ampersand) shiftExpr) r

xorExpr :: XorExpr a -> Doc
xorExpr (XorExpr l r _) =
  andExpr l <>
  foldMapF (beforeF (betweenWhitespace' caret) andExpr) r

notTest :: NotTest a -> Doc
notTest n =
  case n of
    NotTestSome val _ -> beforeF (whitespaceAfter kNot) notTest val
    NotTestNone val _ -> comparison val
      
andTest :: AndTest a -> Doc
andTest (AndTest l r _) =
  notTest l <>
  foldMapF (beforeF (betweenWhitespace' kAnd) andTest) r

orTest :: OrTest a -> Doc
orTest (OrTest l r _) =
  andTest l <>
  foldMapF (beforeF (betweenWhitespace' kOr) andTest) r

test :: Test a -> Doc
test t =
  case t of
    TestCond h t _ ->
      orTest h <>
      foldMapF (whitespaceBeforeF ifThenElse) t
    TestLambdef -> error "testLambdef not implemented"

comment :: Comment a -> Doc
comment (Comment val _) = char '#' <> text (T.unpack val)
