{-# language RankNTypes #-}
module Language.Python.Printer where

import Papa hiding (Product, Sum, Space, zero, o)

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
import Language.Python.AST.LongBytesChar
import Language.Python.AST.ShortBytesChar
import Language.Python.AST.LongStringChar
import Language.Python.AST.ShortStringChar
import Language.Python.AST.Symbols

identifier :: Identifier a -> Doc
identifier i = i ^. identifier_value . to T.unpack . to text

comma :: Comma -> Doc
comma _ = char ','

colon :: Colon -> Doc
colon _ = char ':'

whitespaceAfterF
  :: Foldable g
  => (forall x. f x -> Doc)
  -> Compose (After (g WhitespaceChar)) f a
  -> Doc
whitespaceAfterF f = after (foldMap whitespaceChar) f . getCompose

whitespaceAfter
  :: (a -> Doc) -> After [WhitespaceChar] a -> Doc
whitespaceAfter = after (foldMap whitespaceChar)

whitespaceBefore
  :: (a -> Doc) -> Before [WhitespaceChar] a -> Doc
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
  :: (a -> Doc) -> Between' [WhitespaceChar] a -> Doc
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

before :: Semigroup r => (s -> r) -> (a -> r) -> Before s a -> r
before f g (Before s a) = f s <> g a

after :: Semigroup r => (s -> r) -> (a -> r) -> After s a -> r
after f g (After s a) = g a <> f s

between
  :: Semigroup r
  => (s -> r)
  -> (t -> r)
  -> (a -> r)
  -> Between s t a
  -> r
between f g h (Between s a t) = f s <> h a <> g t

between'
  :: Semigroup r
  => (s -> r)
  -> (a -> r)
  -> Between' s a
  -> r
between' f g (Between' (Between s a t)) = f s <> g a <> f t

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

pointFloat :: PointFloat -> Doc
pointFloat p =
  case p of
    WithDecimalPlaces a b ->
      foldMap (foldMap digit) a <>
      char '.' <>
      foldMap digit b
    NoDecimalPlaces a -> foldMap digit a <> char '.'

float' :: Float' a -> Doc
float' f =
  case f of
    FloatPoint val _ -> pointFloat val
    FloatExponent base ex _ ->
      either (foldMap digit) pointFloat base <>
      before
        (either (const $ char 'e') (const $ char 'E'))
        (before
          (either (const $ char '+') (const $ char '-'))
          (foldMap digit))
        ex

sumElim :: (forall x. f x -> r) -> (forall x. g x -> r) -> Sum f g a -> r
sumElim f _ (InL a) = f a
sumElim _ g (InR a) = g a

imag :: Imag a -> Doc
imag (Imag val _) =
  after
    (either (const $ char 'j') (const $ char 'J'))
    (sumElim float' (foldMap digit . getConst)) $
    getCompose val

stringLiteral :: StringLiteral a -> Doc
stringLiteral (StringLiteral val _) =
  before
    (foldMap stringPrefix)
    (sumElim shortString longString) $
  getCompose val
  
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
    Continued nl -> newlineChar nl

literal :: Literal a -> Doc
literal l =
  case l of
    LiteralString h t _ ->
      sumElim stringLiteral bytesLiteral h <>
      foldMap
        (whitespaceBeforeF (sumElim stringLiteral bytesLiteral))
        (getCompose t)
    LiteralInteger val _ -> integer' val
    LiteralFloat val _ -> float' val
    LiteralImag val _ -> imag val

ifThenElse :: IfThenElse a -> Doc
ifThenElse (IfThenElse i e) =
  text "if" <>
  betweenWhitespace'F orTest i <>
  text "else" <>
  whitespaceBeforeF expression e

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

expressionList :: ExpressionList a -> Doc
expressionList (ExpressionList h t c _) =
  expression h <>
  foldMap
    (tupleElim
      (betweenWhitespace' comma)
      expression .
      getCompose)
    (getCompose t) <>
  foldMap (whitespaceBefore comma) c

subscription :: Subscription a -> Doc
subscription (Subscription o i _) =
  whitespaceAfterF primary o <>
  brackets
    (betweenWhitespace'F expressionList i)

attRef :: AttRef a -> Doc
attRef (AttRef l r _) =
  whitespaceAfterF primary l <>
  char '.' <>
  whitespaceBeforeF identifier r

properSlice :: ProperSlice a -> Doc
properSlice (ProperSlice l u s _) =
  foldMap
    (whitespaceAfterF expression)
    (getCompose l) <>
  char ':' <>
  foldMap
    (betweenWhitespace'F expression)
    (getCompose u) <>
  foldMap
    (tupleElim
      (whitespaceBefore colon)
      (foldMap
        (whitespaceBeforeF expression) .
        getCompose) .
      getCompose)
    (getCompose s)

sliceItem :: SliceItem a -> Doc
sliceItem s =
  case s of
    SliceItemExpr val _ -> expression val
    SliceItemProper val _ -> properSlice val

sliceList :: SliceList a -> Doc
sliceList (SliceList h t c _) =
  sliceItem h <>
  foldMap
    (tupleElim
      (betweenWhitespace' comma)
      sliceItem .
      getCompose)
    (getCompose t) <>
  foldMap (whitespaceBefore comma) c

slicing :: Slicing a -> Doc
slicing (Slicing o i _) =
  whitespaceAfterF primary o <>
  brackets (betweenWhitespace'F sliceList i)

asterisk :: Asterisk -> Doc
asterisk _ = char '*'

doubleAsterisk :: DoubleAsterisk -> Doc
doubleAsterisk _ = text "**"

positionalArgs :: PositionalArgs a -> Doc
positionalArgs (PositionalArgs h t _) =
  tupleElim
    (foldMap $ whitespaceAfter asterisk)
    expression
  (getCompose h) <>
  foldMap
    (tupleElim
      (whitespaceAfter comma)
      (tupleElim
        (foldMap (whitespaceAfter asterisk))
        expression .
        getCompose) .
      getCompose)
    (getCompose t)

keywordItem :: KeywordItem a -> Doc
keywordItem (KeywordItem l r _) =
  whitespaceAfterF identifier l <>
  char '=' <>
  whitespaceBeforeF expression r

starredAndKeywords :: StarredAndKeywords a -> Doc
starredAndKeywords (StarredAndKeywords h t _) =
  sumElim
    (tupleElim (whitespaceAfter asterisk) expression . getCompose)
    keywordItem
    h <>
  tupleElim
    (whitespaceAfter comma)
    (sumElim
      (tupleElim (whitespaceAfter asterisk) expression . getCompose)
      keywordItem)
  (getCompose t)

keywordsArgs :: KeywordsArgs a -> Doc
keywordsArgs (KeywordsArgs h t _) =
  sumElim
    keywordItem
    (tupleElim (whitespaceAfter doubleAsterisk) expression . getCompose)
    h <>
  tupleElim
    (whitespaceAfter comma)
    (sumElim
      keywordItem
      (tupleElim (whitespaceAfter doubleAsterisk) expression . getCompose))
  (getCompose t)

argList :: ArgList a -> Doc
argList a =
  case a of
    ArgListAll p s k _ ->
      positionalArgs p <>
      foldMap
        (tupleElim
          (betweenWhitespace' comma)
          starredAndKeywords .
          getCompose)
        (getCompose s) <>
      foldMap
        (tupleElim (betweenWhitespace' comma) keywordsArgs . getCompose)
        (getCompose k)
    ArgListStarred s k _ ->
      starredAndKeywords s <>
      foldMap
        (tupleElim (betweenWhitespace' comma) keywordsArgs . getCompose)
        (getCompose k)
    ArgListKeywords k _ -> keywordsArgs k

target :: Target a -> Doc
target t =
  case t of
    TargetIdentifier val _ -> identifier val
    TargetTuple val _ -> parens $ betweenWhitespace'F targetList val
    TargetList' val _ ->
      brackets $ betweenWhitespace'F (foldMap targetList . getCompose) val
    TargetAttRef val _ -> attRef val
    TargetSubscription val _ -> subscription val
    TargetSlicing val _ -> slicing val
    TargetUnpacked val _ -> char '*' <> whitespaceBeforeF target val

targetList :: TargetList a -> Doc
targetList (TargetList h t c _) =
  target h <>
  foldMap
    (tupleElim
      (betweenWhitespace' comma)
      target .
      getCompose)
    (getCompose t) <>
  foldMap (whitespaceBefore comma) c

parameterList :: ParameterList a -> Doc
parameterList _ = mempty

lambdaExpressionNocond :: LambdaExpressionNocond a -> Doc
lambdaExpressionNocond (LambdaExprNocond p e _) =
  text "lambda" <>
  (before whitespaceChar $ foldMap (betweenWhitespace'F parameterList) . getCompose) (getCompose p) <>
  char ':' <>
  whitespaceBeforeF expressionNocond e

expressionNocond :: ExpressionNocond a -> Doc
expressionNocond (ExpressionNocond v _) =
  sumElim orTest lambdaExpressionNocond v

compIter :: CompIter a -> Doc
compIter (CompIter val _) = sumElim compFor compIf val

compIf :: CompIf a -> Doc
compIf (CompIf e i _) =
  text "if" <>
  whitespaceBeforeF expressionNocond e <>
  foldMap (whitespaceBeforeF compIter) (getCompose i)

compFor :: CompFor a -> Doc
compFor (CompFor t e i _) =
  text "for" <>
  betweenWhitespace'F targetList t <>
  text "in" <>
  whitespaceBeforeF orTest e <>
  foldMap (whitespaceBeforeF compIter) (getCompose i)

comprehension :: Comprehension a -> Doc
comprehension (Comprehension e f _) =
  whitespaceAfterF expression e <>
  compFor f

prodElim
  :: Semigroup r
  => (forall x. f x -> r)
  -> (forall x. g x -> r)
  -> Product f g a -> r
prodElim f g (Pair a b) = f a <> g b

call :: Call a -> Doc
call (Call n a _) =
  whitespaceAfterF primary n <>
  parens
    (foldMap
      (betweenWhitespace'F $
        sumElim
          (prodElim (whitespaceAfterF argList) (foldMap comma . getConst))
          comprehension)
      (getCompose a))

primary :: Primary a -> Doc
primary p =
  case p of
    PrimaryAtom val _ -> atom val
    PrimaryAttRef val _ -> attRef val
    PrimarySubscription val _ -> subscription val
    PrimarySlicing val _ -> slicing val
    PrimaryCall val _ -> call val

awaitExpr :: AwaitExpr a -> Doc
awaitExpr (Await val _) =
  text "await" <>
  whitespaceBeforeF primary val

power :: Power a -> Doc
power (Power l r _) =
  sumElim awaitExpr primary l <>
  foldMap
    (tupleElim
      ((<> text "**") . foldMap whitespaceChar)
      (whitespaceBeforeF uExpr) .
      getCompose)
    (getCompose r)

uExpr :: UExpr a -> Doc
uExpr u =
  case u of
    UExprNone val _ -> power val
    UExprNeg val _ -> char '-' <> whitespaceBeforeF uExpr val
    UExprPos val _ -> char '+' <> whitespaceBeforeF uExpr val
    UExprInv val _ -> char '~' <> whitespaceBeforeF uExpr val

mExpr :: MExpr a -> Doc
mExpr m =
  case m of
    MExprNone val _ -> uExpr val
    MExprMult l r _ ->
      whitespaceAfterF mExpr l <>
      char '*' <>
      whitespaceBeforeF uExpr r
    MExprAt l r _ ->
      whitespaceAfterF mExpr l <>
      char '@' <>
      whitespaceBeforeF mExpr r
    MExprFloorDiv l r _ ->
      whitespaceAfterF mExpr l <>
      text "//" <>
      whitespaceBeforeF uExpr r
    MExprDiv l r _ ->
      whitespaceAfterF mExpr l <>
      char '/' <>
      whitespaceBeforeF uExpr r
    MExprMod l r _ ->
      whitespaceAfterF mExpr l <>
      char '%' <>
      whitespaceBeforeF uExpr r

aExpr :: AExpr a -> Doc
aExpr a =
  case a of
    AExprNone val _ -> mExpr val
    AExprAdd l r _ ->
      whitespaceAfterF aExpr l <>
      char '+' <>
      whitespaceBeforeF mExpr r
    AExprSubtract l r _ ->
      whitespaceAfterF aExpr l <>
      char '-' <>
      whitespaceBeforeF mExpr r

shiftExpr :: ShiftExpr a -> Doc
shiftExpr a =
  case a of
    ShiftExprNone val _ -> aExpr val
    ShiftExprLeft l r _ ->
      whitespaceAfterF shiftExpr l <>
      text "<<" <>
      whitespaceBeforeF aExpr r
    ShiftExprRight l r _ ->
      whitespaceAfterF shiftExpr l <>
      text ">>" <>
      whitespaceBeforeF aExpr r

andExpr :: AndExpr a -> Doc
andExpr a =
  case a of
    AndExprNone val _ -> shiftExpr val
    AndExprSome l r _ ->
      whitespaceAfterF andExpr l <>
      char '&' <>
      whitespaceBeforeF shiftExpr r

xorExpr :: XorExpr a -> Doc
xorExpr a =
  case a of
    XorExprNone val _ -> andExpr val
    XorExprSome l r _ ->
      whitespaceAfterF xorExpr l <>
      char '^' <>
      whitespaceBeforeF andExpr r
      
orExpr :: OrExpr a -> Doc
orExpr a =
  case a of
    OrExprNone val _ -> xorExpr val
    OrExprSome l r _ ->
      whitespaceAfterF orExpr l <>
      char '|' <>
      whitespaceBeforeF xorExpr r

comparison :: Comparison a -> Doc
comparison (Comparison l r _) =
  orExpr l <>
  foldMap
    (tupleElim
      (betweenWhitespace' compOperator)
      orExpr .
      getCompose)
    (getCompose r)

notTest :: NotTest a -> Doc
notTest n =
  case n of
    NotTestNone val _ -> comparison val
    NotTestSome val _ ->
      text "not" <>
      whitespaceBeforeF notTest val

andTest :: AndTest a -> Doc
andTest a =
  case a of
    AndTestNone val _ -> notTest val
    AndTestSome l r _ ->
      whitespaceAfterF andTest l <>
      text "and" <>
      whitespaceBeforeF notTest r

orTest :: OrTest a -> Doc
orTest o =
  case o of
    OrTestNone val _ -> andTest val
    OrTestSome l r _ ->
      whitespaceAfterF orTest l <>
      text "or" <>
      whitespaceBeforeF andTest r

expression :: Expression a -> Doc
expression e =
  case e of
    ExpressionConditional h t _ ->
      orTest h <>
      foldMap (whitespaceBeforeF ifThenElse) (getCompose t)
    ExpressionLambda -> mempty 

starredItem :: StarredItem a -> Doc
starredItem s =
  case s of
    StarredItemExpr val _ -> expression val
    StarredItemUnpack val _ -> char '*' <> whitespaceBeforeF orExpr val

starredExpression :: StarredExpression a -> Doc
starredExpression s =
  case s of
    StarredExpressionExpr val _ -> expression val
    StarredExpressionTuple i l _ ->
      foldMap
        (after comma (whitespaceAfterF starredItem) . getCompose)
        (getCompose i) <>
      foldMap (whitespaceBeforeF starredItem) (getCompose l)

enclosure :: Enclosure a -> Doc
enclosure e =
  case e of
    EnclosureParen val _ ->
      parens $
      betweenWhitespace'F (foldMap starredExpression . getCompose) val
    EnclosureList -> mempty
    EnclosureDict -> mempty
    EnclosureSet -> mempty
    EnclosureGenerator -> mempty
    EnclosureYield -> mempty

atom :: Atom a -> Doc
atom a =
  case a of
    AtomIdentifier val _ -> identifier val
    AtomLiteral val _ -> literal val
    AtomEnclosure val _ -> enclosure val

comment :: Comment a -> Doc
comment (Comment val _) = char '#' <> text (T.unpack val)
