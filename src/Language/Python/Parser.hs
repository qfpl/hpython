-- from https://docs.python.org/3.5/reference/grammar.html
-- `test` is the production for an expression

{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
module Language.Python.Parser where

import GHC.Stack

import Papa hiding (Space, zero, o, Plus, (\\), Product, argument)
import Data.Functor.Compose
import Data.Functor.Sum
import Text.Parser.LookAhead
import Data.Separated.After (After(..))
import Data.Separated.Before (Before(..))
import Text.Trifecta as P hiding
  (Unspaced(..), stringLiteral, integer, octDigit, hexDigit, comma, colon)

import Language.Python.AST.BytesLiteral
import Language.Python.AST.BytesPrefix
import Language.Python.AST.CompOperator
import Language.Python.AST.Digits
import Language.Python.AST.FactorOperator
import Language.Python.AST.Float
import Language.Python.AST.Imag
import Language.Python.AST.Integer
import Language.Python.AST.Keywords
import Language.Python.AST.LongBytes
import Language.Python.AST.LongBytesChar
import Language.Python.AST.LongString
import Language.Python.AST.LongStringChar
import Language.Python.AST.ShortBytes
import Language.Python.AST.ShortBytesChar
import Language.Python.AST.ShortString
import Language.Python.AST.ShortStringChar
import Language.Python.AST.StringLiteral
import Language.Python.AST.StringPrefix
import Language.Python.AST.Symbols as S
import Language.Python.AST.TermOperator
import Language.Python.Parser.Combinators
import Language.Python.Parser.IR
import Language.Python.Parser.Identifier
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.StringContent
import Language.Python.Parser.Symbols
import Language.Python.Parser.VarargsList

import Text.Parser.Unspaced

ifThenElse :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (IfThenElse SrcInfo)
ifThenElse =
  IfThenElse <$>
  betweenWhitespace1 (string "if" $> KIf) <*>
  orTest <*>
  betweenWhitespace1 (string "else" $> KElse) <*>
  test

test :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Test SrcInfo)
test = try testCond <|> testLambdef
  where
    testLambdef = annotated $ TestLambdef <$> lambdef
    testCond =
      annotated $
      TestCond <$>
      orTest <*>
      optionalF (try $ whitespaceBefore1F ifThenElse)

lambdef :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Lambdef SrcInfo)
lambdef =
  annotated $
  Lambdef <$>
  (string "lambda" *> optionalF (whitespaceBefore1F $ varargsList test)) <*>
  beforeF (betweenWhitespace colon) test

kOr :: (DeltaParsing m, LookAheadParsing m) => m KOr
kOr = string "or" $> KOr

kAnd :: (DeltaParsing m, LookAheadParsing m) => m KAnd
kAnd = string "and" $> KAnd

orTest :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (OrTest SrcInfo)
orTest =
  annotated $
  OrTest <$>
  andTest <*>
  manyF (try $ beforeF (betweenWhitespace1 kOr) andTest)

lambdefNocond :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (LambdefNocond SrcInfo)
lambdefNocond =
  annotated $
  LambdefNocond <$>
  optionalF
    (try $ betweenF
      (some1 whitespaceChar)
      (many whitespaceChar)
      (varargsList test)) <*>
  whitespaceBeforeF testNocond

testNocond :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (TestNocond SrcInfo)
testNocond =
  annotated $
  TestNocond <$> (try (InL <$> orTest) <|> (InR <$> lambdefNocond))

compIf :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (CompIf SrcInfo)
compIf =
  annotated $
  CompIf <$>
  betweenWhitespace1 (string "if" $> KIf) <*>
  testNocond <*>
  optionalF (try $ whitespaceBeforeF compIter)

compIter :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (CompIter SrcInfo)
compIter =
  annotated $
  CompIter <$> (try (InL <$> compFor) <|> (InR <$> compIf))

starExpr :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (StarExpr SrcInfo)
starExpr =
  annotated $
  StarExpr <$>
  (char '*' *> whitespaceBeforeF expr)

exprList :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (ExprList SrcInfo)
exprList =
  annotated $
  ExprList <$>
  exprOrStar <*>
  manyF (try $ beforeF (betweenWhitespace comma) exprOrStar) <*>
  optional (try $ whitespaceBefore comma)
  where
    exprOrStar = try (InL <$> expr) <|> (InR <$> starExpr)

compFor
  :: (DeltaParsing m, LookAheadParsing m)
  => Unspaced m (CompFor SrcInfo)
compFor =
  annotated $
  CompFor <$>
  beforeF
    (betweenWhitespace1 $ string "for" $> KFor)
    (whitespaceAfter1F exprList) <*>
  (string "in" *> whitespaceBefore1F orTest) <*>
  optionalF (try $ whitespaceBeforeF compIter)

stringPrefix :: (DeltaParsing m, LookAheadParsing m) => m StringPrefix
stringPrefix =
  try (char 'r' $> StringPrefix_r) <|>
  try (char 'u' $> StringPrefix_u) <|>
  try (char 'R' $> StringPrefix_R) <|>
  (char 'u' $> StringPrefix_U)

shortString :: (HasCallStack, DeltaParsing m, LookAheadParsing m) => Unspaced m (ShortString SrcInfo)
shortString = try shortStringSingle <|> shortStringDouble
  where
    shortStringSingle =
      annotated $
      ShortStringSingle <$>
      between
        singleQuote
        singleQuote
        (parseStringContentSingle parseShortStringCharSingle singleQuote)

    shortStringDouble =
      annotated $
      ShortStringDouble <$>
      between
        doubleQuote
        doubleQuote
        (parseStringContentDouble parseShortStringCharDouble doubleQuote)

longString :: (HasCallStack, DeltaParsing m, LookAheadParsing m) => Unspaced m (LongString SrcInfo)
longString =
  try longStringDouble <|> longStringSingle
  where
    longStringSingle =
      annotated $
      LongStringSingle <$>
      between
        tripleSinglequote
        tripleSinglequote
        (parseStringContentSingle parseLongStringChar tripleSinglequote)

    longStringDouble =
      annotated $
      LongStringDouble <$>
      between
        tripleDoublequote
        tripleDoublequote
        (parseStringContentDouble parseLongStringChar tripleDoublequote)

stringLiteral :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (StringLiteral SrcInfo)
stringLiteral =
  annotated $
  StringLiteral <$>
  beforeF
    (optional $ try stringPrefix)
    ((InR <$> longString) <|> (InL <$> shortString))

bytesPrefix :: (DeltaParsing m, LookAheadParsing m) => m BytesPrefix
bytesPrefix =
  try (char 'b' $> BytesPrefix_b) <|>
  try (char 'B' $> BytesPrefix_B) <|>
  try (string "br" $> BytesPrefix_br) <|>
  try (string "Br" $> BytesPrefix_Br) <|>
  try (string "bR" $> BytesPrefix_bR) <|>
  try (string "BR" $> BytesPrefix_BR) <|>
  try (string "rb" $> BytesPrefix_rb) <|>
  try (string "rB" $> BytesPrefix_rB) <|>
  try (string "Rb" $> BytesPrefix_Rb) <|>
  (string "RB" $> BytesPrefix_RB)

shortBytes :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (ShortBytes SrcInfo)
shortBytes = try shortBytesSingle <|> shortBytesDouble
  where
    shortBytesSingle =
      annotated $
      ShortBytesSingle <$>
      between
        singleQuote
        singleQuote
        (parseStringContentSingle parseShortBytesCharSingle singleQuote)

    shortBytesDouble =
      annotated $
      ShortBytesDouble <$>
      between
        doubleQuote
        doubleQuote
        (parseStringContentDouble parseShortBytesCharDouble doubleQuote)

longBytes :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (LongBytes SrcInfo)
longBytes =
  (try longBytesSingle <|> longBytesSingleEmpty) <|>
  (try longBytesDouble <|> longBytesDoubleEmpty)
  where
    longBytesSingleEmpty =
      annotated $
      tripleSinglequote *>
      tripleSinglequote $>
      LongBytesSingleEmpty

    longBytesDoubleEmpty =
      annotated $
      tripleDoublequote *>
      tripleDoublequote $>
      LongBytesDoubleEmpty

    longBytesSingle =
      annotated $
      LongBytesSingle <$>
      between
        tripleSinglequote
        tripleSinglequote
        (parseStringContentSingle parseLongBytesChar tripleSinglequote)

    longBytesDouble =
      annotated $
      LongBytesDouble <$>
      between
        tripleDoublequote
        tripleDoublequote
        (parseStringContentDouble parseLongBytesChar tripleDoublequote)

bytesLiteral :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (BytesLiteral SrcInfo)
bytesLiteral =
  annotated $
  BytesLiteral <$>
  bytesPrefix <*>
  ((InR <$> longBytes) <|> (InL <$> shortBytes))

nonZeroDigit :: (DeltaParsing m, LookAheadParsing m) => m NonZeroDigit
nonZeroDigit =
  try (char '1' $> NonZeroDigit_1) <|>
  try (char '2' $> NonZeroDigit_2) <|>
  try (char '3' $> NonZeroDigit_3) <|>
  try (char '4' $> NonZeroDigit_4) <|>
  try (char '5' $> NonZeroDigit_5) <|>
  try (char '6' $> NonZeroDigit_6) <|>
  try (char '7' $> NonZeroDigit_7) <|>
  try (char '8' $> NonZeroDigit_8) <|>
  (char '9' $> NonZeroDigit_9)

digit' :: (DeltaParsing m, LookAheadParsing m) => m Digit
digit' =
  try (char '0' $> Digit_0) <|>
  try (char '1' $> Digit_1) <|>
  try (char '2' $> Digit_2) <|>
  try (char '3' $> Digit_3) <|>
  try (char '4' $> Digit_4) <|>
  try (char '5' $> Digit_5) <|>
  try (char '6' $> Digit_6) <|>
  try (char '7' $> Digit_7) <|>
  try (char '8' $> Digit_8) <|>
  (char '9' $> Digit_9)

octDigit :: (DeltaParsing m, LookAheadParsing m) => m OctDigit
octDigit =
  try (char '0' $> OctDigit_0) <|>
  try (char '1' $> OctDigit_1) <|>
  try (char '2' $> OctDigit_2) <|>
  try (char '3' $> OctDigit_3) <|>
  try (char '4' $> OctDigit_4) <|>
  try (char '5' $> OctDigit_5) <|>
  try (char '6' $> OctDigit_6) <|>
  (char '7' $> OctDigit_7)

hexDigit :: (DeltaParsing m, LookAheadParsing m) => m HexDigit
hexDigit =
  try (char '0' $> HexDigit_0) <|>
  try (char '1' $> HexDigit_1) <|>
  try (char '2' $> HexDigit_2) <|>
  try (char '3' $> HexDigit_3) <|>
  try (char '4' $> HexDigit_4) <|>
  try (char '5' $> HexDigit_5) <|>
  try (char '6' $> HexDigit_6) <|>
  try (char '7' $> HexDigit_7) <|>
  try (char '8' $> HexDigit_8) <|>
  try (char '9' $> HexDigit_9) <|>
  try (char 'a' $> HexDigit_a) <|>
  try (char 'A' $> HexDigit_A) <|>
  try (char 'b' $> HexDigit_b) <|>
  try (char 'B' $> HexDigit_B) <|>
  try (char 'c' $> HexDigit_c) <|>
  try (char 'C' $> HexDigit_C) <|>
  try (char 'd' $> HexDigit_d) <|>
  try (char 'D' $> HexDigit_D) <|>
  try (char 'e' $> HexDigit_e) <|>
  try (char 'E' $> HexDigit_E) <|>
  try (char 'f' $> HexDigit_f) <|>
  (char 'F' $> HexDigit_F)

binDigit :: (DeltaParsing m, LookAheadParsing m) => m BinDigit
binDigit = try (char '0' $> BinDigit_0) <|> (char '1' $> BinDigit_1)

integer :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Integer' SrcInfo)
integer =
  try integerBin <|>
  try integerOct <|>
  try integerHex <|>
  integerDecimal
  where
    integerDecimal =
      annotated $
      IntegerDecimal <$>
      (try (Left <$> liftA2 (,) nonZeroDigit (many digit')) <|>
      (Right <$> some1 zero))
    integerOct =
      annotated .
      fmap IntegerOct $
      Before <$> (zero *> oO) <*> some1 octDigit
    integerHex =
      annotated .
      fmap IntegerHex $
      Before <$> (zero *> xX) <*> some1 hexDigit
    integerBin =
      annotated .
      fmap IntegerBin $
      Before <$> (zero *> bB) <*> some1 binDigit

plusOrMinus :: (DeltaParsing m, LookAheadParsing m) => m (Either Plus Minus)
plusOrMinus =
  try (fmap Left plus) <|>
  fmap Right minus

float :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Float' SrcInfo)
float = try floatDecimalBase <|> try floatDecimalNoBase <|> floatNoDecimal
  where
    floatDecimalBase =
      annotated $
      FloatDecimalBase <$>
      try (some1 digit') <*>
      (char '.' *> optionalF (some1 digit')) <*>
      ex

    floatDecimalNoBase =
      annotated $
      FloatDecimalNoBase <$>
      (char '.' *> some1 digit') <*>
      ex

    floatNoDecimal =
      annotated $
      FloatNoDecimal <$>
      try (some1 digit') <*>
      ex

    ex = optional (try $ Before <$> eE <*> some1 digit')

imag :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Imag SrcInfo)
imag =
  annotated . fmap Imag $
  Compose <$>
  (flip After <$> floatOrInt <*> jJ)
  where
    floatOrInt = fmap InL float <|> fmap (InR . Const) (some1 digit')

dictItem
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => Unspaced m (DictItem SrcInfo)
dictItem =
  annotated $
  DictItem <$>
  test <*>
  betweenWhitespace colon <*>
  test

dictUnpacking
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => Unspaced m (DictUnpacking SrcInfo)
dictUnpacking =
  annotated $
  DictUnpacking <$>
  beforeF (betweenWhitespace doubleAsterisk) expr

dictOrSetMaker :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (DictOrSetMaker SrcInfo)
dictOrSetMaker = try dictOrSetMakerDict <|> dictOrSetMakerSet
  where
    dictOrSetMakerDict =
      annotated $
      DictOrSetMakerDict <$>
      itemOrUnpacking <*>
      dictCompOrList

    itemOrUnpacking = try (InL <$> dictItem) <|> (InR <$> dictUnpacking)

    dictCompOrList =
      (InL <$> try compFor) <|>
      (InR <$> afterF
        (optional . try $ betweenWhitespace comma)
        (manyF . try $ beforeF (betweenWhitespace comma) itemOrUnpacking))

    dictOrSetMakerSet =
      annotated $
      DictOrSetMakerSet <$>
      testOrStar <*>
      setCompOrList

    testOrStar = try (InL <$> test) <|> (InR <$> starExpr)

    setCompOrList =
      (InL <$> try compFor) <|>
      (InR <$> afterF
        (optional . try $ betweenWhitespace comma)
        (manyF . try $ beforeF (betweenWhitespace comma) testOrStar))


tupleTestlistComp :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (TupleTestlistComp SrcInfo)
tupleTestlistComp = try tupleTestlistCompFor <|> tupleTestlistCompList
  where
    tupleTestlistCompFor =
      annotated $
      TupleTestlistCompFor <$>
      testOrStar <*>
      compFor

    tupleTestlistCompList =
      annotated $
      TupleTestlistCompList <$>
      testOrStar <*>
      manyF (try $ beforeF (betweenWhitespace comma) testOrStar) <*>
      optional (try $ whitespaceBefore comma)

    testOrStar = try (InL <$> test) <|> (InR <$> starExpr)

listTestlistComp :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (ListTestlistComp SrcInfo)
listTestlistComp = try listTestlistCompFor <|> listTestlistCompList
  where
    listTestlistCompFor =
      annotated $
      ListTestlistCompFor <$>
      testOrStar <*>
      compFor

    listTestlistCompList =
      annotated $
      ListTestlistCompList <$>
      testOrStar <*>
      manyF (try $ beforeF (betweenWhitespace comma) testOrStar) <*>
      optional (try $ whitespaceBefore comma)

    testOrStar = try (InL <$> test) <|> (InR <$> starExpr)

testList :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (TestList SrcInfo)
testList =
  annotated $
  TestList <$>
  test <*>
  beforeF (betweenWhitespace comma) test <*>
  optional (try $ whitespaceBefore comma)

yieldArg :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (YieldArg SrcInfo)
yieldArg = try yieldArgFrom <|> yieldArgList
  where
    yieldArgFrom =
      annotated $
      YieldArgFrom <$>
      (string "from" *> whitespaceBefore1F test)
    yieldArgList =
      annotated $
      YieldArgList <$> testList

yieldExpr :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (YieldExpr SrcInfo)
yieldExpr =
  annotated $
  YieldExpr <$>
  (string "yield" *> optionalF (try $ whitespaceBefore1F yieldArg))


atom :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Atom SrcInfo)
atom =
  try atomParen <|>
  try atomBracket <|>
  try atomCurly <|>
  try atomInteger <|>
  try atomFloat <|>
  try atomString <|>
  try atomEllipsis <|>
  try atomNone <|>
  try atomTrue <|>
  try atomFalse <|>
  atomIdentifier
  where
    atomIdentifier =
      annotated $
      AtomIdentifier <$>
      identifier

    atomParen =
      annotated $
      AtomParen <$>
      between (char '(') (char ')')
      (betweenWhitespaceF
        (optionalF
          (try $ (InL <$> try yieldExpr) <|> (InR <$> tupleTestlistComp))))

    atomBracket =
      annotated $
      AtomBracket <$>
      between
        (char '[')
        (char ']')
        (betweenWhitespaceF $
          optionalF $ try listTestlistComp)

    atomCurly =
      annotated $
      AtomCurly <$>
      between
        (char '{')
        (char '}')
        (betweenWhitespaceF $
          optionalF $ try dictOrSetMaker)

    atomInteger =
      annotated $
      AtomInteger <$> integer

    atomFloat =
      annotated $
      AtomFloat <$> float

    stringOrBytes = (InL <$> try stringLiteral) <|> (InR <$> bytesLiteral)

    atomString =
      annotated $
      AtomString <$>
      stringOrBytes <*>
      manyF (try $ whitespaceBeforeF stringOrBytes)

    atomEllipsis =
      annotated $
      string "..." $> AtomEllipsis

    atomNone =
      annotated $
      (string "None" *> notFollowedBy idContinue) $> AtomNone

    atomTrue =
      annotated $
      (string "True" *> notFollowedBy idContinue) $> AtomTrue

    atomFalse =
      annotated $
      (string "False" *> notFollowedBy idContinue) $> AtomFalse

sliceOp :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (SliceOp SrcInfo)
sliceOp =
  annotated $
  SliceOp <$>
  (char ':' *> optionalF (try $ whitespaceBeforeF test))

argument :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Argument SrcInfo)
argument = try argumentUnpack <|> try argumentDefault <|> argumentFor
  where
    argumentFor =
      annotated $
      ArgumentFor <$>
      test <*>
      optionalF (try compFor)
    argumentDefault =
      annotated $
      ArgumentDefault <$>
      (whitespaceAfterF test <* char '=') <*>
      whitespaceBeforeF test
    argumentUnpack =
      annotated $
      ArgumentUnpack <$>
      (try (Right <$> doubleAsterisk) <|> (Left <$> asterisk)) <*>
      whitespaceBeforeF test

subscript :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Subscript SrcInfo)
subscript = try subscriptTest <|> subscriptSlice
  where
    subscriptTest =
      annotated $
      SubscriptTest <$>
      test <* notFollowedBy (try $ many whitespaceChar *> char ':')
    subscriptSlice =
      annotated $
      SubscriptSlice <$>
      whitespaceAfterF (optionalF $ try test) <*>
      whitespaceAfter (char ':' $> Colon) <*>
      optionalF (try $ whitespaceAfterF test) <*>
      optionalF (try $ whitespaceAfterF sliceOp)

argList :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (ArgList SrcInfo)
argList =
  annotated $
  ArgList <$>
  argument <*>
  manyF (try $ beforeF (betweenWhitespace comma) argument) <*>
  optional (try $ whitespaceBefore comma)

subscriptList :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (SubscriptList SrcInfo)
subscriptList =
  annotated $
  SubscriptList <$>
  subscript <*>
  manyF (try $ beforeF (betweenWhitespace comma) subscript) <*>
  optional (try $ whitespaceBefore comma)

trailer :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Trailer SrcInfo)
trailer = try trailerCall <|> try trailerSubscript <|> trailerAccess
  where
    trailerCall =
      annotated $
      TrailerCall <$>
      between
        (char '(')
        (char ')')
        (betweenWhitespaceF . optionalF $ try argList)

    trailerSubscript =
      annotated $
      TrailerSubscript <$>
      between
        (char '[')
        (char ']')
        (betweenWhitespaceF subscriptList)

    trailerAccess =
      annotated $
      TrailerAccess <$>
      (char '.' *> whitespaceBeforeF identifier)

atomExpr :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (AtomExpr SrcInfo)
atomExpr =
  annotated $
  AtomExpr <$>
  (optional . try $ string "await" *> whitespaceAfter1 (pure KAwait)) <*>
  atom <*>
  manyF (try $ whitespaceBeforeF trailer)

power :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Power SrcInfo)
power =
  annotated $
  Power <$>
  atomExpr <*>
  optionalF (try $ beforeF (betweenWhitespace doubleAsterisk) factor)

factorOp :: (DeltaParsing m, LookAheadParsing m) => m FactorOperator
factorOp =
  try (char '-' $> FactorNeg) <|>
  try (char '+' $> FactorPos) <|>
  (char '~' $> FactorInv)

factor :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Factor SrcInfo)
factor = try factorOne <|> factorNone
  where
    factorNone = annotated $ FactorNone <$> power
    factorOne =
      annotated $
      FactorOne <$>
      whitespaceAfter factorOp <*>
      factor

termOp :: (DeltaParsing m, LookAheadParsing m) => m TermOperator
termOp =
  try (char '*' $> TermMult) <|>
  try (char '@' $> TermAt) <|>
  try (string "//" $> TermFloorDiv) <|>
  try (char '/' $> TermDiv) <|>
  (char '%' $> TermMod)

term :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Term SrcInfo)
term =
  annotated $
  Term <$>
  factor <*>
  manyF (try $ beforeF (betweenWhitespace termOp) factor)

arithExpr :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (ArithExpr SrcInfo)
arithExpr =
  annotated $
  ArithExpr <$>
  term <*>
  manyF (try $ beforeF (betweenWhitespace plusOrMinus) term)

shiftExpr :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (ShiftExpr SrcInfo)
shiftExpr =
  annotated $
  ShiftExpr <$>
  arithExpr <*>
  manyF (try $ beforeF (betweenWhitespace shiftLeftOrRight) arithExpr)
  where
    shiftLeftOrRight =
      (symbol "<<" $> Left DoubleLT) <|>
      (symbol ">>" $> Right DoubleGT)

andExpr :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (AndExpr SrcInfo)
andExpr =
  annotated $
  AndExpr <$>
  shiftExpr <*>
  manyF (try $ beforeF (betweenWhitespace $ char '&' $> Ampersand) shiftExpr)

xorExpr :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (XorExpr SrcInfo)
xorExpr =
  annotated $
  XorExpr <$>
  andExpr <*>
  manyF (try $ beforeF (betweenWhitespace $ char '^' $> S.Caret) andExpr)

expr :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Expr SrcInfo)
expr =
  annotated $
  Expr <$>
  xorExpr <*>
  manyF (try $ beforeF (betweenWhitespace $ char '|' $> Pipe) xorExpr)

compOperator :: (DeltaParsing m, LookAheadParsing m) => m CompOperator
compOperator =
  try compEq <|>
  try compGEq <|>
  try compLEq <|>
  try compNEq <|>
  try compLT <|>
  try compGT <|>
  try compIsNot <|>
  try compIs <|>
  try compIn <|>
  compNotIn
  where
    compEq =
      CompEq <$>
      (many (try whitespaceChar) <* string "==") <*>
      many whitespaceChar

    compGEq =
      CompGEq <$>
      (many (try whitespaceChar) <* string ">=") <*>
      many whitespaceChar

    compNEq =
      CompNEq <$>
      (many (try whitespaceChar) <* string "!=") <*>
      many whitespaceChar

    compLEq =
      CompLEq <$>
      (many (try whitespaceChar) <* string "<=") <*>
      many whitespaceChar

    compLT =
      CompLT <$>
      (many (try whitespaceChar) <* string "<") <*>
      many whitespaceChar

    compGT =
      CompGT <$>
      (many (try whitespaceChar) <* string ">") <*>
      many whitespaceChar

    compIsNot =
      CompIsNot <$>
      (some1 whitespaceChar <* string "is") <*>
      (some1 whitespaceChar <* string "not") <*>
      some1 whitespaceChar

    compIs =
      CompIs <$>
      (some1 whitespaceChar <* string "is") <*>
      some1 whitespaceChar

    compIn =
      CompIn <$>
      (some1 whitespaceChar <* string "in") <*>
      some1 whitespaceChar

    compNotIn =
      CompNotIn <$>
      (some1 whitespaceChar <* string "not") <*>
      (some1 whitespaceChar <* string "in") <*>
      some1 whitespaceChar

comparison :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (Comparison SrcInfo)
comparison =
  annotated $
  Comparison <$>
  expr <*>
  manyF (try $ beforeF compOperator expr)

notTest :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (NotTest SrcInfo)
notTest = try notTestMany <|> notTestOne
  where
    notTestMany =
      annotated $
      NotTestMany <$>
      beforeF (whitespaceAfter1 $ string "not" $> KNot) notTest

    notTestOne =
      annotated $ NotTestOne <$> comparison

andTest :: (DeltaParsing m, LookAheadParsing m) => Unspaced m (AndTest SrcInfo)
andTest =
  annotated $
  AndTest <$>
  notTest <*>
  manyF (try $ beforeF (betweenWhitespace1 kAnd) andTest)
