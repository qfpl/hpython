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
import Prelude (error)
import Debug.Trace

import Papa hiding (Space, zero, o, Plus, (\\), Product, argument)
import Data.CharSet ((\\))
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After (After(..))
import Data.Separated.Before (Before(..))
import Data.Separated.Between (Between(..), Between'(..))
import Text.Trifecta as P hiding
  (stringLiteral, integer, octDigit, hexDigit, comma, colon)

import qualified Data.CharSet as CharSet
import qualified Data.CharSet.Common as CharSet
import qualified Data.Text as T

import Language.Python.AST.BytesLiteral
import Language.Python.AST.BytesPrefix
import Language.Python.AST.CompOperator
import Language.Python.AST.Digits
import Language.Python.AST.EscapeSeq
import Language.Python.AST.FactorOperator
import Language.Python.AST.Float
import Language.Python.AST.Identifier
import Language.Python.AST.Imag
import Language.Python.AST.Integer
import Language.Python.AST.Keywords
import Language.Python.AST.LongBytes
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
import Language.Python.Parser.IR
import Language.Python.Parser.SrcInfo

leftParen :: DeltaParsing m => m LeftParen
leftParen = char '(' $> LeftParen

rightParen :: DeltaParsing m => m RightParen
rightParen = char ')' $> RightParen

whitespaceChar :: CharParsing m => m WhitespaceChar
whitespaceChar =
  (char ' ' $> Space) <|>
  (char '\t' $> Tab) <|>
  fmap Continued (char '\\' *> newlineChar)

whitespaceBefore :: CharParsing m => m a -> m (Before [WhitespaceChar] a)
whitespaceBefore m = Before <$> many whitespaceChar <*> m

whitespaceBeforeF
  :: CharParsing m
  => m (f a)
  -> m (Compose (Before [WhitespaceChar]) f a)
whitespaceBeforeF = fmap Compose . whitespaceBefore

whitespaceBefore1
  :: CharParsing m
  => m a
  -> m (Before (NonEmpty WhitespaceChar) a)
whitespaceBefore1 m = Before <$> some1 whitespaceChar <*> m

whitespaceBefore1F
  :: CharParsing m
  => m (f a)
  -> m (Compose (Before (NonEmpty WhitespaceChar)) f a)
whitespaceBefore1F = fmap Compose . whitespaceBefore1

whitespaceAfter :: CharParsing m => m a -> m (After [WhitespaceChar] a)
whitespaceAfter m = flip After <$> m <*> many whitespaceChar

whitespaceAfterF
  :: CharParsing m
  => m (f a)
  -> m (Compose (After [WhitespaceChar]) f a)
whitespaceAfterF = fmap Compose . whitespaceAfter

whitespaceAfter1
  :: CharParsing m
  => m a
  -> m (After (NonEmpty WhitespaceChar) a)
whitespaceAfter1 m = After <$> some1 whitespaceChar <*> m

whitespaceAfter1F
  :: CharParsing m
  => m (f a)
  -> m (Compose (After (NonEmpty WhitespaceChar)) f a)
whitespaceAfter1F = fmap Compose . whitespaceAfter1

betweenWhitespace
  :: CharParsing m
  => m a
  -> m (Between' [WhitespaceChar] a)
betweenWhitespace m =
  fmap Between' $
  Between <$>
  many whitespaceChar <*>
  m <*>
  many whitespaceChar

betweenWhitespaceF
  :: CharParsing m
  => m (f a)
  -> m (Compose (Between' [WhitespaceChar]) f a)
betweenWhitespaceF = fmap Compose . betweenWhitespace

betweenWhitespace1
  :: CharParsing m
  => m a
  -> m (Between' (NonEmpty WhitespaceChar) a)
betweenWhitespace1 m =
  fmap Between' $
  Between <$>
  some1 whitespaceChar <*>
  m <*>
  some1 whitespaceChar

betweenWhitespace1F
  :: CharParsing m
  => m (f a)
  -> m (Compose (Between' (NonEmpty WhitespaceChar)) f a)
betweenWhitespace1F = fmap Compose . betweenWhitespace1

ifThenElse :: DeltaParsing m => m (IfThenElse SrcInfo)
ifThenElse =
  IfThenElse <$>
  (string "if" *> betweenWhitespace1F orTest) <*>
  (string "else" *> whitespaceBefore1F test)


test :: DeltaParsing m => m (Test SrcInfo)
test = try testCond <|> testLambdef
  where
    testLambdef = error "testLambdef not implemented"
    testCond =
      annotated $
      TestCond <$>
      orTest <*>
      optionalF (whitespaceBefore1F ifThenElse)

kOr :: DeltaParsing m => m KOr
kOr = string "or" $> KOr

kAnd :: DeltaParsing m => m KAnd
kAnd = string "and" $> KAnd

orTest :: DeltaParsing m => m (OrTest SrcInfo)
orTest = try orTestMany <|> orTestOne
  where
    orTestOne =
      annotated $
      OrTestOne <$>
      andTest

    orTestMany =
      annotated $
      OrTestMany <$>
      andTest <*>
      some1F (beforeF (betweenWhitespace1 kOr) andTest)

varargsList :: DeltaParsing m => m (VarargsList SrcInfo)
varargsList = error "varargsList not implemented"

lambdefNocond :: DeltaParsing m => m (LambdefNocond SrcInfo)
lambdefNocond =
  annotated $
  LambdefNocond <$>
  optionalF
    (try $ betweenF
      (some1 whitespaceChar)
      (many whitespaceChar)
      varargsList) <*>
  whitespaceBeforeF testNocond

testNocond :: DeltaParsing m => m (TestNocond SrcInfo)
testNocond =
  annotated $
  TestNocond <$> (try (InL <$> orTest) <|> (InR <$> lambdefNocond))

compIf :: DeltaParsing m => m (CompIf SrcInfo)
compIf =
  annotated $
  CompIf <$>
  (string "if" *> whitespaceBeforeF testNocond) <*>
  optionalF (try $ whitespaceBeforeF compIter)

compIter :: DeltaParsing m => m (CompIter SrcInfo)
compIter =
  annotated $
  CompIter <$> (try (InL <$> compFor) <|> (InR <$> compIf))

starExpr :: DeltaParsing m => m (StarExpr SrcInfo)
starExpr =
  annotated $
  StarExpr <$>
  (char '*' *> whitespaceBeforeF expr)

exprList :: DeltaParsing m => m (ExprList SrcInfo)
exprList =
  annotated $
  ExprList <$>
  exprOrStar <*>
  manyF (try $ beforeF (betweenWhitespace comma) exprOrStar)
  where
    exprOrStar = try (InL <$> expr) <|> (InR <$> starExpr)

compFor
  :: DeltaParsing m
  => m (CompFor SrcInfo)
compFor =
  annotated $
  CompFor <$>
  beforeF
    (betweenWhitespace1 $ string "for" $> KFor)
    (whitespaceAfter1F exprList) <*>
  (string "in" *> whitespaceBefore1F orTest) <*>
  optionalF (try $ whitespaceBeforeF compIter)

doubleAsterisk :: DeltaParsing m => m DoubleAsterisk
doubleAsterisk = string "**" $> DoubleAsterisk

asterisk :: DeltaParsing m => m Asterisk
asterisk = char '*' $> Asterisk

colon :: DeltaParsing m => m Colon
colon = char ':' $> Colon

idStart :: DeltaParsing m => m Char
idStart = try letter <|> char '_'

idContinue :: DeltaParsing m => m Char
idContinue = try idStart <|> digit

identifier :: DeltaParsing m => m (Identifier SrcInfo)
identifier =
  annotated $
  Identifier . T.pack <$>
  liftA2 (:) idStart (many idContinue)

stringPrefix :: DeltaParsing m => m StringPrefix
stringPrefix =
  try (char 'r' $> StringPrefix_r) <|>
  try (char 'u' $> StringPrefix_u) <|>
  try (char 'R' $> StringPrefix_R) <|>
  (char 'u' $> StringPrefix_U)

shortString :: (HasCallStack, DeltaParsing m) => m (ShortString SrcInfo)
shortString = try shortStringSingle <|> shortStringDouble
  where
    shortStringSingle =
      annotated $
      ShortStringSingle <$>
      (singleQuote *> manyTill charOrEscapeSingle (try singleQuote))

    shortStringDouble =
      annotated $
      ShortStringDouble <$>
      (doubleQuote *> manyTill charOrEscapeDouble (try doubleQuote))

    charOrEscapeSingle =
      try (Right <$> escapeSeq) <|>
      (Left <$> shortStringCharSingle)

    charOrEscapeDouble =
      try (Right <$> escapeSeq) <|>
      (Left <$> shortStringCharDouble)

    shortStringCharSingle
      :: (HasCallStack, DeltaParsing m) => m (ShortStringChar SingleQuote)
    shortStringCharSingle =
      (\c -> fromMaybe (error $ show c) $ c ^? _ShortStringCharSingle) <$>
      oneOfSet
        (CharSet.ascii \\ CharSet.singleton '\'')

    shortStringCharDouble
      :: (HasCallStack, DeltaParsing m) => m (ShortStringChar DoubleQuote)
    shortStringCharDouble =
      (\c -> fromMaybe (error $ show c) $ c ^? _ShortStringCharDouble) <$>
      oneOfSet
        (CharSet.ascii \\ CharSet.singleton '"')

longString :: (HasCallStack, DeltaParsing m) => m (LongString SrcInfo)
longString = try longStringSingle <|> longStringDouble
  where
    longStringSingle =
      annotated $
      LongStringSingle <$>
      (tripleSinglequote *> manyTill charOrEscape (try tripleSinglequote))

    longStringDouble =
      annotated $
      LongStringDouble <$>
      (tripleDoublequote *> manyTill charOrEscape (try tripleDoublequote))

    charOrEscape =
      try (Right <$> escapeSeq) <|>
      (Left <$> longStringChar)

    longStringChar
      :: (HasCallStack, DeltaParsing m) => m LongStringChar
    longStringChar =
      -- (^?! _LongStringChar) <$> satisfy (/= '\\')
      (\c -> fromMaybe (error $ show c) $ c ^? _LongStringChar) <$>
      oneOfSet CharSet.ascii
      
stringLiteral :: DeltaParsing m => m (StringLiteral SrcInfo)
stringLiteral =
  annotated $
  StringLiteral <$>
  beforeF
    (optional $ try stringPrefix)
    (try (InR <$> longString) <|> (InL <$> shortString))

bytesPrefix :: DeltaParsing m => m BytesPrefix
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

shortBytes :: DeltaParsing m => m (ShortBytes SrcInfo)
shortBytes = try shortBytesSingle <|> shortBytesDouble
  where
    shortBytesSingle =
      annotated $
      ShortBytesSingle <$>
      (singleQuote *> manyTill charOrEscapeSingle (try singleQuote))
      
    shortBytesDouble =
      annotated $
      ShortBytesDouble <$>
      (doubleQuote *> manyTill charOrEscapeDouble (try doubleQuote))

    charOrEscapeSingle =
      try (Right <$> escapeSeq) <|>
      (Left <$> shortBytesCharSingle)

    charOrEscapeDouble =
      try (Right <$> escapeSeq) <|>
      (Left <$> shortBytesCharDouble)

    shortBytesCharSingle
      :: (HasCallStack, DeltaParsing m) => m (ShortBytesChar SingleQuote)
    shortBytesCharSingle =
      (\c -> fromMaybe (error $ show c) $ c ^? _ShortBytesCharSingle) <$>
      oneOfSet
        (CharSet.ascii \\ CharSet.singleton '\'')

    shortBytesCharDouble
      :: (HasCallStack, DeltaParsing m) => m (ShortBytesChar DoubleQuote)
    shortBytesCharDouble =
      (\c -> fromMaybe (error $ show c) $ c ^? _ShortBytesCharDouble) <$>
      oneOfSet
        (CharSet.ascii \\ CharSet.singleton '"')

tripleDoublequote :: DeltaParsing m => m ()
tripleDoublequote = string "\"\"\"" $> ()

tripleSinglequote :: DeltaParsing m => m ()
tripleSinglequote = string "'''" $> ()

doubleQuote :: DeltaParsing m => m ()
doubleQuote = char '"' $> ()

singleQuote :: DeltaParsing m => m ()
singleQuote = char '\'' $> ()

longBytes :: DeltaParsing m => m (LongBytes SrcInfo)
longBytes = try longBytesSingle <|> longBytesDouble
  where
    longBytesSingle =
      annotated $
      LongBytesSingle <$>
      (tripleSinglequote *> manyTill charOrEscape (try tripleSinglequote))

    longBytesDouble =
      annotated $
      LongBytesDouble <$>
      (tripleDoublequote *> manyTill charOrEscape (try tripleDoublequote))

    charOrEscape =
      try (Right <$> escapeSeq) <|>
      (Left <$> anyChar)

bytesLiteral :: DeltaParsing m => m (BytesLiteral SrcInfo)
bytesLiteral =
  annotated $
  BytesLiteral <$>
  bytesPrefix <*>
  (try (InR <$> longBytes) <|> (InL <$> shortBytes))

nonZeroDigit :: DeltaParsing m => m NonZeroDigit
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

digit' :: DeltaParsing m => m Digit
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

zero :: DeltaParsing m => m Zero
zero = char '0' $> Zero

o :: DeltaParsing m => m (Either Char_o Char_O)
o =
  try (fmap Left $ char 'o' $> Char_o) <|>
  fmap Right (char 'O' $> Char_O)
  
x :: DeltaParsing m => m (Either Char_x Char_X)
x =
  try (fmap Left $ char 'x' $> Char_x) <|>
  fmap Right (char 'X' $> Char_X)
  
b :: DeltaParsing m => m (Either Char_b Char_B)
b =
  try (fmap Left $ char 'b' $> Char_b) <|>
  fmap Right (char 'B' $> Char_B)
  
octDigit :: DeltaParsing m => m OctDigit
octDigit = 
  try (char '0' $> OctDigit_0) <|>
  try (char '1' $> OctDigit_1) <|>
  try (char '2' $> OctDigit_2) <|>
  try (char '3' $> OctDigit_3) <|>
  try (char '4' $> OctDigit_4) <|>
  try (char '5' $> OctDigit_5) <|>
  try (char '6' $> OctDigit_6) <|>
  (char '7' $> OctDigit_7)
  
hexDigit :: DeltaParsing m => m HexDigit
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
  
binDigit :: DeltaParsing m => m BinDigit
binDigit = try (char '0' $> BinDigit_0) <|> (char '1' $> BinDigit_1)

integer :: DeltaParsing m => m (Integer' SrcInfo)
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
      Before <$> (zero *> o) <*> some1 octDigit
    integerHex =
      annotated .
      fmap IntegerHex $
      Before <$> (zero *> x) <*> some1 hexDigit
    integerBin =
      annotated .
      fmap IntegerBin $
      Before <$> (zero *> b) <*> some1 binDigit

e :: DeltaParsing m => m (Either Char_e Char_E)
e = try (fmap Left $ char 'e' $> Char_e) <|> fmap Right (char 'E' $> Char_E)

plusOrMinus :: DeltaParsing m => m (Either Plus Minus)
plusOrMinus =
  try (fmap Left $ char '+' $> Plus) <|>
  fmap Right (char '+' $> Minus)

float :: DeltaParsing m => m (Float' SrcInfo)
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

    ex = optional (try $ Before <$> e <*> some1 digit')

j :: DeltaParsing m => m (Either Char_j Char_J)
j = try (fmap Left $ char 'j' $> Char_j) <|> fmap Right (char 'J' $> Char_J)

imag :: DeltaParsing m => m (Imag SrcInfo)
imag =
  annotated . fmap Imag $
  Compose <$>
  (flip After <$> floatOrInt <*> j)
  where
    floatOrInt = fmap InL float <|> fmap (InR . Const) (some1 digit')

optionalF :: DeltaParsing m => m (f a) -> m (Compose Maybe f a)
optionalF m = Compose <$> optional m

some1F :: DeltaParsing m => m (f a) -> m (Compose NonEmpty f a)
some1F m = Compose <$> some1 m

manyF :: DeltaParsing m => m (f a) -> m (Compose [] f a)
manyF m = Compose <$> many m

afterF :: DeltaParsing m => m s -> m (f a) -> m (Compose (After s) f a)
afterF ms ma = fmap Compose $ flip After <$> ma <*> ms

beforeF :: DeltaParsing m => m s -> m (f a) -> m (Compose (Before s) f a)
beforeF ms ma = fmap Compose $ Before <$> ms <*> ma

betweenF
  :: DeltaParsing m
  => m s
  -> m t
  -> m (f a)
  -> m (Compose (Between s t) f a)
betweenF ms mt ma = fmap Compose $ Between <$> ms <*> ma <*> mt

between'F :: DeltaParsing m => m s -> m (f a) -> m (Compose (Between' s) f a)
between'F ms ma = fmap (Compose . Between') $ Between <$> ms <*> ma <*> ms

between' :: DeltaParsing m => m s -> m a -> m (Between' s a)
between' ms ma = fmap Between' $ Between <$> ms <*> ma <*> ms

comma :: DeltaParsing m => m Comma
comma = char ',' $> Comma

dictOrSetMaker :: DeltaParsing m => m (DictOrSetMaker SrcInfo)
dictOrSetMaker = error "dictOrSetMaker not implemented"

testlistComp :: DeltaParsing m => m (TestlistComp SrcInfo)
testlistComp = try testlistCompFor <|> testlistCompList
  where
    testlistCompFor =
      annotated $
      TestlistCompFor <$>
      testOrStar <*>
      whitespaceBeforeF compFor

    testlistCompList =
      annotated $
      TestlistCompList <$>
      testOrStar <*>
      manyF (try $ beforeF (betweenWhitespace comma) testOrStar) <*>
      optional (try $ whitespaceBefore comma)

    testOrStar = try (InL <$> test) <|> (InR <$> starExpr)

testList :: DeltaParsing m => m (TestList SrcInfo)
testList =
  annotated $
  TestList <$>
  test <*>
  beforeF (betweenWhitespace comma) test <*>
  optional (try $ whitespaceBefore comma)

yieldArg :: DeltaParsing m => m (YieldArg SrcInfo)
yieldArg = try yieldArgFrom <|> yieldArgList
  where
    yieldArgFrom =
      annotated $
      YieldArgFrom <$>
      (string "from" *> whitespaceBefore1F test)
    yieldArgList =
      annotated $
      YieldArgList <$> testList

yieldExpr :: DeltaParsing m => m (YieldExpr SrcInfo)
yieldExpr =
  annotated $
  YieldExpr <$>
  (string "yield" *> optionalF (try $ whitespaceBefore1F yieldArg))


atom :: DeltaParsing m => m (Atom SrcInfo)
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
          (try $ (InL <$> try yieldExpr) <|> (InR <$> testlistComp))))

    atomBracket =
      annotated $
      AtomBracket <$>
      between
        (char '[')
        (char ']')
        (betweenWhitespaceF $
          optionalF $ try testlistComp)

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

sliceOp :: DeltaParsing m => m (SliceOp SrcInfo)
sliceOp =
  annotated $
  SliceOp <$>
  (char ':' *> optionalF (try $ whitespaceBeforeF test))

argument :: DeltaParsing m => m (Argument SrcInfo)
argument = try argumentUnpack <|> try argumentDefault <|> argumentFor
  where
    argumentFor =
      annotated $
      ArgumentFor <$>
      test <*>
      optionalF (try $ whitespaceBeforeF compFor)
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

subscript :: DeltaParsing m => m (Subscript SrcInfo)
subscript = try subscriptSlice <|> subscriptTest
  where
    subscriptTest = annotated $ SubscriptTest <$> test
    subscriptSlice =
      annotated $
      SubscriptSlice <$>
      optionalF (try $ whitespaceAfterF test) <*>
      (char ':' *> optionalF (try $ whitespaceBeforeF test)) <*>
      optionalF (try $ whitespaceBeforeF sliceOp)

argList :: DeltaParsing m => m (ArgList SrcInfo)
argList =
  annotated $
  ArgList <$>
  argument <*>
  manyF (try $ beforeF (betweenWhitespace comma) argument) <*>
  optional (try $ whitespaceBefore comma)

subscriptList :: DeltaParsing m => m (SubscriptList SrcInfo)
subscriptList =
  annotated $
  SubscriptList <$>
  subscript <*>
  optionalF (try $ beforeF (betweenWhitespace comma) subscript) <*>
  optional (try $ whitespaceBefore comma)

trailer :: DeltaParsing m => m (Trailer SrcInfo)
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
        (betweenWhitespaceF . optionalF $ try subscriptList)

    trailerAccess =
      annotated $
      TrailerAccess <$>
      (char '.' *> whitespaceBeforeF identifier)

atomExpr :: DeltaParsing m => m (AtomExpr SrcInfo)
atomExpr = try atomExprAwait <|> atomExprNoAwait 
  where
    atomExprNoAwait =
      annotated $
      AtomExprNoAwait <$>
      atom <*>
      manyF (try $ whitespaceBeforeF trailer)

    atomExprAwait =
      annotated $
      AtomExprAwait <$>
      (string "await" *> whitespaceAfter1 (pure KAwait)) <*>
      atom <*>
      manyF (try $ whitespaceBeforeF trailer)

power :: DeltaParsing m => m (Power SrcInfo)
power = try powerMany <|> powerOne
  where
    powerMany =
      annotated $
      PowerMany <$>
      atomExpr <*>
      beforeF (whitespaceAfter doubleAsterisk) factor

    powerOne =
      annotated $
      PowerOne <$>
      atomExpr

factorOp :: DeltaParsing m => m FactorOperator
factorOp =
  try (char '-' $> FactorNeg) <|>
  try (char '+' $> FactorPos) <|>
  (char '~' $> FactorInv)

factor :: DeltaParsing m => m (Factor SrcInfo)
factor = try factorMany <|> factorOne
  where
    factorMany =
      annotated $
      FactorMany <$>
      beforeF (whitespaceAfter factorOp) factor

    factorOne = annotated $ FactorOne <$> power

termOp :: DeltaParsing m => m TermOperator
termOp =
  try (char '*' $> TermMult) <|>
  try (char '@' $> TermAt) <|>
  try (string "//" $> TermFloorDiv) <|>
  try (char '/' $> TermDiv) <|>
  (char '%' $> TermMod)

term :: DeltaParsing m => m (Term SrcInfo)
term = try termMany <|> termOne
  where
    termMany =
      annotated $
      TermMany <$>
      factor <*>
      some1F (try $ beforeF (betweenWhitespace termOp) factor)

    termOne =
      annotated $
      TermOne <$>
      factor

arithExpr :: DeltaParsing m => m (ArithExpr SrcInfo)
arithExpr = try arithExprMany <|> arithExprOne
  where
    arithExprMany =
      annotated $
      ArithExprMany <$>
      term <*>
      some1F (try $ beforeF (betweenWhitespace plusOrMinus) term)

    arithExprOne = annotated $ ArithExprOne <$> term

shiftExpr :: DeltaParsing m => m (ShiftExpr SrcInfo)
shiftExpr = try shiftExprMany <|> shiftExprOne
  where
    shiftExprMany =
      annotated $
      ShiftExprMany <$>
      arithExpr <*>
      some1F (try $ beforeF (betweenWhitespace shiftLeftOrRight) arithExpr)
      where
        shiftLeftOrRight =
          (Left <$> try (string "<<" $> DoubleLT)) <|>
          (Right <$> (string ">>" $> DoubleGT))

    shiftExprOne =
      annotated $ ShiftExprOne <$> arithExpr

andExpr :: DeltaParsing m => m (AndExpr SrcInfo)
andExpr = try andExprMany <|> andExprOne
  where
    andExprMany =
      annotated $
      AndExprMany <$>
      shiftExpr <*>
      some1F (try $ beforeF (betweenWhitespace $ char '&' $> Ampersand) shiftExpr)

    andExprOne =
      annotated $
      AndExprOne <$>
      shiftExpr

xorExpr :: DeltaParsing m => m (XorExpr SrcInfo)
xorExpr = try xorExprMany <|> xorExprOne
  where
    xorExprMany =
      annotated $
      XorExprMany <$>
      andExpr <*>
      some1F (try $ beforeF (betweenWhitespace $ char '^' $> S.Caret) andExpr)

    xorExprOne =
      annotated $
      XorExprOne <$>
      andExpr
  
expr :: DeltaParsing m => m (Expr SrcInfo)
expr = try exprMany <|> exprOne
  where
    exprMany =
      annotated $
      ExprMany <$>
      xorExpr <*>
      some1F (try $ beforeF (betweenWhitespace $ char '|' $> Pipe) xorExpr)

    exprOne =
      annotated $ ExprOne <$> xorExpr

compOperator :: DeltaParsing m => m CompOperator
compOperator =
  try (string "==" $> CompEq) <|>
  try (string ">=" $> CompGEq) <|>
  try (string "!=" $> CompNEq) <|>
  try (string "<=" $> CompLEq) <|>
  try (char '<' $> CompLT) <|>
  try (char '>' $> CompGT) <|>
  try (string "is" *>
    (CompIsNot <$> some1 whitespaceChar) <*
    string "not" <*>
    whitespaceChar) <|>
  try (string "is" *> (CompIs <$> whitespaceChar)) <|>
  try (string "in" *> (CompIn <$> whitespaceChar)) <|>
  (string "not" *>
    (CompNotIn <$> some1 whitespaceChar) <*
    string "in" <*>
    whitespaceChar)

comparison :: DeltaParsing m => m (Comparison SrcInfo)
comparison = try comparisonMany <|> comparisonOne
  where
    comparisonMany =
      annotated $
      ComparisonMany <$>
      expr <*>
      some1F
        (try $ beforeF
          (betweenWhitespace compOperator)
          expr)

    comparisonOne =
      annotated $ ComparisonOne <$> expr

notTest :: DeltaParsing m => m (NotTest SrcInfo)
notTest = try notTestMany <|> notTestOne
  where
    notTestMany =
      annotated $
      NotTestMany <$>
      beforeF (whitespaceAfter1 $ string "not" $> KNot) notTest

    notTestOne =
      annotated $ NotTestOne <$> comparison

andTest :: DeltaParsing m => m (AndTest SrcInfo)
andTest = try andTestMany <|> andTestOne
  where
    andTestMany =
      annotated $
      AndTestMany <$>
      notTest <*>
      some1F
        (try $
          beforeF
            (betweenWhitespace1 kAnd)
            andTest)

    andTestOne =
      annotated $ AndTestOne <$> notTest

newlineChar :: CharParsing m => m NewlineChar
newlineChar =
  (char '\r' $> CR) <|>
  (char '\n' $> LF) <|>
  (string "\r\n" $> CRLF)
