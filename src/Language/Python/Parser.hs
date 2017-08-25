-- from https://docs.python.org/3.5/reference/grammar.html
-- `test` is the production for an expression

{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
module Language.Python.Parser where

import GHC.Stack
import Prelude (error)

import Papa hiding (Space, zero, o, Plus, (\\), Product, argument)
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Set (Set)
import Data.Text (Text)
import Text.Trifecta as P hiding
  (stringLiteral, integer, octDigit, hexDigit, comma, colon)

import Data.CharSet ((\\))
import qualified Data.CharSet as CharSet
import qualified Data.CharSet.Common as CharSet
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Separated.After (After(..))
import Data.Separated.Before (Before(..))
import Data.Separated.Between (Between(..), Between'(..))
import Language.Python.AST
import Language.Python.AST.Digits
import Language.Python.AST.EscapeSeq
import Language.Python.AST.Keywords
import Language.Python.AST.LongBytesChar
import Language.Python.AST.LongStringChar
import Language.Python.AST.ShortBytesChar
import Language.Python.AST.ShortStringChar
import Language.Python.AST.Symbols as S

data SrcInfo
  = SrcInfo
  { _srcCaret :: P.Caret
  , _srcSpan :: Span
  }
  deriving (Eq, Show)

annotated
  :: ( Monad m
     , Functor f
     , DeltaParsing m
     )
  => m (SrcInfo -> f SrcInfo)
  -> m (f SrcInfo)
annotated m = do
  c <- careting
  f :~ s <- spanned m
  pure . f $ SrcInfo c s

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

ifThenElse
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , DeltaParsing m
     )
  => m (IfThenElse 'NotAssignable ctxt SrcInfo)
ifThenElse =
  IfThenElse <$>
  (string "if" *> betweenWhitespace1F orTest) <*>
  (string "else" *> whitespaceBefore1F test)

testLambdef = error "testLambdef not implemented"

class TestParsing (atomType :: AtomType) where
  test
    :: ( OrTestParsing atomType ctxt
       , AtomExprParsing atomType ctxt
       , AtomParsing 'Assignable ctxt
       , AtomParsing 'NotAssignable ctxt
       , TestParsing atomType
       , TestParsing 'Assignable
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (Test atomType ctxt SrcInfo)

instance TestParsing 'Assignable where
  test = testCondNoIf

instance TestParsing 'NotAssignable where
  test = try testCondIf <|> try testCondNoIf <|> testLambdef

testCondIf
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , OrTestParsing 'NotAssignable ctxt
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , DeltaParsing m
     )
  => m (Test 'NotAssignable ctxt SrcInfo)
testCondIf =
  annotated $
  TestCondIf <$>
  orTest <*>
  whitespaceBefore1F ifThenElse

testCondNoIf
  :: ( AtomExprParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , DeltaParsing m
     , TestParsing atomType
     , TestParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , OrTestParsing atomType ctxt
     )
  => m (Test atomType ctxt SrcInfo)
testCondNoIf =
  annotated $
  TestCondNoIf <$>
  orTest

kOr :: DeltaParsing m => m KOr
kOr = string "or" $> KOr

kAnd :: DeltaParsing m => m KAnd
kAnd = string "and" $> KAnd

class OrTestParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  orTest
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , AtomParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , TestlistCompParsing 'Assignable
       , DeltaParsing m
       )
    => m (OrTest atomType ctxt SrcInfo)

instance OrTestParsing 'NotAssignable ctxt where
  orTest = try orTestMany <|> orTestOne

instance OrTestParsing 'Assignable ctxt where
  orTest = orTestOne

orTestOne
  :: ( AtomExprParsing atomType ctxt
     , AndTestParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (OrTest atomType ctxt SrcInfo)
orTestOne =
  annotated $
  OrTestOne <$>
  andTest

orTestMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (OrTest 'NotAssignable ctxt SrcInfo)
orTestMany =
  annotated $
  OrTestMany <$>
  andTest <*>
  some1F (beforeF (betweenWhitespace1 kOr) andTest)

varargsList :: DeltaParsing m => m (VarargsList atomType ctxt SrcInfo)
varargsList = error "varargsList not implemented"

lambdefNocond
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , DeltaParsing m
     , OrTestParsing 'NotAssignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     )
  => m (LambdefNocond 'NotAssignable ctxt SrcInfo)
lambdefNocond =
  annotated $
  LambdefNocond <$>
  optionalF
    (try $ betweenF
      (some1 whitespaceChar)
      (many whitespaceChar)
      varargsList) <*>
  whitespaceBeforeF testNocond

class TestNocondParsing (atomType :: AtomType) where
  testNocond
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , AtomParsing 'Assignable ctxt
       , DeltaParsing m
       , TestParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , TestlistCompParsing 'Assignable
       , OrTestParsing atomType ctxt
       )
    => m (TestNocond atomType ctxt SrcInfo)

instance TestNocondParsing 'NotAssignable where
  testNocond =
    annotated $
    TestNocond <$> (try (InL <$> orTest) <|> (InR <$> lambdefNocond))

instance TestNocondParsing 'Assignable where
  testNocond =
    annotated $
    TestNocond . InL <$> orTest

compIf
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , DeltaParsing m
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     )
  => m (CompIf 'NotAssignable ctxt SrcInfo)
compIf =
  annotated $
  CompIf <$>
  (string "if" *> whitespaceBeforeF testNocond) <*>
  optionalF (try $ whitespaceBeforeF compIter)

compIter
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , DeltaParsing m
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     )
  => m (CompIter 'NotAssignable ctxt SrcInfo)
compIter =
  annotated $
  CompIter <$> (try (InL <$> compFor) <|> (InR <$> compIf))

starExpr
  :: ( AtomExprParsing atomType ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (StarExpr atomType ctxt SrcInfo)
starExpr =
  annotated $
  StarExpr <$>
  (char '*' *> whitespaceBeforeF expr)

exprList
  :: ( AtomExprParsing atomType ctxt
     , AtomExprParsing 'Assignable ctxt
     , ExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (ExprList atomType ctxt SrcInfo)
exprList =
  annotated $
  ExprList <$>
  exprOrStar <*>
  manyF (try $ beforeF (betweenWhitespace comma) exprOrStar)
  where
    exprOrStar = try (InL <$> expr) <|> (InR <$> starExpr)

compFor
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , DeltaParsing m
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     )
  => m (CompFor 'NotAssignable ctxt SrcInfo)
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

identifier :: DeltaParsing m => Set String -> m (Identifier SrcInfo)
identifier keywords = do
  ident <- liftA2 (:) idStart (many idContinue)
  when (ident `S.member` keywords) .
    unexpected $ "keyword '" ++ ident ++ "'"
  annotated . pure $ Identifier (T.pack ident)
  where
    idStart = try letter <|> char '_'
    idContinue = try idStart <|> digit

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

literal :: DeltaParsing m => m (Literal SrcInfo)
literal =
  try literalString <|>
  try literalInteger <|>
  try literalFloat <|>
  literalImag
  where
    stringOrBytes = try (InL <$> stringLiteral) <|> (InR <$> bytesLiteral)
    literalString =
      annotated $
      LiteralString <$>
      stringOrBytes <*>
      manyF (try $ whitespaceBeforeF stringOrBytes)
    literalInteger = annotated $ LiteralInteger <$> integer
    literalFloat = annotated $ LiteralFloat <$> float
    literalImag = annotated $ LiteralImag <$> imag
    
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

dictOrSetMaker :: DeltaParsing m => m (DictOrSetMaker atomType ctxt SrcInfo)
dictOrSetMaker = error "dictOrSetMaker not implemented"

class TestlistCompParsing (atomType :: AtomType) where
  testlistComp
    :: ( DeltaParsing m
       , AtomExprParsing atomType ctxt
       , AtomExprParsing 'Assignable ctxt
       , AtomParsing 'NotAssignable ctxt
       , AtomParsing 'Assignable ctxt
       , TestParsing atomType
       , OrTestParsing atomType ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       )
    => m (TestlistComp atomType ctxt SrcInfo)

instance TestlistCompParsing 'NotAssignable where
  testlistComp = try testlistCompFor <|> testlistCompList

instance TestlistCompParsing 'Assignable where
  testlistComp = testlistCompList

testlistCompFor
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (TestlistComp 'NotAssignable ctxt SrcInfo)
testlistCompFor =
  annotated $
  TestlistCompFor <$>
  testOrStar <*>
  whitespaceBeforeF compFor
  where
    testOrStar = try (InL <$> test) <|> (InR <$> starExpr)

testlistCompList
  :: ( AtomExprParsing atomType ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , TestParsing atomType
     , TestParsing 'Assignable
     , OrTestParsing atomType ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (TestlistComp atomType ctxt SrcInfo)
testlistCompList =
  annotated $
  TestlistCompList <$>
  testOrStar <*>
  manyF (try $ beforeF (betweenWhitespace comma) testOrStar) <*>
  optional (try $ whitespaceBefore comma)
  where
    testOrStar = try (InL <$> test) <|> (InR <$> starExpr)

testList
  :: ( TestParsing atomType
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , OrTestParsing atomType ctxt
     , DeltaParsing m
     , AtomExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     )
  => m (TestList atomType ctxt SrcInfo)
testList =
  annotated $
  TestList <$>
  test <*>
  beforeF (betweenWhitespace comma) test <*>
  optional (try $ whitespaceBefore comma)

yieldArg
  :: ( TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , OrTestParsing 'NotAssignable ('FunDef 'Normal)
     , AtomExprParsing 'NotAssignable ('FunDef 'Normal)
     , AtomParsing 'NotAssignable ('FunDef 'Normal)
     , AtomParsing 'Assignable ('FunDef 'Normal)
     , DeltaParsing m
     )
  => m (YieldArg 'NotAssignable ('FunDef 'Normal) SrcInfo)
yieldArg = try yieldArgFrom <|> yieldArgList
  where
    yieldArgFrom =
      annotated $
      YieldArgFrom <$>
      (string "from" *> whitespaceBefore1F test)
    yieldArgList =
      annotated $
      YieldArgList <$> testList

yieldExpr
  :: ( OrTestParsing 'NotAssignable ('FunDef 'Normal)
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (YieldExpr SrcInfo)
yieldExpr =
  annotated $
  YieldExpr <$>
  (string "yield" *> optionalF (try $ whitespaceBefore1F yieldArg))

atomParenYield
  :: ( DeltaParsing m
     , OrTestParsing 'NotAssignable ('FunDef 'Normal)
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     )
  => m (Atom 'NotAssignable ('FunDef 'Normal) SrcInfo)
atomParenYield =
  annotated $
  AtomParenYield <$>
  between (char '(') (char ')') (betweenWhitespaceF yieldExpr)

class AtomParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  atom
    :: ( AtomExprParsing atomType ctxt
       , TestParsing atomType
       , TestParsing 'Assignable
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , OrTestParsing atomType ctxt
       , DeltaParsing m
       , AtomParsing 'NotAssignable ctxt
       )
    => m (Atom atomType ctxt SrcInfo)

instance AtomParsing 'Assignable ('FunDef 'Async) where
  atom = try atomAssign <|> atomIdentifierAsync

instance AtomParsing 'Assignable ('FunDef 'Normal) where
  atom = try atomAssign <|> atomIdentifierNormal

instance AtomParsing 'Assignable 'TopLevel where
  atom = try atomAssign <|> atomIdentifierToplevel

instance AtomParsing 'NotAssignable ('FunDef 'Async) where
  atom =
    try atomNoAssign <|>
    try atomAssign <|>
    atomIdentifierAsync

instance AtomParsing 'NotAssignable ('FunDef 'Normal) where
  atom =
    try atomParenYield <|>
    try atomAssign <|>
    try atomNoAssign <|>
    atomIdentifierNormal

instance AtomParsing 'NotAssignable 'TopLevel where
  atom =
    try atomNoAssign <|>
    try atomAssign <|>
    atomIdentifierToplevel

atomIdentifierToplevel
  :: ( DeltaParsing m
     )
  => m (Atom atomType 'TopLevel SrcInfo)
atomIdentifierToplevel =
  annotated $
  AtomIdentifier <$> identifier alwaysKeywords

atomIdentifierNormal
  :: ( DeltaParsing m
     )
  => m (Atom atomType ('FunDef 'Normal) SrcInfo)
atomIdentifierNormal =
  annotated $
  AtomIdentifier <$> identifier alwaysKeywords

atomIdentifierAsync
  :: ( DeltaParsing m
     )
  => m (Atom atomType ('FunDef 'Async) SrcInfo)
atomIdentifierAsync =
  annotated $
  AtomIdentifier <$>
  identifier (alwaysKeywords `S.union` S.fromList ["async", "await"])

atomAssign
  :: ( AtomExprParsing atomType ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , TestParsing atomType
     , TestParsing 'Assignable
     , OrTestParsing atomType ctxt
     , TestlistCompParsing atomType
     , DeltaParsing m
     )
  => m (Atom atomType ctxt SrcInfo)
atomAssign =
  try atomParenNoYield <|>
  try atomBracket <|>
  atomCurly
  where
    atomParenNoYield =
      annotated $
      AtomParenNoYield <$>
      between (char '(') (char ')')
      (betweenWhitespaceF
        (optionalF
          (try testlistComp)))
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

atomNoAssign
  :: ( AtomExprParsing 'NotAssignable ctxt
     , TestParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (Atom 'NotAssignable ctxt SrcInfo)
atomNoAssign =
  try atomInteger <|>
  try atomFloat <|>
  try atomString <|>
  try atomEllipsis <|>
  try atomNone <|>
  try atomTrue <|>
  atomFalse
  where
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
      string "None" $> AtomNone
    atomTrue =
      annotated $
      string "True" $> AtomTrue
    atomFalse =
      annotated $
      string "False" $> AtomFalse

sliceOp
  :: ( TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , OrTestParsing 'NotAssignable ctxt
     , AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , DeltaParsing m
     )
  => m (SliceOp 'NotAssignable ctxt SrcInfo)
sliceOp =
  annotated $
  SliceOp <$>
  (char ':' *> optionalF (try $ whitespaceBeforeF test))

argument
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (Argument 'NotAssignable ctxt SrcInfo)
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


subscript
  :: ( TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , OrTestParsing 'NotAssignable ctxt
     , AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , DeltaParsing m
     )
  => m (Subscript 'NotAssignable ctxt SrcInfo)
subscript = try subscriptSlice <|> subscriptTest
  where
    subscriptTest = annotated $ SubscriptTest <$> test
    subscriptSlice =
      annotated $
      SubscriptSlice <$>
      optionalF (try $ whitespaceAfterF test) <*>
      (char ':' *> optionalF (try $ whitespaceBeforeF test)) <*>
      optionalF (try $ whitespaceBeforeF sliceOp)

argList
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (ArgList 'NotAssignable ctxt SrcInfo)
argList =
  annotated $
  ArgList <$>
  argument <*>
  manyF (try $ beforeF (betweenWhitespace comma) argument) <*>
  optional (try $ whitespaceBefore comma)

subscriptList
  :: ( TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , OrTestParsing 'NotAssignable ctxt
     , AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , DeltaParsing m
     )
  => m (SubscriptList 'NotAssignable ctxt SrcInfo)
subscriptList =
  annotated $
  SubscriptList <$>
  subscript <*>
  optionalF (try $ beforeF (betweenWhitespace comma) subscript) <*>
  optional (try $ whitespaceBefore comma)

trailer
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (Trailer 'NotAssignable ctxt SrcInfo)
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
      (char '.' *> whitespaceBeforeF (identifier alwaysKeywords))

class AtomExprParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  atomExpr
    :: ( AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , OrTestParsing atomType ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestParsing atomType
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (AtomExpr atomType ctxt SrcInfo)

instance AtomExprParsing atomType ('FunDef 'Normal) where
  atomExpr = atomExprNoAwait

instance AtomExprParsing atomType 'TopLevel where
  atomExpr = atomExprNoAwait

instance AtomExprParsing 'Assignable ('FunDef 'Async) where
  atomExpr = atomExprNoAwait

instance AtomExprParsing 'NotAssignable ('FunDef 'Async) where
  atomExpr = atomExprAwait

atomExprNoAwait
  :: ( AtomParsing atomType ctxt
     , AtomExprParsing atomType ctxt
     , AtomExprParsing 'NotAssignable ctxt
     , AtomExprParsing 'Assignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , AtomParsing 'Assignable ctxt
     , TestParsing atomType
     , TestParsing 'NotAssignable
     , TestParsing 'Assignable
     , OrTestParsing atomType ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (AtomExpr atomType ctxt SrcInfo)
atomExprNoAwait =
  annotated $
  AtomExprNoAwait <$>
  atom <*>
  manyF (try $ whitespaceBeforeF trailer)

atomExprAwait
  :: ( DeltaParsing m
     , AtomParsing 'NotAssignable ('FunDef 'Async)
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , TestParsing 'Assignable
     , TestParsing 'NotAssignable
     , OrTestParsing 'Assignable ('FunDef 'Async)
     , OrTestParsing 'NotAssignable ('FunDef 'Async)
     )
  => m (AtomExpr 'NotAssignable ('FunDef 'Async) SrcInfo)
atomExprAwait =
  annotated $
  AtomExprAwait <$>
  optionalF (string "await" *> whitespaceAfter1 (pure KAwait)) <*>
  atom <*>
  manyF (try $ whitespaceBeforeF trailer)

class PowerParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  power
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , TestlistCompParsing 'NotAssignable
       , TestlistCompParsing 'Assignable
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , DeltaParsing m
       )
    => m (Power atomType ctxt SrcInfo)

instance PowerParsing 'NotAssignable ctxt where
  power = try powerSome <|> powerOne

instance PowerParsing 'Assignable ctxt where
  power = powerOne

powerSome
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , DeltaParsing m
     )
  => m (Power 'NotAssignable ctxt SrcInfo)
powerSome =
  annotated $
  PowerSome <$>
  atomExpr <*>
  beforeF (whitespaceAfter doubleAsterisk) factor

powerOne
  :: ( AtomExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , OrTestParsing atomType ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing atomType
     , TestParsing 'Assignable
     , DeltaParsing m
     )
  => m (Power atomType ctxt SrcInfo)
powerOne =
  annotated $
  PowerOne <$>
  atomExpr

factorOp :: DeltaParsing m => m FactorOp
factorOp =
  try (char '-' $> FactorNeg) <|>
  try (char '+' $> FactorPos) <|>
  (char '~' $> FactorInv)

class FactorParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  factor
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , OrTestParsing 'Assignable ctxt
       , OrTestParsing 'NotAssignable ctxt
       , TestParsing 'Assignable
       , DeltaParsing m
       )
    => m (Factor atomType ctxt SrcInfo)

instance FactorParsing 'NotAssignable ctxt where
  factor = try factorSome <|> factorNone

instance FactorParsing 'Assignable ctxt where
  factor = factorNone

factorSome
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , OrTestParsing 'Assignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , TestParsing 'Assignable
     , DeltaParsing m
     )
  => m (Factor 'NotAssignable ctxt SrcInfo)
factorSome =
  annotated $
  FactorSome <$>
  beforeF (whitespaceAfter factorOp) factor

factorNone
  :: ( AtomExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , OrTestParsing atomType ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , PowerParsing atomType ctxt
     , DeltaParsing m
     )
  => m (Factor atomType ctxt SrcInfo)
factorNone = annotated $ FactorNone <$> power

termOp :: DeltaParsing m => m TermOp
termOp =
  try (char '*' $> TermMult) <|>
  try (char '@' $> TermAt) <|>
  try (string "//" $> TermFloorDiv) <|>
  try (char '/' $> TermDiv) <|>
  (char '%' $> TermMod)

class TermParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  term
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , DeltaParsing m
       ) => m (Term atomType ctxt SrcInfo)

instance TermParsing 'NotAssignable ctxt where
  term = try termSome <|> termOne

instance TermParsing 'Assignable ctxt where
  term = termOne

termSome
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , OrTestParsing 'Assignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , TestParsing 'Assignable
     , DeltaParsing m
     )
  => m (Term 'NotAssignable ctxt SrcInfo)
termSome =
  annotated $
  TermSome <$>
  factor <*>
  some1F (try $ beforeF (betweenWhitespace termOp) factor)

termOne
  :: ( AtomExprParsing atomType ctxt
     , FactorParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , OrTestParsing 'Assignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , TestParsing 'Assignable
     , DeltaParsing m
     )
  => m (Term atomType ctxt SrcInfo)
termOne =
  annotated $
  TermOne <$>
  factor

class ArithExprParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  arithExpr
    :: ( DeltaParsing m
       , AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       )
    => m (ArithExpr atomType ctxt SrcInfo)

instance ArithExprParsing 'NotAssignable ctxt where
  arithExpr = try arithExprMany <|> arithExprOne

instance ArithExprParsing 'Assignable ctxt where
  arithExpr = arithExprOne

arithExprMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , DeltaParsing m
     )
  => m (ArithExpr 'NotAssignable ctxt SrcInfo)
arithExprMany =
  annotated $
  ArithExprMany <$>
  term <*>
  some1F (try $ beforeF (betweenWhitespace plusOrMinus) term)
  where
    plusOrMinus =
      (Left <$> try (char '+' $> Plus)) <|> (Right <$> (char '-' $> Minus))

arithExprOne
  :: ( AtomExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , TermParsing atomType ctxt
     , DeltaParsing m
     )
  => m (ArithExpr atomType ctxt SrcInfo)
arithExprOne = annotated $ ArithExprOne <$> term

class ShiftExprParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  shiftExpr
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (ShiftExpr atomType ctxt SrcInfo)

instance ShiftExprParsing 'NotAssignable ctxt where
  shiftExpr = try shiftExprMany <|> shiftExprOne

instance ShiftExprParsing 'Assignable ctxt where
  shiftExpr = shiftExprOne

shiftExprMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , DeltaParsing m
     )
  => m (ShiftExpr 'NotAssignable ctxt SrcInfo)
shiftExprMany =
  annotated $
  ShiftExprMany <$>
  arithExpr <*>
  some1F (try $ beforeF (betweenWhitespace shiftLeftOrRight) arithExpr)
  where
    shiftLeftOrRight =
      (Left <$> try (string "<<" $> DoubleLT)) <|>
      (Right <$> (string ">>" $> DoubleGT))

shiftExprOne
  :: ( AtomExprParsing atomType ctxt
     , ArithExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (ShiftExpr atomType ctxt SrcInfo)
shiftExprOne =
  annotated $ ShiftExprOne <$> arithExpr

class AndExprParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  andExpr
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (AndExpr atomType ctxt SrcInfo)

instance AndExprParsing 'NotAssignable ctxt where
  andExpr = try andExprMany <|> andExprOne

instance AndExprParsing 'Assignable ctxt where
  andExpr = andExprOne

andExprMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , DeltaParsing m
     )
  => m (AndExpr 'NotAssignable ctxt SrcInfo)
andExprMany =
  annotated $
  AndExprMany <$>
  shiftExpr <*>
  some1F (try $ beforeF (betweenWhitespace $ char '&' $> Ampersand) shiftExpr)

andExprOne
  :: ( AtomExprParsing atomType ctxt
     , ShiftExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (AndExpr atomType ctxt SrcInfo)
andExprOne =
  annotated $
  AndExprOne <$>
  shiftExpr

class XorExprParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  xorExpr
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (XorExpr atomType ctxt SrcInfo)

instance XorExprParsing 'NotAssignable ctxt where
  xorExpr = try xorExprMany <|> xorExprOne

instance XorExprParsing 'Assignable ctxt where
  xorExpr = xorExprOne

xorExprMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , DeltaParsing m
     )
  => m (XorExpr 'NotAssignable ctxt SrcInfo)
xorExprMany =
  annotated $
  XorExprMany <$>
  andExpr <*>
  some1F (try $ beforeF (betweenWhitespace $ char '^' $> S.Caret) andExpr)

xorExprOne
  :: ( AtomExprParsing atomType ctxt
     , AndExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (XorExpr atomType ctxt SrcInfo)
xorExprOne =
  annotated $
  XorExprOne <$>
  andExpr
  
class ExprParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  expr
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (Expr atomType ctxt SrcInfo)

instance ExprParsing 'NotAssignable ctxt where
  expr = try exprMany <|> exprOne

instance ExprParsing 'Assignable ctxt where
  expr = exprOne

exprMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , TestlistCompParsing 'Assignable
     , DeltaParsing m
     )
  => m (Expr 'NotAssignable ctxt SrcInfo)
exprMany =
  annotated $
  ExprMany <$>
  xorExpr <*>
  some1F (try $ beforeF (betweenWhitespace $ char '|' $> Pipe) xorExpr)

exprOne
  :: ( AtomExprParsing atomType ctxt
     , XorExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (Expr atomType ctxt SrcInfo)
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
  
class ComparisonParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  comparison
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (Comparison atomType ctxt SrcInfo)

instance ComparisonParsing 'NotAssignable ctxt where
  comparison = try comparisonMany <|> comparisonOne

instance ComparisonParsing 'Assignable ctxt where
  comparison = comparisonOne

comparisonMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (Comparison 'NotAssignable ctxt SrcInfo)
comparisonMany =
  annotated $
  ComparisonMany <$>
  expr <*>
  some1F
    (try $ beforeF
      (betweenWhitespace compOperator)
      expr)

comparisonOne
  :: ( AtomExprParsing atomType ctxt
     , ExprParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (Comparison atomType ctxt SrcInfo)
comparisonOne =
  annotated $ ComparisonOne <$> expr
  
class NotTestParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  notTest
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (NotTest atomType ctxt SrcInfo)

instance NotTestParsing 'NotAssignable ctxt where
  notTest = try notTestMany <|> notTestOne

instance NotTestParsing 'Assignable ctxt where
  notTest = notTestOne

notTestMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (NotTest 'NotAssignable ctxt SrcInfo)
notTestMany =
  annotated $
  NotTestMany <$>
  beforeF (whitespaceAfter1 $ string "not" $> KNot) notTest

notTestOne
  :: ( AtomExprParsing atomType ctxt
     , ComparisonParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (NotTest atomType ctxt SrcInfo)
notTestOne =
  annotated $ NotTestNone <$> comparison

class AndTestParsing (atomType :: AtomType) (ctxt :: ExprContext) where
  andTest
    :: ( AtomExprParsing atomType ctxt
       , AtomParsing atomType ctxt
       , AtomParsing 'NotAssignable ctxt
       , OrTestParsing 'NotAssignable ctxt
       , OrTestParsing 'Assignable ctxt
       , TestParsing 'Assignable
       , TestlistCompParsing atomType
       , TestlistCompParsing 'Assignable
       , TestlistCompParsing 'NotAssignable
       , DeltaParsing m
       )
    => m (AndTest atomType ctxt SrcInfo)

instance AndTestParsing 'NotAssignable ctxt where
  andTest = try andTestMany <|> andTestOne

instance AndTestParsing 'Assignable ctxt where
  andTest = andTestOne

andTestMany
  :: ( AtomExprParsing 'NotAssignable ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (AndTest 'NotAssignable ctxt SrcInfo)
andTestMany =
  annotated $
  AndTestMany <$>
  notTest <*>
  some1F
    (try $
      beforeF
        (betweenWhitespace1 kAnd)
        andTest)

andTestOne
  :: ( AtomExprParsing atomType ctxt
     , NotTestParsing atomType ctxt
     , AtomParsing atomType ctxt
     , AtomParsing 'NotAssignable ctxt
     , OrTestParsing 'NotAssignable ctxt
     , OrTestParsing 'Assignable ctxt
     , TestParsing 'Assignable
     , TestlistCompParsing atomType
     , TestlistCompParsing 'Assignable
     , TestlistCompParsing 'NotAssignable
     , DeltaParsing m
     )
  => m (AndTest atomType ctxt SrcInfo)
andTestOne =
  annotated $ AndTestOne <$> notTest

newlineChar :: CharParsing m => m NewlineChar
newlineChar =
  (char '\r' $> CR) <|>
  (char '\n' $> LF) <|>
  (string "\r\n" $> CRLF)
