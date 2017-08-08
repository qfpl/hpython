-- from https://docs.python.org/3.5/reference/grammar.html

module Language.Python.Parser where

import Prelude (error)

import Papa hiding (Space, zero, o, Plus, (\\), Product)
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Text.Trifecta hiding
  (stringLiteral, integer, octDigit, hexDigit, comma, colon)

import Data.CharSet ((\\))
import qualified Data.CharSet as CharSet
import qualified Data.CharSet.Common as CharSet
import qualified Data.Text as T

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

data SrcInfo
  = SrcInfo
  { _srcCaret :: Caret
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
  
target :: DeltaParsing m => m (Target SrcInfo)
target =
  try targetIdentifier <|>
  try targetTuple <|>
  try targetList' <|>
  try targetAttRef <|>
  try targetSubscription <|>
  try targetSlicing <|>
  targetUnpacked
  where
    targetIdentifier = annotated $ TargetIdentifier <$> identifier
    targetTuple =
      annotated $
      TargetTuple <$>
      between (char '(') (char ')') (betweenWhitespaceF targetList)
    targetList' = 
      annotated $
      TargetList' <$>
      between
        (char '[')
        (char ']')
        (betweenWhitespaceF (Compose <$> optional targetList))
    targetAttRef = annotated $ TargetAttRef <$> attRef
    targetSubscription = annotated $ TargetSubscription <$> subscription
    targetSlicing = annotated $ TargetSlicing <$> slicing
    targetUnpacked =
      annotated $
      TargetUnpacked <$> (char '*' *> whitespaceBeforeF target)

targetList :: DeltaParsing m => m (TargetList SrcInfo)
targetList =
  annotated $
  TargetList <$>
  target <*>
  (Compose <$>
    many (Compose <$> liftA2 (,) (betweenWhitespace comma) target)) <*>
  optional (whitespaceBefore comma)

parameterList :: DeltaParsing m => m (ParameterList SrcInfo)
parameterList = error "parameterList not implemented" -- ParameterList

lambdaExpressionNocond :: DeltaParsing m => m (LambdaExpressionNocond SrcInfo)
lambdaExpressionNocond =
  annotated $
  LambdaExprNocond <$>
  (string "lambda" *>
    beforeF
    whitespaceChar
    (Compose <$> optional (betweenWhitespaceF parameterList))) <*>
  (colon *> whitespaceBeforeF expressionNocond)

expressionNocond :: DeltaParsing m => m (ExpressionNocond SrcInfo)
expressionNocond =
  annotated $
  ExpressionNocond <$>
  (try (InL <$> orTest) <|> (InR <$> lambdaExpressionNocond))
  
compIf :: DeltaParsing m => m (CompIf SrcInfo)
compIf =
  annotated $
  CompIf <$>
  (string "if" *> whitespaceBeforeF expressionNocond) <*>
  (Compose <$> optional (whitespaceBeforeF compIter))
  
compIter :: DeltaParsing m => m (CompIter SrcInfo)
compIter =
  annotated $
  CompIter <$> (try (InL <$> compFor) <|> (InR <$> compIf))
  
compFor :: DeltaParsing m => m (CompFor SrcInfo)
compFor =
  annotated $
  CompFor <$>
  (string "for" *> betweenWhitespaceF targetList) <*>
  (string "in" *> whitespaceBeforeF orTest) <*>
  (Compose <$> optional (whitespaceBeforeF compIter))
  
comprehension :: DeltaParsing m => m (Comprehension SrcInfo)
comprehension =
  annotated $
  Comprehension <$>
  whitespaceAfterF expression <*>
  compFor

starredAndKeywords :: DeltaParsing m => m (StarredAndKeywords SrcInfo)
starredAndKeywords =
  annotated $
  StarredAndKeywords <$>
  unpackedOrKeyword <*>
  (Compose <$> liftA2 (,) (whitespaceAfter comma) unpackedOrKeyword)
  where
    unpackedOrKeyword = try (InL <$> unpackedExpr) <|> (InR <$> keywordItem)
    unpackedExpr =
      Compose <$> liftA2 (,) (whitespaceAfter asterisk) expression

keywordItem :: DeltaParsing m => m (KeywordItem SrcInfo)
keywordItem =
  annotated $
  KeywordItem <$>
  (whitespaceAfterF identifier <* char '=') <*>
  whitespaceBeforeF expression

doubleAsterisk :: DeltaParsing m => m DoubleAsterisk
doubleAsterisk = string "**" $> DoubleAsterisk

keywordsArgs :: DeltaParsing m => m (KeywordsArgs SrcInfo)
keywordsArgs =
  annotated $
  KeywordsArgs <$>
  keywordOrUnpacked <*>
  (Compose <$> liftA2 (,) (whitespaceAfter comma) keywordOrUnpacked)
  where
    keywordOrUnpacked = try (InL <$> keywordItem) <|> (InR <$> unpackedExpr)
    unpackedExpr =
      Compose <$> liftA2 (,) (whitespaceAfter doubleAsterisk) expression

asterisk :: DeltaParsing m => m Asterisk
asterisk = char '*' $> Asterisk

positionalArgs :: DeltaParsing m => m (PositionalArgs SrcInfo)
positionalArgs =
  annotated $
  PositionalArgs <$>
  (Compose <$>
    liftA2 (,) (optional $ whitespaceAfter asterisk) expression) <*>
  (Compose <$>
    many
      (Compose <$>
        liftA2
          (,)
          (whitespaceAfter comma)
          (Compose <$>
            liftA2 (,) (optional $ whitespaceAfter asterisk) expression)))

argList :: DeltaParsing m => m (ArgList SrcInfo)
argList = try argListAll <|> try argListStarred <|> try argListKeywords
  where
    argListAll =
      annotated $
      ArgListAll <$>
      positionalArgs <*>
      (Compose <$>
        optional
          (Compose <$>
            liftA2 (,) (betweenWhitespace comma) starredAndKeywords)) <*>
      (Compose <$>
        optional
          (Compose <$>
            liftA2 (,) (betweenWhitespace comma) keywordsArgs))
    argListStarred =
      annotated $
      ArgListStarred <$>
      starredAndKeywords <*>
      (Compose <$>
        optional
          (Compose <$>
            liftA2 (,) (betweenWhitespace comma) keywordsArgs))
    argListKeywords = annotated $ ArgListKeywords <$> keywordsArgs

call :: DeltaParsing m => m (Call SrcInfo)
call =
  annotated $
  Call <$>
  whitespaceAfterF primary <*>
  between (char '(') (char ')')
    (Compose <$> optional
      (betweenWhitespaceF
        (try (InL <$> args) <|> (InR <$> comprehension))))
  where
    args = Pair <$> whitespaceAfterF argList <*> (Const <$> optional comma)

colon :: DeltaParsing m => m Colon
colon = char ':' $> Colon

properSlice :: DeltaParsing m => m (ProperSlice SrcInfo)
properSlice =
  annotated $
  ProperSlice <$>
  (Compose <$> optional (whitespaceAfterF expression)) <*>
  fmap Compose (colon *> optional (betweenWhitespaceF expression)) <*>
  (Compose <$> optional
    (Compose <$>
      liftA2
        (,)
        (whitespaceBefore colon)
        (Compose <$> optional (whitespaceBeforeF expression))))

sliceItem :: DeltaParsing m => m (SliceItem SrcInfo)
sliceItem = try sliceItemExpr <|> sliceItemProper
  where
    sliceItemExpr = annotated $ SliceItemExpr <$> expression
    sliceItemProper = annotated $ SliceItemProper <$> properSlice

sliceList :: DeltaParsing m => m (SliceList SrcInfo)
sliceList =
  annotated $
  SliceList <$>
  sliceItem <*>
  (Compose <$>
    many (Compose <$> liftA2 (,) (betweenWhitespace comma) sliceItem)) <*>
  optional (whitespaceBefore comma)
  
slicing :: DeltaParsing m => m (Slicing SrcInfo)
slicing =
  annotated $
  Slicing <$>
  whitespaceAfterF primary <*>
  between (char '[') (char ']') (betweenWhitespaceF sliceList)

expressionList :: DeltaParsing m => m (ExpressionList SrcInfo)
expressionList =
  annotated $
  ExpressionList <$>
  expression <*>
  (Compose <$>
    many
      (Compose <$>
        liftA2
          (,)
          (betweenWhitespace comma)
          expression)) <*>
  optional (whitespaceBefore comma)

subscription :: DeltaParsing m => m (Subscription SrcInfo)
subscription =
  annotated $
  Subscription <$>
  afterF (many whitespaceChar) primary <*>
  between'F (many whitespaceChar) expressionList

attRef :: DeltaParsing m => m (AttRef SrcInfo)
attRef =
  annotated $
  AttRef <$>
  afterF (many whitespaceChar) primary <*>
  beforeF (many whitespaceChar) identifier

identifier :: DeltaParsing m => m (Identifier SrcInfo)
identifier =
  annotated $ Identifier . T.pack <$> liftA2 (:) idStart (many idContinue)
  where
    idStart = try letter <|> char '_'
    idContinue = try idStart <|> digit

stringPrefix :: DeltaParsing m => m StringPrefix
stringPrefix =
  try (char 'r' $> StringPrefix_r) <|>
  try (char 'u' $> StringPrefix_u) <|>
  try (char 'R' $> StringPrefix_R) <|>
  (char 'u' $> StringPrefix_U)
  
shortString :: DeltaParsing m => m (ShortString SrcInfo)
shortString = try shortStringSingle <|> shortStringDouble
  where
    shortStringSingle =
      annotated $
      ShortStringSingle <$>
      between
        tripleSinglequote
        tripleSinglequote
        (many charOrEscapeSingle)
      
    shortStringDouble =
      annotated $
      ShortStringDouble <$>
      between
        tripleDoublequote
        tripleDoublequote
        (many charOrEscapeDouble)

    charOrEscapeSingle =
      try (Left <$> shortStringCharSingle) <|>
      (Right <$> stringEscape)

    charOrEscapeDouble =
      try (Left <$> shortStringCharDouble) <|>
      (Right <$> stringEscape)

    stringEscape = StringEscapeSeq <$> (char '\\' *> anyChar)

    shortStringCharSingle =
      (^?! _ShortStringCharSingle) <$>
      oneOfSet
        (CharSet.ascii \\ CharSet.singleton '\\' \\ CharSet.singleton '\'')

    shortStringCharDouble =
      (^?! _ShortStringCharDouble) <$>
      oneOfSet
        (CharSet.ascii \\ CharSet.singleton '\\' \\ CharSet.singleton '"')

longString :: DeltaParsing m => m (LongString SrcInfo)
longString = try longStringSingle <|> longStringDouble
  where
    longStringSingle =
      annotated $
      LongStringSingle <$>
      between tripleSinglequote tripleSinglequote (many charOrEscape)
      
    longStringDouble =
      annotated $
      LongStringDouble <$>
      between tripleDoublequote tripleDoublequote (many charOrEscape)

    charOrEscape =
      try (Left <$> longStringChar) <|> (Right <$> stringEscape)
      
    stringEscape = StringEscapeSeq <$> (char '\\' *> anyChar)
    longStringChar =
      (^?! _LongStringChar) <$> oneOfSet (CharSet.ascii CharSet.\\ CharSet.singleton '\\')
    
stringLiteral :: DeltaParsing m => m (StringLiteral SrcInfo)
stringLiteral =
  annotated $
  StringLiteral <$>
  beforeF
    (optional stringPrefix)
    (try (InL <$> shortString) <|> (InR <$> longString))

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
      between
        tripleSinglequote
        tripleSinglequote
        (many charOrEscapeSingle)
      
    shortBytesDouble =
      annotated $
      ShortBytesDouble <$>
      between
        tripleDoublequote
        tripleDoublequote
        (many charOrEscapeDouble)

    charOrEscapeSingle =
      try (Left <$> shortBytesCharSingle) <|>
      (Right <$> bytesEscape)

    charOrEscapeDouble =
      try (Left <$> shortBytesCharDouble) <|>
      (Right <$> bytesEscape)

    bytesEscape =
      char '\\' *>
      ((^?! _BytesEscapeSeq) <$> oneOfSet CharSet.ascii)

    shortBytesCharSingle =
      (^?! _ShortBytesCharSingle) <$>
      oneOfSet
        (CharSet.ascii \\ CharSet.singleton '\\' \\ CharSet.singleton '\'')

    shortBytesCharDouble =
      (^?! _ShortBytesCharDouble) <$>
      oneOfSet
        (CharSet.ascii \\ CharSet.singleton '\\' \\ CharSet.singleton '"')

tripleDoublequote :: DeltaParsing m => m ()
tripleDoublequote = string "\"\"\"" $> ()

tripleSinglequote :: DeltaParsing m => m ()
tripleSinglequote = string "'''" $> ()

longBytes :: DeltaParsing m => m (LongBytes SrcInfo)
longBytes = try longBytesSingle <|> longBytesDouble
  where
    longBytesSingle =
      annotated $
      LongBytesSingle <$>
      between tripleSinglequote tripleSinglequote (many charOrEscape)
      
    longBytesDouble =
      annotated $
      LongBytesDouble <$>
      between tripleDoublequote tripleDoublequote (many charOrEscape)

    charOrEscape =
      try (Left <$> longBytesChar) <|> (Right <$> bytesEscape)
      
    bytesEscape =
      char '\\' *>
      ((^?! _BytesEscapeSeq) <$> oneOfSet CharSet.ascii)
    longBytesChar =
      (^?! _LongBytesChar) <$> oneOfSet (CharSet.ascii CharSet.\\ CharSet.singleton '\\')

bytesLiteral :: DeltaParsing m => m (BytesLiteral SrcInfo)
bytesLiteral =
  annotated $
  BytesLiteral <$>
  bytesPrefix <*>
  (try (fmap InL shortBytes) <|> fmap InR longBytes)

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
  try integerDecimal <|>
  try integerOct <|>
  try integerHex <|>
  integerBin
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
float = try floatPoint <|> floatExponent
  where
    fraction = char '.' *> some1 digit'
    withDecimals = WithDecimalPlaces <$> optional (some1 digit') <*> fraction
    noDecimals = NoDecimalPlaces <$> (some1 digit' <* char '.')
    pointFloat = try withDecimals <|> noDecimals
    floatPoint =
      annotated $
      FloatPoint <$> pointFloat
      
    floatExponent =
      annotated $
      FloatExponent <$>
      (try (Left <$> some1 digit') <|> (Right <$> pointFloat)) <*>
      (Before <$> e <*> (Before <$> plusOrMinus <*> some1 digit'))

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
      (Compose <$> many (whitespaceBeforeF stringOrBytes))
    literalInteger = annotated $ LiteralInteger <$> integer
    literalFloat = annotated $ LiteralFloat <$> float
    literalImag = annotated $ LiteralImag <$> imag
    
leftParen :: DeltaParsing m => m LeftParen
leftParen = char '(' $> LeftParen

rightParen :: DeltaParsing m => m RightParen
rightParen = char ')' $> RightParen

afterF :: DeltaParsing m => m s -> m (f a) -> m (Compose (After s) f a)
afterF ms ma = fmap Compose $ flip After <$> ma <*> ms

beforeF :: DeltaParsing m => m s -> m (f a) -> m (Compose (Before s) f a)
beforeF ms ma = fmap Compose $ Before <$> ms <*> ma

between'F :: DeltaParsing m => m s -> m (f a) -> m (Compose (Between' s) f a)
between'F ms ma = fmap (Compose . Between') $ Between <$> ms <*> ma <*> ms

starredItem :: DeltaParsing m => m (StarredItem SrcInfo)
starredItem =
  try starredItemExpr <|>
  starredItemUnpack
  where
    starredItemExpr = annotated $ StarredItemExpr <$> expression
    starredItemUnpack =
      annotated $
      StarredItemUnpack <$> (char '*' *> whitespaceBeforeF orExpr)

comma :: DeltaParsing m => m Comma
comma = char ',' $> Comma

starredExpression :: DeltaParsing m => m (StarredExpression SrcInfo)
starredExpression =
  try starredExpressionExpr <|>
  starredExpressionTuple
  where
    starredExpressionExpr =
      annotated $ StarredExpressionExpr <$> expression
    starredExpressionTuple =
      annotated $
      StarredExpressionTuple <$>
      (Compose <$> many (afterF comma (whitespaceAfterF starredItem))) <*>
      (Compose <$> optional (whitespaceBeforeF starredItem))
    
enclosure :: DeltaParsing m => m (Enclosure SrcInfo)
enclosure =
  try enclosureParen <|>
  try enclosureList <|>
  try enclosureDict <|>
  try enclosureSet <|>
  try enclosureGenerator <|>
  enclosureYield
  where
    enclosureParen =
      annotated .
      fmap EnclosureParen $
      between
        (char '(')
        (char ')')
        (betweenWhitespaceF $ Compose <$> optional starredExpression)
    enclosureList =
      error "enclosureList not implemented" -- EnclosureList
    enclosureDict =
      error "enclosureDict not implemented" -- EnclosureDict
    enclosureSet =
      error "enclosureSet not implemented" -- EnclosureSet
    enclosureGenerator =
      error "enclosureGenerator not implemented" -- EnclosureGenerator
    enclosureYield =
      error "enclosureYield not implemented" -- EnclosureYield

atom :: DeltaParsing m => m (Atom SrcInfo)
atom =
  try (annotated $ AtomIdentifier <$> identifier) <|>
  try (annotated $ AtomLiteral <$> literal) <|>
  annotated (AtomEnclosure <$> enclosure)
  
primary :: DeltaParsing m => m (Primary SrcInfo)
primary =
  try primaryAtom <|>
  try primaryAttRef <|>
  try primarySubscription <|>
  try primarySlicing <|>
  primaryCall
  where
    primaryAtom = annotated $ PrimaryAtom <$> atom
    primaryAttRef = annotated $ PrimaryAttRef <$> attRef
    primarySubscription = annotated $ PrimarySubscription <$> subscription
    primarySlicing = annotated $ PrimarySlicing <$> slicing
    primaryCall = annotated $ PrimaryCall <$> call

awaitExpr :: DeltaParsing m => m (AwaitExpr SrcInfo)
awaitExpr = annotated $ Await <$> (string "await" *> whitespaceBeforeF primary)
  
power :: DeltaParsing m => m (Power SrcInfo)
power = annotated $ Power <$> powerLeft <*> powerRight
  where
    powerLeft = try (InL <$> awaitExpr) <|> (InR <$> primary)
    powerRight =
      Compose <$>
      optional
        (Compose <$>
          liftA2
            (,)
            (many whitespaceChar <* string "**")
            (whitespaceBeforeF uExpr))
  
uExpr :: DeltaParsing m => m (UExpr SrcInfo)
uExpr =
  try uExprNeg <|>
  try uExprPos <|>
  try uExprInv <|>
  uExprNone
  where
    uExprNeg =
      annotated $ UExprNeg <$> (char '-' *> whitespaceBeforeF uExpr)
    uExprPos =
      annotated $ UExprNeg <$> (char '+' *> whitespaceBeforeF uExpr)
    uExprInv =
      annotated $ UExprNeg <$> (char '~' *> whitespaceBeforeF uExpr)
    uExprNone = annotated $ UExprNone <$> power
  
mExpr :: DeltaParsing m => m (MExpr SrcInfo)
mExpr =
  try mExprNone <|>
  try mExprMult <|>
  try mExprAt <|>
  try mExprFloorDiv <|>
  try mExprDiv <|>
  mExprMod
  where
    mExprMult =
      annotated $
      MExprMult <$>
      (whitespaceAfterF mExpr <* char '*') <*>
      whitespaceBeforeF uExpr
    mExprAt =
      annotated $
      MExprAt <$>
      (whitespaceAfterF mExpr <* char '@') <*>
      whitespaceBeforeF mExpr
    mExprFloorDiv =
      annotated $
      MExprFloorDiv <$>
      (whitespaceAfterF mExpr <* string "//") <*>
      whitespaceBeforeF uExpr
    mExprDiv =
      annotated $
      MExprDiv <$>
      (whitespaceAfterF mExpr <* char '/') <*>
      whitespaceBeforeF uExpr
    mExprMod =
      annotated $
      MExprMod <$>
      (whitespaceAfterF mExpr <* char '%') <*>
      whitespaceBeforeF uExpr
    mExprNone = annotated $ MExprNone <$> uExpr
  
aExpr :: DeltaParsing m => m (AExpr SrcInfo)
aExpr =
  try aExprNone <|>
  try aExprAdd <|>
  aExprSubtract
  where
    aExprSubtract =
      annotated $
      AExprSubtract <$>
      (whitespaceAfterF aExpr <* char '-') <*>
      whitespaceBeforeF mExpr
    aExprAdd =
      annotated $
      AExprAdd <$>
      (whitespaceAfterF aExpr <* char '+') <*>
      whitespaceBeforeF mExpr
    aExprNone = annotated $ AExprNone <$> mExpr
  
shiftExpr :: DeltaParsing m => m (ShiftExpr SrcInfo)
shiftExpr = try shiftExprNone <|> try shiftExprLeft <|> shiftExprRight
  where
    shiftExprLeft =
      annotated $
      ShiftExprLeft <$>
      whitespaceAfterF shiftExpr <*>
      whitespaceBeforeF aExpr
    shiftExprRight =
      annotated $
      ShiftExprRight <$>
      whitespaceAfterF shiftExpr <*>
      whitespaceBeforeF aExpr
    shiftExprNone = annotated $ ShiftExprNone <$> aExpr
  
andExpr :: DeltaParsing m => m (AndExpr SrcInfo)
andExpr = try andExprNone <|> andExprSome
  where
    andExprNone = annotated $ AndExprNone <$> shiftExpr
    andExprSome =
      annotated $
      AndExprSome <$>
      (whitespaceAfterF andExpr <* char '&') <*>
      whitespaceBeforeF shiftExpr

xorExpr :: DeltaParsing m => m (XorExpr SrcInfo)
xorExpr = try xorExprNone <|> xorExprSome
  where
    xorExprNone = annotated $ XorExprNone <$> andExpr
    xorExprSome =
      annotated $
      XorExprSome <$>
      (whitespaceAfterF xorExpr <* char '^') <*>
      whitespaceBeforeF andExpr
  
orExpr :: DeltaParsing m => m (OrExpr SrcInfo)
orExpr = try orExprNone <|> orExprSome
  where
    orExprNone = annotated $ OrExprNone <$> xorExpr
    orExprSome =
      annotated $
      OrExprSome <$>
      (whitespaceAfterF orExpr <* char '|') <*>
      whitespaceBeforeF xorExpr

compOperator :: DeltaParsing m => m CompOperator
compOperator =
  try (char '<' $> CompLT) <|>
  try (char '>' $> CompGT) <|>
  try (string "==" $> CompEq) <|>
  try (string ">=" $> CompGEq) <|>
  try (string "!=" $> CompNEq) <|>
  try (string "<=" $> CompLEq) <|>
  try (string "is" $> CompIs) <|>
  try (string "is" *> (CompIsNot <$> some1 whitespaceChar) <* string "not") <|>
  try (string "in" $> CompIn) <|>
  (string "not" *> (CompNotIn <$> some1 whitespaceChar) <* string "in")

betweenWhitespace
  :: CharParsing m
  => m a
  -> m (Between' [WhitespaceChar] a)
betweenWhitespace m =
  fmap getConst . getCompose <$> betweenWhitespaceF (Const <$> m)
  
betweenWhitespace1
  :: CharParsing m
  => m a
  -> m (Between' (NonEmpty WhitespaceChar) a)
betweenWhitespace1 m =
  fmap getConst . getCompose <$> betweenWhitespace1F (Const <$> m)
  
comparison :: DeltaParsing m => m (Comparison SrcInfo)
comparison =
  annotated $
  Comparison <$>
  orExpr <*>
  (Compose <$> many opSection)
  where
    opSection =
      fmap Compose $
      (,) <$>
      betweenWhitespace compOperator <*>
      orExpr

notTest :: DeltaParsing m => m (NotTest SrcInfo)
notTest = try notTestNone <|> notTestSome
  where
    notTestSome =
      annotated $
      NotTestSome <$>
      (string "not" *> whitespaceBeforeF1 notTest)
    notTestNone = annotated $ NotTestNone <$> comparison

andTest :: DeltaParsing m => m (AndTest SrcInfo)
andTest = try andTestNone <|> andTestSome
  where
    andTestSome =
      annotated $
        AndTestSome <$>
        whitespaceAfterF1 andTest <*>
        whitespaceBeforeF1 notTest
        
    andTestNone = annotated $ AndTestNone <$> notTest

orTest :: DeltaParsing m => m (OrTest SrcInfo)
orTest = try orTestNone <|> orTestSome
  where
    orTestSome =
      annotated $
        OrTestSome <$>
        whitespaceAfterF1 orTest <*>
        (string "or" *> whitespaceBeforeF1 andTest)
      
    orTestNone = annotated (OrTestNone <$> andTest)

newlineChar :: CharParsing m => m NewlineChar
newlineChar =
  (char '\r' $> CR) <|>
  (char '\n' $> LF) <|>
  (string "\r\n" $> CRLF)

whitespaceChar :: CharParsing m => m WhitespaceChar
whitespaceChar =
  (char ' ' $> Space) <|>
  (char '\t' $> Tab) <|>
  (Continued <$> newlineChar)

whitespaceBeforeF
  :: CharParsing m
  => m (f a)
  -> m (Compose (Before [WhitespaceChar]) f a)
whitespaceBeforeF m =
  fmap Compose $
    Before <$>
    many whitespaceChar <*>
    m
    
whitespaceAfterF
  :: CharParsing m
  => m (f a)
  -> m (Compose (After [WhitespaceChar]) f a)
whitespaceAfterF m =
  fmap Compose $
    flip After <$>
    m <*>
    many whitespaceChar
    
whitespaceBefore
  :: CharParsing m
  => m a
  -> m (Before [WhitespaceChar] a)
whitespaceBefore m =
  fmap getConst . getCompose <$> whitespaceBeforeF (fmap Const m)
    
whitespaceAfter
  :: CharParsing m
  => m a
  -> m (After [WhitespaceChar] a)
whitespaceAfter m =
  fmap getConst . getCompose <$> whitespaceAfterF (fmap Const m)
    
whitespaceAfterF1
  :: CharParsing m
  => m (f a)
  -> m (Compose (After (NonEmpty WhitespaceChar)) f a)
whitespaceAfterF1 m =
  fmap Compose $
    flip After <$>
    m <*>
    some1 whitespaceChar
    
whitespaceBeforeF1
  :: CharParsing m
  => m (f a)
  -> m (Compose (Before (NonEmpty WhitespaceChar)) f a)
whitespaceBeforeF1 m =
  fmap Compose $
    Before <$>
    some1 whitespaceChar <*>
    m
    
betweenWhitespaceF
  :: CharParsing m
  => m (f a)
  -> m (Compose (Between' [WhitespaceChar]) f a)
betweenWhitespaceF m =
  fmap (Compose . Between') $
  Between <$>
  many whitespaceChar <*>
  m <*>
  many whitespaceChar

betweenWhitespace1F
  :: CharParsing m
  => m (f a)
  -> m (Compose (Between' (NonEmpty WhitespaceChar)) f a)
betweenWhitespace1F m =
  fmap (Compose . Between') $
  Between <$>
  some1 whitespaceChar <*>
  m <*>
  some1 whitespaceChar

ifThenElse :: DeltaParsing m => m (IfThenElse SrcInfo)
ifThenElse =
  IfThenElse <$>
  (string "if" *> betweenWhitespace1F orTest) <*>
  (string "else" *> whitespaceBeforeF1 expression)

expression :: DeltaParsing m => m (Expression SrcInfo)
expression = try expressionConditional <|> expressionLambda
  where
    expressionConditional :: DeltaParsing m => m (Expression SrcInfo)
    expressionConditional =
      annotated $
      ExpressionConditional <$>
      orTest <*>
      (Compose <$> optional (whitespaceBeforeF ifThenElse))

    expressionLambda :: DeltaParsing m => m (Expression SrcInfo)
    expressionLambda =
      error "expressionLambda not implemented" -- ExpressionLambda
