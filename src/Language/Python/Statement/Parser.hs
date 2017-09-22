module Language.Python.Statement.Parser where

import Papa hiding (Sum, Product)
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Text.Parser.LookAhead
import Data.Separated.Before (Before(..))
import Text.Trifecta hiding (Unspaced(..), comma, dot, colon)
import Language.Python.AST.Symbols
import Language.Python.Expr.Parser
import Language.Python.Parser.ArgsList
import Language.Python.Parser.Combinators
import Language.Python.Parser.Identifier
import Language.Python.Parser.Keywords
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols
import Language.Python.Statement.IR

import Text.Parser.Unspaced

statement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (Statement SrcInfo)
statement =
  annotated $
  (StatementSimple <$> try simpleStatement) <|>
  (StatementCompound <$> compoundStatement)

simpleStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (SimpleStatement SrcInfo)
simpleStatement =
  annotated $
  SimpleStatement <$>
  smallStatement <*>
  manyF (try $ beforeF (betweenWhitespace semicolon) smallStatement) <*>
  optional (try $ whitespaceBefore semicolon) <*>
  whitespaceBefore newlineChar

testlistStarExpr
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (TestlistStarExpr SrcInfo)
testlistStarExpr =
  annotated $
  TestlistStarExpr <$>
  testOrStar <*>
  manyF (try $ beforeF (betweenWhitespace comma) testOrStar) <*>
  optional (try $ betweenWhitespace comma)
  where
    testOrStar = (InL <$> try test) <|> (InR <$> starExpr)

augAssign :: DeltaParsing m => Unspaced m AugAssign
augAssign =
  (string "+=" $> PlusEquals) <|>
  (string "-=" $> MinusEquals) <|>
  (try (string "*=") $> StarEquals) <|>
  (string "@=" $> AtEquals) <|>
  (try (string "/=") $> SlashEquals) <|>
  (string "&=" $> AmphersandEquals) <|>
  (string "|=" $> PipeEquals) <|>
  (string "^=" $> CaretEquals) <|>
  (string "<<=" $> ShiftLeftEquals) <|>
  (string ">>=" $> ShiftRightEquals) <|>
  (string "**=" $> DoubleStarEquals) <|>
  (string "//=" $> DoubleSlashEquals)

smallStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (SmallStatement SrcInfo)
smallStatement =
  annotated $
  try smallStatementExpr <|>
  try smallStatementDel <|>
  try smallStatementPass <|>
  try smallStatementFlow <|>
  try smallStatementImport <|>
  try smallStatementGlobal <|>
  try smallStatementNonlocal <|>
  smallStatementAssert
  where
    augAssignSequence =
      beforeF
        (betweenWhitespace augAssign)
        ((InL <$> try yieldExpr) <|>
         (InR <$> testList))

    regularAssignSequence =
      manyF
        (beforeF
          (betweenWhitespace equals)
          ((InL <$> try yieldExpr) <|> (InR <$> testlistStarExpr)))

    smallStatementExpr =
      SmallStatementExpr <$>
      testlistStarExpr <*>
      ((InL <$> try augAssignSequence) <|>
       (InR <$> regularAssignSequence))

    smallStatementDel =
      SmallStatementDel <$>
      (string "del" *>
       whitespaceBefore1F exprList)

    smallStatementPass =
      string "pass" $>
      SmallStatementPass

    smallStatementFlow =
      SmallStatementFlow <$>
      flowStatement

    smallStatementImport =
      SmallStatementImport <$>
      importStatement

    smallStatementGlobal =
      SmallStatementGlobal <$>
      (string "global" *>
       identifier) <*>
      manyF (beforeF (betweenWhitespace comma) identifier)

    smallStatementNonlocal =
      SmallStatementNonlocal <$>
      identifier <*>
      manyF (beforeF (betweenWhitespace comma) identifier)

    smallStatementAssert =
      SmallStatementAssert <$>
      test <*>
      manyF (beforeF (betweenWhitespace comma) test)

importStatement :: DeltaParsing m => Unspaced m (ImportStatement SrcInfo)
importStatement =
  annotated $
  try (ImportStatementName <$> importName) <|>
  (ImportStatementFrom <$> importFrom)

dottedAsNames :: DeltaParsing m => Unspaced m (DottedAsNames SrcInfo)
dottedAsNames =
  annotated $
  DottedAsNames <$>
  dottedAsName <*>
  manyF (beforeF (betweenWhitespace comma) dottedAsName)

dottedAsName :: DeltaParsing m => Unspaced m (DottedAsName SrcInfo)
dottedAsName =
  annotated $
  DottedAsName <$>
  dottedName <*>
  optionalF (try $ beforeF (betweenWhitespace1 kAs) identifier)

dottedName :: DeltaParsing m => Unspaced m (DottedName SrcInfo)
dottedName =
  annotated $
  DottedName <$>
  identifier <*>
  manyF (beforeF (betweenWhitespace dot) identifier)

importName :: DeltaParsing m => Unspaced m (ImportName SrcInfo)
importName =
  annotated $
  ImportName <$>
  whitespaceBefore1F dottedAsNames

importFrom :: DeltaParsing m => Unspaced m (ImportFrom SrcInfo)
importFrom =
  annotated $
  ImportFrom <$>
  (string "from" *> fromPart) <*>
  (string "import" *> importPart)
  where
    dotOrEllipsis = (Left <$> try dot) <|> (Right <$> ellipsis)
    fromPart =
      (InL <$>
        try (beforeF (many $ betweenWhitespace dotOrEllipsis) dottedName)) <|>
      (InR . Const <$>
        some1 dotOrEllipsis)

    importPart =
      whitespaceBefore1F $
      try (InL . InL . Const <$> asterisk) <|>
      try
        (InL . InR <$>
          betweenF leftParen rightParen (betweenWhitespaceF importAsNames)) <|>
       (InR <$> importAsNames)

importAsNames :: DeltaParsing m => Unspaced m (ImportAsNames SrcInfo)
importAsNames =
  annotated $
  ImportAsNames <$>
  importAsName <*>
  manyF (beforeF (betweenWhitespace comma) importAsName) <*>
  optional (try $ betweenWhitespace comma)

importAsName :: DeltaParsing m => Unspaced m (ImportAsName SrcInfo)
importAsName =
  annotated $
  ImportAsName <$>
  identifier <*>
  optionalF (try $ beforeF (betweenWhitespace1 kAs) identifier)

flowStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (FlowStatement SrcInfo)
flowStatement =
  annotated $
  try (symbol "break" $> FlowStatementBreak) <|>
  try (symbol "continue" $> FlowStatementContinue) <|>
  try
    (FlowStatementReturn <$> optionalF (try $ whitespaceBefore1F testList)) <|>
  try
    (FlowStatementRaise <$> optionalF (try $ whitespaceBefore1F raiseStatement)) <|>
  (FlowStatementYield <$> yieldExpr)

raiseStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (RaiseStatement SrcInfo)
raiseStatement =
  annotated $
  RaiseStatement <$>
  test <*>
  optionalF (try $ whitespaceBefore1F test)

compoundStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (CompoundStatement SrcInfo)
compoundStatement =
  annotated $
  try (CompoundStatementIf <$> ifStatement) <|>
  try (CompoundStatementWhile <$> whileStatement) <|>
  try (CompoundStatementFor <$> forStatement) <|>
  try (CompoundStatementTry <$> tryStatement) <|>
  try (CompoundStatementWith <$> withStatement) <|>
  try (CompoundStatementFuncDef <$> funcDef) <|>
  try (CompoundStatementClassDef <$> classDef) <|>
  try (CompoundStatementDecorated <$> decorated) <|>
  (CompoundStatementAsync <$> asyncStatement)
  
asyncStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (AsyncStatement SrcInfo)
asyncStatement =
  annotated $
  AsyncStatement <$>
  (string "async" *>
   whitespaceBefore1F
     ((InL . InL <$> try funcDef) <|>
      (InL . InR <$> try whileStatement) <|>
      (InR <$> forStatement)))

asyncFuncDef
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (AsyncFuncDef SrcInfo)
asyncFuncDef =
  annotated $
  AsyncFuncDef <$>
  (string "async" *>
   whitespaceBefore1F funcDef)

decorator
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (Decorator SrcInfo)
decorator =
  annotated $
  Decorator <$>
  (char '@' *>
   whitespaceBeforeF dottedName) <*>
  optionalF (try . betweenWhitespaceF . optionalF $ try argList) <*>
  newlineChar

decorated
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (Decorated SrcInfo)
decorated =
  annotated $
  Decorated <$>
  some1F decorator <*>
  ((InL . InL <$> try classDef) <|>
   (InL . InR <$> try funcDef) <|>
   (InR <$> asyncFuncDef))

classDef
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (ClassDef SrcInfo)
classDef =
  annotated $
  ClassDef <$>
  (string "class" *>
   whitespaceBefore1F identifier) <*>
  optionalF
    (try .
     whitespaceBeforeF .
     parens .
     betweenWhitespaceF .
     optionalF $
     try argList) <*>
  beforeF (betweenWhitespace colon) suite

typedArg
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (TypedArg SrcInfo)
typedArg =
  annotated $
  TypedArg <$>
  identifier <*>
  optionalF (try $ beforeF (betweenWhitespace colon) test)

parameters
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (Parameters SrcInfo)
parameters =
  annotated $
  Parameters <$>
  betweenWhitespaceF (optionalF . try $ argsList test typedArg)

funcDef
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (FuncDef SrcInfo)
funcDef =
  annotated $
  FuncDef <$>
  (string "def" *>
   whitespaceBefore1F identifier) <*>
  whitespaceBeforeF parameters <*>
  optionalF (try $ beforeF (betweenWhitespace rightArrow) test) <*>
  beforeF (betweenWhitespace colon) suite

withItem
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (WithItem SrcInfo)
withItem =
  annotated $
  WithItem <$>
  test <*>
  optionalF (try $ beforeF (betweenWhitespace1 kAs) identifier)

withStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (WithStatement SrcInfo)
withStatement =
  annotated $
  WithStatement <$>
  (string "with" *>
   whitespaceBefore1F withItem) <*>
  manyF (beforeF (betweenWhitespace comma) withItem) <*>
  beforeF (betweenWhitespace colon) suite

exceptClause
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (ExceptClause SrcInfo)
exceptClause =
  annotated $
  ExceptClause <$>
  optionalF
    (try $
      Pair <$>
      test <*>
      optionalF
        (try $ beforeF (betweenWhitespace1 kAs) identifier))

tryStatement
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => Unspaced m (TryStatement SrcInfo)
tryStatement =
  annotated $
  try tryStatementExcepts <|>
  tryStatementFinally
  where
    tryStatementExcepts =
      TryStatementExcepts <$>
      (string "try" *>
       beforeF (betweenWhitespace colon) suite) <*>
      some1F
        (Pair <$>
         exceptClause <*>
         beforeF (betweenWhitespace colon) suite) <*>
      optionalF
        (try $ string "else" *> beforeF (betweenWhitespace colon) suite) <*>
      optionalF
        (try $ string "finally" *> beforeF (betweenWhitespace colon) suite)
    tryStatementFinally =
      TryStatementFinally <$>
      (string "try" *>
       beforeF (betweenWhitespace colon) suite) <*>
       (string "finally" *> beforeF (betweenWhitespace colon) suite)

whileStatement
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => Unspaced m (WhileStatement SrcInfo)
whileStatement =
  annotated $
  WhileStatement <$>
  (string "while" *>
   whitespaceBefore1F test) <*>
  beforeF (betweenWhitespace colon) suite <*>
  optionalF (try $ string "else" *> beforeF (betweenWhitespace colon) suite)

forStatement
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => Unspaced m (ForStatement SrcInfo)
forStatement =
  annotated $
  ForStatement <$>
  (string "for" *>
   betweenWhitespace1F exprList) <*>
  (string "in" *>
   whitespaceBefore1F testList) <*>
  beforeF (betweenWhitespace colon) suite <*>
  optionalF (try $ string "else" *> beforeF (betweenWhitespace colon) suite)

ifStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (IfStatement SrcInfo)
ifStatement =
  annotated $
  IfStatement <$>
  (string "if" *>
   whitespaceBefore1F test) <*>
  beforeF (betweenWhitespace colon) suite <*>
  manyF
    (string "elif" *>
     (Pair <$>
      whitespaceBefore1F test <*>
      beforeF (betweenWhitespace colon) suite)) <*>
  optionalF (try $ string "else" *> beforeF (betweenWhitespace colon) suite)

suite
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced m (Suite SrcInfo)
suite = annotated $ try suiteSingle <|> suiteMulti
  where
    suiteSingle =
      SuiteSingle <$>
      simpleStatement

    suiteMulti =
      SuiteMulti <$>
      newlineChar <*>
      suiteStatements

    someIndentation =
      (formFeed *> some1 indentationChar) <?> "some indentation"

    sameIndent IndentSpace = char ' ' $> IndentSpace
    sameIndent IndentTab = char '\t' $> IndentTab

    suiteStatements = do
      indent <- someIndentation
      st <- statement
      Compose . (Compose (Before indent st) :|) <$>
        many (beforeF (traverse sameIndent indent) statement)
