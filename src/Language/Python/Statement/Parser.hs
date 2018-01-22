{-# language FlexibleContexts #-}
module Language.Python.Statement.Parser where

import Papa hiding (Sum, Product)
import Control.Monad.State
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Separated.Between (Between'(..))
import Text.Parser.LookAhead
import Text.Trifecta hiding (Unspaced(..), comma, dot, colon, between)
import Language.Python.AST.Symbols
import Language.Python.Expr.Parser
import Language.Python.Parser.ArgumentList
import Language.Python.Parser.Combinators
import Language.Python.Parser.Comment
import Language.Python.Parser.DottedName
import Language.Python.Parser.Identifier
import Language.Python.Parser.Keywords
import Language.Python.Parser.SrcInfo
import Language.Python.Parser.Symbols
import Language.Python.Parser.TestlistStarExpr
import Language.Python.Statement.IR
import Language.Python.Statement.Parser.AugAssign
import Language.Python.Statement.Parser.Imports

import qualified Data.List.NonEmpty as NonEmpty

import Text.Parser.Unspaced

statement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (Statement SrcInfo)
statement =
  annotated $
  (StatementCompound <$> compoundStatement) <|>
  (StatementSimple <$> simpleStatement)

simpleStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (SimpleStatement SrcInfo)
simpleStatement =
  annotated $
  SimpleStatement <$>
  smallStatement <*>
  manyF
    (beforeF
       (try $
        betweenWhitespace semicolon <*
        notFollowedBy (void newlineChar <|> void (char '#')))
       smallStatement) <*>
  optional (try $ whitespaceBefore semicolon) <*>
  betweenF (many whitespaceChar) newlineChar (optionalF comment)

smallStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (SmallStatement SrcInfo)
smallStatement =
  annotated $
  smallStatementExpr <|>
  smallStatementDel <|>
  smallStatementPass <|>
  smallStatementFlow <|>
  smallStatementImport <|>
  smallStatementGlobal <|>
  smallStatementNonlocal <|>
  smallStatementAssert
  where
    augAssignSequence =
      beforeF
        (try $
         Between' <$>
         between
           (many whitespaceChar <* notFollowedBy (char '#'))
           (many whitespaceChar)
           augAssign)
        ((InL <$> yieldExpr whitespaceChar) <|>
         (InR <$> testList whitespaceChar))

    regularAssignSequence =
      manyF
        (beforeF
          (try $
           Between' <$>
           between
             (many whitespaceChar <* notFollowedBy (char '#'))
             (many whitespaceChar)
             equals)
          ((InL <$> yieldExpr whitespaceChar) <|>
           (InR <$> testlistStarExpr whitespaceChar test starExpr)))

    smallStatementExpr =
      SmallStatementExpr <$>
      testlistStarExpr whitespaceChar test starExpr <*>
      ((InL <$> augAssignSequence) <|>
       (InR <$> regularAssignSequence))

    smallStatementDel =
      SmallStatementDel <$>
      (kDel *>
       whitespaceBefore1F (exprList whitespaceChar))

    smallStatementPass =
      kPass $>
      SmallStatementPass

    smallStatementFlow =
      SmallStatementFlow <$>
      flowStatement

    smallStatementImport =
      SmallStatementImport <$>
      importStatement

    smallStatementGlobal =
      SmallStatementGlobal <$>
      (kGlobal *>
       whitespaceBefore1F identifier) <*>
      manyF (beforeF (betweenWhitespace comma) identifier)

    smallStatementNonlocal =
      SmallStatementNonlocal <$>
      (kNonlocal *>
       whitespaceBefore1F identifier) <*>
      manyF (beforeF (betweenWhitespace comma) identifier)

    smallStatementAssert =
      SmallStatementAssert <$>
      (kAssert *>
       whitespaceBefore1F (test whitespaceChar)) <*>
      optionalF (beforeF (betweenWhitespace comma) (test whitespaceChar))

flowStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (FlowStatement SrcInfo)
flowStatement =
  annotated $
  (kBreak $> FlowStatementBreak) <|>
  (kContinue $> FlowStatementContinue) <|>
  (FlowStatementReturn <$>
    (kReturn *>
    optionalF (try $ whitespaceBefore1F (testList whitespaceChar)))) <|>
  (FlowStatementRaise <$>
    (kRaise *>
    optionalF (try $ whitespaceBefore1F raiseStatement))) <|>
  (FlowStatementYield <$> yieldExpr whitespaceChar)

raiseStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (RaiseStatement SrcInfo)
raiseStatement =
  annotated $
  RaiseStatement <$>
  test whitespaceChar <*>
  optionalF (beforeF kFrom . try $ whitespaceBefore1F (test whitespaceChar))

compoundStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (CompoundStatement SrcInfo)
compoundStatement =
  annotated $
  (CompoundStatementIf <$> ifStatement) <|>
  (CompoundStatementWhile <$> whileStatement) <|>
  (CompoundStatementFor <$> forStatement) <|>
  (CompoundStatementTry <$> tryStatement) <|>
  (CompoundStatementWith <$> withStatement) <|>
  (CompoundStatementFuncDef <$> funcDef) <|>
  (CompoundStatementClassDef <$> classDef) <|>
  (CompoundStatementDecorated <$> decorated) <|>
  (CompoundStatementAsync <$> asyncStatement)

asyncStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (AsyncStatement SrcInfo)
asyncStatement =
  annotated $ do
  ws <- kAsync *> some1 whitespaceChar
  asyncStatementFuncDef ws <|> asyncStatementWith ws <|> asyncStatementFor ws
  where
    asyncStatementFuncDef ws = AsyncStatementFuncDef <$> beforeF (pure ws) funcDef
    asyncStatementFor ws = AsyncStatementFor <$> beforeF (pure ws) forStatement
    asyncStatementWith ws = AsyncStatementWith <$> beforeF (pure ws) withStatement

asyncFuncDef
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (AsyncFuncDef SrcInfo)
asyncFuncDef =
  annotated $
  AsyncFuncDef <$>
  (kAsync *>
   whitespaceBefore1F funcDef)

decorator
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (Decorator SrcInfo)
decorator =
  annotated $
  Decorator <$>
  (char '@' *>
   whitespaceBeforeF dottedName) <*>
  optionalF
    (parens .
     between'F (many anyWhitespaceChar) .
     optionalF $
     argumentList
       (test anyWhitespaceChar)
       (test anyWhitespaceChar)
       identifier
       test) <*>
  newlineChar

decorated
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (Decorated SrcInfo)
decorated =
  annotated $
  Decorated <$>
  (Compose <$> liftA2 (:|) decorator (many . try $ level *> decorator)) <*>
  (level *>
   ((InL . InL <$> classDef) <|>
    (InL . InR <$> funcDef) <|>
    (InR <$> asyncFuncDef)))

classDef
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (ClassDef SrcInfo)
classDef =
  annotated $
  ClassDef <$>
  (kClass *>
   whitespaceBefore1F identifier) <*>
  optionalF
    (try .
     whitespaceBeforeF .
     parens .
     between'F (many anyWhitespaceChar) .
     optionalF $
     argumentList
       (test anyWhitespaceChar)
       (test anyWhitespaceChar)
       identifier
       test) <*>
  beforeF (betweenWhitespace colon) suite

typedArg
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) ws
  -> Unspaced (StateT [NonEmpty IndentationChar] m) (TypedArg ws SrcInfo)
typedArg ws =
  annotated $
  TypedArg <$>
  identifier <*>
  optionalF (try $ beforeF (between' (many ws) colon) (test ws))

parameters
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (Parameters SrcInfo)
parameters =
  annotated $
  Parameters <$>
  parens
    (between'F
      (many anyWhitespaceChar)
      (optionalF $
       argumentList
         (typedArg anyWhitespaceChar)
         (typedArg anyWhitespaceChar)
         (typedArg anyWhitespaceChar)
         test))

funcDef
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (FuncDef SrcInfo)
funcDef =
  annotated $
  FuncDef <$>
  (kDef *> whitespaceBefore1F identifier) <*>
  whitespaceBeforeF parameters <*>
  optionalF (beforeF (try $ betweenWhitespace rightArrow) (test whitespaceChar)) <*>
  beforeF (betweenWhitespace colon) suite

withItem
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (WithItem SrcInfo)
withItem =
  annotated $
  WithItem <$>
  test whitespaceChar <*>
  optionalF (beforeF (try $ betweenWhitespace1 kAs) (expr whitespaceChar))

withStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (WithStatement SrcInfo)
withStatement =
  annotated $
  WithStatement <$>
  (kWith *>
   whitespaceBefore1F withItem) <*>
  manyF (beforeF (betweenWhitespace comma) withItem) <*>
  beforeF (betweenWhitespace colon) suite

exceptClause
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (ExceptClause SrcInfo)
exceptClause =
  annotated $
  ExceptClause <$>
  (try (level *> kExcept) *>
   optionalF
     (try . whitespaceBefore1F $
       Pair <$>
       test whitespaceChar <*>
       optionalF
         (beforeF (try $ betweenWhitespace1 kAs) identifier)))

tryStatement
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (TryStatement SrcInfo)
tryStatement =
  annotated $
  try tryStatementExcepts <|>
  tryStatementFinally
  where
    tryStatementExcepts =
      TryStatementExcepts <$>
      (kTry *>
       beforeF (betweenWhitespace colon) suite) <*>
      some1F
        (Pair <$>
         exceptClause <*>
         beforeF (betweenWhitespace colon) suite) <*>
      optionalF
        (try (level *> kElse) *> beforeF (betweenWhitespace colon) suite) <*>
      optionalF
        (try (level *> kFinally) *> beforeF (betweenWhitespace colon) suite)
    tryStatementFinally =
      TryStatementFinally <$>
      (kTry *>
       beforeF (betweenWhitespace colon) suite) <*>
       (try (level *> kFinally) *>
        beforeF (betweenWhitespace colon) suite)

whileStatement
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (WhileStatement SrcInfo)
whileStatement =
  annotated $
  WhileStatement <$>
  (kWhile *>
   whitespaceBefore1F (test whitespaceChar)) <*>
  beforeF (betweenWhitespace colon) suite <*>
  optionalF
    (try (level *> kElse) *>
     beforeF (betweenWhitespace colon) suite)

forStatement
  :: ( DeltaParsing m
     , LookAheadParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (ForStatement SrcInfo)
forStatement =
  annotated $
  ForStatement <$>
  (kFor *>
   betweenWhitespace1F (testlistStarExpr whitespaceChar expr starExpr)) <*>
  (kIn *>
   whitespaceBefore1F (testList whitespaceChar)) <*>
  beforeF (betweenWhitespace colon) suite <*>
  optionalF
    (try (level *> kElse) *>
     beforeF (betweenWhitespace colon) suite)

ifStatement
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (IfStatement SrcInfo)
ifStatement =
  annotated $
  IfStatement <$>
  (kIf *>
   whitespaceBefore1F (test whitespaceChar)) <*>
  beforeF (betweenWhitespace colon) suite <*>
  manyF
    (try (level *> kElif) *>
     (Pair <$>
      whitespaceBefore1F (test whitespaceChar) <*>
      beforeF (betweenWhitespace colon) suite)) <*>
  optionalF
    (try (level *> kElse) *>
     beforeF (betweenWhitespace colon) suite)

indentations
  :: (Traversable t, CharParsing m)
  => t (NonEmpty IndentationChar)
  -> m ()
indentations is = traverseOf_ (traverse.traverse) indentParser is
  where
    indentParser IndentSpace = void $ char ' '
    indentParser IndentTab = void $ char '\t'
    indentParser (IndentContinued nl is) =
      void $
      string (case nl of; CR -> "\r"; LF -> "\n"; CRLF -> "\r\n") *>
      traverse indentParser is

level
  :: DeltaParsing m
  => Unspaced (StateT [NonEmpty IndentationChar] m) [IndentationChar]
level = (do
  is <- lift get
  indentations is
  pure . maybe [] NonEmpty.toList $ last is) <?> "level indentation (possibly none)"

level1
  :: DeltaParsing m
  => Unspaced (StateT [NonEmpty IndentationChar] m) (NonEmpty IndentationChar)
level1 = (do
  is <- lift get
  indentations is
  maybe empty pure $ last is) <?> "level indentation"

indent
  :: (DeltaParsing m, LookAheadParsing m)
  => Unspaced (StateT [NonEmpty IndentationChar] m) ()
indent = (do
  is' <- lookAhead $ level *> some1 indentationChar
  lift $ modify (++ [is'])) <?> "indent"

dedent :: (MonadTrans t, MonadState [a] m) => t m ()
dedent = lift $ modify (fromMaybe [] . init)

suite
  :: ( LookAheadParsing m
     , DeltaParsing m
     )
  => Unspaced (StateT [NonEmpty IndentationChar] m) (Suite SrcInfo)
suite = annotated $ suiteSingle <|> suiteMulti
  where
    suiteSingle =
      SuiteSingle <$>
      simpleStatement

    suiteMulti =
      SuiteMulti <$>
      whitespaceBeforeF (optionalF comment) <*>
      newlineChar <*>
      manyF lineComment <*>
      (indent *> suiteStatements <* dedent)

    suiteStatements =
      some1F $
        (InL <$> lineComment) <|>
        (InR <$> beforeF (try level1) statement)
    lineComment =
      betweenF
        (try $ many whitespaceChar <* lookAhead (void newlineChar <|> void (char '#')))
        newlineChar
        (optionalF comment)
