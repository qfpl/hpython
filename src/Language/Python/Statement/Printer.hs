{-# language GADTs #-}
module Language.Python.Statement.Printer where

import Papa
import Data.Functor.Compose
import Data.Separated.Before
import Text.PrettyPrint hiding ((<>), equals, comma, colon)
import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Expr.Printer
import Language.Python.Printer.ArgsList
import Language.Python.Printer.ArgumentList
import Language.Python.Printer.Combinators hiding (before)
import Language.Python.Printer.DottedName
import Language.Python.Printer.Identifier
import Language.Python.Printer.IndentedLines
import Language.Python.Printer.Keywords
import Language.Python.Printer.Symbols
import Language.Python.Printer.TestlistStarExpr
import Language.Python.Statement.AST
import Language.Python.Statement.Printer.AugAssign
import Language.Python.Statement.Printer.Imports

statement :: Ord a => Statement lctxt ectxt a -> [Doc]
statement s =
  case s of
    StatementSimple v _ -> [ simpleStatement v ]
    StatementCompound v _ -> compoundStatement v

simpleStatement :: Ord a => SimpleStatement lctxt ectxt a -> Doc
simpleStatement (SimpleStatement h t s n _) =
  smallStatement h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' semicolon) smallStatement)
    t <>
  foldMap (whitespaceBefore semicolon) s <>
  whitespaceBefore newlineChar n

smallStatement :: Ord a => SmallStatement lctxt ectxt a -> Doc
smallStatement s =
  case s of
    SmallStatementExpr v _ -> test v
    SmallStatementAssign l m r _ ->
      testlistStarExpr test starExpr l <>
      foldMapOf
        (_Wrapped.folded)
        (beforeF
          (betweenWhitespace' equals)
          (testlistStarExpr test starExpr))
        m <>
      beforeF
        (betweenWhitespace' equals)
        (sumElim yieldExpr $ testlistStarExpr test starExpr)
        r
    SmallStatementAugAssign l r _ ->
      test l <>
      beforeF (betweenWhitespace' augAssign) (sumElim yieldExpr testList) r
    SmallStatementDel v _ ->
      text "del" <>
      whitespaceBeforeF exprList v
    SmallStatementPass _ -> text "pass"
    SmallStatementFlow v _ -> flowStatement v
    SmallStatementImport v _ -> importStatement v
    SmallStatementGlobal h t _ ->
      text "global" <>
      whitespaceBeforeF identifier h <>
      foldMapOf
        (_Wrapped.folded)
        (beforeF (betweenWhitespace' comma) identifier)
        t
    SmallStatementNonlocal h t _ ->
      text "nonlocal" <>
      whitespaceBeforeF identifier h <>
      foldMapOf
        (_Wrapped.folded)
        (beforeF (betweenWhitespace' comma) identifier)
        t
    SmallStatementAssert h t _ ->
      text "assert" <>
      whitespaceBeforeF test h <>
      foldMapOf
        (_Wrapped.folded)
        (beforeF (betweenWhitespace' comma) test)
        t

flowStatement :: Ord a => FlowStatement lctxt ectxt a -> Doc
flowStatement s =
  case s of
    FlowStatementBreak _ -> text "break"
    FlowStatementContinue _ -> text "continue"
    FlowStatementReturn v _ ->
      text "return" <>
      foldMapOf
        (_Wrapped.folded)
        (whitespaceBeforeF testList)
        v
    FlowStatementRaise v _ ->
      text "raise" <>
      foldMapOf
        (_Wrapped.folded)
        (whitespaceBeforeF raiseStatement)
        v
    FlowStatementYield v _ -> yieldExpr v

raiseStatement :: Ord a => RaiseStatement ectxt a -> Doc
raiseStatement (RaiseStatement l r _) =
  test l <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF kFrom $ whitespaceBeforeF test)
    r

compoundStatement :: Ord a => CompoundStatement lctxt ectxt a -> [Doc]
compoundStatement s =
  case s of
    CompoundStatementIf v _ -> ifStatement v
    CompoundStatementWhile v _ -> whileStatement v
    CompoundStatementFor v _ -> forStatement v
    CompoundStatementTry v _ -> tryStatement v
    CompoundStatementWith v _ -> withStatement v
    CompoundStatementFuncDef v _ -> funcDef v
    CompoundStatementClassDef v _ -> classDef v
    CompoundStatementDecorated v _ -> decorated v
    CompoundStatementAsync v _ -> asyncStatement v

ifStatement :: Ord a => IfStatement lctxt ectxt a -> [Doc]
ifStatement (IfStatement a b elif el _) =
  suite
    (text "if" <>
     whitespaceBeforeF test a <>
     betweenWhitespace' colon (b ^. _Wrapped.before._1))
    (b ^. _Wrapped.before._2) <>
  foldMap
    (\a ->
       suite
         (text "elif" <>
          whitespaceBeforeF test (a ^. _1) <>
          betweenWhitespace' colon (a ^. _2._Wrapped.before._1))
         (a ^. _2._Wrapped.before._2))
    (getCompose elif) <>
  foldMap
    (\a ->
       suite
         (text "else" <> betweenWhitespace' colon (a ^. _Wrapped.before._1))
         (a ^. _Wrapped.before._2))
    (getCompose el)

whileStatement :: Ord a => WhileStatement lctxt ectxt a -> [Doc]
whileStatement (WhileStatement c b e _) =
  suite
    (text "while" <>
     whitespaceBeforeF test c <>
     betweenWhitespace' colon (b ^. _Wrapped.before._1))
    (b ^. _Wrapped.before._2) <>
  foldMapOf
    (_Wrapped.folded)
    (\a ->
       suite
         (text "else" <> betweenWhitespace' colon (a ^. _Wrapped.before._1))
         (a ^. _Wrapped.before._2))
    e

forStatement :: Ord a => ForStatement lctxt ectxt a -> [Doc]
forStatement (ForStatement f i b e _) =
  suite
    (text "for" <>
     betweenWhitespace'F (testlistStarExpr expr starExpr) f <>
     text "in" <>
     whitespaceBeforeF testList i <>
     betweenWhitespace' colon (b ^. _Wrapped.before._1))
    (b ^. _Wrapped.before._2) <>
  foldMapOf
    (_Wrapped.folded)
    (\a ->
       suite
         (text "else" <> betweenWhitespace' colon (a ^. _Wrapped.before._1))
         (a ^. _Wrapped.before._2))
    e

tryStatement :: Ord a => TryStatement lctxt ectxt a -> [Doc]
tryStatement s =
  case s of
    TryStatementExcepts t ex el f _ ->
      suite
        (text "try" <> betweenWhitespace' colon (t ^. _Wrapped.before._1))
        (t ^. _Wrapped.before._2) <>
      foldMapOf
        (_Wrapped.folded)
        (\a ->
           suite
             (exceptClause (a ^. _1) <> betweenWhitespace' colon (a ^. _2._Wrapped.before._1))
             (a ^. _2._Wrapped.before._2))
        ex <>
      foldMapOf
        (_Wrapped.folded)
        (\a ->
           suite
             (text "else" <> betweenWhitespace' colon (a ^. _Wrapped.before._1))
             (a ^. _Wrapped.before._2))
        el <>
      foldMapOf
        (_Wrapped.folded)
        (\a ->
           suite
             (text "finally" <> betweenWhitespace' colon (a ^. _Wrapped.before._1))
             (a ^. _Wrapped.before._2))
        f
    TryStatementFinally t f _ ->
      suite
        (text "try" <> betweenWhitespace' colon (t ^. _Wrapped.before._1))
        (t ^. _Wrapped.before._2) <>
      suite
        (text "finally" <> betweenWhitespace' colon (f ^. _Wrapped.before._1))
        (f ^. _Wrapped.before._2)

exceptClause :: Ord a => ExceptClause ctxt a -> Doc
exceptClause (ExceptClause v _) =
  text "except" <>
  foldMapOf
    (_Wrapped.folded)
    (whitespaceBeforeF . prodElim test $
     foldMapOf (_Wrapped.folded) $
     beforeF (betweenWhitespace' kAs) identifier)
    v

withStatement :: Ord a => WithStatement lctxt ectxt a -> [Doc]
withStatement (WithStatement h t s _) =
  suite
    (text "with" <>
     whitespaceBeforeF withItem h <>
     foldMapOf
       (_Wrapped.folded)
       (beforeF (betweenWhitespace' comma) withItem)
       t <>
     betweenWhitespace' colon (s ^. _Wrapped.before._1))
    (s ^. _Wrapped.before._2)

withItem :: Ord a => WithItem ctxt a -> Doc
withItem (WithItem l r _) =
  test l <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' kAs) expr)
    r

asyncStatement :: Ord a => AsyncStatement lctxt ectxt a -> [Doc]
asyncStatement (AsyncStatement v _) =
  case sumElim (sumElim funcDef withStatement) forStatement (v ^. _Wrapped.before._2) of
    (x:xs) -> (text "async" <> foldMap whitespaceChar (v ^. _Wrapped.before._1) <> x) : xs

funcDef :: Ord a => FuncDef outer inner a -> [Doc]
funcDef (FuncDef n p t b _) =
  suite
    (text "def" <>
     whitespaceBeforeF identifier n <>
     whitespaceBeforeF parameters p <>
     foldMapOf
       (_Wrapped.folded)
       (beforeF (betweenWhitespace' rightArrow) test)
       t <>
     betweenWhitespace' colon (b ^. _Wrapped.before._1))
    (b ^. _Wrapped.before._2)

parameters :: Ord a => Parameters ctxt a -> Doc
parameters (Parameters v _) =
  parens $
  betweenWhitespace'F (foldMapOf (_Wrapped.folded) (argsList typedArg test)) v

typedArg :: Ord a => TypedArg a -> Doc
typedArg (TypedArg v t _) =
  identifier v <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' colon) test)
    t

classDef :: Ord a => ClassDef ctxt a -> [Doc]
classDef (ClassDef n a b _) =
  suite
    (text "class" <>
     whitespaceBeforeF identifier n <>
     foldMapOf
       (_Wrapped.folded)
       (parens .
       whitespaceBeforeF
         (betweenWhitespace'F $
          foldMapOf (_Wrapped.folded) (argumentList identifier test)))
       a <>
     betweenWhitespace' colon (b ^. _Wrapped.before._1))
  (b ^. _Wrapped.before._2)

decorated :: Ord a => Decorated ctxt a -> [Doc]
decorated (Decorated ds b _) =
  fmap decorator (NonEmpty.toList $ getCompose ds) <>
  sumElim (sumElim classDef funcDef) asyncFuncDef b

decorator :: Ord a => Decorator ctxt a -> Doc
decorator (Decorator name args n _) =
  text "@" <>
  whitespaceBeforeF dottedName name <>
  foldMapOf
    (_Wrapped.folded)
    (parens .
     betweenWhitespace'F
       (foldMapOf (_Wrapped.folded) (argumentList identifier test)))
    args <>
  newlineChar n

asyncFuncDef :: Ord a => AsyncFuncDef ctxt a -> [Doc]
asyncFuncDef (AsyncFuncDef v _) =
  case funcDef (v ^. _Wrapped.before._2) of
    (x:xs) -> (text "async" <> foldMap whitespaceChar (v ^. _Wrapped.before._1) <> x) : xs

suite :: Ord a => Doc -> Suite lctxt ctxt a -> [Doc]
suite preceding s =
  case s of
    SuiteSingle v _ -> [preceding <> simpleStatement v]
    SuiteMulti n sts _ ->
      (preceding <> newlineChar n) :
      indentedLines
        (pure . whitespaceBeforeF (afterF newlineChar comment))
        statement
        sts
