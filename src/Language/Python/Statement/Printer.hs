{-# language GADTs #-}
module Language.Python.Statement.Printer where

import Papa
import Data.Functor.Compose
import Data.Functor.Sum
import Text.PrettyPrint hiding ((<>), semicolon, equals, comma, colon)

import Language.Python.Expr.Printer
import Language.Python.Printer.ArgsList
import Language.Python.Printer.Combinators
import Language.Python.Printer.DottedName
import Language.Python.Printer.Identifier
import Language.Python.Printer.IndentedLines
import Language.Python.Printer.Keywords
import Language.Python.Printer.Symbols
import Language.Python.Statement.AST
import Language.Python.Statement.AST.TestlistStarExpr
import Language.Python.Statement.Printer.AugAssign
import Language.Python.Statement.Printer.Imports

statement :: Statement lctxt ectxt a -> Doc
statement s =
  case s of
    StatementSimple v _ -> simpleStatement v
    StatementCompound v _ -> compoundStatement v

simpleStatement :: SimpleStatement lctxt ectxt a -> Doc
simpleStatement (SimpleStatement h t s n _) =
  smallStatement h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' semicolon) smallStatement)
    t <>
  foldMap (whitespaceBefore semicolon) s <>
  whitespaceBefore newlineChar n

testlistStarExpr :: TestlistStarExpr assignable ectxt a -> Doc
testlistStarExpr s =
  case s of
    TestlistStarExprSingle v _ -> test v
    TestlistStarExprSingleComma v c _ ->
      sumElim test starExpr v <>
      betweenWhitespace' comma c
    _
      | Just (h, t, c, _) <- Just s ^? _TestlistStarExprMany ->
          sumElim test starExpr h <>
          foldMapOf
            (_Wrapped.folded)
            (beforeF (betweenWhitespace' comma) (sumElim test starExpr))
            t <>
          foldMap (betweenWhitespace' comma) c

smallStatement :: SmallStatement lctxt ectxt a -> Doc
smallStatement s =
  case s of
    SmallStatementExpr v _ -> test v
    SmallStatementAssign l r _ ->
      testlistStarExpr l <>
      foldMapOf
        (_Wrapped.folded)
        (beforeF
          (betweenWhitespace' equals)
          (sumElim yieldExpr testlistStarExpr))
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

flowStatement :: FlowStatement lctxt ectxt a -> Doc
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

raiseStatement :: RaiseStatement ectxt a -> Doc
raiseStatement (RaiseStatement l r _) =
  test l <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF kFrom $ whitespaceBeforeF test)
    r

compoundStatement :: CompoundStatement lctxt ectxt a -> Doc
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

ifStatement :: IfStatement lctxt ectxt a -> Doc
ifStatement (IfStatement c t elif el _) =
  text "if" <>
  whitespaceBeforeF test c <>
  text "then" <>
  beforeF (betweenWhitespace' colon) suite t <>
  foldMapOf
    (_Wrapped.folded)
    ((text "elif" <>) .
     prodElim
       (whitespaceBeforeF test)
       (beforeF (betweenWhitespace' colon) suite))
    elif <>
  foldMapOf
    (_Wrapped.folded)
    ((text "else" <>) .
     beforeF (betweenWhitespace' colon) suite)
    el

whileStatement :: WhileStatement lctxt ectxt a -> Doc
whileStatement (WhileStatement c b e _) =
  text "while" <>
  whitespaceBeforeF test c <>
  beforeF (betweenWhitespace' colon) suite b <>
  foldMapOf
    (_Wrapped.folded)
    ((text "else" <>) .
     beforeF (betweenWhitespace' colon) suite)
    e

forStatement :: ForStatement lctxt ectxt a -> Doc
forStatement (ForStatement f i b e _) =
  text "for" <>
  betweenWhitespace'F exprList f <>
  text "in" <>
  whitespaceBeforeF testList i <>
  beforeF (betweenWhitespace' colon) suite b <>
  foldMapOf
    (_Wrapped.folded)
    ((text "else" <>) . beforeF (betweenWhitespace' colon) suite)
    e

tryStatement :: TryStatement lctxt ectxt a -> Doc
tryStatement s =
  case s of
    TryStatementExcepts t ex el f _ ->
      text "try" <>
      foldMapOf
        (_Wrapped.folded)
        (prodElim exceptClause $ beforeF (betweenWhitespace' colon) suite)
        ex <>
      foldMapOf
        (_Wrapped.folded)
        ((text "else" <>) .
         beforeF (betweenWhitespace' colon) suite)
        el <>
      foldMapOf
        (_Wrapped.folded)
        ((text "finally" <>) .
         beforeF (betweenWhitespace' colon) suite)
        f
    TryStatementFinally t f _ ->
      text "try" <>
      beforeF (betweenWhitespace' colon) suite t <>
      text "finally" <>
      beforeF (betweenWhitespace' colon) suite f

exceptClause :: ExceptClause ctxt a -> Doc
exceptClause (ExceptClause v _) =
  text "except" <>
  foldMapOf
    (_Wrapped.folded)
    (prodElim test $
     foldMapOf (_Wrapped.folded) $
     beforeF (betweenWhitespace' kAs) identifier)
    v

withStatement :: WithStatement lctxt ectxt a -> Doc
withStatement (WithStatement h t s _) =
  text "with" <>
  whitespaceBeforeF withItem h <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' comma) withItem)
    t <>
  beforeF (betweenWhitespace' colon) suite s

withItem :: WithItem ctxt a -> Doc
withItem (WithItem l r _) =
  test l <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' kAs) expr)
    r

asyncStatement :: AsyncStatement lctxt ectxt a -> Doc
asyncStatement (AsyncStatement v _) =
  text "async" <>
  whitespaceBeforeF
    (sumElim (sumElim funcDef withStatement) forStatement)
    v

funcDef :: FuncDef ctxt a -> Doc
funcDef (FuncDef n p t b _) =
  text "def" <>
  whitespaceBeforeF identifier n <>
  whitespaceBeforeF parameters p <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' rightArrow) test)
    t <>
  beforeF (betweenWhitespace' colon) suite b

parameters :: Parameters ctxt a -> Doc
parameters (Parameters v _) =
  parens $
  betweenWhitespace'F (foldMapOf (_Wrapped.folded) (argsList typedArg test)) v

typedArg :: TypedArg a -> Doc
typedArg (TypedArg v t _) =
  identifier v <>
  foldMapOf
    (_Wrapped.folded)
    (beforeF (betweenWhitespace' colon) test)
    t

classDef :: ClassDef ctxt a -> Doc
classDef (ClassDef n a b _) =
  text "class" <>
  whitespaceBeforeF identifier n <>
  foldMapOf
    (_Wrapped.folded)
    (parens .
     whitespaceBeforeF
       (betweenWhitespace'F $
        foldMapOf (_Wrapped.folded) argList))
    a <>
  beforeF (betweenWhitespace' colon) suite b

decorated :: Decorated ctxt a -> Doc
decorated (Decorated ds b _) =
  foldMapOf (_Wrapped.folded) decorator ds <>
  sumElim (sumElim classDef funcDef) asyncFuncDef b

decorator :: Decorator ctxt a -> Doc
decorator (Decorator name args n _) =
  text "@" <>
  whitespaceBeforeF dottedName name <>
  foldMapOf
    (_Wrapped.folded)
    (parens . betweenWhitespace'F (foldMapOf (_Wrapped.folded) argList))
    args <>
  newlineChar n

asyncFuncDef :: AsyncFuncDef ctxt a -> Doc
asyncFuncDef (AsyncFuncDef v _) =
  text "async" <>
  whitespaceBeforeF funcDef v

suite :: Suite lctxt ctxt a -> Doc
suite s =
  case s of
    SuiteSingle v _ -> simpleStatement v
    SuiteMulti n sts _ ->
      newlineChar n <>
      indentedLines statement (getCompose sts)
