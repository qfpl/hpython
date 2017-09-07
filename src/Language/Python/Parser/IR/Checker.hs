{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Language.Python.Parser.IR.Checker where

import Prelude (error)
import Control.Monad.Writer hiding (Alt)
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.Between
import Data.Text (Text)
import Data.Validation
import Papa

import qualified Data.DList as D
import qualified Data.Set as S

import Language.Python.AST.Identifier
import Language.Python.AST.Keywords
import Language.Python.Parser.IR.SyntaxConfig

import qualified Language.Python.AST as Safe
import qualified Language.Python.Parser.IR as IR

data InvalidLHS
  = LHSIf
  | LHSOperator
  | LHSAwaitExpr
  | LHSTrailer
  | LHSArgument
  | LHSFor
  | LHSIfOrFor
  | LHSSubscript
  | LHSYieldExpr
  | LHSLiteral
  | LHSEllipsis
  | LHSNone
  | LHSTrue
  | LHSFalse
  deriving (Eq, Show, Ord)

data SyntaxError a
  = CannotAssignTo InvalidLHS a
  | AwaitNotInAsyncFunction a
  | YieldNotInFunction a
  | YieldInAsyncFunction a
  | IdentifierIsKeyword Text a
  deriving (Eq, Show, Ord)

newtype SyntaxChecker ann a
  = SyntaxChecker
  { runSyntaxChecker :: AccValidation (D.DList (SyntaxError ann)) a
  } deriving
  ( Functor
  , Applicative
  )

instance Alt (SyntaxChecker ann) where
  SyntaxChecker a <!> SyntaxChecker b = SyntaxChecker $ a <!> b

runChecker :: SyntaxChecker ann a -> Either [SyntaxError ann] a
runChecker c =
  case runSyntaxChecker c of
    AccFailure es -> Left $ D.toList es
    AccSuccess a -> Right a

syntaxError :: SyntaxError ann -> SyntaxChecker ann a
syntaxError = SyntaxChecker . AccFailure . D.singleton

traverseCompose
  :: Traversable f
  => Traversal (Compose f g a) (Compose f g' a') (g a) (g' a')
traverseCompose = _Wrapping Compose . traverse

checkIdentifier
  :: SyntaxConfig atomType ctxt
  -> Identifier ann
  -> SyntaxChecker ann (Identifier ann)
checkIdentifier cfg e@(Identifier v ann) =
  let
    extraKeywords =
      case cfg ^. exprContext of
        Safe.SFunDef Safe.SAsync -> S.fromList ["async", "await"]
        _ -> S.empty
  in
    if v `elem` (alwaysKeywords `S.union` extraKeywords)
    then syntaxError $ IdentifierIsKeyword v ann
    else pure e

checkTest
  :: SyntaxConfig atomType ctxt
  -> IR.Test ann
  -> SyntaxChecker ann (Safe.Test atomType ctxt ann)
checkTest cfg e =
  case e of
    IR.TestCond h t ann ->
      case getCompose t of
        Nothing ->
          Safe.TestCondNoIf <$>
          checkOrTest cfg h <*>
          pure ann
        Just t' ->
          case cfg ^. atomType of
            Safe.SAssignable ->
              syntaxError $ CannotAssignTo LHSIf ann
            Safe.SNotAssignable ->
              Safe.TestCondIf <$>
              checkOrTest cfg h <*>
              traverseOf traverseCompose (checkIfThenElse cfg) t' <*>
              pure ann
    IR.TestLambdef -> error "checkTestLambdef not implemented"

checkIfThenElse
  :: SyntaxConfig atomType ctxt
  -> IR.IfThenElse ann
  -> SyntaxChecker ann (Safe.IfThenElse 'Safe.NotAssignable ctxt ann)
checkIfThenElse cfg (IR.IfThenElse i v1 e v2) =
  Safe.IfThenElse <$>
  pure i <*>
  (checkOrTest $ set atomType Safe.SNotAssignable cfg) v1 <*>
  pure e <*>
  (checkTest $ set atomType Safe.SNotAssignable cfg) v2

checkOrTest
  :: SyntaxConfig atomType ctxt
  -> IR.OrTest ann
  -> SyntaxChecker ann (Safe.OrTest atomType ctxt ann)
checkOrTest cfg (IR.OrTest l r ann) =
  case getCompose r of
    [] ->
      Safe.OrTestOne <$>
      checkAndTest cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.OrTestMany <$>
          checkAndTest cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkAndTest cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkAndTest
  :: SyntaxConfig atomType ctxt
  -> IR.AndTest ann
  -> SyntaxChecker ann (Safe.AndTest atomType ctxt ann)
checkAndTest cfg (IR.AndTest l r ann) =
  case getCompose r of
    [] ->
      Safe.AndTestOne <$>
      checkNotTest cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.AndTestMany <$>
          checkNotTest cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkAndTest cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkNotTest
  :: SyntaxConfig atomType ctxt
  -> IR.NotTest ann
  -> SyntaxChecker ann (Safe.NotTest atomType ctxt ann)
checkNotTest cfg e =
  case cfg ^. atomType of
    Safe.SAssignable ->
      case e of
        IR.NotTestOne val ann ->
          Safe.NotTestOne <$>
          checkComparison cfg val <*>
          pure ann
        IR.NotTestMany _ ann ->
          syntaxError $ CannotAssignTo LHSOperator ann
    Safe.SNotAssignable ->
      case e of
        IR.NotTestOne val ann ->
          Safe.NotTestOne <$>
          checkComparison cfg val <*>
          pure ann
        IR.NotTestMany val ann ->
          Safe.NotTestMany <$>
          traverseOf
            traverseCompose
            (checkNotTest cfg) val <*>
          pure ann

checkComparison
  :: SyntaxConfig atomType ctxt
  -> IR.Comparison ann
  -> SyntaxChecker ann (Safe.Comparison atomType ctxt ann)
checkComparison cfg (IR.Comparison l r ann) =
  case getCompose r of
    [] ->
      Safe.ComparisonOne <$>
      checkExpr cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.ComparisonMany <$>
          checkExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkExpr cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkExpr
  :: SyntaxConfig atomType ctxt
  -> IR.Expr ann
  -> SyntaxChecker ann (Safe.Expr atomType ctxt ann)
checkExpr cfg (IR.Expr l r ann) =
  case getCompose r of
    [] ->
      Safe.ExprOne <$>
      checkXorExpr cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.ExprMany <$>
          checkXorExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkXorExpr cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkXorExpr
  :: SyntaxConfig atomType ctxt
  -> IR.XorExpr ann
  -> SyntaxChecker ann (Safe.XorExpr atomType ctxt ann)
checkXorExpr cfg (IR.XorExpr l r ann) =
  case getCompose r of
    [] ->
      Safe.XorExprOne <$>
      checkAndExpr cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.XorExprMany <$>
          checkAndExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkAndExpr cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkAndExpr
  :: SyntaxConfig atomType ctxt
  -> IR.AndExpr ann
  -> SyntaxChecker ann (Safe.AndExpr atomType ctxt ann)
checkAndExpr cfg (IR.AndExpr l r ann) =
  case getCompose r of
    [] -> 
      Safe.AndExprOne <$>
      checkShiftExpr cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.AndExprMany <$>
          checkShiftExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkShiftExpr cfg)
            (Compose $ r' :| rs')<*>
          pure ann

checkShiftExpr
  :: SyntaxConfig atomType ctxt
  -> IR.ShiftExpr ann
  -> SyntaxChecker ann (Safe.ShiftExpr atomType ctxt ann)
checkShiftExpr cfg (IR.ShiftExpr l r ann) =
  case getCompose r of
    [] ->
      Safe.ShiftExprOne <$>
      checkArithExpr cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.ShiftExprMany <$>
          checkArithExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkArithExpr cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkArithExpr
  :: SyntaxConfig atomType ctxt
  -> IR.ArithExpr ann
  -> SyntaxChecker ann (Safe.ArithExpr atomType ctxt ann)
checkArithExpr cfg (IR.ArithExpr l r ann) =
  case getCompose r of
    [] -> 
      Safe.ArithExprOne <$>
      checkTerm cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.ArithExprMany <$>
          checkTerm cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkTerm cfg)
            (Compose $ r' :| rs')<*>
          pure ann

checkTerm
  :: SyntaxConfig atomType ctxt
  -> IR.Term ann
  -> SyntaxChecker ann (Safe.Term atomType ctxt ann)
checkTerm cfg (IR.Term l r ann) =
  case getCompose r of
    [] ->
      Safe.TermOne <$>
      checkFactor cfg l <*>
      pure ann
    (r':rs') ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.TermMany <$>
          checkFactor cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkFactor cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkFactor
  :: SyntaxConfig atomType ctxt
  -> IR.Factor ann
  -> SyntaxChecker ann (Safe.Factor atomType ctxt ann)
checkFactor cfg e =
  case e of
    IR.FactorNone val ann ->
      Safe.FactorNone <$>
      checkPower cfg val <*>
      pure ann
    IR.FactorOne compOp val ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.FactorOne compOp <$>
          checkFactor cfg val <*>
          pure ann

checkPower
  :: SyntaxConfig atomType ctxt
  -> IR.Power ann
  -> SyntaxChecker ann (Safe.Power atomType ctxt ann)
checkPower cfg (IR.Power l r ann) =
  case getCompose r of
    Nothing ->
      Safe.PowerOne <$>
      checkAtomExpr cfg l <*>
      pure ann
    Just r' ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        Safe.SNotAssignable ->
          Safe.PowerMany <$>
          checkAtomExpr cfg l <*>
          traverseCompose
            (checkFactor cfg)
            r' <*>
          pure ann

checkAtomExpr
  :: SyntaxConfig atomType ctxt
  -> IR.AtomExpr ann
  -> SyntaxChecker ann (Safe.AtomExpr atomType ctxt ann)
checkAtomExpr cfg (IR.AtomExpr kw a ts ann) =
  case kw of
    Nothing ->
      Safe.AtomExprNoAwait <$>
      checkAtom cfg a <*>
      traverseOf
        (traverseCompose.traverseCompose)
        (checkTrailer $ set atomType Safe.SNotAssignable cfg)
        ts <*>
      pure ann
    Just kw' ->
      case (cfg ^. atomType, cfg ^. exprContext) of
        (Safe.SNotAssignable, Safe.SFunDef Safe.SAsync) ->
          Safe.AtomExprAwait kw' <$>
          checkAtom cfg a <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkTrailer cfg)
            ts <*>
          pure ann
        (Safe.SNotAssignable, _) ->
          syntaxError $ AwaitNotInAsyncFunction ann
        (Safe.SAssignable, Safe.SFunDef Safe.SAsync) ->
          syntaxError $ CannotAssignTo LHSAwaitExpr ann
        (Safe.SAssignable, Safe.SFunDef Safe.SNormal) ->
          syntaxError (CannotAssignTo LHSAwaitExpr ann) *>
          syntaxError (AwaitNotInAsyncFunction ann)
        (Safe.SAssignable, Safe.STopLevel) ->
          syntaxError (CannotAssignTo LHSAwaitExpr ann) *>
          syntaxError (AwaitNotInAsyncFunction ann)

checkTrailer
  :: SyntaxConfig atomType ctxt
  -> IR.Trailer ann
  -> SyntaxChecker ann (Safe.Trailer atomType ctxt ann)
checkTrailer cfg e =
  case cfg ^. atomType of
    Safe.SAssignable ->
      syntaxError $ CannotAssignTo LHSTrailer (e ^. IR.trailer_ann)
    Safe.SNotAssignable ->
      case e of
        IR.TrailerCall v ann ->
          Safe.TrailerCall <$>
          traverseOf (traverseCompose.traverseCompose) (checkArgList cfg) v <*>
          pure ann
        IR.TrailerSubscript v ann ->
          Safe.TrailerSubscript <$>
          traverseCompose
            (checkSubscriptList cfg)
            v <*>
          pure ann
        IR.TrailerAccess v ann -> pure $ Safe.TrailerAccess v ann

checkArgList
  :: SyntaxConfig atomType ctxt
  -> IR.ArgList ann
  -> SyntaxChecker ann (Safe.ArgList atomType ctxt ann)
checkArgList cfg (IR.ArgList h t comma ann) =
  Safe.ArgList <$>
  checkArgument cfg h <*>
  traverseOf (traverseCompose.traverseCompose) (checkArgument cfg) t <*>
  pure comma <*>
  pure ann

checkArgument
  :: SyntaxConfig atomType ctxt
  -> IR.Argument ann
  -> SyntaxChecker ann (Safe.Argument atomType ctxt ann)
checkArgument cfg e =
  case cfg ^. atomType of
    Safe.SAssignable ->
      syntaxError $ CannotAssignTo LHSArgument (e ^. IR.argument_ann)
    Safe.SNotAssignable ->
      case e of
        IR.ArgumentFor ex f ann ->
          Safe.ArgumentFor <$>
          checkTest cfg ex <*>
          traverseOf (traverseCompose.traverseCompose) (checkCompFor cfg) f <*>
          pure ann
        IR.ArgumentDefault l r ann ->
          Safe.ArgumentDefault <$>
          traverseOf
            traverseCompose
            (checkTest $ cfg & atomType .~ Safe.SAssignable)
            l <*>
          traverseOf
            traverseCompose
            (checkTest cfg)
            r <*>
          pure ann
        IR.ArgumentUnpack sym val ann ->
          Safe.ArgumentUnpack sym <$>
          traverseOf traverseCompose (checkTest cfg) val <*>
          pure ann

checkCompFor
  :: SyntaxConfig atomType ctxt
  -> IR.CompFor ann
  -> SyntaxChecker ann (Safe.CompFor atomType ctxt ann)
checkCompFor cfg e =
  case cfg ^. atomType of
    Safe.SAssignable ->
      syntaxError $ CannotAssignTo LHSFor (e ^. IR.compFor_ann)
    Safe.SNotAssignable ->
      case e of
        IR.CompFor ts ex i ann ->
          Safe.CompFor <$>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkExprList $ cfg & atomType .~ Safe.SAssignable)
            ts <*>
          traverseOf
            traverseCompose
            (checkOrTest cfg)
            ex <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkCompIter cfg)
            i <*>
          pure ann

checkExprList
  :: SyntaxConfig atomType ctxt
  -> IR.ExprList ann
  -> SyntaxChecker ann (Safe.ExprList atomType ctxt ann)
checkExprList cfg (IR.ExprList h t ann) =
  Safe.ExprList <$>
  checkExprOrStar h <*>
  traverseOf (traverseCompose.traverseCompose) checkExprOrStar t <*>
  pure ann
  where
    checkExprOrStar e =
      case e of
        InL a -> InL <$> checkExpr cfg a
        InR a -> InR <$> checkStarExpr cfg a

checkStarExpr
  :: SyntaxConfig atomType ctxt
  -> IR.StarExpr ann
  -> SyntaxChecker ann (Safe.StarExpr atomType ctxt ann)
checkStarExpr cfg (IR.StarExpr val ann) =
  Safe.StarExpr <$>
  traverseOf
    traverseCompose
    (checkExpr $ cfg & atomType .~ Safe.SAssignable) val <*>
  pure ann

checkCompIter
  :: SyntaxConfig atomType ctxt
  -> IR.CompIter ann
  -> SyntaxChecker ann (Safe.CompIter atomType ctxt ann)
checkCompIter cfg (IR.CompIter val ann) =
  case cfg ^. atomType of
    Safe.SAssignable ->
      syntaxError $ CannotAssignTo LHSIfOrFor ann
    Safe.SNotAssignable ->
      Safe.CompIter <$>
      checkCompForOrIf cfg val <*>
      pure ann
  where
    checkCompForOrIf cfg' e' =
      case e' of
        InL a -> InL <$> checkCompFor cfg' a
        InR a -> InR <$> checkCompIf cfg' a

checkCompIf
  :: SyntaxConfig atomType ctxt
  -> IR.CompIf ann
  -> SyntaxChecker ann (Safe.CompIf atomType ctxt ann)
checkCompIf cfg (IR.CompIf kw ex it ann) =
  case cfg ^. atomType of
    Safe.SAssignable ->
      syntaxError $ CannotAssignTo LHSIf ann
    Safe.SNotAssignable ->
      Safe.CompIf kw <$>
      checkTestNocond cfg ex <*>
      traverseOf (traverseCompose.traverseCompose) (checkCompIter cfg) it <*>
      pure ann

checkTestNocond
  :: SyntaxConfig atomType ctxt
  -> IR.TestNocond ann
  -> SyntaxChecker ann (Safe.TestNocond atomType ctxt ann)
checkTestNocond cfg (IR.TestNocond v ann) =
  Safe.TestNocond <$>
  orTestOrLambdef v <*>
  pure ann
  where
    orTestOrLambdef e =
      case e of
        InL a -> InL <$> checkOrTest cfg a
        InR a -> InR <$> checkLambdefNocond cfg a

checkLambdefNocond
  :: SyntaxConfig atomType ctxt
  -> IR.LambdefNocond ann
  -> SyntaxChecker ann (Safe.LambdefNocond atomType ctxt ann)
checkLambdefNocond cfg (IR.LambdefNocond args ex ann) =
  Safe.LambdefNocond <$>
  traverseOf (traverseCompose.traverseCompose) (checkVarargsList cfg) args <*>
  traverseCompose (checkTestNocond cfg) ex <*>
  pure ann

checkVarargsList
  :: SyntaxConfig atomType ctxt
  -> IR.VarargsList ann
  -> SyntaxChecker ann (Safe.VarargsList atomType ctxt ann)
checkVarargsList _ _ = error "checkVarargsList not implemented"

checkSubscriptList
  :: SyntaxConfig atomType ctxt
  -> IR.SubscriptList ann
  -> SyntaxChecker ann (Safe.SubscriptList atomType ctxt ann)
checkSubscriptList cfg (IR.SubscriptList h t comma ann) =
  case cfg ^. atomType of
    Safe.SAssignable ->
      syntaxError $ CannotAssignTo LHSSubscript ann
    Safe.SNotAssignable ->
      Safe.SubscriptList <$>
      checkSubscript cfg h <*>
      traverseOf (traverseCompose.traverseCompose) (checkSubscript cfg) t <*>
      pure comma <*>
      pure ann

checkSubscript
  :: SyntaxConfig atomType ctxt
  -> IR.Subscript ann
  -> SyntaxChecker ann (Safe.Subscript atomType ctxt ann)
checkSubscript cfg e =
  case cfg ^. atomType of
    Safe.SAssignable ->
      syntaxError $ CannotAssignTo LHSSubscript (e ^. IR.subscript_ann)
    Safe.SNotAssignable ->
      case e of
        IR.SubscriptTest v ann ->
          Safe.SubscriptTest <$>
          checkTest cfg v <*>
          pure ann
        IR.SubscriptSlice l colon r sl ann ->
          Safe.SubscriptSlice <$>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkTest cfg)
            l <*>
          pure colon <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkTest cfg)
            r <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkSliceOp cfg)
            sl <*>
          pure ann

checkSliceOp
  :: SyntaxConfig atomType ctxt
  -> IR.SliceOp ann
  -> SyntaxChecker ann (Safe.SliceOp atomType ctxt ann)
checkSliceOp cfg (IR.SliceOp v ann) =
  case cfg ^. atomType of
    Safe.SAssignable ->
      syntaxError $ CannotAssignTo LHSSubscript ann
    Safe.SNotAssignable ->
      Safe.SliceOp <$>
      traverseOf (traverseCompose.traverseCompose) (checkTest cfg) v <*>
      pure ann

checkAtom
  :: SyntaxConfig atomType ctxt
  -> IR.Atom ann
  -> SyntaxChecker ann (Safe.Atom atomType ctxt ann)
checkAtom cfg e =
  case e of
    IR.AtomParen v ann ->
      case v ^. _Wrapping Compose . between'._2._Wrapping Compose of
        Nothing ->
          pure $
          Safe.AtomParenNoYield
            (v &
               _Wrapping Compose . between'._2._Wrapping Compose .~ Nothing)
            ann
        Just (InL a) ->
          case (cfg  ^. atomType, cfg ^. exprContext) of
            (Safe.SAssignable, _) ->
              syntaxError $ CannotAssignTo LHSYieldExpr ann
            (_, Safe.STopLevel) ->
              syntaxError $ YieldNotInFunction ann
            (_, Safe.SFunDef Safe.SAsync) ->
              syntaxError $ YieldInAsyncFunction ann
            (Safe.SNotAssignable, Safe.SFunDef Safe.SNormal) ->
              Safe.AtomParenYield <$>
              traverseCompose
                (checkYieldExpr cfg)
                (v &
                  _Wrapping Compose . between'._2 .~ a) <*>
              pure ann
        Just (InR a) ->
          Safe.AtomParenNoYield <$>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkTestlistComp cfg)
            (v &
              _Wrapping Compose . between'._2._Wrapping Compose .~ Just a) <*>
          pure ann
    IR.AtomBracket v ann ->
      Safe.AtomBracket <$>
      traverseOf
        (traverseCompose.traverseCompose)
        (checkTestlistComp cfg)
        v <*>
      pure ann
    IR.AtomCurly _ _ -> error "checkDictOrSetMaker not implemented"
    IR.AtomIdentifier v ann ->
      Safe.AtomIdentifier <$>
      checkIdentifier cfg v <*>
      pure ann
    IR.AtomInteger v ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSLiteral ann
        Safe.SNotAssignable ->
          pure $ Safe.AtomInteger v ann
    IR.AtomFloat v ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSLiteral ann
        Safe.SNotAssignable ->
          pure $ Safe.AtomFloat v ann
    IR.AtomString h t ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSLiteral ann
        Safe.SNotAssignable ->
          pure $ Safe.AtomString h t ann
    IR.AtomImag v ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSLiteral ann
        Safe.SNotAssignable ->
          pure $ Safe.AtomImag v ann
    IR.AtomEllipsis ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSEllipsis ann
        Safe.SNotAssignable ->
          pure $ Safe.AtomEllipsis ann
    IR.AtomNone ann -> 
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSNone ann
        Safe.SNotAssignable ->
          pure $ Safe.AtomNone ann
    IR.AtomTrue ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSTrue ann
        Safe.SNotAssignable ->
          pure $ Safe.AtomTrue ann
    IR.AtomFalse ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSFalse ann
        Safe.SNotAssignable ->
          pure $ Safe.AtomFalse ann

checkYieldExpr
  :: SyntaxConfig atomType ctxt
  -> IR.YieldExpr ann
  -> SyntaxChecker ann (Safe.YieldExpr ann)
checkYieldExpr cfg (IR.YieldExpr val ann) =
  Safe.YieldExpr <$>
  traverseOf
    (traverseCompose.traverseCompose)
    (checkYieldArg $
      cfg
        & atomType .~ Safe.SNotAssignable
        & exprContext .~ Safe.SFunDef Safe.SNormal)
    val <*>
  pure ann

checkYieldArg
  :: SyntaxConfig atomType ctxt
  -> IR.YieldArg ann
  -> SyntaxChecker ann (Safe.YieldArg atomType ctxt ann)
checkYieldArg cfg e =
  case e of
    IR.YieldArgFrom v ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSYieldExpr ann
        Safe.SNotAssignable ->
          Safe.YieldArgFrom <$>
          traverseCompose (checkTest cfg) v <*>
          pure ann
    IR.YieldArgList v ann ->
      Safe.YieldArgList <$>
      checkTestList cfg v <*>
      pure ann

checkTestList
  :: SyntaxConfig atomType ctxt
  -> IR.TestList ann
  -> SyntaxChecker ann (Safe.TestList atomType ctxt ann)
checkTestList cfg (IR.TestList h t comma ann) =
  Safe.TestList <$>
  checkTest cfg h <*>
  traverseCompose (checkTest cfg) t <*>
  pure comma <*>
  pure ann

checkTestlistComp
  :: SyntaxConfig atomType ctxt
  -> IR.TestlistComp ann
  -> SyntaxChecker ann (Safe.TestlistComp atomType ctxt ann)
checkTestlistComp cfg e =
  case e of
    IR.TestlistCompFor h t ann ->
      case cfg ^. atomType of
        Safe.SAssignable ->
          syntaxError $ CannotAssignTo LHSFor ann
        Safe.SNotAssignable ->
          Safe.TestlistCompFor <$>
          testOrStarExpr cfg h <*>
          traverseCompose (checkCompFor cfg) t <*>
          pure ann
    IR.TestlistCompList h t comma ann ->
      Safe.TestlistCompList <$>
      testOrStarExpr cfg h <*>
      traverseOf (traverseCompose.traverseCompose) (testOrStarExpr cfg) t <*>
      pure comma <*>
      pure ann
  where
    testOrStarExpr cfg' e' =
      case e' of
        InL a -> InL <$> checkTest cfg' a
        InR a -> InR <$> checkStarExpr cfg' a
