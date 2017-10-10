{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Language.Python.Expr.IR.Checker where

import Data.Functor.Compose
import Data.Functor.Sum
import Data.Separated.After
import Data.Separated.Between
import Papa

import qualified Data.Set as S

import Language.Python.AST.Identifier
import Language.Python.AST.Keywords
import Language.Python.IR.ExprConfig
import Language.Python.IR.SyntaxChecker
import Language.Python.IR.Checker.ArgsList

import qualified Language.Python.Expr.AST as Safe
import qualified Language.Python.Expr.IR as IR

traverseCompose
  :: Traversable f
  => Traversal (Compose f g a) (Compose f g' a') (g a) (g' a')
traverseCompose = _Wrapping Compose . traverse

checkIdentifier
  :: ExprConfig atomType ctxt
  -> Identifier ann
  -> SyntaxChecker ann (Identifier ann)
checkIdentifier cfg e@(Identifier v ann) =
  let
    extraKeywords =
      case cfg ^. definitionContext of
        SFunDef SAsync -> S.fromList ["async", "await"]
        _ -> S.empty
  in
    if v `elem` (alwaysKeywords `S.union` extraKeywords)
    then syntaxError $ IdentifierIsKeyword v ann
    else pure e

checkTest
  :: ExprConfig atomType ctxt
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
            SAssignable ->
              syntaxError $ CannotAssignTo LHSIf ann
            SNotAssignable ->
              Safe.TestCondIf <$>
              checkOrTest cfg h <*>
              traverseOf traverseCompose (checkIfThenElse cfg) t' <*>
              pure ann
    IR.TestLambdef l ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSLambda ann
        SNotAssignable ->
          Safe.TestLambdef <$>
          checkLambdef cfg l <*>
          pure ann

checkLambdef
  :: ExprConfig atomType ctxt
  -> IR.Lambdef ann
  -> SyntaxChecker ann (Safe.Lambdef 'NotAssignable ctxt ann)
checkLambdef cfg (IR.Lambdef as b ann) =
  case cfg ^. atomType of
    SAssignable ->
      syntaxError $ CannotAssignTo LHSLambda ann
    SNotAssignable ->
      Safe.Lambdef <$>
      traverseOf
        (traverseCompose.traverseCompose)
        (checkArgsList cfg checkTest checkIdentifier)
        as <*>
      traverseCompose (checkTest $ cfg & definitionContext .~ SFunDef SNormal) b <*>
      pure ann

checkIfThenElse
  :: ExprConfig atomType ctxt
  -> IR.IfThenElse ann
  -> SyntaxChecker ann (Safe.IfThenElse 'NotAssignable ctxt ann)
checkIfThenElse cfg (IR.IfThenElse i v1 e v2) =
  Safe.IfThenElse <$>
  pure i <*>
  (checkOrTest $ set atomType SNotAssignable cfg) v1 <*>
  pure e <*>
  (checkTest $ set atomType SNotAssignable cfg) v2

checkOrTest
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.OrTestMany <$>
          checkAndTest cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkAndTest cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkAndTest
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.AndTestMany <$>
          checkNotTest cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkAndTest cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkNotTest
  :: ExprConfig atomType ctxt
  -> IR.NotTest ann
  -> SyntaxChecker ann (Safe.NotTest atomType ctxt ann)
checkNotTest cfg e =
  case cfg ^. atomType of
    SAssignable ->
      case e of
        IR.NotTestOne val ann ->
          Safe.NotTestOne <$>
          checkComparison cfg val <*>
          pure ann
        IR.NotTestMany _ ann ->
          syntaxError $ CannotAssignTo LHSOperator ann
    SNotAssignable ->
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
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.ComparisonMany <$>
          checkExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkExpr cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkExpr
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.ExprMany <$>
          checkXorExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkXorExpr cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkXorExpr
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.XorExprMany <$>
          checkAndExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkAndExpr cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkAndExpr
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.AndExprMany <$>
          checkShiftExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkShiftExpr cfg)
            (Compose $ r' :| rs')<*>
          pure ann

checkShiftExpr
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.ShiftExprMany <$>
          checkArithExpr cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkArithExpr cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkArithExpr
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.ArithExprMany <$>
          checkTerm cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkTerm cfg)
            (Compose $ r' :| rs')<*>
          pure ann

checkTerm
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.TermMany <$>
          checkFactor cfg l <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkFactor cfg)
            (Compose $ r' :| rs') <*>
          pure ann

checkFactor
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.FactorOne compOp <$>
          checkFactor cfg val <*>
          pure ann

checkPower
  :: ExprConfig atomType ctxt
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
        SAssignable ->
          syntaxError $ CannotAssignTo LHSOperator ann
        SNotAssignable ->
          Safe.PowerMany <$>
          checkAtomExpr cfg l <*>
          traverseCompose
            (checkFactor cfg)
            r' <*>
          pure ann

checkAtomExpr
  :: ExprConfig atomType ctxt
  -> IR.AtomExpr ann
  -> SyntaxChecker ann (Safe.AtomExpr atomType ctxt ann)
checkAtomExpr cfg (IR.AtomExpr kw a ts ann) =
  case kw of
    Nothing ->
      Safe.AtomExprNoAwait <$>
      checkAtom cfg a <*>
      traverseOf
        (traverseCompose.traverseCompose)
        (checkTrailer cfg)
        ts <*>
      pure ann
    Just kw' ->
      case (cfg ^. atomType, cfg ^. definitionContext) of
        (SNotAssignable, SFunDef SAsync) ->
          Safe.AtomExprAwait kw' <$>
          checkAtom cfg a <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkTrailer cfg)
            ts <*>
          pure ann
        (SNotAssignable, _) ->
          syntaxError $ AwaitNotInAsyncFunction ann
        (SAssignable, SFunDef SAsync) ->
          syntaxError $ CannotAssignTo LHSAwaitExpr ann
        (SAssignable, SFunDef SNormal) ->
          syntaxError (CannotAssignTo LHSAwaitExpr ann) *>
          syntaxError (AwaitNotInAsyncFunction ann)
        (SAssignable, STopLevel) ->
          syntaxError (CannotAssignTo LHSAwaitExpr ann) *>
          syntaxError (AwaitNotInAsyncFunction ann)

checkTrailer
  :: ExprConfig atomType ctxt
  -> IR.Trailer ann
  -> SyntaxChecker ann (Safe.Trailer atomType ctxt ann)
checkTrailer cfg e =
  case e of
    IR.TrailerCall v ann ->
      case cfg ^. atomType of
        SAssignable -> syntaxError $ CannotAssignTo LHSFunCall ann
        SNotAssignable ->
          Safe.TrailerCall <$>
          traverseOf (traverseCompose.traverseCompose) (checkArgList cfg) v <*>
          pure ann
    IR.TrailerSubscript v ann ->
      Safe.TrailerSubscript <$>
      traverseCompose
        (checkSubscriptList $ cfg & atomType .~ SNotAssignable)
        v <*>
      pure ann
    IR.TrailerAccess v ann -> pure $ Safe.TrailerAccess v ann

checkArgList
  :: ExprConfig atomType ctxt
  -> IR.ArgList ann
  -> SyntaxChecker ann (Safe.ArgList atomType ctxt ann)
checkArgList cfg (IR.ArgList h t comma ann) =
  case (h, t) of
    (IR.ArgumentFor e (Compose (Just f)) _, Compose []) ->
      Safe.ArgListSingleFor <$>
      checkTest (cfg & atomType .~ SNotAssignable) e <*>
      checkCompFor (cfg & atomType .~ SNotAssignable) f <*>
      pure comma <*>
      pure ann
    _ ->
      Safe.ArgListMany <$>
      checkArgument (cfg & atomType .~ SNotAssignable) h <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkArgument $ cfg & atomType .~ SNotAssignable)
        t <*>
      pure comma <*>
      pure ann
  where
    forWithoutParens (IR.ArgumentFor _ (Compose (Just _)) _) = True
    forWithoutParens _ = False

checkArgument
  :: ExprConfig atomType ctxt
  -> IR.Argument ann
  -> SyntaxChecker ann (Safe.Argument atomType ctxt ann)
checkArgument cfg e =
  case cfg ^. atomType of
    SAssignable ->
      syntaxError $ CannotAssignTo LHSArgument (e ^. IR.argument_ann)
    SNotAssignable ->
      case e of
        IR.ArgumentFor e (Compose Nothing) ann ->
          Safe.ArgumentExpr <$> checkTest cfg e <*> pure ann
        IR.ArgumentFor _ (Compose (Just _)) ann ->
          syntaxError $ UnparenthesisedGeneratorInArgs ann
        IR.ArgumentForParens l e f r ann ->
          Safe.ArgumentFor l <$>
          checkTest cfg e <*>
          checkCompFor cfg f <*>
          pure r <*>
          pure ann
        IR.ArgumentDefault l r ann ->
          Safe.ArgumentDefault <$>
          traverseOf
            traverseCompose
            (checkTest $ cfg & atomType .~ SAssignable)
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
  :: ExprConfig atomType ctxt
  -> IR.CompFor ann
  -> SyntaxChecker ann (Safe.CompFor atomType ctxt ann)
checkCompFor cfg e =
  case cfg ^. atomType of
    SAssignable ->
      syntaxError $ CannotAssignTo LHSFor (e ^. IR.compFor_ann)
    SNotAssignable ->
      case e of
        IR.CompFor ts ex i ann ->
          Safe.CompFor <$>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkExprList $ cfg & atomType .~ SAssignable)
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
  :: ExprConfig atomType ctxt
  -> IR.ExprList ann
  -> SyntaxChecker ann (Safe.ExprList atomType ctxt ann)
checkExprList cfg (IR.ExprList h t comma ann) =
  case getCompose t of
    [] -> case h of
      InL expr ->
        Safe.ExprListSingle <$>
        checkExpr cfg expr <*>
        pure comma <*>
        pure ann
      InR starred ->
        case comma of
          Nothing ->
            case cfg ^. atomType of
              SAssignable ->
                syntaxError $ CannotAssignTo LHSSingleStarExpr ann
              SNotAssignable ->
                Safe.ExprListSingleStarredNoComma <$>
                checkStarExpr cfg starred <*>
                pure ann
          Just comma' ->
            Safe.ExprListSingleStarredComma <$>
            checkStarExpr cfg starred <*>
            pure comma' <*>
            pure ann
    (t':ts') ->
      Safe.ExprListMany <$>
      checkExprOrStar h <*>
      traverseOf
        (traverseCompose.traverseCompose)
        checkExprOrStar
        (Compose $ t' :| ts') <*>
      pure comma <*>
      pure ann
  where
    checkExprOrStar e =
      case e of
        InL a -> InL <$> checkExpr cfg a
        InR a -> InR <$> checkStarExpr cfg a

checkStarExpr
  :: ExprConfig atomType ctxt
  -> IR.StarExpr ann
  -> SyntaxChecker ann (Safe.StarExpr atomType ctxt ann)
checkStarExpr cfg (IR.StarExpr val ann) =
  Safe.StarExpr <$>
  traverseOf
    traverseCompose
    (checkExpr $ cfg & atomType .~ SAssignable) val <*>
  pure ann

checkCompIter
  :: ExprConfig atomType ctxt
  -> IR.CompIter ann
  -> SyntaxChecker ann (Safe.CompIter atomType ctxt ann)
checkCompIter cfg (IR.CompIter val ann) =
  case cfg ^. atomType of
    SAssignable ->
      syntaxError $ CannotAssignTo LHSIfOrFor ann
    SNotAssignable ->
      Safe.CompIter <$>
      checkCompForOrIf cfg val <*>
      pure ann
  where
    checkCompForOrIf cfg' e' =
      case e' of
        InL a -> InL <$> checkCompFor cfg' a
        InR a -> InR <$> checkCompIf cfg' a

checkCompIf
  :: ExprConfig atomType ctxt
  -> IR.CompIf ann
  -> SyntaxChecker ann (Safe.CompIf atomType ctxt ann)
checkCompIf cfg (IR.CompIf kw ex it ann) =
  case cfg ^. atomType of
    SAssignable ->
      syntaxError $ CannotAssignTo LHSIf ann
    SNotAssignable ->
      Safe.CompIf kw <$>
      checkTestNocond cfg ex <*>
      traverseOf (traverseCompose.traverseCompose) (checkCompIter cfg) it <*>
      pure ann

checkTestNocond
  :: ExprConfig atomType ctxt
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
  :: ExprConfig atomType ctxt
  -> IR.LambdefNocond ann
  -> SyntaxChecker ann (Safe.LambdefNocond atomType ctxt ann)
checkLambdefNocond cfg (IR.LambdefNocond args ex ann) =
  Safe.LambdefNocond <$>
  traverseOf
    (traverseCompose.traverseCompose)
    (checkArgsList cfg checkTest checkIdentifier)
    args <*>
  traverseCompose (checkTestNocond $ cfg & definitionContext .~ SFunDef SNormal) ex <*>
  pure ann

checkSubscriptList
  :: ExprConfig atomType ctxt
  -> IR.SubscriptList ann
  -> SyntaxChecker ann (Safe.SubscriptList atomType ctxt ann)
checkSubscriptList cfg (IR.SubscriptList h t comma ann) =
  case cfg ^. atomType of
    SAssignable ->
      syntaxError $ CannotAssignTo LHSSubscript ann
    SNotAssignable ->
      Safe.SubscriptList <$>
      checkSubscript cfg h <*>
      traverseOf (traverseCompose.traverseCompose) (checkSubscript cfg) t <*>
      pure comma <*>
      pure ann

checkSubscript
  :: ExprConfig atomType ctxt
  -> IR.Subscript ann
  -> SyntaxChecker ann (Safe.Subscript atomType ctxt ann)
checkSubscript cfg e =
  case cfg ^. atomType of
    SAssignable ->
      syntaxError $ CannotAssignTo LHSSubscript (e ^. IR.subscript_ann)
    SNotAssignable ->
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
  :: ExprConfig atomType ctxt
  -> IR.SliceOp ann
  -> SyntaxChecker ann (Safe.SliceOp atomType ctxt ann)
checkSliceOp cfg (IR.SliceOp v ann) =
  case cfg ^. atomType of
    SAssignable ->
      syntaxError $ CannotAssignTo LHSSubscript ann
    SNotAssignable ->
      Safe.SliceOp <$>
      traverseOf (traverseCompose.traverseCompose) (checkTest cfg) v <*>
      pure ann

checkAtom
  :: ExprConfig atomType ctxt
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
          case (cfg  ^. atomType, cfg ^. definitionContext) of
            (SAssignable, _) ->
              syntaxError $ CannotAssignTo LHSYieldExpr ann
            (_, STopLevel) ->
              syntaxError $ YieldNotInFunction ann
            (_, SFunDef SAsync) ->
              syntaxError $ YieldInAsyncFunction ann
            (SNotAssignable, SFunDef SNormal) ->
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
            (checkTupleTestlistComp cfg)
            (v &
              _Wrapping Compose . between'._2._Wrapping Compose .~ Just a) <*>
          pure ann
    IR.AtomBracket v ann ->
      Safe.AtomBracket <$>
      traverseOf
        (traverseCompose.traverseCompose)
        (checkListTestlistComp cfg)
        v <*>
      pure ann
    IR.AtomCurly v ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSDictOrSet ann
        SNotAssignable ->
          Safe.AtomCurly <$>
          traverseOf
            (traverseCompose.traverseCompose)
            (checkDictOrSetMaker cfg)
            v <*>
          pure ann
    IR.AtomIdentifier v ann ->
      Safe.AtomIdentifier <$>
      checkIdentifier cfg v <*>
      pure ann
    IR.AtomInteger v ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSLiteral ann
        SNotAssignable ->
          pure $ Safe.AtomInteger v ann
    IR.AtomFloat v ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSLiteral ann
        SNotAssignable ->
          pure $ Safe.AtomFloat v ann
    IR.AtomString h t ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSLiteral ann
        SNotAssignable ->
          pure $ Safe.AtomString h t ann
    IR.AtomImag v ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSLiteral ann
        SNotAssignable ->
          pure $ Safe.AtomImag v ann
    IR.AtomEllipsis ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSEllipsis ann
        SNotAssignable ->
          pure $ Safe.AtomEllipsis ann
    IR.AtomNone ann -> 
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSNone ann
        SNotAssignable ->
          pure $ Safe.AtomNone ann
    IR.AtomTrue ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSTrue ann
        SNotAssignable ->
          pure $ Safe.AtomTrue ann
    IR.AtomFalse ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSFalse ann
        SNotAssignable ->
          pure $ Safe.AtomFalse ann

checkDictItem
  :: ExprConfig 'NotAssignable ctxt
  -> IR.DictItem ann
  -> SyntaxChecker ann (Safe.DictItem 'NotAssignable ctxt ann)
checkDictItem cfg (IR.DictItem k c v ann) =
  Safe.DictItem <$>
  checkTest cfg k <*>
  pure c <*>
  checkTest cfg v <*>
  pure ann

checkDictOrSetMaker
  :: ExprConfig 'NotAssignable ctxt
  -> IR.DictOrSetMaker ann
  -> SyntaxChecker ann (Safe.DictOrSetMaker 'NotAssignable ctxt ann)
checkDictOrSetMaker cfg e =
  case e of
    IR.DictOrSetMakerDict h t ann ->
      case t of
        InL t' ->
          case h of
            InL h' ->
              Safe.DictOrSetMakerDictComp <$>
              checkDictItem cfg h' <*>
              checkCompFor cfg t' <*>
              pure ann
            InR _ ->
              syntaxError $ UnpackingInComprehension ann
        InR t' ->
          Safe.DictOrSetMakerDictUnpack <$>
          itemOrUnpacking cfg h <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (itemOrUnpacking cfg)
            (t' ^. _Wrapped.after._2) <*>
          pure (t' ^? _Wrapped.after._1._Just) <*>
          pure ann
    IR.DictOrSetMakerSet h t ann ->
      case t of
        InL t' ->
          case h of
            InL h' ->
              Safe.DictOrSetMakerSetComp <$>
              checkTest cfg h' <*>
              checkCompFor cfg t' <*>
              pure ann
            InR _ ->
              syntaxError $ UnpackingInComprehension ann
        InR t' ->
          Safe.DictOrSetMakerSetUnpack <$>
          testOrStar cfg h <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (testOrStar cfg)
            (t' ^. _Wrapped.after._2) <*>
          pure (t' ^? _Wrapped.after._1._Just) <*>
          pure ann
  where
    itemOrUnpacking cfg' e' =
      case e' of
        InL e'' -> InL <$> checkDictItem cfg' e''
        InR e'' -> InR <$> checkDictUnpacking cfg' e''
    testOrStar cfg' e' =
      case e' of
        InL e'' -> InL <$> checkTest cfg' e''
        InR e'' -> InR <$> checkStarExpr cfg' e''

checkDictUnpacking
  :: ExprConfig 'NotAssignable ctxt
  -> IR.DictUnpacking ann
  -> SyntaxChecker ann (Safe.DictUnpacking 'NotAssignable ctxt ann)
checkDictUnpacking cfg (IR.DictUnpacking v ann) =
  Safe.DictUnpacking <$>
  traverseCompose (checkExpr cfg) v <*>
  pure ann

checkYieldExpr
  :: ExprConfig atomType ('FunDef 'Normal)
  -> IR.YieldExpr ann
  -> SyntaxChecker ann (Safe.YieldExpr ('FunDef 'Normal) ann)
checkYieldExpr cfg (IR.YieldExpr val ann) =
  Safe.YieldExpr <$>
  traverseOf
    (traverseCompose.traverseCompose)
    (checkYieldArg $
      cfg
        & atomType .~ SNotAssignable
        & definitionContext .~ SFunDef SNormal)
    val <*>
  pure ann

checkYieldArg
  :: ExprConfig atomType ctxt
  -> IR.YieldArg ann
  -> SyntaxChecker ann (Safe.YieldArg atomType ctxt ann)
checkYieldArg cfg e =
  case e of
    IR.YieldArgFrom v ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSYieldExpr ann
        SNotAssignable ->
          Safe.YieldArgFrom <$>
          traverseCompose (checkTest cfg) v <*>
          pure ann
    IR.YieldArgList v ann ->
      Safe.YieldArgList <$>
      checkTestList cfg v <*>
      pure ann

checkTestList
  :: ExprConfig atomType ctxt
  -> IR.TestList ann
  -> SyntaxChecker ann (Safe.TestList atomType ctxt ann)
checkTestList cfg (IR.TestList h t comma ann) =
  Safe.TestList <$>
  checkTest cfg h <*>
  traverseCompose (checkTest cfg) t <*>
  pure comma <*>
  pure ann

checkTupleTestlistComp
  :: ExprConfig atomType ctxt
  -> IR.TupleTestlistComp ann
  -> SyntaxChecker ann (Safe.TupleTestlistComp atomType ctxt ann)
checkTupleTestlistComp cfg e =
  case e of
    IR.TupleTestlistCompFor h t ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSFor ann
        SNotAssignable ->
          case h of
            InR (IR.StarExpr _ ann') ->
              syntaxError $ UnpackingInComprehension ann'
            InL h' ->
              Safe.TupleTestlistCompFor <$>
              checkTest cfg h' <*>
              checkCompFor cfg t <*>
              pure ann

    IR.TupleTestlistCompList h t comma ann ->
      case h of
        InL h' ->
          Safe.TupleTestlistCompList <$>
          checkTest cfg h' <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (testOrStarExpr cfg)
            t <*>
          pure comma <*>
          pure ann
        InR h' ->
          case getCompose t of
            [] ->
              case comma of
                Nothing ->
                  syntaxError $ UnpackingInParens (h' ^. IR.starExpr_ann)
                Just comma' ->  
                  Safe.TupleTestlistCompStarredOne <$>
                  checkStarExpr cfg h' <*>
                  pure comma' <*>
                  pure ann
            (t':ts') ->
              Safe.TupleTestlistCompStarredMany <$>
              checkStarExpr cfg h' <*>
              traverseOf
                (traverseCompose.traverseCompose)
                (testOrStarExpr cfg)
                (Compose $ t' :| ts') <*>
              pure comma <*>
              pure ann

  where
    testOrStarExpr cfg' e' =
      case e' of
        InL a -> InL <$> checkTest cfg' a
        InR a -> InR <$> checkStarExpr cfg' a

checkListTestlistComp
  :: ExprConfig atomType ctxt
  -> IR.ListTestlistComp ann
  -> SyntaxChecker ann (Safe.ListTestlistComp atomType ctxt ann)
checkListTestlistComp cfg e =
  case e of
    IR.ListTestlistCompFor h t ann ->
      case cfg ^. atomType of
        SAssignable ->
          syntaxError $ CannotAssignTo LHSFor ann
        SNotAssignable ->
          case h of
            InR (IR.StarExpr _ ann') ->
              syntaxError $ UnpackingInComprehension ann'
            InL h' ->
              Safe.ListTestlistCompFor <$>
              checkTest cfg h' <*>
              checkCompFor cfg t <*>
              pure ann

    IR.ListTestlistCompList h t comma ann ->
      case h of
        InL h' ->
          Safe.ListTestlistCompList <$>
          checkTest cfg h' <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (testOrStarExpr cfg)
            t <*>
          pure comma <*>
          pure ann
        InR h' ->
          Safe.ListTestlistCompStarred <$>
          checkStarExpr cfg h' <*>
          traverseOf
            (traverseCompose.traverseCompose)
            (testOrStarExpr cfg)
            t <*>
          pure comma <*>
          pure ann

  where
    testOrStarExpr cfg' e' =
      case e' of
        InL a -> InL <$> checkTest cfg' a
        InR a -> InR <$> checkStarExpr cfg' a
