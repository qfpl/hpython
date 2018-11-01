{-# language DataKinds, KindSignatures #-}
{-# language MultiParamTypeClasses, TemplateHaskell, FunctionalDependencies,
    FlexibleInstances #-}

{-|
Module      : Language.Python.Validate.Syntax.Error
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Syntax.Error where

-- import Control.Lens.TH
import Control.Lens.Type
import Control.Lens.Prism
import Language.Python.Syntax.Expr (Expr)
import Language.Python.Internal.Syntax.Ident (Ident)

data SyntaxError (v :: [*]) a
  = PositionalAfterKeywordArg a (Expr v a)
  | PositionalAfterKeywordUnpacking a (Expr v a)
  | PositionalAfterKeywordParam a String
  | UnexpectedDoubleStarParam a String
  | CannotAssignTo a (Expr v a)
  | CannotDelete a (Expr v a)
  | CannotAugAssignTo a (Expr v a)
  | DuplicateArgument a String
  | UnexpectedNewline a
  | UnexpectedComment a
  | IdentifierReservedWord a String
  | EmptyIdentifier a
  | BadCharacter a String
  | BreakOutsideLoop a
  | ContinueOutsideLoop a
  | ReturnOutsideFunction a
  | NonlocalOutsideFunction a
  | ParametersNonlocal a [String]
  | NoBindingNonlocal (Ident v a)
  | Can'tJoinStringAndBytes a
  | YieldOutsideGenerator a
  | MalformedDecorator a
  | InvalidDictUnpacking a
  | InvalidSetUnpacking a
  | TypedParamInLambda a
  | TypedUnnamedStarParam a
  | AsyncWithOutsideCoroutine a
  | AsyncForOutsideCoroutine a
  | YieldFromInsideCoroutine a
  | YieldInsideCoroutine a
  | AwaitOutsideCoroutine a
  | AwaitInsideComprehension a
  | NullByte a
  | NonAsciiInBytes a Char
  | DefaultExceptMustBeLast a
  | WildcardImportInDefinition a
  | NoKeywordsAfterEmptyStarArg a
  | ManyStarredTargets a
  | ContinueInsideFinally a
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_actrO v_act2q a_act2r | r_actrO -> v_act2q
                                                          a_act2r where
  _SyntaxError :: Prism' r_actrO (SyntaxError v_act2q a_act2r)
  _PositionalAfterKeywordArg ::
    Prism' r_actrO (a_act2r, Expr v_act2q a_act2r)
  _PositionalAfterKeywordUnpacking ::
    Prism' r_actrO (a_act2r, Expr v_act2q a_act2r)
  _PositionalAfterKeywordParam :: Prism' r_actrO (a_act2r, String)
  _UnexpectedDoubleStarParam :: Prism' r_actrO (a_act2r, String)
  _CannotAssignTo :: Prism' r_actrO (a_act2r, Expr v_act2q a_act2r)
  _CannotDelete :: Prism' r_actrO (a_act2r, Expr v_act2q a_act2r)
  _CannotAugAssignTo ::
    Prism' r_actrO (a_act2r, Expr v_act2q a_act2r)
  _DuplicateArgument :: Prism' r_actrO (a_act2r, String)
  _UnexpectedNewline :: Prism' r_actrO a_act2r
  _UnexpectedComment :: Prism' r_actrO a_act2r
  _IdentifierReservedWord :: Prism' r_actrO (a_act2r, String)
  _EmptyIdentifier :: Prism' r_actrO a_act2r
  _BadCharacter :: Prism' r_actrO (a_act2r, String)
  _BreakOutsideLoop :: Prism' r_actrO a_act2r
  _ContinueOutsideLoop :: Prism' r_actrO a_act2r
  _ReturnOutsideFunction :: Prism' r_actrO a_act2r
  _NonlocalOutsideFunction :: Prism' r_actrO a_act2r
  _ParametersNonlocal :: Prism' r_actrO (a_act2r, [String])
  _NoBindingNonlocal :: Prism' r_actrO (Ident v_act2q a_act2r)
  _Can'tJoinStringAndBytes :: Prism' r_actrO a_act2r
  _YieldOutsideGenerator :: Prism' r_actrO a_act2r
  _MalformedDecorator :: Prism' r_actrO a_act2r
  _InvalidDictUnpacking :: Prism' r_actrO a_act2r
  _InvalidSetUnpacking :: Prism' r_actrO a_act2r
  _TypedParamInLambda :: Prism' r_actrO a_act2r
  _TypedUnnamedStarParam :: Prism' r_actrO a_act2r
  _AsyncWithOutsideCoroutine :: Prism' r_actrO a_act2r
  _AsyncForOutsideCoroutine :: Prism' r_actrO a_act2r
  _YieldFromInsideCoroutine :: Prism' r_actrO a_act2r
  _YieldInsideCoroutine :: Prism' r_actrO a_act2r
  _AwaitOutsideCoroutine :: Prism' r_actrO a_act2r
  _AwaitInsideComprehension :: Prism' r_actrO a_act2r
  _NullByte :: Prism' r_actrO a_act2r
  _NonAsciiInBytes :: Prism' r_actrO (a_act2r, Char)
  _DefaultExceptMustBeLast :: Prism' r_actrO a_act2r
  _WildcardImportInDefinition :: Prism' r_actrO a_act2r
  _NoKeywordsAfterEmptyStarArg :: Prism' r_actrO a_act2r
  _ManyStarredTargets :: Prism' r_actrO a_act2r
  _ContinueInsideFinally :: Prism' r_actrO a_act2r
  _PositionalAfterKeywordArg
    = ((.) _SyntaxError) _PositionalAfterKeywordArg
  _PositionalAfterKeywordUnpacking
    = ((.) _SyntaxError) _PositionalAfterKeywordUnpacking
  _PositionalAfterKeywordParam
    = ((.) _SyntaxError) _PositionalAfterKeywordParam
  _UnexpectedDoubleStarParam
    = ((.) _SyntaxError) _UnexpectedDoubleStarParam
  _CannotAssignTo = ((.) _SyntaxError) _CannotAssignTo
  _CannotDelete = ((.) _SyntaxError) _CannotDelete
  _CannotAugAssignTo = ((.) _SyntaxError) _CannotAugAssignTo
  _DuplicateArgument = ((.) _SyntaxError) _DuplicateArgument
  _UnexpectedNewline = ((.) _SyntaxError) _UnexpectedNewline
  _UnexpectedComment = ((.) _SyntaxError) _UnexpectedComment
  _IdentifierReservedWord
    = ((.) _SyntaxError) _IdentifierReservedWord
  _EmptyIdentifier = ((.) _SyntaxError) _EmptyIdentifier
  _BadCharacter = ((.) _SyntaxError) _BadCharacter
  _BreakOutsideLoop = ((.) _SyntaxError) _BreakOutsideLoop
  _ContinueOutsideLoop = ((.) _SyntaxError) _ContinueOutsideLoop
  _ReturnOutsideFunction = ((.) _SyntaxError) _ReturnOutsideFunction
  _NonlocalOutsideFunction
    = ((.) _SyntaxError) _NonlocalOutsideFunction
  _ParametersNonlocal = ((.) _SyntaxError) _ParametersNonlocal
  _NoBindingNonlocal = ((.) _SyntaxError) _NoBindingNonlocal
  _Can'tJoinStringAndBytes
    = ((.) _SyntaxError) _Can'tJoinStringAndBytes
  _YieldOutsideGenerator = ((.) _SyntaxError) _YieldOutsideGenerator
  _MalformedDecorator = ((.) _SyntaxError) _MalformedDecorator
  _InvalidDictUnpacking = ((.) _SyntaxError) _InvalidDictUnpacking
  _InvalidSetUnpacking = ((.) _SyntaxError) _InvalidSetUnpacking
  _TypedParamInLambda = ((.) _SyntaxError) _TypedParamInLambda
  _TypedUnnamedStarParam = ((.) _SyntaxError) _TypedUnnamedStarParam
  _AsyncWithOutsideCoroutine
    = ((.) _SyntaxError) _AsyncWithOutsideCoroutine
  _AsyncForOutsideCoroutine
    = ((.) _SyntaxError) _AsyncForOutsideCoroutine
  _YieldFromInsideCoroutine
    = ((.) _SyntaxError) _YieldFromInsideCoroutine
  _YieldInsideCoroutine = ((.) _SyntaxError) _YieldInsideCoroutine
  _AwaitOutsideCoroutine = ((.) _SyntaxError) _AwaitOutsideCoroutine
  _AwaitInsideComprehension
    = ((.) _SyntaxError) _AwaitInsideComprehension
  _NullByte = ((.) _SyntaxError) _NullByte
  _NonAsciiInBytes = ((.) _SyntaxError) _NonAsciiInBytes
  _DefaultExceptMustBeLast
    = ((.) _SyntaxError) _DefaultExceptMustBeLast
  _WildcardImportInDefinition
    = ((.) _SyntaxError) _WildcardImportInDefinition
  _NoKeywordsAfterEmptyStarArg
    = ((.) _SyntaxError) _NoKeywordsAfterEmptyStarArg
  _ManyStarredTargets = ((.) _SyntaxError) _ManyStarredTargets
  _ContinueInsideFinally = ((.) _SyntaxError) _ContinueInsideFinally
instance AsSyntaxError (SyntaxError v_act2q a_act2r) v_act2q a_act2r where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (prism
          (\ (x1_actrP, x2_actrQ)
            -> (PositionalAfterKeywordArg x1_actrP) x2_actrQ))
        (\ x_actrR
            -> case x_actrR of
                PositionalAfterKeywordArg y1_actrS y2_actrT
                  -> Right (y1_actrS, y2_actrT)
                _ -> Left x_actrR)
  _PositionalAfterKeywordUnpacking
    = (prism
          (\ (x1_actrU, x2_actrV)
            -> (PositionalAfterKeywordUnpacking x1_actrU) x2_actrV))
        (\ x_actrW
            -> case x_actrW of
                PositionalAfterKeywordUnpacking y1_actrX y2_actrY
                  -> Right (y1_actrX, y2_actrY)
                _ -> Left x_actrW)
  _PositionalAfterKeywordParam
    = (prism
          (\ (x1_actrZ, x2_acts0)
            -> (PositionalAfterKeywordParam x1_actrZ) x2_acts0))
        (\ x_acts1
            -> case x_acts1 of
                PositionalAfterKeywordParam y1_acts2 y2_acts3
                  -> Right (y1_acts2, y2_acts3)
                _ -> Left x_acts1)
  _UnexpectedDoubleStarParam
    = (prism
          (\ (x1_acts4, x2_acts5)
            -> (UnexpectedDoubleStarParam x1_acts4) x2_acts5))
        (\ x_acts6
            -> case x_acts6 of
                UnexpectedDoubleStarParam y1_acts7 y2_acts8
                  -> Right (y1_acts7, y2_acts8)
                _ -> Left x_acts6)
  _CannotAssignTo
    = (prism
          (\ (x1_acts9, x2_actsa) -> (CannotAssignTo x1_acts9) x2_actsa))
        (\ x_actsb
            -> case x_actsb of
                CannotAssignTo y1_actsc y2_actsd -> Right (y1_actsc, y2_actsd)
                _ -> Left x_actsb)
  _CannotDelete
    = (prism
          (\ (x1_actse, x2_actsf) -> (CannotDelete x1_actse) x2_actsf))
        (\ x_actsg
            -> case x_actsg of
                CannotDelete y1_actsh y2_actsi -> Right (y1_actsh, y2_actsi)
                _ -> Left x_actsg)
  _CannotAugAssignTo
    = (prism
          (\ (x1_actsj, x2_actsk) -> (CannotAugAssignTo x1_actsj) x2_actsk))
        (\ x_actsl
            -> case x_actsl of
                CannotAugAssignTo y1_actsm y2_actsn -> Right (y1_actsm, y2_actsn)
                _ -> Left x_actsl)
  _DuplicateArgument
    = (prism
          (\ (x1_actso, x2_actsp) -> (DuplicateArgument x1_actso) x2_actsp))
        (\ x_actsq
            -> case x_actsq of
                DuplicateArgument y1_actsr y2_actss -> Right (y1_actsr, y2_actss)
                _ -> Left x_actsq)
  _UnexpectedNewline
    = (prism (\ x1_actsw -> UnexpectedNewline x1_actsw))
        (\ x_actsx
            -> case x_actsx of
                UnexpectedNewline y1_actsy -> Right y1_actsy
                _ -> Left x_actsx)
  _UnexpectedComment
    = (prism (\ x1_actsz -> UnexpectedComment x1_actsz))
        (\ x_actsA
            -> case x_actsA of
                UnexpectedComment y1_actsB -> Right y1_actsB
                _ -> Left x_actsA)
  _IdentifierReservedWord
    = (prism
          (\ (x1_actsC, x2_actsD)
            -> (IdentifierReservedWord x1_actsC) x2_actsD))
        (\ x_actsE
            -> case x_actsE of
                IdentifierReservedWord y1_actsF y2_actsG
                  -> Right (y1_actsF, y2_actsG)
                _ -> Left x_actsE)
  _EmptyIdentifier
    = (prism (\ x1_actsH -> EmptyIdentifier x1_actsH))
        (\ x_actsI
            -> case x_actsI of
                EmptyIdentifier y1_actsJ -> Right y1_actsJ
                _ -> Left x_actsI)
  _BadCharacter
    = (prism
          (\ (x1_actsK, x2_actsL) -> (BadCharacter x1_actsK) x2_actsL))
        (\ x_actsM
            -> case x_actsM of
                BadCharacter y1_actsN y2_actsO -> Right (y1_actsN, y2_actsO)
                _ -> Left x_actsM)
  _BreakOutsideLoop
    = (prism (\ x1_actsP -> BreakOutsideLoop x1_actsP))
        (\ x_actsQ
            -> case x_actsQ of
                BreakOutsideLoop y1_actsR -> Right y1_actsR
                _ -> Left x_actsQ)
  _ContinueOutsideLoop
    = (prism (\ x1_actsS -> ContinueOutsideLoop x1_actsS))
        (\ x_actsT
            -> case x_actsT of
                ContinueOutsideLoop y1_actsU -> Right y1_actsU
                _ -> Left x_actsT)
  _ReturnOutsideFunction
    = (prism (\ x1_actsV -> ReturnOutsideFunction x1_actsV))
        (\ x_actsW
            -> case x_actsW of
                ReturnOutsideFunction y1_actsX -> Right y1_actsX
                _ -> Left x_actsW)
  _NonlocalOutsideFunction
    = (prism (\ x1_actsY -> NonlocalOutsideFunction x1_actsY))
        (\ x_actsZ
            -> case x_actsZ of
                NonlocalOutsideFunction y1_actt0 -> Right y1_actt0
                _ -> Left x_actsZ)
  _ParametersNonlocal
    = (prism
          (\ (x1_actt1, x2_actt2) -> (ParametersNonlocal x1_actt1) x2_actt2))
        (\ x_actt3
            -> case x_actt3 of
                ParametersNonlocal y1_actt4 y2_actt5 -> Right (y1_actt4, y2_actt5)
                _ -> Left x_actt3)
  _NoBindingNonlocal
    = (prism (\ x1_actt6 -> NoBindingNonlocal x1_actt6))
        (\ x_actt7
            -> case x_actt7 of
                NoBindingNonlocal y1_actt8 -> Right y1_actt8
                _ -> Left x_actt7)
  _Can'tJoinStringAndBytes
    = (prism (\ x1_actt9 -> Can'tJoinStringAndBytes x1_actt9))
        (\ x_actta
            -> case x_actta of
                Can'tJoinStringAndBytes y1_acttb -> Right y1_acttb
                _ -> Left x_actta)
  _YieldOutsideGenerator
    = (prism (\ x1_acttc -> YieldOutsideGenerator x1_acttc))
        (\ x_acttd
            -> case x_acttd of
                YieldOutsideGenerator y1_actte -> Right y1_actte
                _ -> Left x_acttd)
  _MalformedDecorator
    = (prism (\ x1_acttf -> MalformedDecorator x1_acttf))
        (\ x_acttg
            -> case x_acttg of
                MalformedDecorator y1_actth -> Right y1_actth
                _ -> Left x_acttg)
  _InvalidDictUnpacking
    = (prism (\ x1_actti -> InvalidDictUnpacking x1_actti))
        (\ x_acttj
            -> case x_acttj of
                InvalidDictUnpacking y1_acttk -> Right y1_acttk
                _ -> Left x_acttj)
  _InvalidSetUnpacking
    = (prism (\ x1_acttl -> InvalidSetUnpacking x1_acttl))
        (\ x_acttm
            -> case x_acttm of
                InvalidSetUnpacking y1_acttn -> Right y1_acttn
                _ -> Left x_acttm)
  _TypedParamInLambda
    = (prism (\ x1_actto -> TypedParamInLambda x1_actto))
        (\ x_acttp
            -> case x_acttp of
                TypedParamInLambda y1_acttq -> Right y1_acttq
                _ -> Left x_acttp)
  _TypedUnnamedStarParam
    = (prism (\ x1_acttr -> TypedUnnamedStarParam x1_acttr))
        (\ x_actts
            -> case x_actts of
                TypedUnnamedStarParam y1_acttt -> Right y1_acttt
                _ -> Left x_actts)
  _AsyncWithOutsideCoroutine
    = (prism (\ x1_acttu -> AsyncWithOutsideCoroutine x1_acttu))
        (\ x_acttv
            -> case x_acttv of
                AsyncWithOutsideCoroutine y1_acttw -> Right y1_acttw
                _ -> Left x_acttv)
  _AsyncForOutsideCoroutine
    = (prism (\ x1_acttx -> AsyncForOutsideCoroutine x1_acttx))
        (\ x_actty
            -> case x_actty of
                AsyncForOutsideCoroutine y1_acttz -> Right y1_acttz
                _ -> Left x_actty)
  _YieldFromInsideCoroutine
    = (prism (\ x1_acttA -> YieldFromInsideCoroutine x1_acttA))
        (\ x_acttB
            -> case x_acttB of
                YieldFromInsideCoroutine y1_acttC -> Right y1_acttC
                _ -> Left x_acttB)
  _YieldInsideCoroutine
    = (prism (\ x1_acttD -> YieldInsideCoroutine x1_acttD))
        (\ x_acttE
            -> case x_acttE of
                YieldInsideCoroutine y1_acttF -> Right y1_acttF
                _ -> Left x_acttE)
  _AwaitOutsideCoroutine
    = (prism (\ x1_acttG -> AwaitOutsideCoroutine x1_acttG))
        (\ x_acttH
            -> case x_acttH of
                AwaitOutsideCoroutine y1_acttI -> Right y1_acttI
                _ -> Left x_acttH)
  _AwaitInsideComprehension
    = (prism (\ x1_acttJ -> AwaitInsideComprehension x1_acttJ))
        (\ x_acttK
            -> case x_acttK of
                AwaitInsideComprehension y1_acttL -> Right y1_acttL
                _ -> Left x_acttK)
  _NullByte
    = (prism (\ x1_acttM -> NullByte x1_acttM))
        (\ x_acttN
            -> case x_acttN of
                NullByte y1_acttO -> Right y1_acttO
                _ -> Left x_acttN)
  _NonAsciiInBytes
    = (prism
          (\ (x1_acttP, x2_acttQ) -> (NonAsciiInBytes x1_acttP) x2_acttQ))
        (\ x_acttR
            -> case x_acttR of
                NonAsciiInBytes y1_acttS y2_acttT -> Right (y1_acttS, y2_acttT)
                _ -> Left x_acttR)
  _DefaultExceptMustBeLast
    = (prism (\ x1_acttU -> DefaultExceptMustBeLast x1_acttU))
        (\ x_acttV
            -> case x_acttV of
                DefaultExceptMustBeLast y1_acttW -> Right y1_acttW
                _ -> Left x_acttV)
  _WildcardImportInDefinition
    = (prism (\ x1_acttX -> WildcardImportInDefinition x1_acttX))
        (\ x_acttY
            -> case x_acttY of
                WildcardImportInDefinition y1_acttZ -> Right y1_acttZ
                _ -> Left x_acttY)
  _NoKeywordsAfterEmptyStarArg
    = (prism (\ x1_actu0 -> NoKeywordsAfterEmptyStarArg x1_actu0))
        (\ x_actu1
            -> case x_actu1 of
                NoKeywordsAfterEmptyStarArg y1_actu2 -> Right y1_actu2
                _ -> Left x_actu1)
  _ManyStarredTargets
    = (prism (\ x1_actu3 -> ManyStarredTargets x1_actu3))
        (\ x_actu4
            -> case x_actu4 of
                ManyStarredTargets y1_actu5 -> Right y1_actu5
                _ -> Left x_actu4)
  _ContinueInsideFinally
    = (prism (\ x1_actu6 -> ContinueInsideFinally x1_actu6))
        (\ x_actu7
            -> case x_actu7 of
                ContinueInsideFinally y1_actu8 -> Right y1_actu8
                _ -> Left x_actu7)
