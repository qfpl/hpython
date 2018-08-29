{-# language DataKinds, KindSignatures #-}
{-# language MultiParamTypeClasses, TemplateHaskell, FunctionalDependencies,
    FlexibleInstances #-}
module Language.Python.Validate.Syntax.Error where

-- import Control.Lens.TH
import Control.Lens.Type
import Control.Lens.Prism
import Language.Python.Internal.Syntax

data SyntaxError (v :: [*]) a
  = PositionalAfterKeywordArg a (Expr v a)
  | PositionalAfterKeywordUnpacking a (Expr v a)
  | PositionalAfterKeywordParam a String
  | UnexpectedDoubleStarParam a String
  | CannotAssignTo a (Expr v a)
  | CannotAugAssignTo a (Expr v a)
  | DuplicateArgument a String
  | ExpectedNewlineAfter (a, [Whitespace], Statement v a, Maybe Newline)
  | UnexpectedNewline a
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
  | InvalidYield a
  | CommentAfterBackslash a
  | MalformedDecorator a
  | InvalidDictUnpacking a
  | InvalidSetUnpacking a
  | TypedParamInLambda a
  | TypedUnnamedStarParam a
  | AsyncWithOutsideCoroutine a
  | AsyncForOutsideCoroutine a
  | YieldFromInsideCoroutine a
  | AwaitOutsideCoroutine a
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_abzOd v_abztz a_abztA | r_abzOd -> v_abztz
                                                          a_abztA where
  _SyntaxError ::
    Control.Lens.Type.Prism' r_abzOd (SyntaxError v_abztz a_abztA)
  _PositionalAfterKeywordArg ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, Expr v_abztz a_abztA)
  _PositionalAfterKeywordUnpacking ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, Expr v_abztz a_abztA)
  _PositionalAfterKeywordParam ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, String)
  _UnexpectedDoubleStarParam ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, String)
  _CannotAssignTo ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, Expr v_abztz a_abztA)
  _CannotAugAssignTo ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, Expr v_abztz a_abztA)
  _DuplicateArgument ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, String)
  _ExpectedNewlineAfter ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, [Whitespace],
                                      Statement v_abztz a_abztA, Maybe Newline)
  _UnexpectedNewline :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _IdentifierReservedWord ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, String)
  _EmptyIdentifier :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _BadCharacter :: Control.Lens.Type.Prism' r_abzOd (a_abztA, String)
  _BreakOutsideLoop :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _ContinueOutsideLoop :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _ReturnOutsideFunction :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _NonlocalOutsideFunction ::
    Control.Lens.Type.Prism' r_abzOd a_abztA
  _ParametersNonlocal ::
    Control.Lens.Type.Prism' r_abzOd (a_abztA, [String])
  _NoBindingNonlocal ::
    Control.Lens.Type.Prism' r_abzOd (Ident v_abztz a_abztA)
  _Can'tJoinStringAndBytes ::
    Control.Lens.Type.Prism' r_abzOd a_abztA
  _InvalidYield :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _CommentAfterBackslash :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _MalformedDecorator :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _InvalidDictUnpacking :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _InvalidSetUnpacking :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _TypedParamInLambda :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _TypedUnnamedStarParam :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _AsyncWithOutsideCoroutine ::
    Control.Lens.Type.Prism' r_abzOd a_abztA
  _AsyncForOutsideCoroutine ::
    Control.Lens.Type.Prism' r_abzOd a_abztA
  _YieldFromInsideCoroutine ::
    Control.Lens.Type.Prism' r_abzOd a_abztA
  _AwaitOutsideCoroutine :: Control.Lens.Type.Prism' r_abzOd a_abztA
  _PositionalAfterKeywordArg
    = ((.) _SyntaxError) _PositionalAfterKeywordArg
  _PositionalAfterKeywordUnpacking
    = ((.) _SyntaxError) _PositionalAfterKeywordUnpacking
  _PositionalAfterKeywordParam
    = ((.) _SyntaxError) _PositionalAfterKeywordParam
  _UnexpectedDoubleStarParam
    = ((.) _SyntaxError) _UnexpectedDoubleStarParam
  _CannotAssignTo = ((.) _SyntaxError) _CannotAssignTo
  _CannotAugAssignTo = ((.) _SyntaxError) _CannotAugAssignTo
  _DuplicateArgument = ((.) _SyntaxError) _DuplicateArgument
  _ExpectedNewlineAfter = ((.) _SyntaxError) _ExpectedNewlineAfter
  _UnexpectedNewline = ((.) _SyntaxError) _UnexpectedNewline
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
  _InvalidYield = ((.) _SyntaxError) _InvalidYield
  _CommentAfterBackslash = ((.) _SyntaxError) _CommentAfterBackslash
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
  _AwaitOutsideCoroutine = ((.) _SyntaxError) _AwaitOutsideCoroutine
instance AsSyntaxError (SyntaxError v_abztz a_abztA) v_abztz a_abztA where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (Control.Lens.Prism.prism
          (\ (x1_abzOe, x2_abzOf)
            -> (PositionalAfterKeywordArg x1_abzOe) x2_abzOf))
        (\ x_abzOg
            -> case x_abzOg of
                PositionalAfterKeywordArg y1_abzOh y2_abzOi
                  -> Right (y1_abzOh, y2_abzOi)
                _ -> Left x_abzOg)
  _PositionalAfterKeywordUnpacking
    = (Control.Lens.Prism.prism
          (\ (x1_abzOj, x2_abzOk)
            -> (PositionalAfterKeywordUnpacking x1_abzOj) x2_abzOk))
        (\ x_abzOl
            -> case x_abzOl of
                PositionalAfterKeywordUnpacking y1_abzOm y2_abzOn
                  -> Right (y1_abzOm, y2_abzOn)
                _ -> Left x_abzOl)
  _PositionalAfterKeywordParam
    = (Control.Lens.Prism.prism
          (\ (x1_abzOo, x2_abzOp)
            -> (PositionalAfterKeywordParam x1_abzOo) x2_abzOp))
        (\ x_abzOq
            -> case x_abzOq of
                PositionalAfterKeywordParam y1_abzOr y2_abzOs
                  -> Right (y1_abzOr, y2_abzOs)
                _ -> Left x_abzOq)
  _UnexpectedDoubleStarParam
    = (Control.Lens.Prism.prism
          (\ (x1_abzOt, x2_abzOu)
            -> (UnexpectedDoubleStarParam x1_abzOt) x2_abzOu))
        (\ x_abzOv
            -> case x_abzOv of
                UnexpectedDoubleStarParam y1_abzOw y2_abzOx
                  -> Right (y1_abzOw, y2_abzOx)
                _ -> Left x_abzOv)
  _CannotAssignTo
    = (Control.Lens.Prism.prism
          (\ (x1_abzOy, x2_abzOz) -> (CannotAssignTo x1_abzOy) x2_abzOz))
        (\ x_abzOA
            -> case x_abzOA of
                CannotAssignTo y1_abzOB y2_abzOC -> Right (y1_abzOB, y2_abzOC)
                _ -> Left x_abzOA)
  _CannotAugAssignTo
    = (Control.Lens.Prism.prism
          (\ (x1_abzOD, x2_abzOE) -> (CannotAugAssignTo x1_abzOD) x2_abzOE))
        (\ x_abzOF
            -> case x_abzOF of
                CannotAugAssignTo y1_abzOG y2_abzOH -> Right (y1_abzOG, y2_abzOH)
                _ -> Left x_abzOF)
  _DuplicateArgument
    = (Control.Lens.Prism.prism
          (\ (x1_abzOI, x2_abzOJ) -> (DuplicateArgument x1_abzOI) x2_abzOJ))
        (\ x_abzOK
            -> case x_abzOK of
                DuplicateArgument y1_abzOL y2_abzOM -> Right (y1_abzOL, y2_abzOM)
                _ -> Left x_abzOK)
  _ExpectedNewlineAfter
    = (Control.Lens.Prism.prism
          (\ x1_abzON -> ExpectedNewlineAfter x1_abzON))
        (\ x_abzOO
            -> case x_abzOO of
                ExpectedNewlineAfter y1_abzOP -> Right y1_abzOP
                _ -> Left x_abzOO)
  _UnexpectedNewline
    = (Control.Lens.Prism.prism
          (\ x1_abzOQ -> UnexpectedNewline x1_abzOQ))
        (\ x_abzOR
            -> case x_abzOR of
                UnexpectedNewline y1_abzOS -> Right y1_abzOS
                _ -> Left x_abzOR)
  _IdentifierReservedWord
    = (Control.Lens.Prism.prism
          (\ (x1_abzOT, x2_abzOU)
            -> (IdentifierReservedWord x1_abzOT) x2_abzOU))
        (\ x_abzOV
            -> case x_abzOV of
                IdentifierReservedWord y1_abzOW y2_abzOX
                  -> Right (y1_abzOW, y2_abzOX)
                _ -> Left x_abzOV)
  _EmptyIdentifier
    = (Control.Lens.Prism.prism
          (\ x1_abzOY -> EmptyIdentifier x1_abzOY))
        (\ x_abzOZ
            -> case x_abzOZ of
                EmptyIdentifier y1_abzP0 -> Right y1_abzP0
                _ -> Left x_abzOZ)
  _BadCharacter
    = (Control.Lens.Prism.prism
          (\ (x1_abzP1, x2_abzP2) -> (BadCharacter x1_abzP1) x2_abzP2))
        (\ x_abzP3
            -> case x_abzP3 of
                BadCharacter y1_abzP4 y2_abzP5 -> Right (y1_abzP4, y2_abzP5)
                _ -> Left x_abzP3)
  _BreakOutsideLoop
    = (Control.Lens.Prism.prism
          (\ x1_abzP6 -> BreakOutsideLoop x1_abzP6))
        (\ x_abzP7
            -> case x_abzP7 of
                BreakOutsideLoop y1_abzP8 -> Right y1_abzP8
                _ -> Left x_abzP7)
  _ContinueOutsideLoop
    = (Control.Lens.Prism.prism
          (\ x1_abzP9 -> ContinueOutsideLoop x1_abzP9))
        (\ x_abzPa
            -> case x_abzPa of
                ContinueOutsideLoop y1_abzPb -> Right y1_abzPb
                _ -> Left x_abzPa)
  _ReturnOutsideFunction
    = (Control.Lens.Prism.prism
          (\ x1_abzPc -> ReturnOutsideFunction x1_abzPc))
        (\ x_abzPd
            -> case x_abzPd of
                ReturnOutsideFunction y1_abzPe -> Right y1_abzPe
                _ -> Left x_abzPd)
  _NonlocalOutsideFunction
    = (Control.Lens.Prism.prism
          (\ x1_abzPf -> NonlocalOutsideFunction x1_abzPf))
        (\ x_abzPg
            -> case x_abzPg of
                NonlocalOutsideFunction y1_abzPh -> Right y1_abzPh
                _ -> Left x_abzPg)
  _ParametersNonlocal
    = (Control.Lens.Prism.prism
          (\ (x1_abzPi, x2_abzPj) -> (ParametersNonlocal x1_abzPi) x2_abzPj))
        (\ x_abzPk
            -> case x_abzPk of
                ParametersNonlocal y1_abzPl y2_abzPm -> Right (y1_abzPl, y2_abzPm)
                _ -> Left x_abzPk)
  _NoBindingNonlocal
    = (Control.Lens.Prism.prism
          (\ x1_abzPn -> NoBindingNonlocal x1_abzPn))
        (\ x_abzPo
            -> case x_abzPo of
                NoBindingNonlocal y1_abzPp -> Right y1_abzPp
                _ -> Left x_abzPo)
  _Can'tJoinStringAndBytes
    = (Control.Lens.Prism.prism
          (\ x1_abzPq -> Can'tJoinStringAndBytes x1_abzPq))
        (\ x_abzPr
            -> case x_abzPr of
                Can'tJoinStringAndBytes y1_abzPs -> Right y1_abzPs
                _ -> Left x_abzPr)
  _InvalidYield
    = (Control.Lens.Prism.prism (\ x1_abzPt -> InvalidYield x1_abzPt))
        (\ x_abzPu
            -> case x_abzPu of
                InvalidYield y1_abzPv -> Right y1_abzPv
                _ -> Left x_abzPu)
  _CommentAfterBackslash
    = (Control.Lens.Prism.prism
          (\ x1_abzPw -> CommentAfterBackslash x1_abzPw))
        (\ x_abzPx
            -> case x_abzPx of
                CommentAfterBackslash y1_abzPy -> Right y1_abzPy
                _ -> Left x_abzPx)
  _MalformedDecorator
    = (Control.Lens.Prism.prism
          (\ x1_abzPz -> MalformedDecorator x1_abzPz))
        (\ x_abzPA
            -> case x_abzPA of
                MalformedDecorator y1_abzPB -> Right y1_abzPB
                _ -> Left x_abzPA)
  _InvalidDictUnpacking
    = (Control.Lens.Prism.prism
          (\ x1_abzPC -> InvalidDictUnpacking x1_abzPC))
        (\ x_abzPD
            -> case x_abzPD of
                InvalidDictUnpacking y1_abzPE -> Right y1_abzPE
                _ -> Left x_abzPD)
  _InvalidSetUnpacking
    = (Control.Lens.Prism.prism
          (\ x1_abzPF -> InvalidSetUnpacking x1_abzPF))
        (\ x_abzPG
            -> case x_abzPG of
                InvalidSetUnpacking y1_abzPH -> Right y1_abzPH
                _ -> Left x_abzPG)
  _TypedParamInLambda
    = (Control.Lens.Prism.prism
          (\ x1_abzPI -> TypedParamInLambda x1_abzPI))
        (\ x_abzPJ
            -> case x_abzPJ of
                TypedParamInLambda y1_abzPK -> Right y1_abzPK
                _ -> Left x_abzPJ)
  _TypedUnnamedStarParam
    = (Control.Lens.Prism.prism
          (\ x1_abzPL -> TypedUnnamedStarParam x1_abzPL))
        (\ x_abzPM
            -> case x_abzPM of
                TypedUnnamedStarParam y1_abzPN -> Right y1_abzPN
                _ -> Left x_abzPM)
  _AsyncWithOutsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_abzPO -> AsyncWithOutsideCoroutine x1_abzPO))
        (\ x_abzPP
            -> case x_abzPP of
                AsyncWithOutsideCoroutine y1_abzPQ -> Right y1_abzPQ
                _ -> Left x_abzPP)
  _AsyncForOutsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_abzPR -> AsyncForOutsideCoroutine x1_abzPR))
        (\ x_abzPS
            -> case x_abzPS of
                AsyncForOutsideCoroutine y1_abzPT -> Right y1_abzPT
                _ -> Left x_abzPS)
  _YieldFromInsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_abzPU -> YieldFromInsideCoroutine x1_abzPU))
        (\ x_abzPV
            -> case x_abzPV of
                YieldFromInsideCoroutine y1_abzPW -> Right y1_abzPW
                _ -> Left x_abzPV)
  _AwaitOutsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_abzPX -> AwaitOutsideCoroutine x1_abzPX))
        (\ x_abzPY
            -> case x_abzPY of
                AwaitOutsideCoroutine y1_abzPZ -> Right y1_abzPZ
                _ -> Left x_abzPY)
