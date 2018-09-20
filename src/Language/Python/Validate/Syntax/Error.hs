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
  | CannotDelete a (Expr v a)
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
  | NullByte a
  | NonAsciiInBytes a Char
  | DefaultExceptMustBeLast a
  | WildcardImportInDefinition a
  | NoKeywordsAfterEmptyStarArg a
  | ManyStarredTargets a
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_acuEq v_acugI a_acugJ | r_acuEq -> v_acugI
                                                          a_acugJ where
  _SyntaxError :: Prism' r_acuEq (SyntaxError v_acugI a_acugJ)
  _PositionalAfterKeywordArg ::
    Prism' r_acuEq (a_acugJ, Expr v_acugI a_acugJ)
  _PositionalAfterKeywordUnpacking ::
    Prism' r_acuEq (a_acugJ, Expr v_acugI a_acugJ)
  _PositionalAfterKeywordParam :: Prism' r_acuEq (a_acugJ, String)
  _UnexpectedDoubleStarParam :: Prism' r_acuEq (a_acugJ, String)
  _CannotAssignTo :: Prism' r_acuEq (a_acugJ, Expr v_acugI a_acugJ)
  _CannotDelete :: Prism' r_acuEq (a_acugJ, Expr v_acugI a_acugJ)
  _CannotAugAssignTo ::
    Prism' r_acuEq (a_acugJ, Expr v_acugI a_acugJ)
  _DuplicateArgument :: Prism' r_acuEq (a_acugJ, String)
  _ExpectedNewlineAfter ::
    Prism' r_acuEq (a_acugJ, [Whitespace], Statement v_acugI a_acugJ,
                    Maybe Newline)
  _UnexpectedNewline :: Prism' r_acuEq a_acugJ
  _IdentifierReservedWord :: Prism' r_acuEq (a_acugJ, String)
  _EmptyIdentifier :: Prism' r_acuEq a_acugJ
  _BadCharacter :: Prism' r_acuEq (a_acugJ, String)
  _BreakOutsideLoop :: Prism' r_acuEq a_acugJ
  _ContinueOutsideLoop :: Prism' r_acuEq a_acugJ
  _ReturnOutsideFunction :: Prism' r_acuEq a_acugJ
  _NonlocalOutsideFunction :: Prism' r_acuEq a_acugJ
  _ParametersNonlocal :: Prism' r_acuEq (a_acugJ, [String])
  _NoBindingNonlocal :: Prism' r_acuEq (Ident v_acugI a_acugJ)
  _Can'tJoinStringAndBytes :: Prism' r_acuEq a_acugJ
  _InvalidYield :: Prism' r_acuEq a_acugJ
  _CommentAfterBackslash :: Prism' r_acuEq a_acugJ
  _MalformedDecorator :: Prism' r_acuEq a_acugJ
  _InvalidDictUnpacking :: Prism' r_acuEq a_acugJ
  _InvalidSetUnpacking :: Prism' r_acuEq a_acugJ
  _TypedParamInLambda :: Prism' r_acuEq a_acugJ
  _TypedUnnamedStarParam :: Prism' r_acuEq a_acugJ
  _AsyncWithOutsideCoroutine :: Prism' r_acuEq a_acugJ
  _AsyncForOutsideCoroutine :: Prism' r_acuEq a_acugJ
  _YieldFromInsideCoroutine :: Prism' r_acuEq a_acugJ
  _AwaitOutsideCoroutine :: Prism' r_acuEq a_acugJ
  _NullByte :: Prism' r_acuEq a_acugJ
  _NonAsciiInBytes :: Prism' r_acuEq (a_acugJ, Char)
  _DefaultExceptMustBeLast :: Prism' r_acuEq a_acugJ
  _WildcardImportInDefinition :: Prism' r_acuEq a_acugJ
  _NoKeywordsAfterEmptyStarArg :: Prism' r_acuEq a_acugJ
  _ManyStarredTargets :: Prism' r_acuEq a_acugJ
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
  _NullByte = ((.) _SyntaxError) _NullByte
  _NonAsciiInBytes = ((.) _SyntaxError) _NonAsciiInBytes
  _DefaultExceptMustBeLast
    = ((.) _SyntaxError) _DefaultExceptMustBeLast
  _WildcardImportInDefinition
    = ((.) _SyntaxError) _WildcardImportInDefinition
  _NoKeywordsAfterEmptyStarArg
    = ((.) _SyntaxError) _NoKeywordsAfterEmptyStarArg
  _ManyStarredTargets = ((.) _SyntaxError) _ManyStarredTargets
instance AsSyntaxError (SyntaxError v_acugI a_acugJ) v_acugI a_acugJ where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (prism
          (\ (x1_acuEr, x2_acuEs)
            -> (PositionalAfterKeywordArg x1_acuEr) x2_acuEs))
        (\ x_acuEt
            -> case x_acuEt of
                PositionalAfterKeywordArg y1_acuEu y2_acuEv
                  -> Right (y1_acuEu, y2_acuEv)
                _ -> Left x_acuEt)
  _PositionalAfterKeywordUnpacking
    = (prism
          (\ (x1_acuEw, x2_acuEx)
            -> (PositionalAfterKeywordUnpacking x1_acuEw) x2_acuEx))
        (\ x_acuEy
            -> case x_acuEy of
                PositionalAfterKeywordUnpacking y1_acuEz y2_acuEA
                  -> Right (y1_acuEz, y2_acuEA)
                _ -> Left x_acuEy)
  _PositionalAfterKeywordParam
    = (prism
          (\ (x1_acuEB, x2_acuEC)
            -> (PositionalAfterKeywordParam x1_acuEB) x2_acuEC))
        (\ x_acuED
            -> case x_acuED of
                PositionalAfterKeywordParam y1_acuEE y2_acuEF
                  -> Right (y1_acuEE, y2_acuEF)
                _ -> Left x_acuED)
  _UnexpectedDoubleStarParam
    = (prism
          (\ (x1_acuEG, x2_acuEH)
            -> (UnexpectedDoubleStarParam x1_acuEG) x2_acuEH))
        (\ x_acuEI
            -> case x_acuEI of
                UnexpectedDoubleStarParam y1_acuEJ y2_acuEK
                  -> Right (y1_acuEJ, y2_acuEK)
                _ -> Left x_acuEI)
  _CannotAssignTo
    = (prism
          (\ (x1_acuEL, x2_acuEM) -> (CannotAssignTo x1_acuEL) x2_acuEM))
        (\ x_acuEN
            -> case x_acuEN of
                CannotAssignTo y1_acuEO y2_acuEP -> Right (y1_acuEO, y2_acuEP)
                _ -> Left x_acuEN)
  _CannotDelete
    = (prism
          (\ (x1_acuEQ, x2_acuER) -> (CannotDelete x1_acuEQ) x2_acuER))
        (\ x_acuES
            -> case x_acuES of
                CannotDelete y1_acuET y2_acuEU -> Right (y1_acuET, y2_acuEU)
                _ -> Left x_acuES)
  _CannotAugAssignTo
    = (prism
          (\ (x1_acuEV, x2_acuEW) -> (CannotAugAssignTo x1_acuEV) x2_acuEW))
        (\ x_acuEX
            -> case x_acuEX of
                CannotAugAssignTo y1_acuEY y2_acuEZ -> Right (y1_acuEY, y2_acuEZ)
                _ -> Left x_acuEX)
  _DuplicateArgument
    = (prism
          (\ (x1_acuF0, x2_acuF1) -> (DuplicateArgument x1_acuF0) x2_acuF1))
        (\ x_acuF2
            -> case x_acuF2 of
                DuplicateArgument y1_acuF3 y2_acuF4 -> Right (y1_acuF3, y2_acuF4)
                _ -> Left x_acuF2)
  _ExpectedNewlineAfter
    = (prism (\ x1_acuF5 -> ExpectedNewlineAfter x1_acuF5))
        (\ x_acuF6
            -> case x_acuF6 of
                ExpectedNewlineAfter y1_acuF7 -> Right y1_acuF7
                _ -> Left x_acuF6)
  _UnexpectedNewline
    = (prism (\ x1_acuF8 -> UnexpectedNewline x1_acuF8))
        (\ x_acuF9
            -> case x_acuF9 of
                UnexpectedNewline y1_acuFa -> Right y1_acuFa
                _ -> Left x_acuF9)
  _IdentifierReservedWord
    = (prism
          (\ (x1_acuFb, x2_acuFc)
            -> (IdentifierReservedWord x1_acuFb) x2_acuFc))
        (\ x_acuFd
            -> case x_acuFd of
                IdentifierReservedWord y1_acuFe y2_acuFf
                  -> Right (y1_acuFe, y2_acuFf)
                _ -> Left x_acuFd)
  _EmptyIdentifier
    = (prism (\ x1_acuFg -> EmptyIdentifier x1_acuFg))
        (\ x_acuFh
            -> case x_acuFh of
                EmptyIdentifier y1_acuFi -> Right y1_acuFi
                _ -> Left x_acuFh)
  _BadCharacter
    = (prism
          (\ (x1_acuFj, x2_acuFk) -> (BadCharacter x1_acuFj) x2_acuFk))
        (\ x_acuFl
            -> case x_acuFl of
                BadCharacter y1_acuFm y2_acuFn -> Right (y1_acuFm, y2_acuFn)
                _ -> Left x_acuFl)
  _BreakOutsideLoop
    = (prism (\ x1_acuFo -> BreakOutsideLoop x1_acuFo))
        (\ x_acuFp
            -> case x_acuFp of
                BreakOutsideLoop y1_acuFq -> Right y1_acuFq
                _ -> Left x_acuFp)
  _ContinueOutsideLoop
    = (prism (\ x1_acuFr -> ContinueOutsideLoop x1_acuFr))
        (\ x_acuFs
            -> case x_acuFs of
                ContinueOutsideLoop y1_acuFt -> Right y1_acuFt
                _ -> Left x_acuFs)
  _ReturnOutsideFunction
    = (prism (\ x1_acuFu -> ReturnOutsideFunction x1_acuFu))
        (\ x_acuFv
            -> case x_acuFv of
                ReturnOutsideFunction y1_acuFw -> Right y1_acuFw
                _ -> Left x_acuFv)
  _NonlocalOutsideFunction
    = (prism (\ x1_acuFx -> NonlocalOutsideFunction x1_acuFx))
        (\ x_acuFy
            -> case x_acuFy of
                NonlocalOutsideFunction y1_acuFz -> Right y1_acuFz
                _ -> Left x_acuFy)
  _ParametersNonlocal
    = (prism
          (\ (x1_acuFA, x2_acuFB) -> (ParametersNonlocal x1_acuFA) x2_acuFB))
        (\ x_acuFC
            -> case x_acuFC of
                ParametersNonlocal y1_acuFD y2_acuFE -> Right (y1_acuFD, y2_acuFE)
                _ -> Left x_acuFC)
  _NoBindingNonlocal
    = (prism (\ x1_acuFF -> NoBindingNonlocal x1_acuFF))
        (\ x_acuFG
            -> case x_acuFG of
                NoBindingNonlocal y1_acuFH -> Right y1_acuFH
                _ -> Left x_acuFG)
  _Can'tJoinStringAndBytes
    = (prism (\ x1_acuFI -> Can'tJoinStringAndBytes x1_acuFI))
        (\ x_acuFJ
            -> case x_acuFJ of
                Can'tJoinStringAndBytes y1_acuFK -> Right y1_acuFK
                _ -> Left x_acuFJ)
  _InvalidYield
    = (prism (\ x1_acuFL -> InvalidYield x1_acuFL))
        (\ x_acuFM
            -> case x_acuFM of
                InvalidYield y1_acuFN -> Right y1_acuFN
                _ -> Left x_acuFM)
  _CommentAfterBackslash
    = (prism (\ x1_acuFO -> CommentAfterBackslash x1_acuFO))
        (\ x_acuFP
            -> case x_acuFP of
                CommentAfterBackslash y1_acuFQ -> Right y1_acuFQ
                _ -> Left x_acuFP)
  _MalformedDecorator
    = (prism (\ x1_acuFR -> MalformedDecorator x1_acuFR))
        (\ x_acuFS
            -> case x_acuFS of
                MalformedDecorator y1_acuFT -> Right y1_acuFT
                _ -> Left x_acuFS)
  _InvalidDictUnpacking
    = (prism (\ x1_acuFU -> InvalidDictUnpacking x1_acuFU))
        (\ x_acuFV
            -> case x_acuFV of
                InvalidDictUnpacking y1_acuFW -> Right y1_acuFW
                _ -> Left x_acuFV)
  _InvalidSetUnpacking
    = (prism (\ x1_acuFX -> InvalidSetUnpacking x1_acuFX))
        (\ x_acuFY
            -> case x_acuFY of
                InvalidSetUnpacking y1_acuFZ -> Right y1_acuFZ
                _ -> Left x_acuFY)
  _TypedParamInLambda
    = (prism (\ x1_acuG0 -> TypedParamInLambda x1_acuG0))
        (\ x_acuG1
            -> case x_acuG1 of
                TypedParamInLambda y1_acuG2 -> Right y1_acuG2
                _ -> Left x_acuG1)
  _TypedUnnamedStarParam
    = (prism (\ x1_acuG3 -> TypedUnnamedStarParam x1_acuG3))
        (\ x_acuG4
            -> case x_acuG4 of
                TypedUnnamedStarParam y1_acuG5 -> Right y1_acuG5
                _ -> Left x_acuG4)
  _AsyncWithOutsideCoroutine
    = (prism (\ x1_acuG6 -> AsyncWithOutsideCoroutine x1_acuG6))
        (\ x_acuG7
            -> case x_acuG7 of
                AsyncWithOutsideCoroutine y1_acuG8 -> Right y1_acuG8
                _ -> Left x_acuG7)
  _AsyncForOutsideCoroutine
    = (prism (\ x1_acuG9 -> AsyncForOutsideCoroutine x1_acuG9))
        (\ x_acuGa
            -> case x_acuGa of
                AsyncForOutsideCoroutine y1_acuGb -> Right y1_acuGb
                _ -> Left x_acuGa)
  _YieldFromInsideCoroutine
    = (prism (\ x1_acuGc -> YieldFromInsideCoroutine x1_acuGc))
        (\ x_acuGd
            -> case x_acuGd of
                YieldFromInsideCoroutine y1_acuGe -> Right y1_acuGe
                _ -> Left x_acuGd)
  _AwaitOutsideCoroutine
    = (prism (\ x1_acuGf -> AwaitOutsideCoroutine x1_acuGf))
        (\ x_acuGg
            -> case x_acuGg of
                AwaitOutsideCoroutine y1_acuGh -> Right y1_acuGh
                _ -> Left x_acuGg)
  _NullByte
    = (prism (\ x1_acuGi -> NullByte x1_acuGi))
        (\ x_acuGj
            -> case x_acuGj of
                NullByte y1_acuGk -> Right y1_acuGk
                _ -> Left x_acuGj)
  _NonAsciiInBytes
    = (prism
          (\ (x1_acuGl, x2_acuGm) -> (NonAsciiInBytes x1_acuGl) x2_acuGm))
        (\ x_acuGn
            -> case x_acuGn of
                NonAsciiInBytes y1_acuGo y2_acuGp -> Right (y1_acuGo, y2_acuGp)
                _ -> Left x_acuGn)
  _DefaultExceptMustBeLast
    = (prism (\ x1_acuGq -> DefaultExceptMustBeLast x1_acuGq))
        (\ x_acuGr
            -> case x_acuGr of
                DefaultExceptMustBeLast y1_acuGs -> Right y1_acuGs
                _ -> Left x_acuGr)
  _WildcardImportInDefinition
    = (prism (\ x1_acuGt -> WildcardImportInDefinition x1_acuGt))
        (\ x_acuGu
            -> case x_acuGu of
                WildcardImportInDefinition y1_acuGv -> Right y1_acuGv
                _ -> Left x_acuGu)
  _NoKeywordsAfterEmptyStarArg
    = (prism (\ x1_acuGw -> NoKeywordsAfterEmptyStarArg x1_acuGw))
        (\ x_acuGx
            -> case x_acuGx of
                NoKeywordsAfterEmptyStarArg y1_acuGy -> Right y1_acuGy
                _ -> Left x_acuGx)
  _ManyStarredTargets
    = (prism (\ x1_acuGz -> ManyStarredTargets x1_acuGz))
        (\ x_acuGA
            -> case x_acuGA of
                ManyStarredTargets y1_acuGB -> Right y1_acuGB
                _ -> Left x_acuGA)
