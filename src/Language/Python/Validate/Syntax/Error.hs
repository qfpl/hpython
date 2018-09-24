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
  | YieldInsideCoroutine a
  | AwaitOutsideCoroutine a
  | NullByte a
  | NonAsciiInBytes a Char
  | DefaultExceptMustBeLast a
  | WildcardImportInDefinition a
  | NoKeywordsAfterEmptyStarArg a
  | ManyStarredTargets a
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_adlSd v_adlrL a_adlrM | r_adlSd -> v_adlrL
                                                          a_adlrM where
  _SyntaxError :: Prism' r_adlSd (SyntaxError v_adlrL a_adlrM)
  _PositionalAfterKeywordArg ::
    Prism' r_adlSd (a_adlrM, Expr v_adlrL a_adlrM)
  _PositionalAfterKeywordUnpacking ::
    Prism' r_adlSd (a_adlrM, Expr v_adlrL a_adlrM)
  _PositionalAfterKeywordParam :: Prism' r_adlSd (a_adlrM, String)
  _UnexpectedDoubleStarParam :: Prism' r_adlSd (a_adlrM, String)
  _CannotAssignTo :: Prism' r_adlSd (a_adlrM, Expr v_adlrL a_adlrM)
  _CannotDelete :: Prism' r_adlSd (a_adlrM, Expr v_adlrL a_adlrM)
  _CannotAugAssignTo ::
    Prism' r_adlSd (a_adlrM, Expr v_adlrL a_adlrM)
  _DuplicateArgument :: Prism' r_adlSd (a_adlrM, String)
  _ExpectedNewlineAfter ::
    Prism' r_adlSd (a_adlrM, [Whitespace], Statement v_adlrL a_adlrM,
                    Maybe Newline)
  _UnexpectedNewline :: Prism' r_adlSd a_adlrM
  _IdentifierReservedWord :: Prism' r_adlSd (a_adlrM, String)
  _EmptyIdentifier :: Prism' r_adlSd a_adlrM
  _BadCharacter :: Prism' r_adlSd (a_adlrM, String)
  _BreakOutsideLoop :: Prism' r_adlSd a_adlrM
  _ContinueOutsideLoop :: Prism' r_adlSd a_adlrM
  _ReturnOutsideFunction :: Prism' r_adlSd a_adlrM
  _NonlocalOutsideFunction :: Prism' r_adlSd a_adlrM
  _ParametersNonlocal :: Prism' r_adlSd (a_adlrM, [String])
  _NoBindingNonlocal :: Prism' r_adlSd (Ident v_adlrL a_adlrM)
  _Can'tJoinStringAndBytes :: Prism' r_adlSd a_adlrM
  _InvalidYield :: Prism' r_adlSd a_adlrM
  _CommentAfterBackslash :: Prism' r_adlSd a_adlrM
  _MalformedDecorator :: Prism' r_adlSd a_adlrM
  _InvalidDictUnpacking :: Prism' r_adlSd a_adlrM
  _InvalidSetUnpacking :: Prism' r_adlSd a_adlrM
  _TypedParamInLambda :: Prism' r_adlSd a_adlrM
  _TypedUnnamedStarParam :: Prism' r_adlSd a_adlrM
  _AsyncWithOutsideCoroutine :: Prism' r_adlSd a_adlrM
  _AsyncForOutsideCoroutine :: Prism' r_adlSd a_adlrM
  _YieldFromInsideCoroutine :: Prism' r_adlSd a_adlrM
  _YieldInsideCoroutine :: Prism' r_adlSd a_adlrM
  _AwaitOutsideCoroutine :: Prism' r_adlSd a_adlrM
  _NullByte :: Prism' r_adlSd a_adlrM
  _NonAsciiInBytes :: Prism' r_adlSd (a_adlrM, Char)
  _DefaultExceptMustBeLast :: Prism' r_adlSd a_adlrM
  _WildcardImportInDefinition :: Prism' r_adlSd a_adlrM
  _NoKeywordsAfterEmptyStarArg :: Prism' r_adlSd a_adlrM
  _ManyStarredTargets :: Prism' r_adlSd a_adlrM
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
  _YieldInsideCoroutine = ((.) _SyntaxError) _YieldInsideCoroutine
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
instance AsSyntaxError (SyntaxError v_adlrL a_adlrM) v_adlrL a_adlrM where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (prism
          (\ (x1_adlSe, x2_adlSf)
            -> (PositionalAfterKeywordArg x1_adlSe) x2_adlSf))
        (\ x_adlSg
            -> case x_adlSg of
                PositionalAfterKeywordArg y1_adlSh y2_adlSi
                  -> Right (y1_adlSh, y2_adlSi)
                _ -> Left x_adlSg)
  _PositionalAfterKeywordUnpacking
    = (prism
          (\ (x1_adlSj, x2_adlSk)
            -> (PositionalAfterKeywordUnpacking x1_adlSj) x2_adlSk))
        (\ x_adlSl
            -> case x_adlSl of
                PositionalAfterKeywordUnpacking y1_adlSm y2_adlSn
                  -> Right (y1_adlSm, y2_adlSn)
                _ -> Left x_adlSl)
  _PositionalAfterKeywordParam
    = (prism
          (\ (x1_adlSo, x2_adlSp)
            -> (PositionalAfterKeywordParam x1_adlSo) x2_adlSp))
        (\ x_adlSq
            -> case x_adlSq of
                PositionalAfterKeywordParam y1_adlSr y2_adlSs
                  -> Right (y1_adlSr, y2_adlSs)
                _ -> Left x_adlSq)
  _UnexpectedDoubleStarParam
    = (prism
          (\ (x1_adlSt, x2_adlSu)
            -> (UnexpectedDoubleStarParam x1_adlSt) x2_adlSu))
        (\ x_adlSv
            -> case x_adlSv of
                UnexpectedDoubleStarParam y1_adlSw y2_adlSx
                  -> Right (y1_adlSw, y2_adlSx)
                _ -> Left x_adlSv)
  _CannotAssignTo
    = (prism
          (\ (x1_adlSy, x2_adlSz) -> (CannotAssignTo x1_adlSy) x2_adlSz))
        (\ x_adlSA
            -> case x_adlSA of
                CannotAssignTo y1_adlSB y2_adlSC -> Right (y1_adlSB, y2_adlSC)
                _ -> Left x_adlSA)
  _CannotDelete
    = (prism
          (\ (x1_adlSD, x2_adlSE) -> (CannotDelete x1_adlSD) x2_adlSE))
        (\ x_adlSF
            -> case x_adlSF of
                CannotDelete y1_adlSG y2_adlSH -> Right (y1_adlSG, y2_adlSH)
                _ -> Left x_adlSF)
  _CannotAugAssignTo
    = (prism
          (\ (x1_adlSI, x2_adlSJ) -> (CannotAugAssignTo x1_adlSI) x2_adlSJ))
        (\ x_adlSK
            -> case x_adlSK of
                CannotAugAssignTo y1_adlSL y2_adlSM -> Right (y1_adlSL, y2_adlSM)
                _ -> Left x_adlSK)
  _DuplicateArgument
    = (prism
          (\ (x1_adlSN, x2_adlSO) -> (DuplicateArgument x1_adlSN) x2_adlSO))
        (\ x_adlSP
            -> case x_adlSP of
                DuplicateArgument y1_adlSQ y2_adlSR -> Right (y1_adlSQ, y2_adlSR)
                _ -> Left x_adlSP)
  _ExpectedNewlineAfter
    = (prism (\ x1_adlSS -> ExpectedNewlineAfter x1_adlSS))
        (\ x_adlST
            -> case x_adlST of
                ExpectedNewlineAfter y1_adlSU -> Right y1_adlSU
                _ -> Left x_adlST)
  _UnexpectedNewline
    = (prism (\ x1_adlSV -> UnexpectedNewline x1_adlSV))
        (\ x_adlSW
            -> case x_adlSW of
                UnexpectedNewline y1_adlSX -> Right y1_adlSX
                _ -> Left x_adlSW)
  _IdentifierReservedWord
    = (prism
          (\ (x1_adlSY, x2_adlSZ)
            -> (IdentifierReservedWord x1_adlSY) x2_adlSZ))
        (\ x_adlT0
            -> case x_adlT0 of
                IdentifierReservedWord y1_adlT1 y2_adlT2
                  -> Right (y1_adlT1, y2_adlT2)
                _ -> Left x_adlT0)
  _EmptyIdentifier
    = (prism (\ x1_adlT3 -> EmptyIdentifier x1_adlT3))
        (\ x_adlT4
            -> case x_adlT4 of
                EmptyIdentifier y1_adlT5 -> Right y1_adlT5
                _ -> Left x_adlT4)
  _BadCharacter
    = (prism
          (\ (x1_adlT6, x2_adlT7) -> (BadCharacter x1_adlT6) x2_adlT7))
        (\ x_adlT8
            -> case x_adlT8 of
                BadCharacter y1_adlT9 y2_adlTa -> Right (y1_adlT9, y2_adlTa)
                _ -> Left x_adlT8)
  _BreakOutsideLoop
    = (prism (\ x1_adlTb -> BreakOutsideLoop x1_adlTb))
        (\ x_adlTc
            -> case x_adlTc of
                BreakOutsideLoop y1_adlTd -> Right y1_adlTd
                _ -> Left x_adlTc)
  _ContinueOutsideLoop
    = (prism (\ x1_adlTe -> ContinueOutsideLoop x1_adlTe))
        (\ x_adlTf
            -> case x_adlTf of
                ContinueOutsideLoop y1_adlTg -> Right y1_adlTg
                _ -> Left x_adlTf)
  _ReturnOutsideFunction
    = (prism (\ x1_adlTh -> ReturnOutsideFunction x1_adlTh))
        (\ x_adlTi
            -> case x_adlTi of
                ReturnOutsideFunction y1_adlTj -> Right y1_adlTj
                _ -> Left x_adlTi)
  _NonlocalOutsideFunction
    = (prism (\ x1_adlTk -> NonlocalOutsideFunction x1_adlTk))
        (\ x_adlTl
            -> case x_adlTl of
                NonlocalOutsideFunction y1_adlTm -> Right y1_adlTm
                _ -> Left x_adlTl)
  _ParametersNonlocal
    = (prism
          (\ (x1_adlTn, x2_adlTo) -> (ParametersNonlocal x1_adlTn) x2_adlTo))
        (\ x_adlTp
            -> case x_adlTp of
                ParametersNonlocal y1_adlTq y2_adlTr -> Right (y1_adlTq, y2_adlTr)
                _ -> Left x_adlTp)
  _NoBindingNonlocal
    = (prism (\ x1_adlTs -> NoBindingNonlocal x1_adlTs))
        (\ x_adlTt
            -> case x_adlTt of
                NoBindingNonlocal y1_adlTu -> Right y1_adlTu
                _ -> Left x_adlTt)
  _Can'tJoinStringAndBytes
    = (prism (\ x1_adlTv -> Can'tJoinStringAndBytes x1_adlTv))
        (\ x_adlTw
            -> case x_adlTw of
                Can'tJoinStringAndBytes y1_adlTx -> Right y1_adlTx
                _ -> Left x_adlTw)
  _InvalidYield
    = (prism (\ x1_adlTy -> InvalidYield x1_adlTy))
        (\ x_adlTz
            -> case x_adlTz of
                InvalidYield y1_adlTA -> Right y1_adlTA
                _ -> Left x_adlTz)
  _CommentAfterBackslash
    = (prism (\ x1_adlTB -> CommentAfterBackslash x1_adlTB))
        (\ x_adlTC
            -> case x_adlTC of
                CommentAfterBackslash y1_adlTD -> Right y1_adlTD
                _ -> Left x_adlTC)
  _MalformedDecorator
    = (prism (\ x1_adlTE -> MalformedDecorator x1_adlTE))
        (\ x_adlTF
            -> case x_adlTF of
                MalformedDecorator y1_adlTG -> Right y1_adlTG
                _ -> Left x_adlTF)
  _InvalidDictUnpacking
    = (prism (\ x1_adlTH -> InvalidDictUnpacking x1_adlTH))
        (\ x_adlTI
            -> case x_adlTI of
                InvalidDictUnpacking y1_adlTJ -> Right y1_adlTJ
                _ -> Left x_adlTI)
  _InvalidSetUnpacking
    = (prism (\ x1_adlTK -> InvalidSetUnpacking x1_adlTK))
        (\ x_adlTL
            -> case x_adlTL of
                InvalidSetUnpacking y1_adlTM -> Right y1_adlTM
                _ -> Left x_adlTL)
  _TypedParamInLambda
    = (prism (\ x1_adlTN -> TypedParamInLambda x1_adlTN))
        (\ x_adlTO
            -> case x_adlTO of
                TypedParamInLambda y1_adlTP -> Right y1_adlTP
                _ -> Left x_adlTO)
  _TypedUnnamedStarParam
    = (prism (\ x1_adlTQ -> TypedUnnamedStarParam x1_adlTQ))
        (\ x_adlTR
            -> case x_adlTR of
                TypedUnnamedStarParam y1_adlTS -> Right y1_adlTS
                _ -> Left x_adlTR)
  _AsyncWithOutsideCoroutine
    = (prism (\ x1_adlTT -> AsyncWithOutsideCoroutine x1_adlTT))
        (\ x_adlTU
            -> case x_adlTU of
                AsyncWithOutsideCoroutine y1_adlTV -> Right y1_adlTV
                _ -> Left x_adlTU)
  _AsyncForOutsideCoroutine
    = (prism (\ x1_adlTW -> AsyncForOutsideCoroutine x1_adlTW))
        (\ x_adlTX
            -> case x_adlTX of
                AsyncForOutsideCoroutine y1_adlTY -> Right y1_adlTY
                _ -> Left x_adlTX)
  _YieldFromInsideCoroutine
    = (prism (\ x1_adlTZ -> YieldFromInsideCoroutine x1_adlTZ))
        (\ x_adlU0
            -> case x_adlU0 of
                YieldFromInsideCoroutine y1_adlU1 -> Right y1_adlU1
                _ -> Left x_adlU0)
  _YieldInsideCoroutine
    = (prism (\ x1_adlU2 -> YieldInsideCoroutine x1_adlU2))
        (\ x_adlU3
            -> case x_adlU3 of
                YieldInsideCoroutine y1_adlU4 -> Right y1_adlU4
                _ -> Left x_adlU3)
  _AwaitOutsideCoroutine
    = (prism (\ x1_adlU5 -> AwaitOutsideCoroutine x1_adlU5))
        (\ x_adlU6
            -> case x_adlU6 of
                AwaitOutsideCoroutine y1_adlU7 -> Right y1_adlU7
                _ -> Left x_adlU6)
  _NullByte
    = (prism (\ x1_adlU8 -> NullByte x1_adlU8))
        (\ x_adlU9
            -> case x_adlU9 of
                NullByte y1_adlUa -> Right y1_adlUa
                _ -> Left x_adlU9)
  _NonAsciiInBytes
    = (prism
          (\ (x1_adlUb, x2_adlUc) -> (NonAsciiInBytes x1_adlUb) x2_adlUc))
        (\ x_adlUd
            -> case x_adlUd of
                NonAsciiInBytes y1_adlUe y2_adlUf -> Right (y1_adlUe, y2_adlUf)
                _ -> Left x_adlUd)
  _DefaultExceptMustBeLast
    = (prism (\ x1_adlUg -> DefaultExceptMustBeLast x1_adlUg))
        (\ x_adlUh
            -> case x_adlUh of
                DefaultExceptMustBeLast y1_adlUi -> Right y1_adlUi
                _ -> Left x_adlUh)
  _WildcardImportInDefinition
    = (prism (\ x1_adlUj -> WildcardImportInDefinition x1_adlUj))
        (\ x_adlUk
            -> case x_adlUk of
                WildcardImportInDefinition y1_adlUl -> Right y1_adlUl
                _ -> Left x_adlUk)
  _NoKeywordsAfterEmptyStarArg
    = (prism (\ x1_adlUm -> NoKeywordsAfterEmptyStarArg x1_adlUm))
        (\ x_adlUn
            -> case x_adlUn of
                NoKeywordsAfterEmptyStarArg y1_adlUo -> Right y1_adlUo
                _ -> Left x_adlUn)
  _ManyStarredTargets
    = (prism (\ x1_adlUp -> ManyStarredTargets x1_adlUp))
        (\ x_adlUq
            -> case x_adlUq of
                ManyStarredTargets y1_adlUr -> Right y1_adlUr
                _ -> Left x_adlUq)
