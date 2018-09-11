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
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_anSy v_aj2D a_aj2E | r_anSy -> v_aj2D
                                                      a_aj2E where
  _SyntaxError :: Prism' r_anSy (SyntaxError v_aj2D a_aj2E)
  _PositionalAfterKeywordArg ::
    Prism' r_anSy (a_aj2E, Expr v_aj2D a_aj2E)
  _PositionalAfterKeywordUnpacking ::
    Prism' r_anSy (a_aj2E, Expr v_aj2D a_aj2E)
  _PositionalAfterKeywordParam :: Prism' r_anSy (a_aj2E, String)
  _UnexpectedDoubleStarParam :: Prism' r_anSy (a_aj2E, String)
  _CannotAssignTo :: Prism' r_anSy (a_aj2E, Expr v_aj2D a_aj2E)
  _CannotDelete :: Prism' r_anSy (a_aj2E, Expr v_aj2D a_aj2E)
  _CannotAugAssignTo :: Prism' r_anSy (a_aj2E, Expr v_aj2D a_aj2E)
  _DuplicateArgument :: Prism' r_anSy (a_aj2E, String)
  _ExpectedNewlineAfter ::
    Prism' r_anSy (a_aj2E, [Whitespace], Statement v_aj2D a_aj2E,
                    Maybe Newline)
  _UnexpectedNewline :: Prism' r_anSy a_aj2E
  _IdentifierReservedWord :: Prism' r_anSy (a_aj2E, String)
  _EmptyIdentifier :: Prism' r_anSy a_aj2E
  _BadCharacter :: Prism' r_anSy (a_aj2E, String)
  _BreakOutsideLoop :: Prism' r_anSy a_aj2E
  _ContinueOutsideLoop :: Prism' r_anSy a_aj2E
  _ReturnOutsideFunction :: Prism' r_anSy a_aj2E
  _NonlocalOutsideFunction :: Prism' r_anSy a_aj2E
  _ParametersNonlocal :: Prism' r_anSy (a_aj2E, [String])
  _NoBindingNonlocal :: Prism' r_anSy (Ident v_aj2D a_aj2E)
  _Can'tJoinStringAndBytes :: Prism' r_anSy a_aj2E
  _InvalidYield :: Prism' r_anSy a_aj2E
  _CommentAfterBackslash :: Prism' r_anSy a_aj2E
  _MalformedDecorator :: Prism' r_anSy a_aj2E
  _InvalidDictUnpacking :: Prism' r_anSy a_aj2E
  _InvalidSetUnpacking :: Prism' r_anSy a_aj2E
  _TypedParamInLambda :: Prism' r_anSy a_aj2E
  _TypedUnnamedStarParam :: Prism' r_anSy a_aj2E
  _AsyncWithOutsideCoroutine :: Prism' r_anSy a_aj2E
  _AsyncForOutsideCoroutine :: Prism' r_anSy a_aj2E
  _YieldFromInsideCoroutine :: Prism' r_anSy a_aj2E
  _AwaitOutsideCoroutine :: Prism' r_anSy a_aj2E
  _NullByte :: Prism' r_anSy a_aj2E
  _NonAsciiInBytes :: Prism' r_anSy (a_aj2E, Char)
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
instance AsSyntaxError (SyntaxError v_aj2D a_aj2E) v_aj2D a_aj2E where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (prism
          (\ (x1_anSz, x2_anSA)
            -> (PositionalAfterKeywordArg x1_anSz) x2_anSA))
        (\ x_anSB
            -> case x_anSB of
                PositionalAfterKeywordArg y1_anSC y2_anSD
                  -> Right (y1_anSC, y2_anSD)
                _ -> Left x_anSB)
  _PositionalAfterKeywordUnpacking
    = (prism
          (\ (x1_anSE, x2_anSF)
            -> (PositionalAfterKeywordUnpacking x1_anSE) x2_anSF))
        (\ x_anSG
            -> case x_anSG of
                PositionalAfterKeywordUnpacking y1_anSH y2_anSI
                  -> Right (y1_anSH, y2_anSI)
                _ -> Left x_anSG)
  _PositionalAfterKeywordParam
    = (prism
          (\ (x1_anSJ, x2_anSK)
            -> (PositionalAfterKeywordParam x1_anSJ) x2_anSK))
        (\ x_anSL
            -> case x_anSL of
                PositionalAfterKeywordParam y1_anSM y2_anSN
                  -> Right (y1_anSM, y2_anSN)
                _ -> Left x_anSL)
  _UnexpectedDoubleStarParam
    = (prism
          (\ (x1_anSO, x2_anSP)
            -> (UnexpectedDoubleStarParam x1_anSO) x2_anSP))
        (\ x_anSQ
            -> case x_anSQ of
                UnexpectedDoubleStarParam y1_anSR y2_anSS
                  -> Right (y1_anSR, y2_anSS)
                _ -> Left x_anSQ)
  _CannotAssignTo
    = (prism
          (\ (x1_anST, x2_anSU) -> (CannotAssignTo x1_anST) x2_anSU))
        (\ x_anSV
            -> case x_anSV of
                CannotAssignTo y1_anSW y2_anSX -> Right (y1_anSW, y2_anSX)
                _ -> Left x_anSV)
  _CannotDelete
    = (prism (\ (x1_anSY, x2_anSZ) -> (CannotDelete x1_anSY) x2_anSZ))
        (\ x_anT0
            -> case x_anT0 of
                CannotDelete y1_anT1 y2_anT2 -> Right (y1_anT1, y2_anT2)
                _ -> Left x_anT0)
  _CannotAugAssignTo
    = (prism
          (\ (x1_anT3, x2_anT4) -> (CannotAugAssignTo x1_anT3) x2_anT4))
        (\ x_anT5
            -> case x_anT5 of
                CannotAugAssignTo y1_anT6 y2_anT7 -> Right (y1_anT6, y2_anT7)
                _ -> Left x_anT5)
  _DuplicateArgument
    = (prism
          (\ (x1_anT8, x2_anT9) -> (DuplicateArgument x1_anT8) x2_anT9))
        (\ x_anTa
            -> case x_anTa of
                DuplicateArgument y1_anTb y2_anTc -> Right (y1_anTb, y2_anTc)
                _ -> Left x_anTa)
  _ExpectedNewlineAfter
    = (prism (\ x1_anTd -> ExpectedNewlineAfter x1_anTd))
        (\ x_anTe
            -> case x_anTe of
                ExpectedNewlineAfter y1_anTf -> Right y1_anTf
                _ -> Left x_anTe)
  _UnexpectedNewline
    = (prism (\ x1_anTg -> UnexpectedNewline x1_anTg))
        (\ x_anTh
            -> case x_anTh of
                UnexpectedNewline y1_anTi -> Right y1_anTi
                _ -> Left x_anTh)
  _IdentifierReservedWord
    = (prism
          (\ (x1_anTj, x2_anTk) -> (IdentifierReservedWord x1_anTj) x2_anTk))
        (\ x_anTl
            -> case x_anTl of
                IdentifierReservedWord y1_anTm y2_anTn -> Right (y1_anTm, y2_anTn)
                _ -> Left x_anTl)
  _EmptyIdentifier
    = (prism (\ x1_anTo -> EmptyIdentifier x1_anTo))
        (\ x_anTp
            -> case x_anTp of
                EmptyIdentifier y1_anTq -> Right y1_anTq
                _ -> Left x_anTp)
  _BadCharacter
    = (prism (\ (x1_anTr, x2_anTs) -> (BadCharacter x1_anTr) x2_anTs))
        (\ x_anTt
            -> case x_anTt of
                BadCharacter y1_anTu y2_anTv -> Right (y1_anTu, y2_anTv)
                _ -> Left x_anTt)
  _BreakOutsideLoop
    = (prism (\ x1_anTw -> BreakOutsideLoop x1_anTw))
        (\ x_anTx
            -> case x_anTx of
                BreakOutsideLoop y1_anTy -> Right y1_anTy
                _ -> Left x_anTx)
  _ContinueOutsideLoop
    = (prism (\ x1_anTz -> ContinueOutsideLoop x1_anTz))
        (\ x_anTA
            -> case x_anTA of
                ContinueOutsideLoop y1_anTB -> Right y1_anTB
                _ -> Left x_anTA)
  _ReturnOutsideFunction
    = (prism (\ x1_anTC -> ReturnOutsideFunction x1_anTC))
        (\ x_anTD
            -> case x_anTD of
                ReturnOutsideFunction y1_anTE -> Right y1_anTE
                _ -> Left x_anTD)
  _NonlocalOutsideFunction
    = (prism (\ x1_anTF -> NonlocalOutsideFunction x1_anTF))
        (\ x_anTG
            -> case x_anTG of
                NonlocalOutsideFunction y1_anTH -> Right y1_anTH
                _ -> Left x_anTG)
  _ParametersNonlocal
    = (prism
          (\ (x1_anTI, x2_anTJ) -> (ParametersNonlocal x1_anTI) x2_anTJ))
        (\ x_anTK
            -> case x_anTK of
                ParametersNonlocal y1_anTL y2_anTM -> Right (y1_anTL, y2_anTM)
                _ -> Left x_anTK)
  _NoBindingNonlocal
    = (prism (\ x1_anTN -> NoBindingNonlocal x1_anTN))
        (\ x_anTO
            -> case x_anTO of
                NoBindingNonlocal y1_anTP -> Right y1_anTP
                _ -> Left x_anTO)
  _Can'tJoinStringAndBytes
    = (prism (\ x1_anTQ -> Can'tJoinStringAndBytes x1_anTQ))
        (\ x_anTR
            -> case x_anTR of
                Can'tJoinStringAndBytes y1_anTS -> Right y1_anTS
                _ -> Left x_anTR)
  _InvalidYield
    = (prism (\ x1_anTT -> InvalidYield x1_anTT))
        (\ x_anTU
            -> case x_anTU of
                InvalidYield y1_anTV -> Right y1_anTV
                _ -> Left x_anTU)
  _CommentAfterBackslash
    = (prism (\ x1_anTW -> CommentAfterBackslash x1_anTW))
        (\ x_anTX
            -> case x_anTX of
                CommentAfterBackslash y1_anTY -> Right y1_anTY
                _ -> Left x_anTX)
  _MalformedDecorator
    = (prism (\ x1_anTZ -> MalformedDecorator x1_anTZ))
        (\ x_anU0
            -> case x_anU0 of
                MalformedDecorator y1_anU1 -> Right y1_anU1
                _ -> Left x_anU0)
  _InvalidDictUnpacking
    = (prism (\ x1_anU2 -> InvalidDictUnpacking x1_anU2))
        (\ x_anU3
            -> case x_anU3 of
                InvalidDictUnpacking y1_anU4 -> Right y1_anU4
                _ -> Left x_anU3)
  _InvalidSetUnpacking
    = (prism (\ x1_anU5 -> InvalidSetUnpacking x1_anU5))
        (\ x_anU6
            -> case x_anU6 of
                InvalidSetUnpacking y1_anU7 -> Right y1_anU7
                _ -> Left x_anU6)
  _TypedParamInLambda
    = (prism (\ x1_anU8 -> TypedParamInLambda x1_anU8))
        (\ x_anU9
            -> case x_anU9 of
                TypedParamInLambda y1_anUa -> Right y1_anUa
                _ -> Left x_anU9)
  _TypedUnnamedStarParam
    = (prism (\ x1_anUb -> TypedUnnamedStarParam x1_anUb))
        (\ x_anUc
            -> case x_anUc of
                TypedUnnamedStarParam y1_anUd -> Right y1_anUd
                _ -> Left x_anUc)
  _AsyncWithOutsideCoroutine
    = (prism (\ x1_anUe -> AsyncWithOutsideCoroutine x1_anUe))
        (\ x_anUf
            -> case x_anUf of
                AsyncWithOutsideCoroutine y1_anUg -> Right y1_anUg
                _ -> Left x_anUf)
  _AsyncForOutsideCoroutine
    = (prism (\ x1_anUh -> AsyncForOutsideCoroutine x1_anUh))
        (\ x_anUi
            -> case x_anUi of
                AsyncForOutsideCoroutine y1_anUj -> Right y1_anUj
                _ -> Left x_anUi)
  _YieldFromInsideCoroutine
    = (prism (\ x1_anUk -> YieldFromInsideCoroutine x1_anUk))
        (\ x_anUl
            -> case x_anUl of
                YieldFromInsideCoroutine y1_anUm -> Right y1_anUm
                _ -> Left x_anUl)
  _AwaitOutsideCoroutine
    = (prism (\ x1_anUn -> AwaitOutsideCoroutine x1_anUn))
        (\ x_anUo
            -> case x_anUo of
                AwaitOutsideCoroutine y1_anUp -> Right y1_anUp
                _ -> Left x_anUo)
  _NullByte
    = (prism (\ x1_anUq -> NullByte x1_anUq))
        (\ x_anUr
            -> case x_anUr of
                NullByte y1_anUs -> Right y1_anUs
                _ -> Left x_anUr)
  _NonAsciiInBytes
    = (prism
          (\ (x1_anUt, x2_anUu) -> (NonAsciiInBytes x1_anUt) x2_anUu))
        (\ x_anUv
            -> case x_anUv of
                NonAsciiInBytes y1_anUw y2_anUx -> Right (y1_anUw, y2_anUx)
                _ -> Left x_anUv)
