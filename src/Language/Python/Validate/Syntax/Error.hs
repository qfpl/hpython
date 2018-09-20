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
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_actng v_act02 a_act03 | r_actng -> v_act02
                                                          a_act03 where
  _SyntaxError :: Prism' r_actng (SyntaxError v_act02 a_act03)
  _PositionalAfterKeywordArg ::
    Prism' r_actng (a_act03, Expr v_act02 a_act03)
  _PositionalAfterKeywordUnpacking ::
    Prism' r_actng (a_act03, Expr v_act02 a_act03)
  _PositionalAfterKeywordParam :: Prism' r_actng (a_act03, String)
  _UnexpectedDoubleStarParam :: Prism' r_actng (a_act03, String)
  _CannotAssignTo :: Prism' r_actng (a_act03, Expr v_act02 a_act03)
  _CannotDelete :: Prism' r_actng (a_act03, Expr v_act02 a_act03)
  _CannotAugAssignTo ::
    Prism' r_actng (a_act03, Expr v_act02 a_act03)
  _DuplicateArgument :: Prism' r_actng (a_act03, String)
  _ExpectedNewlineAfter ::
    Prism' r_actng (a_act03, [Whitespace], Statement v_act02 a_act03,
                    Maybe Newline)
  _UnexpectedNewline :: Prism' r_actng a_act03
  _IdentifierReservedWord :: Prism' r_actng (a_act03, String)
  _EmptyIdentifier :: Prism' r_actng a_act03
  _BadCharacter :: Prism' r_actng (a_act03, String)
  _BreakOutsideLoop :: Prism' r_actng a_act03
  _ContinueOutsideLoop :: Prism' r_actng a_act03
  _ReturnOutsideFunction :: Prism' r_actng a_act03
  _NonlocalOutsideFunction :: Prism' r_actng a_act03
  _ParametersNonlocal :: Prism' r_actng (a_act03, [String])
  _NoBindingNonlocal :: Prism' r_actng (Ident v_act02 a_act03)
  _Can'tJoinStringAndBytes :: Prism' r_actng a_act03
  _InvalidYield :: Prism' r_actng a_act03
  _CommentAfterBackslash :: Prism' r_actng a_act03
  _MalformedDecorator :: Prism' r_actng a_act03
  _InvalidDictUnpacking :: Prism' r_actng a_act03
  _InvalidSetUnpacking :: Prism' r_actng a_act03
  _TypedParamInLambda :: Prism' r_actng a_act03
  _TypedUnnamedStarParam :: Prism' r_actng a_act03
  _AsyncWithOutsideCoroutine :: Prism' r_actng a_act03
  _AsyncForOutsideCoroutine :: Prism' r_actng a_act03
  _YieldFromInsideCoroutine :: Prism' r_actng a_act03
  _AwaitOutsideCoroutine :: Prism' r_actng a_act03
  _NullByte :: Prism' r_actng a_act03
  _NonAsciiInBytes :: Prism' r_actng (a_act03, Char)
  _DefaultExceptMustBeLast :: Prism' r_actng a_act03
  _WildcardImportInDefinition :: Prism' r_actng a_act03
  _NoKeywordsAfterEmptyStarArg :: Prism' r_actng a_act03
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
instance AsSyntaxError (SyntaxError v_act02 a_act03) v_act02 a_act03 where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (prism
          (\ (x1_actnh, x2_actni)
            -> (PositionalAfterKeywordArg x1_actnh) x2_actni))
        (\ x_actnj
            -> case x_actnj of
                PositionalAfterKeywordArg y1_actnk y2_actnl
                  -> Right (y1_actnk, y2_actnl)
                _ -> Left x_actnj)
  _PositionalAfterKeywordUnpacking
    = (prism
          (\ (x1_actnm, x2_actnn)
            -> (PositionalAfterKeywordUnpacking x1_actnm) x2_actnn))
        (\ x_actno
            -> case x_actno of
                PositionalAfterKeywordUnpacking y1_actnp y2_actnq
                  -> Right (y1_actnp, y2_actnq)
                _ -> Left x_actno)
  _PositionalAfterKeywordParam
    = (prism
          (\ (x1_actnr, x2_actns)
            -> (PositionalAfterKeywordParam x1_actnr) x2_actns))
        (\ x_actnt
            -> case x_actnt of
                PositionalAfterKeywordParam y1_actnu y2_actnv
                  -> Right (y1_actnu, y2_actnv)
                _ -> Left x_actnt)
  _UnexpectedDoubleStarParam
    = (prism
          (\ (x1_actnw, x2_actnx)
            -> (UnexpectedDoubleStarParam x1_actnw) x2_actnx))
        (\ x_actny
            -> case x_actny of
                UnexpectedDoubleStarParam y1_actnz y2_actnA
                  -> Right (y1_actnz, y2_actnA)
                _ -> Left x_actny)
  _CannotAssignTo
    = (prism
          (\ (x1_actnB, x2_actnC) -> (CannotAssignTo x1_actnB) x2_actnC))
        (\ x_actnD
            -> case x_actnD of
                CannotAssignTo y1_actnE y2_actnF -> Right (y1_actnE, y2_actnF)
                _ -> Left x_actnD)
  _CannotDelete
    = (prism
          (\ (x1_actnG, x2_actnH) -> (CannotDelete x1_actnG) x2_actnH))
        (\ x_actnI
            -> case x_actnI of
                CannotDelete y1_actnJ y2_actnK -> Right (y1_actnJ, y2_actnK)
                _ -> Left x_actnI)
  _CannotAugAssignTo
    = (prism
          (\ (x1_actnL, x2_actnM) -> (CannotAugAssignTo x1_actnL) x2_actnM))
        (\ x_actnN
            -> case x_actnN of
                CannotAugAssignTo y1_actnO y2_actnP -> Right (y1_actnO, y2_actnP)
                _ -> Left x_actnN)
  _DuplicateArgument
    = (prism
          (\ (x1_actnQ, x2_actnR) -> (DuplicateArgument x1_actnQ) x2_actnR))
        (\ x_actnS
            -> case x_actnS of
                DuplicateArgument y1_actnT y2_actnU -> Right (y1_actnT, y2_actnU)
                _ -> Left x_actnS)
  _ExpectedNewlineAfter
    = (prism (\ x1_actnV -> ExpectedNewlineAfter x1_actnV))
        (\ x_actnW
            -> case x_actnW of
                ExpectedNewlineAfter y1_actnX -> Right y1_actnX
                _ -> Left x_actnW)
  _UnexpectedNewline
    = (prism (\ x1_actnY -> UnexpectedNewline x1_actnY))
        (\ x_actnZ
            -> case x_actnZ of
                UnexpectedNewline y1_acto0 -> Right y1_acto0
                _ -> Left x_actnZ)
  _IdentifierReservedWord
    = (prism
          (\ (x1_acto1, x2_acto2)
            -> (IdentifierReservedWord x1_acto1) x2_acto2))
        (\ x_acto3
            -> case x_acto3 of
                IdentifierReservedWord y1_acto4 y2_acto5
                  -> Right (y1_acto4, y2_acto5)
                _ -> Left x_acto3)
  _EmptyIdentifier
    = (prism (\ x1_acto6 -> EmptyIdentifier x1_acto6))
        (\ x_acto7
            -> case x_acto7 of
                EmptyIdentifier y1_acto8 -> Right y1_acto8
                _ -> Left x_acto7)
  _BadCharacter
    = (prism
          (\ (x1_acto9, x2_actoa) -> (BadCharacter x1_acto9) x2_actoa))
        (\ x_actob
            -> case x_actob of
                BadCharacter y1_actoc y2_actod -> Right (y1_actoc, y2_actod)
                _ -> Left x_actob)
  _BreakOutsideLoop
    = (prism (\ x1_actoe -> BreakOutsideLoop x1_actoe))
        (\ x_actof
            -> case x_actof of
                BreakOutsideLoop y1_actog -> Right y1_actog
                _ -> Left x_actof)
  _ContinueOutsideLoop
    = (prism (\ x1_actoh -> ContinueOutsideLoop x1_actoh))
        (\ x_actoi
            -> case x_actoi of
                ContinueOutsideLoop y1_actoj -> Right y1_actoj
                _ -> Left x_actoi)
  _ReturnOutsideFunction
    = (prism (\ x1_actok -> ReturnOutsideFunction x1_actok))
        (\ x_actol
            -> case x_actol of
                ReturnOutsideFunction y1_actom -> Right y1_actom
                _ -> Left x_actol)
  _NonlocalOutsideFunction
    = (prism (\ x1_acton -> NonlocalOutsideFunction x1_acton))
        (\ x_actoo
            -> case x_actoo of
                NonlocalOutsideFunction y1_actop -> Right y1_actop
                _ -> Left x_actoo)
  _ParametersNonlocal
    = (prism
          (\ (x1_actoq, x2_actor) -> (ParametersNonlocal x1_actoq) x2_actor))
        (\ x_actos
            -> case x_actos of
                ParametersNonlocal y1_actot y2_actou -> Right (y1_actot, y2_actou)
                _ -> Left x_actos)
  _NoBindingNonlocal
    = (prism (\ x1_actov -> NoBindingNonlocal x1_actov))
        (\ x_actow
            -> case x_actow of
                NoBindingNonlocal y1_actox -> Right y1_actox
                _ -> Left x_actow)
  _Can'tJoinStringAndBytes
    = (prism (\ x1_actoy -> Can'tJoinStringAndBytes x1_actoy))
        (\ x_actoz
            -> case x_actoz of
                Can'tJoinStringAndBytes y1_actoA -> Right y1_actoA
                _ -> Left x_actoz)
  _InvalidYield
    = (prism (\ x1_actoB -> InvalidYield x1_actoB))
        (\ x_actoC
            -> case x_actoC of
                InvalidYield y1_actoD -> Right y1_actoD
                _ -> Left x_actoC)
  _CommentAfterBackslash
    = (prism (\ x1_actoE -> CommentAfterBackslash x1_actoE))
        (\ x_actoF
            -> case x_actoF of
                CommentAfterBackslash y1_actoG -> Right y1_actoG
                _ -> Left x_actoF)
  _MalformedDecorator
    = (prism (\ x1_actoH -> MalformedDecorator x1_actoH))
        (\ x_actoI
            -> case x_actoI of
                MalformedDecorator y1_actoJ -> Right y1_actoJ
                _ -> Left x_actoI)
  _InvalidDictUnpacking
    = (prism (\ x1_actoK -> InvalidDictUnpacking x1_actoK))
        (\ x_actoL
            -> case x_actoL of
                InvalidDictUnpacking y1_actoM -> Right y1_actoM
                _ -> Left x_actoL)
  _InvalidSetUnpacking
    = (prism (\ x1_actoN -> InvalidSetUnpacking x1_actoN))
        (\ x_actoO
            -> case x_actoO of
                InvalidSetUnpacking y1_actoP -> Right y1_actoP
                _ -> Left x_actoO)
  _TypedParamInLambda
    = (prism (\ x1_actoQ -> TypedParamInLambda x1_actoQ))
        (\ x_actoR
            -> case x_actoR of
                TypedParamInLambda y1_actoS -> Right y1_actoS
                _ -> Left x_actoR)
  _TypedUnnamedStarParam
    = (prism (\ x1_actoT -> TypedUnnamedStarParam x1_actoT))
        (\ x_actoU
            -> case x_actoU of
                TypedUnnamedStarParam y1_actoV -> Right y1_actoV
                _ -> Left x_actoU)
  _AsyncWithOutsideCoroutine
    = (prism (\ x1_actoW -> AsyncWithOutsideCoroutine x1_actoW))
        (\ x_actoX
            -> case x_actoX of
                AsyncWithOutsideCoroutine y1_actoY -> Right y1_actoY
                _ -> Left x_actoX)
  _AsyncForOutsideCoroutine
    = (prism (\ x1_actoZ -> AsyncForOutsideCoroutine x1_actoZ))
        (\ x_actp0
            -> case x_actp0 of
                AsyncForOutsideCoroutine y1_actp1 -> Right y1_actp1
                _ -> Left x_actp0)
  _YieldFromInsideCoroutine
    = (prism (\ x1_actp2 -> YieldFromInsideCoroutine x1_actp2))
        (\ x_actp3
            -> case x_actp3 of
                YieldFromInsideCoroutine y1_actp4 -> Right y1_actp4
                _ -> Left x_actp3)
  _AwaitOutsideCoroutine
    = (prism (\ x1_actp5 -> AwaitOutsideCoroutine x1_actp5))
        (\ x_actp6
            -> case x_actp6 of
                AwaitOutsideCoroutine y1_actp7 -> Right y1_actp7
                _ -> Left x_actp6)
  _NullByte
    = (prism (\ x1_actp8 -> NullByte x1_actp8))
        (\ x_actp9
            -> case x_actp9 of
                NullByte y1_actpa -> Right y1_actpa
                _ -> Left x_actp9)
  _NonAsciiInBytes
    = (prism
          (\ (x1_actpb, x2_actpc) -> (NonAsciiInBytes x1_actpb) x2_actpc))
        (\ x_actpd
            -> case x_actpd of
                NonAsciiInBytes y1_actpe y2_actpf -> Right (y1_actpe, y2_actpf)
                _ -> Left x_actpd)
  _DefaultExceptMustBeLast
    = (prism (\ x1_actpg -> DefaultExceptMustBeLast x1_actpg))
        (\ x_actph
            -> case x_actph of
                DefaultExceptMustBeLast y1_actpi -> Right y1_actpi
                _ -> Left x_actph)
  _WildcardImportInDefinition
    = (prism (\ x1_actpj -> WildcardImportInDefinition x1_actpj))
        (\ x_actpk
            -> case x_actpk of
                WildcardImportInDefinition y1_actpl -> Right y1_actpl
                _ -> Left x_actpk)
  _NoKeywordsAfterEmptyStarArg
    = (prism (\ x1_actpm -> NoKeywordsAfterEmptyStarArg x1_actpm))
        (\ x_actpn
            -> case x_actpn of
                NoKeywordsAfterEmptyStarArg y1_actpo -> Right y1_actpo
                _ -> Left x_actpn)
