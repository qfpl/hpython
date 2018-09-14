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
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_anlX v_aivb a_aivc | r_anlX -> v_aivb
                                                      a_aivc where
  _SyntaxError :: Prism' r_anlX (SyntaxError v_aivb a_aivc)
  _PositionalAfterKeywordArg ::
    Prism' r_anlX (a_aivc, Expr v_aivb a_aivc)
  _PositionalAfterKeywordUnpacking ::
    Prism' r_anlX (a_aivc, Expr v_aivb a_aivc)
  _PositionalAfterKeywordParam :: Prism' r_anlX (a_aivc, String)
  _UnexpectedDoubleStarParam :: Prism' r_anlX (a_aivc, String)
  _CannotAssignTo :: Prism' r_anlX (a_aivc, Expr v_aivb a_aivc)
  _CannotDelete :: Prism' r_anlX (a_aivc, Expr v_aivb a_aivc)
  _CannotAugAssignTo :: Prism' r_anlX (a_aivc, Expr v_aivb a_aivc)
  _DuplicateArgument :: Prism' r_anlX (a_aivc, String)
  _ExpectedNewlineAfter ::
    Prism' r_anlX (a_aivc, [Whitespace], Statement v_aivb a_aivc,
                    Maybe Newline)
  _UnexpectedNewline :: Prism' r_anlX a_aivc
  _IdentifierReservedWord :: Prism' r_anlX (a_aivc, String)
  _EmptyIdentifier :: Prism' r_anlX a_aivc
  _BadCharacter :: Prism' r_anlX (a_aivc, String)
  _BreakOutsideLoop :: Prism' r_anlX a_aivc
  _ContinueOutsideLoop :: Prism' r_anlX a_aivc
  _ReturnOutsideFunction :: Prism' r_anlX a_aivc
  _NonlocalOutsideFunction :: Prism' r_anlX a_aivc
  _ParametersNonlocal :: Prism' r_anlX (a_aivc, [String])
  _NoBindingNonlocal :: Prism' r_anlX (Ident v_aivb a_aivc)
  _Can'tJoinStringAndBytes :: Prism' r_anlX a_aivc
  _InvalidYield :: Prism' r_anlX a_aivc
  _CommentAfterBackslash :: Prism' r_anlX a_aivc
  _MalformedDecorator :: Prism' r_anlX a_aivc
  _InvalidDictUnpacking :: Prism' r_anlX a_aivc
  _InvalidSetUnpacking :: Prism' r_anlX a_aivc
  _TypedParamInLambda :: Prism' r_anlX a_aivc
  _TypedUnnamedStarParam :: Prism' r_anlX a_aivc
  _AsyncWithOutsideCoroutine :: Prism' r_anlX a_aivc
  _AsyncForOutsideCoroutine :: Prism' r_anlX a_aivc
  _YieldFromInsideCoroutine :: Prism' r_anlX a_aivc
  _AwaitOutsideCoroutine :: Prism' r_anlX a_aivc
  _NullByte :: Prism' r_anlX a_aivc
  _NonAsciiInBytes :: Prism' r_anlX (a_aivc, Char)
  _DefaultExceptMustBeLast :: Prism' r_anlX a_aivc
  _WildcardImportInDefinition :: Prism' r_anlX a_aivc
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
instance AsSyntaxError (SyntaxError v_aivb a_aivc) v_aivb a_aivc where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (prism
          (\ (x1_anlY, x2_anlZ)
            -> (PositionalAfterKeywordArg x1_anlY) x2_anlZ))
        (\ x_anm0
            -> case x_anm0 of
                PositionalAfterKeywordArg y1_anm1 y2_anm2
                  -> Right (y1_anm1, y2_anm2)
                _ -> Left x_anm0)
  _PositionalAfterKeywordUnpacking
    = (prism
          (\ (x1_anm3, x2_anm4)
            -> (PositionalAfterKeywordUnpacking x1_anm3) x2_anm4))
        (\ x_anm5
            -> case x_anm5 of
                PositionalAfterKeywordUnpacking y1_anm6 y2_anm7
                  -> Right (y1_anm6, y2_anm7)
                _ -> Left x_anm5)
  _PositionalAfterKeywordParam
    = (prism
          (\ (x1_anm8, x2_anm9)
            -> (PositionalAfterKeywordParam x1_anm8) x2_anm9))
        (\ x_anma
            -> case x_anma of
                PositionalAfterKeywordParam y1_anmb y2_anmc
                  -> Right (y1_anmb, y2_anmc)
                _ -> Left x_anma)
  _UnexpectedDoubleStarParam
    = (prism
          (\ (x1_anmd, x2_anme)
            -> (UnexpectedDoubleStarParam x1_anmd) x2_anme))
        (\ x_anmf
            -> case x_anmf of
                UnexpectedDoubleStarParam y1_anmg y2_anmh
                  -> Right (y1_anmg, y2_anmh)
                _ -> Left x_anmf)
  _CannotAssignTo
    = (prism
          (\ (x1_anmi, x2_anmj) -> (CannotAssignTo x1_anmi) x2_anmj))
        (\ x_anmk
            -> case x_anmk of
                CannotAssignTo y1_anml y2_anmm -> Right (y1_anml, y2_anmm)
                _ -> Left x_anmk)
  _CannotDelete
    = (prism (\ (x1_anmn, x2_anmo) -> (CannotDelete x1_anmn) x2_anmo))
        (\ x_anmp
            -> case x_anmp of
                CannotDelete y1_anmq y2_anmr -> Right (y1_anmq, y2_anmr)
                _ -> Left x_anmp)
  _CannotAugAssignTo
    = (prism
          (\ (x1_anms, x2_anmt) -> (CannotAugAssignTo x1_anms) x2_anmt))
        (\ x_anmu
            -> case x_anmu of
                CannotAugAssignTo y1_anmv y2_anmw -> Right (y1_anmv, y2_anmw)
                _ -> Left x_anmu)
  _DuplicateArgument
    = (prism
          (\ (x1_anmx, x2_anmy) -> (DuplicateArgument x1_anmx) x2_anmy))
        (\ x_anmz
            -> case x_anmz of
                DuplicateArgument y1_anmA y2_anmB -> Right (y1_anmA, y2_anmB)
                _ -> Left x_anmz)
  _ExpectedNewlineAfter
    = (prism (\ x1_anmC -> ExpectedNewlineAfter x1_anmC))
        (\ x_anmD
            -> case x_anmD of
                ExpectedNewlineAfter y1_anmE -> Right y1_anmE
                _ -> Left x_anmD)
  _UnexpectedNewline
    = (prism (\ x1_anmF -> UnexpectedNewline x1_anmF))
        (\ x_anmG
            -> case x_anmG of
                UnexpectedNewline y1_anmH -> Right y1_anmH
                _ -> Left x_anmG)
  _IdentifierReservedWord
    = (prism
          (\ (x1_anmI, x2_anmJ) -> (IdentifierReservedWord x1_anmI) x2_anmJ))
        (\ x_anmK
            -> case x_anmK of
                IdentifierReservedWord y1_anmL y2_anmM -> Right (y1_anmL, y2_anmM)
                _ -> Left x_anmK)
  _EmptyIdentifier
    = (prism (\ x1_anmN -> EmptyIdentifier x1_anmN))
        (\ x_anmO
            -> case x_anmO of
                EmptyIdentifier y1_anmP -> Right y1_anmP
                _ -> Left x_anmO)
  _BadCharacter
    = (prism (\ (x1_anmQ, x2_anmR) -> (BadCharacter x1_anmQ) x2_anmR))
        (\ x_anmS
            -> case x_anmS of
                BadCharacter y1_anmT y2_anmU -> Right (y1_anmT, y2_anmU)
                _ -> Left x_anmS)
  _BreakOutsideLoop
    = (prism (\ x1_anmV -> BreakOutsideLoop x1_anmV))
        (\ x_anmW
            -> case x_anmW of
                BreakOutsideLoop y1_anmX -> Right y1_anmX
                _ -> Left x_anmW)
  _ContinueOutsideLoop
    = (prism (\ x1_anmY -> ContinueOutsideLoop x1_anmY))
        (\ x_anmZ
            -> case x_anmZ of
                ContinueOutsideLoop y1_ann0 -> Right y1_ann0
                _ -> Left x_anmZ)
  _ReturnOutsideFunction
    = (prism (\ x1_ann1 -> ReturnOutsideFunction x1_ann1))
        (\ x_ann2
            -> case x_ann2 of
                ReturnOutsideFunction y1_ann3 -> Right y1_ann3
                _ -> Left x_ann2)
  _NonlocalOutsideFunction
    = (prism (\ x1_ann4 -> NonlocalOutsideFunction x1_ann4))
        (\ x_ann5
            -> case x_ann5 of
                NonlocalOutsideFunction y1_ann6 -> Right y1_ann6
                _ -> Left x_ann5)
  _ParametersNonlocal
    = (prism
          (\ (x1_ann7, x2_ann8) -> (ParametersNonlocal x1_ann7) x2_ann8))
        (\ x_ann9
            -> case x_ann9 of
                ParametersNonlocal y1_anna y2_annb -> Right (y1_anna, y2_annb)
                _ -> Left x_ann9)
  _NoBindingNonlocal
    = (prism (\ x1_annc -> NoBindingNonlocal x1_annc))
        (\ x_annd
            -> case x_annd of
                NoBindingNonlocal y1_anne -> Right y1_anne
                _ -> Left x_annd)
  _Can'tJoinStringAndBytes
    = (prism (\ x1_annf -> Can'tJoinStringAndBytes x1_annf))
        (\ x_anng
            -> case x_anng of
                Can'tJoinStringAndBytes y1_annh -> Right y1_annh
                _ -> Left x_anng)
  _InvalidYield
    = (prism (\ x1_anni -> InvalidYield x1_anni))
        (\ x_annj
            -> case x_annj of
                InvalidYield y1_annk -> Right y1_annk
                _ -> Left x_annj)
  _CommentAfterBackslash
    = (prism (\ x1_annl -> CommentAfterBackslash x1_annl))
        (\ x_annm
            -> case x_annm of
                CommentAfterBackslash y1_annn -> Right y1_annn
                _ -> Left x_annm)
  _MalformedDecorator
    = (prism (\ x1_anno -> MalformedDecorator x1_anno))
        (\ x_annp
            -> case x_annp of
                MalformedDecorator y1_annq -> Right y1_annq
                _ -> Left x_annp)
  _InvalidDictUnpacking
    = (prism (\ x1_annr -> InvalidDictUnpacking x1_annr))
        (\ x_anns
            -> case x_anns of
                InvalidDictUnpacking y1_annt -> Right y1_annt
                _ -> Left x_anns)
  _InvalidSetUnpacking
    = (prism (\ x1_annu -> InvalidSetUnpacking x1_annu))
        (\ x_annv
            -> case x_annv of
                InvalidSetUnpacking y1_annw -> Right y1_annw
                _ -> Left x_annv)
  _TypedParamInLambda
    = (prism (\ x1_annx -> TypedParamInLambda x1_annx))
        (\ x_anny
            -> case x_anny of
                TypedParamInLambda y1_annz -> Right y1_annz
                _ -> Left x_anny)
  _TypedUnnamedStarParam
    = (prism (\ x1_annA -> TypedUnnamedStarParam x1_annA))
        (\ x_annB
            -> case x_annB of
                TypedUnnamedStarParam y1_annC -> Right y1_annC
                _ -> Left x_annB)
  _AsyncWithOutsideCoroutine
    = (prism (\ x1_annD -> AsyncWithOutsideCoroutine x1_annD))
        (\ x_annE
            -> case x_annE of
                AsyncWithOutsideCoroutine y1_annF -> Right y1_annF
                _ -> Left x_annE)
  _AsyncForOutsideCoroutine
    = (prism (\ x1_annG -> AsyncForOutsideCoroutine x1_annG))
        (\ x_annH
            -> case x_annH of
                AsyncForOutsideCoroutine y1_annI -> Right y1_annI
                _ -> Left x_annH)
  _YieldFromInsideCoroutine
    = (prism (\ x1_annJ -> YieldFromInsideCoroutine x1_annJ))
        (\ x_annK
            -> case x_annK of
                YieldFromInsideCoroutine y1_annL -> Right y1_annL
                _ -> Left x_annK)
  _AwaitOutsideCoroutine
    = (prism (\ x1_annM -> AwaitOutsideCoroutine x1_annM))
        (\ x_annN
            -> case x_annN of
                AwaitOutsideCoroutine y1_annO -> Right y1_annO
                _ -> Left x_annN)
  _NullByte
    = (prism (\ x1_annP -> NullByte x1_annP))
        (\ x_annQ
            -> case x_annQ of
                NullByte y1_annR -> Right y1_annR
                _ -> Left x_annQ)
  _NonAsciiInBytes
    = (prism
          (\ (x1_annS, x2_annT) -> (NonAsciiInBytes x1_annS) x2_annT))
        (\ x_annU
            -> case x_annU of
                NonAsciiInBytes y1_annV y2_annW -> Right (y1_annV, y2_annW)
                _ -> Left x_annU)
  _DefaultExceptMustBeLast
    = (prism (\ x1_annX -> DefaultExceptMustBeLast x1_annX))
        (\ x_annY
            -> case x_annY of
                DefaultExceptMustBeLast y1_annZ -> Right y1_annZ
                _ -> Left x_annY)
  _WildcardImportInDefinition
    = (prism (\ x1_ano0 -> WildcardImportInDefinition x1_ano0))
        (\ x_ano1
            -> case x_ano1 of
                WildcardImportInDefinition y1_ano2 -> Right y1_ano2
                _ -> Left x_ano1)
