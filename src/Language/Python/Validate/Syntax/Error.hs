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
  | ContinueInsideFinally a
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_adm1P v_adlAT a_adlAU | r_adm1P -> v_adlAT
                                                          a_adlAU where
  _SyntaxError :: Prism' r_adm1P (SyntaxError v_adlAT a_adlAU)
  _PositionalAfterKeywordArg ::
    Prism' r_adm1P (a_adlAU, Expr v_adlAT a_adlAU)
  _PositionalAfterKeywordUnpacking ::
    Prism' r_adm1P (a_adlAU, Expr v_adlAT a_adlAU)
  _PositionalAfterKeywordParam :: Prism' r_adm1P (a_adlAU, String)
  _UnexpectedDoubleStarParam :: Prism' r_adm1P (a_adlAU, String)
  _CannotAssignTo :: Prism' r_adm1P (a_adlAU, Expr v_adlAT a_adlAU)
  _CannotDelete :: Prism' r_adm1P (a_adlAU, Expr v_adlAT a_adlAU)
  _CannotAugAssignTo ::
    Prism' r_adm1P (a_adlAU, Expr v_adlAT a_adlAU)
  _DuplicateArgument :: Prism' r_adm1P (a_adlAU, String)
  _ExpectedNewlineAfter ::
    Prism' r_adm1P (a_adlAU, [Whitespace], Statement v_adlAT a_adlAU,
                    Maybe Newline)
  _UnexpectedNewline :: Prism' r_adm1P a_adlAU
  _IdentifierReservedWord :: Prism' r_adm1P (a_adlAU, String)
  _EmptyIdentifier :: Prism' r_adm1P a_adlAU
  _BadCharacter :: Prism' r_adm1P (a_adlAU, String)
  _BreakOutsideLoop :: Prism' r_adm1P a_adlAU
  _ContinueOutsideLoop :: Prism' r_adm1P a_adlAU
  _ReturnOutsideFunction :: Prism' r_adm1P a_adlAU
  _NonlocalOutsideFunction :: Prism' r_adm1P a_adlAU
  _ParametersNonlocal :: Prism' r_adm1P (a_adlAU, [String])
  _NoBindingNonlocal :: Prism' r_adm1P (Ident v_adlAT a_adlAU)
  _Can'tJoinStringAndBytes :: Prism' r_adm1P a_adlAU
  _InvalidYield :: Prism' r_adm1P a_adlAU
  _CommentAfterBackslash :: Prism' r_adm1P a_adlAU
  _MalformedDecorator :: Prism' r_adm1P a_adlAU
  _InvalidDictUnpacking :: Prism' r_adm1P a_adlAU
  _InvalidSetUnpacking :: Prism' r_adm1P a_adlAU
  _TypedParamInLambda :: Prism' r_adm1P a_adlAU
  _TypedUnnamedStarParam :: Prism' r_adm1P a_adlAU
  _AsyncWithOutsideCoroutine :: Prism' r_adm1P a_adlAU
  _AsyncForOutsideCoroutine :: Prism' r_adm1P a_adlAU
  _YieldFromInsideCoroutine :: Prism' r_adm1P a_adlAU
  _YieldInsideCoroutine :: Prism' r_adm1P a_adlAU
  _AwaitOutsideCoroutine :: Prism' r_adm1P a_adlAU
  _NullByte :: Prism' r_adm1P a_adlAU
  _NonAsciiInBytes :: Prism' r_adm1P (a_adlAU, Char)
  _DefaultExceptMustBeLast :: Prism' r_adm1P a_adlAU
  _WildcardImportInDefinition :: Prism' r_adm1P a_adlAU
  _NoKeywordsAfterEmptyStarArg :: Prism' r_adm1P a_adlAU
  _ManyStarredTargets :: Prism' r_adm1P a_adlAU
  _ContinueInsideFinally :: Prism' r_adm1P a_adlAU
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
  _ContinueInsideFinally = ((.) _SyntaxError) _ContinueInsideFinally
instance AsSyntaxError (SyntaxError v_adlAT a_adlAU) v_adlAT a_adlAU where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (prism
          (\ (x1_adm1Q, x2_adm1R)
            -> (PositionalAfterKeywordArg x1_adm1Q) x2_adm1R))
        (\ x_adm1S
            -> case x_adm1S of
                PositionalAfterKeywordArg y1_adm1T y2_adm1U
                  -> Right (y1_adm1T, y2_adm1U)
                _ -> Left x_adm1S)
  _PositionalAfterKeywordUnpacking
    = (prism
          (\ (x1_adm1V, x2_adm1W)
            -> (PositionalAfterKeywordUnpacking x1_adm1V) x2_adm1W))
        (\ x_adm1X
            -> case x_adm1X of
                PositionalAfterKeywordUnpacking y1_adm1Y y2_adm1Z
                  -> Right (y1_adm1Y, y2_adm1Z)
                _ -> Left x_adm1X)
  _PositionalAfterKeywordParam
    = (prism
          (\ (x1_adm20, x2_adm21)
            -> (PositionalAfterKeywordParam x1_adm20) x2_adm21))
        (\ x_adm22
            -> case x_adm22 of
                PositionalAfterKeywordParam y1_adm23 y2_adm24
                  -> Right (y1_adm23, y2_adm24)
                _ -> Left x_adm22)
  _UnexpectedDoubleStarParam
    = (prism
          (\ (x1_adm25, x2_adm26)
            -> (UnexpectedDoubleStarParam x1_adm25) x2_adm26))
        (\ x_adm27
            -> case x_adm27 of
                UnexpectedDoubleStarParam y1_adm28 y2_adm29
                  -> Right (y1_adm28, y2_adm29)
                _ -> Left x_adm27)
  _CannotAssignTo
    = (prism
          (\ (x1_adm2a, x2_adm2b) -> (CannotAssignTo x1_adm2a) x2_adm2b))
        (\ x_adm2c
            -> case x_adm2c of
                CannotAssignTo y1_adm2d y2_adm2e -> Right (y1_adm2d, y2_adm2e)
                _ -> Left x_adm2c)
  _CannotDelete
    = (prism
          (\ (x1_adm2f, x2_adm2g) -> (CannotDelete x1_adm2f) x2_adm2g))
        (\ x_adm2h
            -> case x_adm2h of
                CannotDelete y1_adm2i y2_adm2j -> Right (y1_adm2i, y2_adm2j)
                _ -> Left x_adm2h)
  _CannotAugAssignTo
    = (prism
          (\ (x1_adm2k, x2_adm2l) -> (CannotAugAssignTo x1_adm2k) x2_adm2l))
        (\ x_adm2m
            -> case x_adm2m of
                CannotAugAssignTo y1_adm2n y2_adm2o -> Right (y1_adm2n, y2_adm2o)
                _ -> Left x_adm2m)
  _DuplicateArgument
    = (prism
          (\ (x1_adm2p, x2_adm2q) -> (DuplicateArgument x1_adm2p) x2_adm2q))
        (\ x_adm2r
            -> case x_adm2r of
                DuplicateArgument y1_adm2s y2_adm2t -> Right (y1_adm2s, y2_adm2t)
                _ -> Left x_adm2r)
  _ExpectedNewlineAfter
    = (prism (\ x1_adm2u -> ExpectedNewlineAfter x1_adm2u))
        (\ x_adm2v
            -> case x_adm2v of
                ExpectedNewlineAfter y1_adm2w -> Right y1_adm2w
                _ -> Left x_adm2v)
  _UnexpectedNewline
    = (prism (\ x1_adm2x -> UnexpectedNewline x1_adm2x))
        (\ x_adm2y
            -> case x_adm2y of
                UnexpectedNewline y1_adm2z -> Right y1_adm2z
                _ -> Left x_adm2y)
  _IdentifierReservedWord
    = (prism
          (\ (x1_adm2A, x2_adm2B)
            -> (IdentifierReservedWord x1_adm2A) x2_adm2B))
        (\ x_adm2C
            -> case x_adm2C of
                IdentifierReservedWord y1_adm2D y2_adm2E
                  -> Right (y1_adm2D, y2_adm2E)
                _ -> Left x_adm2C)
  _EmptyIdentifier
    = (prism (\ x1_adm2F -> EmptyIdentifier x1_adm2F))
        (\ x_adm2G
            -> case x_adm2G of
                EmptyIdentifier y1_adm2H -> Right y1_adm2H
                _ -> Left x_adm2G)
  _BadCharacter
    = (prism
          (\ (x1_adm2I, x2_adm2J) -> (BadCharacter x1_adm2I) x2_adm2J))
        (\ x_adm2K
            -> case x_adm2K of
                BadCharacter y1_adm2L y2_adm2M -> Right (y1_adm2L, y2_adm2M)
                _ -> Left x_adm2K)
  _BreakOutsideLoop
    = (prism (\ x1_adm2N -> BreakOutsideLoop x1_adm2N))
        (\ x_adm2O
            -> case x_adm2O of
                BreakOutsideLoop y1_adm2P -> Right y1_adm2P
                _ -> Left x_adm2O)
  _ContinueOutsideLoop
    = (prism (\ x1_adm2Q -> ContinueOutsideLoop x1_adm2Q))
        (\ x_adm2R
            -> case x_adm2R of
                ContinueOutsideLoop y1_adm2S -> Right y1_adm2S
                _ -> Left x_adm2R)
  _ReturnOutsideFunction
    = (prism (\ x1_adm2T -> ReturnOutsideFunction x1_adm2T))
        (\ x_adm2U
            -> case x_adm2U of
                ReturnOutsideFunction y1_adm2V -> Right y1_adm2V
                _ -> Left x_adm2U)
  _NonlocalOutsideFunction
    = (prism (\ x1_adm2W -> NonlocalOutsideFunction x1_adm2W))
        (\ x_adm2X
            -> case x_adm2X of
                NonlocalOutsideFunction y1_adm2Y -> Right y1_adm2Y
                _ -> Left x_adm2X)
  _ParametersNonlocal
    = (prism
          (\ (x1_adm2Z, x2_adm30) -> (ParametersNonlocal x1_adm2Z) x2_adm30))
        (\ x_adm31
            -> case x_adm31 of
                ParametersNonlocal y1_adm32 y2_adm33 -> Right (y1_adm32, y2_adm33)
                _ -> Left x_adm31)
  _NoBindingNonlocal
    = (prism (\ x1_adm34 -> NoBindingNonlocal x1_adm34))
        (\ x_adm35
            -> case x_adm35 of
                NoBindingNonlocal y1_adm36 -> Right y1_adm36
                _ -> Left x_adm35)
  _Can'tJoinStringAndBytes
    = (prism (\ x1_adm37 -> Can'tJoinStringAndBytes x1_adm37))
        (\ x_adm38
            -> case x_adm38 of
                Can'tJoinStringAndBytes y1_adm39 -> Right y1_adm39
                _ -> Left x_adm38)
  _InvalidYield
    = (prism (\ x1_adm3a -> InvalidYield x1_adm3a))
        (\ x_adm3b
            -> case x_adm3b of
                InvalidYield y1_adm3c -> Right y1_adm3c
                _ -> Left x_adm3b)
  _CommentAfterBackslash
    = (prism (\ x1_adm3d -> CommentAfterBackslash x1_adm3d))
        (\ x_adm3e
            -> case x_adm3e of
                CommentAfterBackslash y1_adm3f -> Right y1_adm3f
                _ -> Left x_adm3e)
  _MalformedDecorator
    = (prism (\ x1_adm3g -> MalformedDecorator x1_adm3g))
        (\ x_adm3h
            -> case x_adm3h of
                MalformedDecorator y1_adm3i -> Right y1_adm3i
                _ -> Left x_adm3h)
  _InvalidDictUnpacking
    = (prism (\ x1_adm3j -> InvalidDictUnpacking x1_adm3j))
        (\ x_adm3k
            -> case x_adm3k of
                InvalidDictUnpacking y1_adm3l -> Right y1_adm3l
                _ -> Left x_adm3k)
  _InvalidSetUnpacking
    = (prism (\ x1_adm3m -> InvalidSetUnpacking x1_adm3m))
        (\ x_adm3n
            -> case x_adm3n of
                InvalidSetUnpacking y1_adm3o -> Right y1_adm3o
                _ -> Left x_adm3n)
  _TypedParamInLambda
    = (prism (\ x1_adm3p -> TypedParamInLambda x1_adm3p))
        (\ x_adm3q
            -> case x_adm3q of
                TypedParamInLambda y1_adm3r -> Right y1_adm3r
                _ -> Left x_adm3q)
  _TypedUnnamedStarParam
    = (prism (\ x1_adm3s -> TypedUnnamedStarParam x1_adm3s))
        (\ x_adm3t
            -> case x_adm3t of
                TypedUnnamedStarParam y1_adm3u -> Right y1_adm3u
                _ -> Left x_adm3t)
  _AsyncWithOutsideCoroutine
    = (prism (\ x1_adm3v -> AsyncWithOutsideCoroutine x1_adm3v))
        (\ x_adm3w
            -> case x_adm3w of
                AsyncWithOutsideCoroutine y1_adm3x -> Right y1_adm3x
                _ -> Left x_adm3w)
  _AsyncForOutsideCoroutine
    = (prism (\ x1_adm3y -> AsyncForOutsideCoroutine x1_adm3y))
        (\ x_adm3z
            -> case x_adm3z of
                AsyncForOutsideCoroutine y1_adm3A -> Right y1_adm3A
                _ -> Left x_adm3z)
  _YieldFromInsideCoroutine
    = (prism (\ x1_adm3B -> YieldFromInsideCoroutine x1_adm3B))
        (\ x_adm3C
            -> case x_adm3C of
                YieldFromInsideCoroutine y1_adm3D -> Right y1_adm3D
                _ -> Left x_adm3C)
  _YieldInsideCoroutine
    = (prism (\ x1_adm3E -> YieldInsideCoroutine x1_adm3E))
        (\ x_adm3F
            -> case x_adm3F of
                YieldInsideCoroutine y1_adm3G -> Right y1_adm3G
                _ -> Left x_adm3F)
  _AwaitOutsideCoroutine
    = (prism (\ x1_adm3H -> AwaitOutsideCoroutine x1_adm3H))
        (\ x_adm3I
            -> case x_adm3I of
                AwaitOutsideCoroutine y1_adm3J -> Right y1_adm3J
                _ -> Left x_adm3I)
  _NullByte
    = (prism (\ x1_adm3K -> NullByte x1_adm3K))
        (\ x_adm3L
            -> case x_adm3L of
                NullByte y1_adm3M -> Right y1_adm3M
                _ -> Left x_adm3L)
  _NonAsciiInBytes
    = (prism
          (\ (x1_adm3N, x2_adm3O) -> (NonAsciiInBytes x1_adm3N) x2_adm3O))
        (\ x_adm3P
            -> case x_adm3P of
                NonAsciiInBytes y1_adm3Q y2_adm3R -> Right (y1_adm3Q, y2_adm3R)
                _ -> Left x_adm3P)
  _DefaultExceptMustBeLast
    = (prism (\ x1_adm3S -> DefaultExceptMustBeLast x1_adm3S))
        (\ x_adm3T
            -> case x_adm3T of
                DefaultExceptMustBeLast y1_adm3U -> Right y1_adm3U
                _ -> Left x_adm3T)
  _WildcardImportInDefinition
    = (prism (\ x1_adm3V -> WildcardImportInDefinition x1_adm3V))
        (\ x_adm3W
            -> case x_adm3W of
                WildcardImportInDefinition y1_adm3X -> Right y1_adm3X
                _ -> Left x_adm3W)
  _NoKeywordsAfterEmptyStarArg
    = (prism (\ x1_adm3Y -> NoKeywordsAfterEmptyStarArg x1_adm3Y))
        (\ x_adm3Z
            -> case x_adm3Z of
                NoKeywordsAfterEmptyStarArg y1_adm40 -> Right y1_adm40
                _ -> Left x_adm3Z)
  _ManyStarredTargets
    = (prism (\ x1_adm41 -> ManyStarredTargets x1_adm41))
        (\ x_adm42
            -> case x_adm42 of
                ManyStarredTargets y1_adm43 -> Right y1_adm43
                _ -> Left x_adm42)
  _ContinueInsideFinally
    = (prism (\ x1_adm44 -> ContinueInsideFinally x1_adm44))
        (\ x_adm45
            -> case x_adm45 of
                ContinueInsideFinally y1_adm46 -> Right y1_adm46
                _ -> Left x_adm45)
