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
class AsSyntaxError r_adm2s v_adlB2 a_adlB3 | r_adm2s -> v_adlB2
                                                          a_adlB3 where
  _SyntaxError :: Prism' r_adm2s (SyntaxError v_adlB2 a_adlB3)
  _PositionalAfterKeywordArg ::
    Prism' r_adm2s (a_adlB3, Expr v_adlB2 a_adlB3)
  _PositionalAfterKeywordUnpacking ::
    Prism' r_adm2s (a_adlB3, Expr v_adlB2 a_adlB3)
  _PositionalAfterKeywordParam :: Prism' r_adm2s (a_adlB3, String)
  _UnexpectedDoubleStarParam :: Prism' r_adm2s (a_adlB3, String)
  _CannotAssignTo :: Prism' r_adm2s (a_adlB3, Expr v_adlB2 a_adlB3)
  _CannotDelete :: Prism' r_adm2s (a_adlB3, Expr v_adlB2 a_adlB3)
  _CannotAugAssignTo ::
    Prism' r_adm2s (a_adlB3, Expr v_adlB2 a_adlB3)
  _DuplicateArgument :: Prism' r_adm2s (a_adlB3, String)
  _ExpectedNewlineAfter ::
    Prism' r_adm2s (a_adlB3, [Whitespace], Statement v_adlB2 a_adlB3,
                    Maybe Newline)
  _UnexpectedNewline :: Prism' r_adm2s a_adlB3
  _IdentifierReservedWord :: Prism' r_adm2s (a_adlB3, String)
  _EmptyIdentifier :: Prism' r_adm2s a_adlB3
  _BadCharacter :: Prism' r_adm2s (a_adlB3, String)
  _BreakOutsideLoop :: Prism' r_adm2s a_adlB3
  _ContinueOutsideLoop :: Prism' r_adm2s a_adlB3
  _ReturnOutsideFunction :: Prism' r_adm2s a_adlB3
  _NonlocalOutsideFunction :: Prism' r_adm2s a_adlB3
  _ParametersNonlocal :: Prism' r_adm2s (a_adlB3, [String])
  _NoBindingNonlocal :: Prism' r_adm2s (Ident v_adlB2 a_adlB3)
  _Can'tJoinStringAndBytes :: Prism' r_adm2s a_adlB3
  _InvalidYield :: Prism' r_adm2s a_adlB3
  _CommentAfterBackslash :: Prism' r_adm2s a_adlB3
  _MalformedDecorator :: Prism' r_adm2s a_adlB3
  _InvalidDictUnpacking :: Prism' r_adm2s a_adlB3
  _InvalidSetUnpacking :: Prism' r_adm2s a_adlB3
  _TypedParamInLambda :: Prism' r_adm2s a_adlB3
  _TypedUnnamedStarParam :: Prism' r_adm2s a_adlB3
  _AsyncWithOutsideCoroutine :: Prism' r_adm2s a_adlB3
  _AsyncForOutsideCoroutine :: Prism' r_adm2s a_adlB3
  _YieldFromInsideCoroutine :: Prism' r_adm2s a_adlB3
  _YieldInsideCoroutine :: Prism' r_adm2s a_adlB3
  _AwaitOutsideCoroutine :: Prism' r_adm2s a_adlB3
  _AwaitInsideComprehension :: Prism' r_adm2s a_adlB3
  _NullByte :: Prism' r_adm2s a_adlB3
  _NonAsciiInBytes :: Prism' r_adm2s (a_adlB3, Char)
  _DefaultExceptMustBeLast :: Prism' r_adm2s a_adlB3
  _WildcardImportInDefinition :: Prism' r_adm2s a_adlB3
  _NoKeywordsAfterEmptyStarArg :: Prism' r_adm2s a_adlB3
  _ManyStarredTargets :: Prism' r_adm2s a_adlB3
  _ContinueInsideFinally :: Prism' r_adm2s a_adlB3
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
instance AsSyntaxError (SyntaxError v_adlB2 a_adlB3) v_adlB2 a_adlB3 where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (prism
          (\ (x1_adm2t, x2_adm2u)
            -> (PositionalAfterKeywordArg x1_adm2t) x2_adm2u))
        (\ x_adm2v
            -> case x_adm2v of
                PositionalAfterKeywordArg y1_adm2w y2_adm2x
                  -> Right (y1_adm2w, y2_adm2x)
                _ -> Left x_adm2v)
  _PositionalAfterKeywordUnpacking
    = (prism
          (\ (x1_adm2y, x2_adm2z)
            -> (PositionalAfterKeywordUnpacking x1_adm2y) x2_adm2z))
        (\ x_adm2A
            -> case x_adm2A of
                PositionalAfterKeywordUnpacking y1_adm2B y2_adm2C
                  -> Right (y1_adm2B, y2_adm2C)
                _ -> Left x_adm2A)
  _PositionalAfterKeywordParam
    = (prism
          (\ (x1_adm2D, x2_adm2E)
            -> (PositionalAfterKeywordParam x1_adm2D) x2_adm2E))
        (\ x_adm2F
            -> case x_adm2F of
                PositionalAfterKeywordParam y1_adm2G y2_adm2H
                  -> Right (y1_adm2G, y2_adm2H)
                _ -> Left x_adm2F)
  _UnexpectedDoubleStarParam
    = (prism
          (\ (x1_adm2I, x2_adm2J)
            -> (UnexpectedDoubleStarParam x1_adm2I) x2_adm2J))
        (\ x_adm2K
            -> case x_adm2K of
                UnexpectedDoubleStarParam y1_adm2L y2_adm2M
                  -> Right (y1_adm2L, y2_adm2M)
                _ -> Left x_adm2K)
  _CannotAssignTo
    = (prism
          (\ (x1_adm2N, x2_adm2O) -> (CannotAssignTo x1_adm2N) x2_adm2O))
        (\ x_adm2P
            -> case x_adm2P of
                CannotAssignTo y1_adm2Q y2_adm2R -> Right (y1_adm2Q, y2_adm2R)
                _ -> Left x_adm2P)
  _CannotDelete
    = (prism
          (\ (x1_adm2S, x2_adm2T) -> (CannotDelete x1_adm2S) x2_adm2T))
        (\ x_adm2U
            -> case x_adm2U of
                CannotDelete y1_adm2V y2_adm2W -> Right (y1_adm2V, y2_adm2W)
                _ -> Left x_adm2U)
  _CannotAugAssignTo
    = (prism
          (\ (x1_adm2X, x2_adm2Y) -> (CannotAugAssignTo x1_adm2X) x2_adm2Y))
        (\ x_adm2Z
            -> case x_adm2Z of
                CannotAugAssignTo y1_adm30 y2_adm31 -> Right (y1_adm30, y2_adm31)
                _ -> Left x_adm2Z)
  _DuplicateArgument
    = (prism
          (\ (x1_adm32, x2_adm33) -> (DuplicateArgument x1_adm32) x2_adm33))
        (\ x_adm34
            -> case x_adm34 of
                DuplicateArgument y1_adm35 y2_adm36 -> Right (y1_adm35, y2_adm36)
                _ -> Left x_adm34)
  _ExpectedNewlineAfter
    = (prism (\ x1_adm37 -> ExpectedNewlineAfter x1_adm37))
        (\ x_adm38
            -> case x_adm38 of
                ExpectedNewlineAfter y1_adm39 -> Right y1_adm39
                _ -> Left x_adm38)
  _UnexpectedNewline
    = (prism (\ x1_adm3a -> UnexpectedNewline x1_adm3a))
        (\ x_adm3b
            -> case x_adm3b of
                UnexpectedNewline y1_adm3c -> Right y1_adm3c
                _ -> Left x_adm3b)
  _IdentifierReservedWord
    = (prism
          (\ (x1_adm3d, x2_adm3e)
            -> (IdentifierReservedWord x1_adm3d) x2_adm3e))
        (\ x_adm3f
            -> case x_adm3f of
                IdentifierReservedWord y1_adm3g y2_adm3h
                  -> Right (y1_adm3g, y2_adm3h)
                _ -> Left x_adm3f)
  _EmptyIdentifier
    = (prism (\ x1_adm3i -> EmptyIdentifier x1_adm3i))
        (\ x_adm3j
            -> case x_adm3j of
                EmptyIdentifier y1_adm3k -> Right y1_adm3k
                _ -> Left x_adm3j)
  _BadCharacter
    = (prism
          (\ (x1_adm3l, x2_adm3m) -> (BadCharacter x1_adm3l) x2_adm3m))
        (\ x_adm3n
            -> case x_adm3n of
                BadCharacter y1_adm3o y2_adm3p -> Right (y1_adm3o, y2_adm3p)
                _ -> Left x_adm3n)
  _BreakOutsideLoop
    = (prism (\ x1_adm3q -> BreakOutsideLoop x1_adm3q))
        (\ x_adm3r
            -> case x_adm3r of
                BreakOutsideLoop y1_adm3s -> Right y1_adm3s
                _ -> Left x_adm3r)
  _ContinueOutsideLoop
    = (prism (\ x1_adm3t -> ContinueOutsideLoop x1_adm3t))
        (\ x_adm3u
            -> case x_adm3u of
                ContinueOutsideLoop y1_adm3v -> Right y1_adm3v
                _ -> Left x_adm3u)
  _ReturnOutsideFunction
    = (prism (\ x1_adm3w -> ReturnOutsideFunction x1_adm3w))
        (\ x_adm3x
            -> case x_adm3x of
                ReturnOutsideFunction y1_adm3y -> Right y1_adm3y
                _ -> Left x_adm3x)
  _NonlocalOutsideFunction
    = (prism (\ x1_adm3z -> NonlocalOutsideFunction x1_adm3z))
        (\ x_adm3A
            -> case x_adm3A of
                NonlocalOutsideFunction y1_adm3B -> Right y1_adm3B
                _ -> Left x_adm3A)
  _ParametersNonlocal
    = (prism
          (\ (x1_adm3C, x2_adm3D) -> (ParametersNonlocal x1_adm3C) x2_adm3D))
        (\ x_adm3E
            -> case x_adm3E of
                ParametersNonlocal y1_adm3F y2_adm3G -> Right (y1_adm3F, y2_adm3G)
                _ -> Left x_adm3E)
  _NoBindingNonlocal
    = (prism (\ x1_adm3H -> NoBindingNonlocal x1_adm3H))
        (\ x_adm3I
            -> case x_adm3I of
                NoBindingNonlocal y1_adm3J -> Right y1_adm3J
                _ -> Left x_adm3I)
  _Can'tJoinStringAndBytes
    = (prism (\ x1_adm3K -> Can'tJoinStringAndBytes x1_adm3K))
        (\ x_adm3L
            -> case x_adm3L of
                Can'tJoinStringAndBytes y1_adm3M -> Right y1_adm3M
                _ -> Left x_adm3L)
  _InvalidYield
    = (prism (\ x1_adm3N -> InvalidYield x1_adm3N))
        (\ x_adm3O
            -> case x_adm3O of
                InvalidYield y1_adm3P -> Right y1_adm3P
                _ -> Left x_adm3O)
  _CommentAfterBackslash
    = (prism (\ x1_adm3Q -> CommentAfterBackslash x1_adm3Q))
        (\ x_adm3R
            -> case x_adm3R of
                CommentAfterBackslash y1_adm3S -> Right y1_adm3S
                _ -> Left x_adm3R)
  _MalformedDecorator
    = (prism (\ x1_adm3T -> MalformedDecorator x1_adm3T))
        (\ x_adm3U
            -> case x_adm3U of
                MalformedDecorator y1_adm3V -> Right y1_adm3V
                _ -> Left x_adm3U)
  _InvalidDictUnpacking
    = (prism (\ x1_adm3W -> InvalidDictUnpacking x1_adm3W))
        (\ x_adm3X
            -> case x_adm3X of
                InvalidDictUnpacking y1_adm3Y -> Right y1_adm3Y
                _ -> Left x_adm3X)
  _InvalidSetUnpacking
    = (prism (\ x1_adm3Z -> InvalidSetUnpacking x1_adm3Z))
        (\ x_adm40
            -> case x_adm40 of
                InvalidSetUnpacking y1_adm41 -> Right y1_adm41
                _ -> Left x_adm40)
  _TypedParamInLambda
    = (prism (\ x1_adm42 -> TypedParamInLambda x1_adm42))
        (\ x_adm43
            -> case x_adm43 of
                TypedParamInLambda y1_adm44 -> Right y1_adm44
                _ -> Left x_adm43)
  _TypedUnnamedStarParam
    = (prism (\ x1_adm45 -> TypedUnnamedStarParam x1_adm45))
        (\ x_adm46
            -> case x_adm46 of
                TypedUnnamedStarParam y1_adm47 -> Right y1_adm47
                _ -> Left x_adm46)
  _AsyncWithOutsideCoroutine
    = (prism (\ x1_adm48 -> AsyncWithOutsideCoroutine x1_adm48))
        (\ x_adm49
            -> case x_adm49 of
                AsyncWithOutsideCoroutine y1_adm4a -> Right y1_adm4a
                _ -> Left x_adm49)
  _AsyncForOutsideCoroutine
    = (prism (\ x1_adm4b -> AsyncForOutsideCoroutine x1_adm4b))
        (\ x_adm4c
            -> case x_adm4c of
                AsyncForOutsideCoroutine y1_adm4d -> Right y1_adm4d
                _ -> Left x_adm4c)
  _YieldFromInsideCoroutine
    = (prism (\ x1_adm4e -> YieldFromInsideCoroutine x1_adm4e))
        (\ x_adm4f
            -> case x_adm4f of
                YieldFromInsideCoroutine y1_adm4g -> Right y1_adm4g
                _ -> Left x_adm4f)
  _YieldInsideCoroutine
    = (prism (\ x1_adm4h -> YieldInsideCoroutine x1_adm4h))
        (\ x_adm4i
            -> case x_adm4i of
                YieldInsideCoroutine y1_adm4j -> Right y1_adm4j
                _ -> Left x_adm4i)
  _AwaitOutsideCoroutine
    = (prism (\ x1_adm4k -> AwaitOutsideCoroutine x1_adm4k))
        (\ x_adm4l
            -> case x_adm4l of
                AwaitOutsideCoroutine y1_adm4m -> Right y1_adm4m
                _ -> Left x_adm4l)
  _AwaitInsideComprehension
    = (prism (\ x1_adm4n -> AwaitInsideComprehension x1_adm4n))
        (\ x_adm4o
            -> case x_adm4o of
                AwaitInsideComprehension y1_adm4p -> Right y1_adm4p
                _ -> Left x_adm4o)
  _NullByte
    = (prism (\ x1_adm4q -> NullByte x1_adm4q))
        (\ x_adm4r
            -> case x_adm4r of
                NullByte y1_adm4s -> Right y1_adm4s
                _ -> Left x_adm4r)
  _NonAsciiInBytes
    = (prism
          (\ (x1_adm4t, x2_adm4u) -> (NonAsciiInBytes x1_adm4t) x2_adm4u))
        (\ x_adm4v
            -> case x_adm4v of
                NonAsciiInBytes y1_adm4w y2_adm4x -> Right (y1_adm4w, y2_adm4x)
                _ -> Left x_adm4v)
  _DefaultExceptMustBeLast
    = (prism (\ x1_adm4y -> DefaultExceptMustBeLast x1_adm4y))
        (\ x_adm4z
            -> case x_adm4z of
                DefaultExceptMustBeLast y1_adm4A -> Right y1_adm4A
                _ -> Left x_adm4z)
  _WildcardImportInDefinition
    = (prism (\ x1_adm4B -> WildcardImportInDefinition x1_adm4B))
        (\ x_adm4C
            -> case x_adm4C of
                WildcardImportInDefinition y1_adm4D -> Right y1_adm4D
                _ -> Left x_adm4C)
  _NoKeywordsAfterEmptyStarArg
    = (prism (\ x1_adm4E -> NoKeywordsAfterEmptyStarArg x1_adm4E))
        (\ x_adm4F
            -> case x_adm4F of
                NoKeywordsAfterEmptyStarArg y1_adm4G -> Right y1_adm4G
                _ -> Left x_adm4F)
  _ManyStarredTargets
    = (prism (\ x1_adm4H -> ManyStarredTargets x1_adm4H))
        (\ x_adm4I
            -> case x_adm4I of
                ManyStarredTargets y1_adm4J -> Right y1_adm4J
                _ -> Left x_adm4I)
  _ContinueInsideFinally
    = (prism (\ x1_adm4K -> ContinueInsideFinally x1_adm4K))
        (\ x_adm4L
            -> case x_adm4L of
                ContinueInsideFinally y1_adm4M -> Right y1_adm4M
                _ -> Left x_adm4L)
