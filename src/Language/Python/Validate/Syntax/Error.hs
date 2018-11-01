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
  | ParameterMarkedGlobal a String
  deriving (Eq, Show)

-- makeClassyPrisms ''SyntaxError
class AsSyntaxError r_acZ7I v_acYG7 a_acYG8 | r_acZ7I -> v_acYG7
                                                          a_acYG8 where
  _SyntaxError ::
    Control.Lens.Type.Prism' r_acZ7I (SyntaxError v_acYG7 a_acYG8)
  _PositionalAfterKeywordArg ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, Expr v_acYG7 a_acYG8)
  _PositionalAfterKeywordUnpacking ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, Expr v_acYG7 a_acYG8)
  _PositionalAfterKeywordParam ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, String)
  _UnexpectedDoubleStarParam ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, String)
  _CannotAssignTo ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, Expr v_acYG7 a_acYG8)
  _CannotDelete ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, Expr v_acYG7 a_acYG8)
  _CannotAugAssignTo ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, Expr v_acYG7 a_acYG8)
  _DuplicateArgument ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, String)
  _UnexpectedNewline :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _UnexpectedComment :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _IdentifierReservedWord ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, String)
  _EmptyIdentifier :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _BadCharacter :: Control.Lens.Type.Prism' r_acZ7I (a_acYG8, String)
  _BreakOutsideLoop :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _ContinueOutsideLoop :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _ReturnOutsideFunction :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _NonlocalOutsideFunction ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _ParametersNonlocal ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, [String])
  _NoBindingNonlocal ::
    Control.Lens.Type.Prism' r_acZ7I (Ident v_acYG7 a_acYG8)
  _Can'tJoinStringAndBytes ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _YieldOutsideGenerator :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _MalformedDecorator :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _InvalidDictUnpacking :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _InvalidSetUnpacking :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _TypedParamInLambda :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _TypedUnnamedStarParam :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _AsyncWithOutsideCoroutine ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _AsyncForOutsideCoroutine ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _YieldFromInsideCoroutine ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _YieldInsideCoroutine :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _AwaitOutsideCoroutine :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _AwaitInsideComprehension ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _NullByte :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _NonAsciiInBytes ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, Char)
  _DefaultExceptMustBeLast ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _WildcardImportInDefinition ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _NoKeywordsAfterEmptyStarArg ::
    Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _ManyStarredTargets :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _ContinueInsideFinally :: Control.Lens.Type.Prism' r_acZ7I a_acYG8
  _ParameterMarkedGlobal ::
    Control.Lens.Type.Prism' r_acZ7I (a_acYG8, String)
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
  _ParameterMarkedGlobal = ((.) _SyntaxError) _ParameterMarkedGlobal
instance AsSyntaxError (SyntaxError v_acYG7 a_acYG8) v_acYG7 a_acYG8 where
  _SyntaxError = id
  _PositionalAfterKeywordArg
    = (Control.Lens.Prism.prism
          (\ (x1_acZ7J, x2_acZ7K)
            -> (PositionalAfterKeywordArg x1_acZ7J) x2_acZ7K))
        (\ x_acZ7L
            -> case x_acZ7L of
                PositionalAfterKeywordArg y1_acZ7M y2_acZ7N
                  -> Right (y1_acZ7M, y2_acZ7N)
                _ -> Left x_acZ7L)
  _PositionalAfterKeywordUnpacking
    = (Control.Lens.Prism.prism
          (\ (x1_acZ7O, x2_acZ7P)
            -> (PositionalAfterKeywordUnpacking x1_acZ7O) x2_acZ7P))
        (\ x_acZ7Q
            -> case x_acZ7Q of
                PositionalAfterKeywordUnpacking y1_acZ7R y2_acZ7S
                  -> Right (y1_acZ7R, y2_acZ7S)
                _ -> Left x_acZ7Q)
  _PositionalAfterKeywordParam
    = (Control.Lens.Prism.prism
          (\ (x1_acZ7T, x2_acZ7U)
            -> (PositionalAfterKeywordParam x1_acZ7T) x2_acZ7U))
        (\ x_acZ7V
            -> case x_acZ7V of
                PositionalAfterKeywordParam y1_acZ7W y2_acZ7X
                  -> Right (y1_acZ7W, y2_acZ7X)
                _ -> Left x_acZ7V)
  _UnexpectedDoubleStarParam
    = (Control.Lens.Prism.prism
          (\ (x1_acZ7Y, x2_acZ7Z)
            -> (UnexpectedDoubleStarParam x1_acZ7Y) x2_acZ7Z))
        (\ x_acZ80
            -> case x_acZ80 of
                UnexpectedDoubleStarParam y1_acZ81 y2_acZ82
                  -> Right (y1_acZ81, y2_acZ82)
                _ -> Left x_acZ80)
  _CannotAssignTo
    = (Control.Lens.Prism.prism
          (\ (x1_acZ83, x2_acZ84) -> (CannotAssignTo x1_acZ83) x2_acZ84))
        (\ x_acZ85
            -> case x_acZ85 of
                CannotAssignTo y1_acZ86 y2_acZ87 -> Right (y1_acZ86, y2_acZ87)
                _ -> Left x_acZ85)
  _CannotDelete
    = (Control.Lens.Prism.prism
          (\ (x1_acZ88, x2_acZ89) -> (CannotDelete x1_acZ88) x2_acZ89))
        (\ x_acZ8a
            -> case x_acZ8a of
                CannotDelete y1_acZ8b y2_acZ8c -> Right (y1_acZ8b, y2_acZ8c)
                _ -> Left x_acZ8a)
  _CannotAugAssignTo
    = (Control.Lens.Prism.prism
          (\ (x1_acZ8d, x2_acZ8e) -> (CannotAugAssignTo x1_acZ8d) x2_acZ8e))
        (\ x_acZ8f
            -> case x_acZ8f of
                CannotAugAssignTo y1_acZ8g y2_acZ8h -> Right (y1_acZ8g, y2_acZ8h)
                _ -> Left x_acZ8f)
  _DuplicateArgument
    = (Control.Lens.Prism.prism
          (\ (x1_acZ8i, x2_acZ8j) -> (DuplicateArgument x1_acZ8i) x2_acZ8j))
        (\ x_acZ8k
            -> case x_acZ8k of
                DuplicateArgument y1_acZ8l y2_acZ8m -> Right (y1_acZ8l, y2_acZ8m)
                _ -> Left x_acZ8k)
  _UnexpectedNewline
    = (Control.Lens.Prism.prism
          (\ x1_acZ8n -> UnexpectedNewline x1_acZ8n))
        (\ x_acZ8o
            -> case x_acZ8o of
                UnexpectedNewline y1_acZ8p -> Right y1_acZ8p
                _ -> Left x_acZ8o)
  _UnexpectedComment
    = (Control.Lens.Prism.prism
          (\ x1_acZ8q -> UnexpectedComment x1_acZ8q))
        (\ x_acZ8r
            -> case x_acZ8r of
                UnexpectedComment y1_acZ8s -> Right y1_acZ8s
                _ -> Left x_acZ8r)
  _IdentifierReservedWord
    = (Control.Lens.Prism.prism
          (\ (x1_acZ8t, x2_acZ8u)
            -> (IdentifierReservedWord x1_acZ8t) x2_acZ8u))
        (\ x_acZ8v
            -> case x_acZ8v of
                IdentifierReservedWord y1_acZ8w y2_acZ8x
                  -> Right (y1_acZ8w, y2_acZ8x)
                _ -> Left x_acZ8v)
  _EmptyIdentifier
    = (Control.Lens.Prism.prism
          (\ x1_acZ8y -> EmptyIdentifier x1_acZ8y))
        (\ x_acZ8z
            -> case x_acZ8z of
                EmptyIdentifier y1_acZ8A -> Right y1_acZ8A
                _ -> Left x_acZ8z)
  _BadCharacter
    = (Control.Lens.Prism.prism
          (\ (x1_acZ8B, x2_acZ8C) -> (BadCharacter x1_acZ8B) x2_acZ8C))
        (\ x_acZ8D
            -> case x_acZ8D of
                BadCharacter y1_acZ8E y2_acZ8F -> Right (y1_acZ8E, y2_acZ8F)
                _ -> Left x_acZ8D)
  _BreakOutsideLoop
    = (Control.Lens.Prism.prism
          (\ x1_acZ8G -> BreakOutsideLoop x1_acZ8G))
        (\ x_acZ8H
            -> case x_acZ8H of
                BreakOutsideLoop y1_acZ8I -> Right y1_acZ8I
                _ -> Left x_acZ8H)
  _ContinueOutsideLoop
    = (Control.Lens.Prism.prism
          (\ x1_acZ8J -> ContinueOutsideLoop x1_acZ8J))
        (\ x_acZ8K
            -> case x_acZ8K of
                ContinueOutsideLoop y1_acZ8L -> Right y1_acZ8L
                _ -> Left x_acZ8K)
  _ReturnOutsideFunction
    = (Control.Lens.Prism.prism
          (\ x1_acZ8M -> ReturnOutsideFunction x1_acZ8M))
        (\ x_acZ8N
            -> case x_acZ8N of
                ReturnOutsideFunction y1_acZ8O -> Right y1_acZ8O
                _ -> Left x_acZ8N)
  _NonlocalOutsideFunction
    = (Control.Lens.Prism.prism
          (\ x1_acZ8P -> NonlocalOutsideFunction x1_acZ8P))
        (\ x_acZ8Q
            -> case x_acZ8Q of
                NonlocalOutsideFunction y1_acZ8R -> Right y1_acZ8R
                _ -> Left x_acZ8Q)
  _ParametersNonlocal
    = (Control.Lens.Prism.prism
          (\ (x1_acZ8S, x2_acZ8T) -> (ParametersNonlocal x1_acZ8S) x2_acZ8T))
        (\ x_acZ8U
            -> case x_acZ8U of
                ParametersNonlocal y1_acZ8V y2_acZ8W -> Right (y1_acZ8V, y2_acZ8W)
                _ -> Left x_acZ8U)
  _NoBindingNonlocal
    = (Control.Lens.Prism.prism
          (\ x1_acZ8X -> NoBindingNonlocal x1_acZ8X))
        (\ x_acZ8Y
            -> case x_acZ8Y of
                NoBindingNonlocal y1_acZ8Z -> Right y1_acZ8Z
                _ -> Left x_acZ8Y)
  _Can'tJoinStringAndBytes
    = (Control.Lens.Prism.prism
          (\ x1_acZ90 -> Can'tJoinStringAndBytes x1_acZ90))
        (\ x_acZ91
            -> case x_acZ91 of
                Can'tJoinStringAndBytes y1_acZ92 -> Right y1_acZ92
                _ -> Left x_acZ91)
  _YieldOutsideGenerator
    = (Control.Lens.Prism.prism
          (\ x1_acZ93 -> YieldOutsideGenerator x1_acZ93))
        (\ x_acZ94
            -> case x_acZ94 of
                YieldOutsideGenerator y1_acZ95 -> Right y1_acZ95
                _ -> Left x_acZ94)
  _MalformedDecorator
    = (Control.Lens.Prism.prism
          (\ x1_acZ96 -> MalformedDecorator x1_acZ96))
        (\ x_acZ97
            -> case x_acZ97 of
                MalformedDecorator y1_acZ98 -> Right y1_acZ98
                _ -> Left x_acZ97)
  _InvalidDictUnpacking
    = (Control.Lens.Prism.prism
          (\ x1_acZ99 -> InvalidDictUnpacking x1_acZ99))
        (\ x_acZ9a
            -> case x_acZ9a of
                InvalidDictUnpacking y1_acZ9b -> Right y1_acZ9b
                _ -> Left x_acZ9a)
  _InvalidSetUnpacking
    = (Control.Lens.Prism.prism
          (\ x1_acZ9c -> InvalidSetUnpacking x1_acZ9c))
        (\ x_acZ9d
            -> case x_acZ9d of
                InvalidSetUnpacking y1_acZ9e -> Right y1_acZ9e
                _ -> Left x_acZ9d)
  _TypedParamInLambda
    = (Control.Lens.Prism.prism
          (\ x1_acZ9f -> TypedParamInLambda x1_acZ9f))
        (\ x_acZ9g
            -> case x_acZ9g of
                TypedParamInLambda y1_acZ9h -> Right y1_acZ9h
                _ -> Left x_acZ9g)
  _TypedUnnamedStarParam
    = (Control.Lens.Prism.prism
          (\ x1_acZ9i -> TypedUnnamedStarParam x1_acZ9i))
        (\ x_acZ9j
            -> case x_acZ9j of
                TypedUnnamedStarParam y1_acZ9k -> Right y1_acZ9k
                _ -> Left x_acZ9j)
  _AsyncWithOutsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_acZ9l -> AsyncWithOutsideCoroutine x1_acZ9l))
        (\ x_acZ9m
            -> case x_acZ9m of
                AsyncWithOutsideCoroutine y1_acZ9n -> Right y1_acZ9n
                _ -> Left x_acZ9m)
  _AsyncForOutsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_acZ9o -> AsyncForOutsideCoroutine x1_acZ9o))
        (\ x_acZ9p
            -> case x_acZ9p of
                AsyncForOutsideCoroutine y1_acZ9q -> Right y1_acZ9q
                _ -> Left x_acZ9p)
  _YieldFromInsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_acZ9r -> YieldFromInsideCoroutine x1_acZ9r))
        (\ x_acZ9s
            -> case x_acZ9s of
                YieldFromInsideCoroutine y1_acZ9t -> Right y1_acZ9t
                _ -> Left x_acZ9s)
  _YieldInsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_acZ9u -> YieldInsideCoroutine x1_acZ9u))
        (\ x_acZ9v
            -> case x_acZ9v of
                YieldInsideCoroutine y1_acZ9w -> Right y1_acZ9w
                _ -> Left x_acZ9v)
  _AwaitOutsideCoroutine
    = (Control.Lens.Prism.prism
          (\ x1_acZ9x -> AwaitOutsideCoroutine x1_acZ9x))
        (\ x_acZ9y
            -> case x_acZ9y of
                AwaitOutsideCoroutine y1_acZ9z -> Right y1_acZ9z
                _ -> Left x_acZ9y)
  _AwaitInsideComprehension
    = (Control.Lens.Prism.prism
          (\ x1_acZ9A -> AwaitInsideComprehension x1_acZ9A))
        (\ x_acZ9B
            -> case x_acZ9B of
                AwaitInsideComprehension y1_acZ9C -> Right y1_acZ9C
                _ -> Left x_acZ9B)
  _NullByte
    = (Control.Lens.Prism.prism (\ x1_acZ9D -> NullByte x1_acZ9D))
        (\ x_acZ9E
            -> case x_acZ9E of
                NullByte y1_acZ9F -> Right y1_acZ9F
                _ -> Left x_acZ9E)
  _NonAsciiInBytes
    = (Control.Lens.Prism.prism
          (\ (x1_acZ9G, x2_acZ9H) -> (NonAsciiInBytes x1_acZ9G) x2_acZ9H))
        (\ x_acZ9I
            -> case x_acZ9I of
                NonAsciiInBytes y1_acZ9J y2_acZ9K -> Right (y1_acZ9J, y2_acZ9K)
                _ -> Left x_acZ9I)
  _DefaultExceptMustBeLast
    = (Control.Lens.Prism.prism
          (\ x1_acZ9L -> DefaultExceptMustBeLast x1_acZ9L))
        (\ x_acZ9M
            -> case x_acZ9M of
                DefaultExceptMustBeLast y1_acZ9N -> Right y1_acZ9N
                _ -> Left x_acZ9M)
  _WildcardImportInDefinition
    = (Control.Lens.Prism.prism
          (\ x1_acZ9O -> WildcardImportInDefinition x1_acZ9O))
        (\ x_acZ9P
            -> case x_acZ9P of
                WildcardImportInDefinition y1_acZ9Q -> Right y1_acZ9Q
                _ -> Left x_acZ9P)
  _NoKeywordsAfterEmptyStarArg
    = (Control.Lens.Prism.prism
          (\ x1_acZ9R -> NoKeywordsAfterEmptyStarArg x1_acZ9R))
        (\ x_acZ9S
            -> case x_acZ9S of
                NoKeywordsAfterEmptyStarArg y1_acZ9T -> Right y1_acZ9T
                _ -> Left x_acZ9S)
  _ManyStarredTargets
    = (Control.Lens.Prism.prism
          (\ x1_acZ9U -> ManyStarredTargets x1_acZ9U))
        (\ x_acZ9V
            -> case x_acZ9V of
                ManyStarredTargets y1_acZ9W -> Right y1_acZ9W
                _ -> Left x_acZ9V)
  _ContinueInsideFinally
    = (Control.Lens.Prism.prism
          (\ x1_acZ9X -> ContinueInsideFinally x1_acZ9X))
        (\ x_acZ9Y
            -> case x_acZ9Y of
                ContinueInsideFinally y1_acZ9Z -> Right y1_acZ9Z
                _ -> Left x_acZ9Y)
  _ParameterMarkedGlobal
    = (Control.Lens.Prism.prism
          (\ (x1_acZa0, x2_acZa1)
            -> (ParameterMarkedGlobal x1_acZa0) x2_acZa1))
        (\ x_acZa2
            -> case x_acZa2 of
                ParameterMarkedGlobal y1_acZa3 y2_acZa4
                  -> Right (y1_acZa3, y2_acZa4)
                _ -> Left x_acZa2)