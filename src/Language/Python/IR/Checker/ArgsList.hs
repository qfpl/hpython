{-# language RankNTypes #-}
module Language.Python.IR.Checker.ArgsList where

import Papa
import Data.Functor.Sum

import qualified Language.Python.AST.ArgsList as Safe
import qualified Language.Python.IR.ArgsList as IR
import Language.Python.IR.SyntaxChecker
import Language.Python.IR.SyntaxConfig

checkArgsListArg
  :: SyntaxConfig atomType ctxt -- ^ Desired syntax configuration
  -> ( forall atomType' ctxt'
     . SyntaxConfig atomType' ctxt'
    -> unchecked ann
    -> SyntaxChecker ann (checked atomType' ctxt' ann)) -- ^ Test-checking function
  -> ( forall atomType' ctxt'
     . SyntaxConfig atomType' ctxt'
    -> uncheckedName ann
    -> SyntaxChecker ann (checkedName ann)) -- ^ Name-checking function
  -> Safe.ArgsListArg uncheckedName unchecked ann -- ^ Item to check
  -> SyntaxChecker
       ann
       (Safe.ArgsListArg
         checkedName
         (checked atomType ctxt)
         ann)
checkArgsListArg cfg check checkName (Safe.ArgsListArg l r ann) =
  Safe.ArgsListArg <$>
  checkName cfg l <*>
  traverseOf
    (_Wrapped.traverse._Wrapped.traverse)
    (check cfg)
    r <*>
  pure ann

checkArgsListDoublestarArg
  :: SyntaxConfig atomType ctxt
  -> ( forall atomType' ctxt'
     . SyntaxConfig atomType' ctxt'
    -> uncheckedName ann
    -> SyntaxChecker ann (checkedName ann))
  -> Safe.ArgsListDoublestarArg uncheckedName unchecked ann
  -> SyntaxChecker
       ann
       (Safe.ArgsListDoublestarArg
         checkedName
         (checked atomType ctxt)
         ann)
checkArgsListDoublestarArg cfg checkName (Safe.ArgsListDoublestarArg a ann) =
  Safe.ArgsListDoublestarArg <$>
  traverseOf (_Wrapped.traverse) (checkName cfg) a <*>
  pure ann

checkArgsListStarPart
  :: SyntaxConfig atomType ctxt
  -> ( forall atomType' ctxt'
     . SyntaxConfig atomType' ctxt'
    -> unchecked ann
    -> SyntaxChecker ann (checked atomType' ctxt' ann))
  -> ( forall atomType' ctxt'
     . SyntaxConfig atomType' ctxt'
    -> uncheckedName ann
    -> SyntaxChecker ann (checkedName ann))
  -> Safe.ArgsListStarPart uncheckedName unchecked ann
  -> SyntaxChecker
       ann
       (Safe.ArgsListStarPart
         checkedName
         (checked atomType ctxt)
         ann)
checkArgsListStarPart cfg check checkName e =
  case e of
    Safe.ArgsListStarPartEmpty ann -> pure $ Safe.ArgsListStarPartEmpty ann
    Safe.ArgsListStarPart h t r ann ->
      Safe.ArgsListStarPart <$>
      traverseOf (_Wrapped.traverse) (checkName cfg) h <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkArgsListArg cfg check checkName)
        t <*>
      traverseOf
        (_Wrapped.traverse._Wrapped.traverse)
        (checkArgsListDoublestarArg cfg checkName)
        r <*>
      pure ann

checkArgsList
  :: Safe.HasName checkedName
  => SyntaxConfig atomType ctxt
  -> ( forall atomType' ctxt'
     . SyntaxConfig atomType' ctxt'
    -> unchecked ann
    -> SyntaxChecker ann (checked atomType' ctxt' ann))
  -> ( forall atomType' ctxt'
     . SyntaxConfig atomType' ctxt'
    -> uncheckedName ann
    -> SyntaxChecker ann (checkedName ann))
  -> IR.ArgsList uncheckedName unchecked ann
  -> SyntaxChecker
       ann
       (Safe.ArgsList
         checkedName
         (checked atomType ctxt)
         ann)
checkArgsList cfg check checkName e =
  case e of
    IR.ArgsListAll a b c ann ->
      liftError
        (\(Safe.DuplicateArgumentsError e') -> DuplicateArguments e' ann) $
        Safe.mkArgsListAll <$>
        checkArgsListArg cfg check checkName a <*>
        traverseOf
          (_Wrapped.traverse._Wrapped.traverse)
          (checkArgsListArg cfg check checkName)
          b <*>
        traverseOf
          (_Wrapped.traverse._Wrapped.traverse._Wrapped.traverse)
          starOrDouble
          c <*>
        pure ann
    IR.ArgsListArgsKwargs a ann ->
      liftError
        (\(Safe.DuplicateArgumentsError e') -> DuplicateArguments e' ann) $
        Safe.mkArgsListArgsKwargs <$>
        starOrDouble a <*>
        pure ann
  where
    starOrDouble e' =
      case e' of
        InL a -> InL <$> checkArgsListStarPart cfg check checkName a
        InR a -> InR <$> checkArgsListDoublestarArg cfg checkName a
