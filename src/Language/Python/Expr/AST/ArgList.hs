{-# language DataKinds #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
module Language.Python.Expr.AST.ArgList
  ( Argument(..)
  , KeywordArgument(..)
  , PositionalArgument(..)
  , ArgList
  , mkArgListSingleFor
  , mkArgListMany
  , _ArgListSingleFor
  , _ArgListMany
  )
  where

import Papa

import Data.Deriving
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Separated.After
import Data.Separated.Before
import Data.Separated.Between

import Language.Python.AST.IsArgList
import Language.Python.AST.Symbols
import Language.Python.IR.ExprConfig

data Argument test compFor (as :: AtomType) (dctxt :: DefinitionContext) a where
  ArgumentExpr ::
    { _argumentExpr_value :: test 'NotAssignable ctxt a
    , _argumentExpr_ann :: a
    } -> Argument test compFor 'NotAssignable ctxt a
  ArgumentFor ::
    { _argumentFor_lparen :: After [WhitespaceChar] LeftParen
    , _argumentFor_expr :: test 'NotAssignable ctxt a
    , _argumentFor_for
      :: compFor 'NotAssignable ctxt a
    , _argumentFor_rparen :: Before [WhitespaceChar] RightParen
    , _argumentFor_ann :: a
    } -> Argument test compFor 'NotAssignable ctxt a
  ArgumentDefault ::
    { _argumentDefault_left
      :: Compose
           (After [WhitespaceChar])
           (test 'Assignable ctxt)
           a
    , _argumentDefault_right
      :: Compose
           (Before [WhitespaceChar])
           (test 'NotAssignable ctxt)
           a
    , _argumentDefault_ann :: a
    } -> Argument test compFor 'NotAssignable ctxt a
  ArgumentUnpack ::
    { _argumentUnpack_symbol :: Either Asterisk DoubleAsterisk
    , _argumentUnpack_val
      :: Compose
           (Before [WhitespaceChar])
           (test 'NotAssignable ctxt)
           a
    , _argumentUnpack_ann :: a
    } -> Argument test compFor 'NotAssignable ctxt a
$(return [])
deriving instance
  ( Eq1 (test 'Assignable dctxt)
  , Eq1 (test 'NotAssignable dctxt)
  , Eq1 (compFor as dctxt)
  , Eq (test 'NotAssignable dctxt c)
  , Eq (compFor 'NotAssignable dctxt c)
  , Eq c
  )
  => Eq (Argument test compFor as dctxt c)
deriving instance
  ( Show1 (test 'Assignable dctxt)
  , Show1 (test 'NotAssignable dctxt)
  , Show1 (compFor as dctxt)
  , Show (test 'NotAssignable dctxt c)
  , Show (compFor 'NotAssignable dctxt c)
  , Show c)
  => Show (Argument test compFor as dctxt c)
deriving instance
  ( Functor (test 'Assignable dctxt)
  , Functor (test 'NotAssignable dctxt)
  , Functor (compFor as dctxt)
  )
  => Functor (Argument test compFor as dctxt)
deriving instance
  ( Foldable (test 'Assignable dctxt)
  , Foldable (test 'NotAssignable dctxt)
  , Foldable (compFor as dctxt)
  )
  => Foldable (Argument test compFor as dctxt)
deriving instance
  ( Traversable (test 'Assignable dctxt)
  , Traversable (test 'NotAssignable dctxt)
  , Traversable (compFor as dctxt)
  )
  => Traversable (Argument test compFor as dctxt)

data ArgList test compFor (atomType :: AtomType) (ctxt :: DefinitionContext) a
  = ArgListSingleFor
  { _argListSingleFor_expr :: test 'NotAssignable ctxt a
  , _argListSingleFor_for :: compFor 'NotAssignable ctxt a
  , _argListSingleFor_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _argListSingleFor_ann :: a
  }
  | ArgListMany
  { _argListMany_head :: Argument test compFor 'NotAssignable ctxt a
  , _argListMany_tail
    :: Compose
         []
         (Compose
           (Before (Between' [WhitespaceChar] Comma))
           (Argument test compFor 'NotAssignable ctxt))
         a
  , _argListMany_comma :: Maybe (Before [WhitespaceChar] Comma)
  , _argListMany_ann :: a
  }
$(return [])
deriving instance
  ( Eq1 (test 'Assignable dctxt)
  , Eq1 (test 'NotAssignable dctxt)
  , Eq1 (compFor 'NotAssignable dctxt)
  , Eq (test 'NotAssignable dctxt c)
  , Eq (compFor 'NotAssignable dctxt c)
  , Eq c
  )
  => Eq (ArgList test compFor as dctxt c)
deriving instance
  ( Show1 (test 'Assignable dctxt)
  , Show1 (test 'NotAssignable dctxt)
  , Show1 (compFor 'NotAssignable dctxt)
  , Show (test 'NotAssignable dctxt c)
  , Show (compFor 'NotAssignable dctxt c)
  , Show c
  )
  => Show (ArgList test compFor as dctxt c)
deriving instance
  ( Functor (test 'Assignable dctxt)
  , Functor (test 'NotAssignable dctxt)
  , Functor (compFor 'NotAssignable dctxt)
  )
  => Functor (ArgList test compFor as dctxt)
deriving instance
  ( Foldable (test 'Assignable dctxt)
  , Foldable (test 'NotAssignable dctxt)
  , Foldable (compFor 'NotAssignable dctxt)
  )
  => Foldable (ArgList test compFor as dctxt)
deriving instance
  ( Traversable (test 'Assignable dctxt)
  , Traversable (test 'NotAssignable dctxt)
  , Traversable (compFor 'NotAssignable dctxt)
  )
  => Traversable (ArgList test compFor as dctxt)

mkArgListSingleFor
  :: test 'NotAssignable dctxt a
  -> compFor 'NotAssignable dctxt a
  -> Maybe (Before [WhitespaceChar] Comma)
  -> a
  -> Either Void (ArgList test compFor as dctxt a)
mkArgListSingleFor a b c d = Right $ ArgListSingleFor a b c d

_ArgListSingleFor
  :: Prism'
       (Maybe (ArgList test compFor as dctxt a))
       ( test 'NotAssignable dctxt a
       , compFor 'NotAssignable dctxt a
       , Maybe (Before [WhitespaceChar] Comma)
       , a
       )
_ArgListSingleFor =
  prism'
    (\(a, b, c, d) -> either (const Nothing) Just $ mkArgListSingleFor a b c d)
    (\case
        Just (ArgListSingleFor a b c d) -> Just (a, b, c, d)
        _ -> Nothing)

mkArgListMany
  :: Argument test compFor 'NotAssignable dctxt a
  -> Compose
       []
       (Compose
         (Before (Between' [WhitespaceChar] Comma))
         (Argument test compFor 'NotAssignable dctxt))
       a
  -> Maybe (Before [WhitespaceChar] Comma)
  -> a
  -> Either
       (ArgumentError (ArgList test compFor as dctxt a))
       (ArgList test compFor as dctxt a)
mkArgListMany a b c d = keywordBeforePositional $ ArgListMany a b c d

_ArgListMany
  :: Prism'
       (Maybe (ArgList test compFor as dctxt a))
       ( Argument test compFor 'NotAssignable dctxt a
       , Compose
           []
           (Compose
             (Before (Between' [WhitespaceChar] Comma))
             (Argument test compFor 'NotAssignable dctxt))
           a
       , Maybe (Before [WhitespaceChar] Comma)
       , a
       )
_ArgListMany =
  prism'
    (\(a, b, c, d) -> either (const Nothing) Just $ mkArgListMany a b c d)
    (\case
        Just (ArgListMany a b c d) -> Just (a, b, c, d)
        _ -> Nothing)

instance IsArgList (ArgList test compFor as dctxt a) where
  data KeywordArgument (ArgList test compFor as dctxt a)
    = KAKeywordArg (test 'Assignable dctxt a) (test 'NotAssignable dctxt a)
    | KADoubleUnpackArg DoubleAsterisk (test 'NotAssignable dctxt a)

  data PositionalArgument (ArgList test compFor as dctxt a)
    = PASingleFor
        (test 'NotAssignable dctxt a)
        (compFor 'NotAssignable dctxt a)
        (Maybe (Before [WhitespaceChar] Comma))
    | PAExprArg (test 'NotAssignable dctxt a)
    | PAForArg
        (After [WhitespaceChar] LeftParen)
        (test 'NotAssignable dctxt a)
        (compFor 'NotAssignable dctxt a)
        (Before [WhitespaceChar] RightParen)
    | PAUnpackArg
      Asterisk
      (Compose
        (Before [WhitespaceChar])
        (test 'NotAssignable dctxt)
        a)

  arguments a =
    case a of
      ArgListSingleFor b c d _ -> [Right $ PASingleFor b c d]
      ArgListMany h t _ _ ->
        fromArgument h :
        toListOf (_Wrapped.folded._Wrapped.folded.to fromArgument) t
    where
      fromArgument
        :: Argument test compFor as dctxt a
        -> Either
             (KeywordArgument (ArgList test compFor as' dctxt a))
             (PositionalArgument (ArgList test compFor as' dctxt a))
      fromArgument arg =
        case arg of
          ArgumentExpr v _ -> Right $ PAExprArg v
          ArgumentFor b c d e _ -> Right $ PAForArg b c d e
          ArgumentDefault b c _ ->
            Left $ KAKeywordArg (b ^. _Wrapped.after._2) (c ^. _Wrapped.before._2)
          ArgumentUnpack b c _ ->
            case b of
              Left b' -> Right $ PAUnpackArg b' c
              Right b' -> Left $ KADoubleUnpackArg b' (c ^. _Wrapped.before._2)

instance
  ( Eq1 (test 'NotAssignable dctxt)
  , Eq1 (test 'Assignable dctxt)
  , Eq1 (compFor 'NotAssignable dctxt)
  ) => Eq1 (Argument test compFor as dctxt) where
  liftEq = $(makeLiftEq ''Argument)

instance
  ( Show1 (test 'NotAssignable dctxt)
  , Show1 (test 'Assignable dctxt)
  , Show1 (compFor 'NotAssignable dctxt)
  ) => Show1 (Argument test compFor as dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''Argument)

instance
  ( Eq1 (test 'NotAssignable dctxt)
  , Eq1 (test 'Assignable dctxt)
  , Eq1 (compFor 'NotAssignable dctxt)
  ) => Eq1 (ArgList test compFor as dctxt) where
  liftEq = $(makeLiftEq ''ArgList)

instance
  ( Show1 (test 'NotAssignable dctxt)
  , Show1 (test 'Assignable dctxt)
  , Show1 (compFor 'NotAssignable dctxt)
  ) => Show1 (ArgList test compFor as dctxt) where
  liftShowsPrec = $(makeLiftShowsPrec ''ArgList)
