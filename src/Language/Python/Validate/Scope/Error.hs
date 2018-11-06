{-# language DataKinds, KindSignatures #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{-|
Module      : Language.Python.Validate.Scope.Error
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Validate.Scope.Error where

-- import Control.Lens.TH
import Control.Lens.Type
import Control.Lens.Prism
import Language.Python.Syntax.Ident

data ScopeError (v :: [*]) a
  = FoundNonlocal a
  | FoundGlobal a
  | DeletedIdent a
  | FoundDynamic a (Ident v a)
  | NotInScope (Ident v a)
  | BadShadowing (Ident v a)
  deriving (Eq, Show)

-- makeClassyPrisms ''ScopeError
class AsScopeError r_ab7T3 v_ab7Nz a_ab7NA | r_ab7T3 -> v_ab7Nz
                                                        a_ab7NA where
  _ScopeError ::
    Control.Lens.Type.Prism' r_ab7T3 (ScopeError v_ab7Nz a_ab7NA)
  _FoundNonlocal :: Control.Lens.Type.Prism' r_ab7T3 a_ab7NA
  _FoundGlobal :: Control.Lens.Type.Prism' r_ab7T3 a_ab7NA
  _DeletedIdent :: Control.Lens.Type.Prism' r_ab7T3 a_ab7NA
  _FoundDynamic ::
    Control.Lens.Type.Prism' r_ab7T3 (a_ab7NA, Ident v_ab7Nz a_ab7NA)
  _NotInScope ::
    Control.Lens.Type.Prism' r_ab7T3 (Ident v_ab7Nz a_ab7NA)
  _BadShadowing ::
    Control.Lens.Type.Prism' r_ab7T3 (Ident v_ab7Nz a_ab7NA)
  _FoundNonlocal = ((.) _ScopeError) _FoundNonlocal
  _FoundGlobal = ((.) _ScopeError) _FoundGlobal
  _DeletedIdent = ((.) _ScopeError) _DeletedIdent
  _FoundDynamic = ((.) _ScopeError) _FoundDynamic
  _NotInScope = ((.) _ScopeError) _NotInScope
  _BadShadowing = ((.) _ScopeError) _BadShadowing
instance AsScopeError (ScopeError v_ab7Nz a_ab7NA) v_ab7Nz a_ab7NA where
  _ScopeError = id
  _FoundNonlocal
    = (Control.Lens.Prism.prism (\ x1_ab7T4 -> FoundNonlocal x1_ab7T4))
        (\ x_ab7T5
            -> case x_ab7T5 of
                FoundNonlocal y1_ab7T6 -> Right y1_ab7T6
                _ -> Left x_ab7T5)
  _FoundGlobal
    = (Control.Lens.Prism.prism (\ x1_ab7T7 -> FoundGlobal x1_ab7T7))
        (\ x_ab7T8
            -> case x_ab7T8 of
                FoundGlobal y1_ab7T9 -> Right y1_ab7T9
                _ -> Left x_ab7T8)
  _DeletedIdent
    = (Control.Lens.Prism.prism (\ x1_ab7Ta -> DeletedIdent x1_ab7Ta))
        (\ x_ab7Tb
            -> case x_ab7Tb of
                DeletedIdent y1_ab7Tc -> Right y1_ab7Tc
                _ -> Left x_ab7Tb)
  _FoundDynamic
    = (Control.Lens.Prism.prism
          (\ (x1_ab7Td, x2_ab7Te) -> (FoundDynamic x1_ab7Td) x2_ab7Te))
        (\ x_ab7Tf
            -> case x_ab7Tf of
                FoundDynamic y1_ab7Tg y2_ab7Th -> Right (y1_ab7Tg, y2_ab7Th)
                _ -> Left x_ab7Tf)
  _NotInScope
    = (Control.Lens.Prism.prism (\ x1_ab7Ti -> NotInScope x1_ab7Ti))
        (\ x_ab7Tj
            -> case x_ab7Tj of
                NotInScope y1_ab7Tk -> Right y1_ab7Tk
                _ -> Left x_ab7Tj)
  _BadShadowing
    = (Control.Lens.Prism.prism (\ x1_ab7Tl -> BadShadowing x1_ab7Tl))
        (\ x_ab7Tm
            -> case x_ab7Tm of
                BadShadowing y1_ab7Tn -> Right y1_ab7Tn
                _ -> Left x_ab7Tm)
