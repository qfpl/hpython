{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# language FunctionalDependencies #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving, QuantifiedConstraints, UndecidableInstances #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}

{-|
Module      : Language.Python.Syntax.Numbers
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Numerical literal values in Python
-}

module Language.Python.Syntax.Numbers
  ( -- * Floats
    module Language.Python.Syntax.Numbers.Float
  , module Language.Python.Syntax.Numbers.FloatLiteral
    -- * Ints
  , module Language.Python.Syntax.Numbers.Imag
  , module Language.Python.Syntax.Numbers.ImagLiteral
    -- * Imaginary numbers
  , module Language.Python.Syntax.Numbers.Int
  , module Language.Python.Syntax.Numbers.IntLiteral
  )
where

import Language.Python.Syntax.Numbers.Float
import Language.Python.Syntax.Numbers.FloatLiteral
import Language.Python.Syntax.Numbers.Imag
import Language.Python.Syntax.Numbers.ImagLiteral
import Language.Python.Syntax.Numbers.Int
import Language.Python.Syntax.Numbers.IntLiteral