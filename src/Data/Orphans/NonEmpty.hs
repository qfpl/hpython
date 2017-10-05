{-# language TemplateHaskell #-}
module Data.Orphans.NonEmpty where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Deriving

deriveEq1 ''NonEmpty
deriveShow1 ''NonEmpty
