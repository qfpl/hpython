{-# language MultiParamTypeClasses #-}
module Language.Python.Optics.Version where

-- |
-- Must obey the law: @downgrade . upgrade = Just@
--
-- This implies that
-- @'fmap' 'upgrade' . 'downgrade'@ is idempotent under Klesili composition:
--
-- @('fmap' 'upgrade' '.' 'downgrade') '<=<' ('fmap' 'upgrade' '.' downgrade) = 'fmap' 'upgrade' '.' 'downgrade'@
class Upgrade from to where
  upgrade :: from -> to
  downgrade :: to -> Maybe from