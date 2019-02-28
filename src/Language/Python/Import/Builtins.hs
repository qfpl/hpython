{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Language.Python.Import.Builtins where

import Data.ByteString (ByteString)
import Data.Map (Map)

import qualified Data.Map as Map

import System.OS (os, isUnix, isSolaris, isLinux)
import Language.Python.Parse (SrcInfo)
import Language.Python.Syntax.ModuleNames
  (ModuleName, makeModuleName, sameModuleName)
import Language.Python.Syntax.Raw (Raw)
import Language.Python.Validate.Scope (Entry(..),  EntryType(..), Level(..))

lookupModuleName :: ModuleName v a -> [(ModuleName v' a', x)] -> Maybe x
lookupModuleName _ [] = Nothing
lookupModuleName mn ((mn', a) : rest)
  | sameModuleName mn mn' = Just a
  | otherwise = lookupModuleName mn rest

lookupBuiltinModule :: ModuleName v a -> Maybe (Map ByteString (Entry SrcInfo))
lookupBuiltinModule mn = lookupModuleName mn builtinModules

builtinModules :: [(Raw ModuleName, Map ByteString (Entry SrcInfo))]
builtinModules =
  [ (timeModuleName, timeModule)
  , (mathModuleName, mathModule)
  ]

timeModuleName :: Raw ModuleName
timeModuleName = makeModuleName "time" []

timeModule :: Map ByteString (Entry SrcInfo)
timeModule =
  Map.fromList $
  fmap (, Entry Nothing (Just FunctionEntry) [Toplevel] mempty) functions <>
  fmap (, Entry Nothing (Just ClassEntry) [Toplevel] mempty) classes
  where
    functions :: [ByteString]
    functions =
      [ "altzone"
      , "asctime"
      , "clock"
      , "ctime"
      , "daylight"
      , "get_clock_info"
      , "gmtime"
      , "localtime"
      , "mktime"
      , "monotonic"
      , "perf_counter"
      , "process_time"
      , "sleep"
      , "strftime"
      , "strptime"
      , "time"
      , "timezone"
      , "tzname"
      , "tzset"
      ] <>
      either
        (const [])
        (\x ->
           if isUnix x
           then
             [ "clock_getres"
             , "clock_gettime"
             , "clock_settime"
             , "CLOCK_MONOTONIC"
             , "CLOCK_PROCESS_CPUTIME_ID"
             , "CLOCK_REALTIME"
             , "CLOCK_THREAD_CPUTIME_ID"
             ]
           else [])
        os <>
      either
        (const [])
        (\x -> ["CLOCK_HIGHRES" | isSolaris x])
        os <>
      either
        (const [])
        (\x -> ["CLOCK_MONOTONIC_RAW" | isLinux x])
        os

    classes :: [ByteString]
    classes = [ "struct_time" ]

mathModuleName :: Raw ModuleName
mathModuleName = makeModuleName "math" []

mathModule :: Map ByteString (Entry SrcInfo)
mathModule =
  Map.fromList $
  fmap (, Entry Nothing (Just FunctionEntry) [Toplevel] mempty) functions <>
  fmap (, Entry Nothing (Just VarEntry) [Toplevel] mempty) vars
  where
    functions :: [ByteString]
    functions =
      [ "ceil"
      , "copysign"
      , "fabs"
      , "factorial"
      , "floor"
      , "fmod"
      , "frexp"
      , "fsum"
      , "gcd"
      , "isclose"
      , "isfinite"
      , "isinf"
      , "isnan"
      , "ldexp"
      , "modf"
      , "trunc"
      , "exp"
      , "expm1"
      , "log"
      , "log1p"
      , "log2"
      , "log10"
      , "pow"
      , "sqrt"
      , "acos"
      , "asin"
      , "atan"
      , "atan2"
      , "cos"
      , "hypot"
      , "sin"
      , "tan"
      , "degrees"
      , "radians"
      , "acosh"
      , "asinh"
      , "atanh"
      , "cosh"
      , "sinh"
      , "tanh"
      , "erf"
      , "erfc"
      , "gamma"
      , "lgamma"
      ]

    vars :: [ByteString]
    vars =
      [ "pi"
      , "e"
      , "inf"
      , "nan"
      ]