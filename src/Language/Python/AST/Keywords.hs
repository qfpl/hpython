{-# language OverloadedStrings #-}
module Language.Python.AST.Keywords where

import Papa
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as S

alwaysKeywords :: Set Text
alwaysKeywords =
  S.fromList
  [ "False"
  , "None"
  , "True"
  , "and"
  , "as"
  , "assert"
  , "break"
  , "continue"
  , "def"
  , "del"
  , "elif"
  , "else"
  , "except"
  , "finally"
  , "for"
  , "from"
  , "global"
  , "if"
  , "import"
  , "in"
  , "is"
  , "lambda"
  , "nonlocal"
  , "not"
  , "or"
  , "pass"
  , "raise"
  , "return"
  , "try"
  , "while"
  , "with"
  , "yield"
  ]

data KFalse = KFalse deriving (Eq, Ord, Show)
data KNone = KNone deriving (Eq, Ord, Show)
data KTrue = KTrue deriving (Eq, Ord, Show)
data KAnd = KAnd deriving (Eq, Ord, Show)
data KAs = KAs deriving (Eq, Ord, Show)
data KAssert = KAssert deriving (Eq, Ord, Show)
data KBreak = KBreak deriving (Eq, Ord, Show)
data KClass = KClass deriving (Eq, Ord, Show)
data KContinue = KContinue deriving (Eq, Ord, Show)
data KDef = KDef deriving (Eq, Ord, Show)
data KDel = KDel deriving (Eq, Ord, Show)
data KElif = KElif deriving (Eq, Ord, Show)
data KElse = KElse deriving (Eq, Ord, Show)
data KExcept = KExcept deriving (Eq, Ord, Show)
data KFinally = KFinally deriving (Eq, Ord, Show)
data KFor = KFor deriving (Eq, Ord, Show)
data KFrom = KFrom deriving (Eq, Ord, Show)
data KGlobal = KGlobal deriving (Eq, Ord, Show)
data KIf = KIf deriving (Eq, Ord, Show)
data KImport = KImport deriving (Eq, Ord, Show)
data KIn = KIn deriving (Eq, Ord, Show)
data KIs = KIs deriving (Eq, Ord, Show)
data KLambda = KLambda deriving (Eq, Ord, Show)
data KNonlocal = KNonlocal deriving (Eq, Ord, Show)
data KNot = KNot deriving (Eq, Ord, Show)
data KOr = KOr deriving (Eq, Ord, Show)
data KPass = KPass deriving (Eq, Ord, Show)
data KRaise = KRaise deriving (Eq, Ord, Show)
data KReturn = KReturn deriving (Eq, Ord, Show)
data KTry = KTry deriving (Eq, Ord, Show)
data KWhile = KWhile deriving (Eq, Ord, Show)
data KWith = KWith deriving (Eq, Ord, Show)
data KYield = KYield deriving (Eq, Ord, Show)
data KAwait = KAwait deriving (Eq, Ord, Show)
data KAsync = KAsync deriving (Eq, Ord, Show)
