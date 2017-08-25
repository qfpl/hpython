module Language.Python.AST.Keywords where

import Papa
import Data.Set (Set)

import qualified Data.Set as S

alwaysKeywords :: Set String
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

data KFalse = KFalse deriving (Eq, Show)
data KNone = KNone deriving (Eq, Show)
data KTrue = KTrue deriving (Eq, Show)
data KAnd = KAnd deriving (Eq, Show)
data KAs = KAs deriving (Eq, Show)
data KAssert = KAssert deriving (Eq, Show)
data KBreak = KBreak deriving (Eq, Show)
data KClass = KClass deriving (Eq, Show)
data KContinue = KContinue deriving (Eq, Show)
data KDef = KDef deriving (Eq, Show)
data KDel = KDel deriving (Eq, Show)
data KElif = KElif deriving (Eq, Show)
data KElse = KElse deriving (Eq, Show)
data KExcept = KExcept deriving (Eq, Show)
data KFinally = KFinally deriving (Eq, Show)
data KFor = KFor deriving (Eq, Show)
data KFrom = KFrom deriving (Eq, Show)
data KGlobal = KGlobal deriving (Eq, Show)
data KIf = KIf deriving (Eq, Show)
data KImport = KImport deriving (Eq, Show)
data KIn = KIn deriving (Eq, Show)
data KIs = KIs deriving (Eq, Show)
data KLambda = KLambda deriving (Eq, Show)
data KNonlocal = KNonlocal deriving (Eq, Show)
data KNot = KNot deriving (Eq, Show)
data KOr = KOr deriving (Eq, Show)
data KPass = KPass deriving (Eq, Show)
data KRaise = KRaise deriving (Eq, Show)
data KReturn = KReturn deriving (Eq, Show)
data KTry = KTry deriving (Eq, Show)
data KWhile = KWhile deriving (Eq, Show)
data KWith = KWith deriving (Eq, Show)
data KYield = KYield deriving (Eq, Show)
data KAwait = KAwait deriving (Eq, Show)
