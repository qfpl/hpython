module Language.Python.Parser.Keywords where

import Papa

import Text.Trifecta hiding (Unspaced(..))
import Language.Python.AST.Keywords
import Language.Python.Parser.Identifier

import Text.Parser.Unspaced

kOr :: (TokenParsing m, Monad m) => Unspaced m KOr
kOr = reserve idStyle "or" $> KOr

kAnd :: (TokenParsing m, Monad m) => m KAnd
kAnd = reserve idStyle "and" $> KAnd

kAs :: (TokenParsing m, Monad m) => m KAs
kAs = reserve idStyle "as" $> KAs

kFrom :: (TokenParsing m, Monad m) => m KFrom
kFrom = reserve idStyle "from" $> KFrom

kIf :: (TokenParsing m, Monad m) => m KIf
kIf = reserve idStyle "if" $> KIf

kIn :: (TokenParsing m, Monad m) => m KIn
kIn = reserve idStyle "in" $> KIn

kFor :: (TokenParsing m, Monad m) => m KFor
kFor = reserve idStyle "for" $> KFor

kDel :: (TokenParsing m, Monad m) => m KDel
kDel = reserve idStyle "del" $> KDel

kPass :: (TokenParsing m, Monad m) => m KPass
kPass = reserve idStyle "pass" $> KPass

kGlobal :: (TokenParsing m, Monad m) => m KGlobal
kGlobal = reserve idStyle "global" $> KGlobal

kNonlocal :: (TokenParsing m, Monad m) => m KNonlocal
kNonlocal = reserve idStyle "nonlocal" $> KNonlocal

kAssert :: (TokenParsing m, Monad m) => m KAssert
kAssert = reserve idStyle "assert" $> KAssert

kBreak :: (TokenParsing m, Monad m) => m KBreak
kBreak = reserve idStyle "break" $> KBreak

kContinue :: (TokenParsing m, Monad m) => m KContinue
kContinue = reserve idStyle "continue" $> KContinue

kReturn :: (TokenParsing m, Monad m) => m KReturn
kReturn = reserve idStyle "return" $> KReturn

kRaise :: (TokenParsing m, Monad m) => m KRaise
kRaise = reserve idStyle "raise" $> KRaise

kClass :: (TokenParsing m, Monad m) => m KClass
kClass = reserve idStyle "class" $> KClass

kDef :: (TokenParsing m, Monad m) => m KDef
kDef = reserve idStyle "def" $> KDef

kWith :: (TokenParsing m, Monad m) => m KWith
kWith = reserve idStyle "with" $> KWith

kExcept :: (TokenParsing m, Monad m) => m KExcept
kExcept = reserve idStyle "except" $> KExcept

kTry :: (TokenParsing m, Monad m) => m KTry
kTry = reserve idStyle "try" $> KTry

kElse :: (TokenParsing m, Monad m) => m KElse
kElse = reserve idStyle "else" $> KElse

kFinally :: (TokenParsing m, Monad m) => m KFinally
kFinally = reserve idStyle "finally" $> KFinally

kWhile :: (TokenParsing m, Monad m) => m KWhile
kWhile = reserve idStyle "while" $> KWhile

kElif :: (TokenParsing m, Monad m) => m KElif
kElif = reserve idStyle "elif" $> KElif

kAsync :: (TokenParsing m, Monad m) => m KAsync
kAsync = reserve idStyle "async" $> KAsync
