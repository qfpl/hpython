{-# language LambdaCase #-}
module Language.Python.AST.Digits where

import Papa
import Data.Digit

data Digit
  = Digit_0
  | Digit_1
  | Digit_2
  | Digit_3
  | Digit_4
  | Digit_5
  | Digit_6
  | Digit_7
  | Digit_8
  | Digit_9
  deriving (Eq, Show)

instance D0 Digit where
  d0 =
    prism'
      (const Digit_0)
      (\case
          Digit_0 -> Just ()
          _ -> Nothing)

instance D1 Digit where
  d1 =
    prism'
      (const Digit_1)
      (\case
          Digit_1 -> Just ()
          _ -> Nothing)

instance D2 Digit where
  d2 =
    prism'
      (const Digit_2)
      (\case
          Digit_2 -> Just ()
          _ -> Nothing)

instance D3 Digit where
  d3 =
    prism'
      (const Digit_3)
      (\case
          Digit_3 -> Just ()
          _ -> Nothing)

instance D4 Digit where
  d4 =
    prism'
      (const Digit_4)
      (\case
          Digit_4 -> Just ()
          _ -> Nothing)

instance D5 Digit where
  d5 =
    prism'
      (const Digit_5)
      (\case
          Digit_5 -> Just ()
          _ -> Nothing)

instance D6 Digit where
  d6 =
    prism'
      (const Digit_6)
      (\case
          Digit_6 -> Just ()
          _ -> Nothing)

instance D7 Digit where
  d7 =
    prism'
      (const Digit_7)
      (\case
          Digit_7 -> Just ()
          _ -> Nothing)

instance D8 Digit where
  d8 =
    prism'
      (const Digit_8)
      (\case
          Digit_8 -> Just ()
          _ -> Nothing)

instance D9 Digit where
  d9 =
    prism'
      (const Digit_9)
      (\case
          Digit_9 -> Just ()
          _ -> Nothing)

data NonZeroDigit
  = NonZeroDigit_1
  | NonZeroDigit_2
  | NonZeroDigit_3
  | NonZeroDigit_4
  | NonZeroDigit_5
  | NonZeroDigit_6
  | NonZeroDigit_7
  | NonZeroDigit_8
  | NonZeroDigit_9
  deriving (Eq, Show)

instance D1 NonZeroDigit where
  d1 =
    prism'
      (const NonZeroDigit_1)
      (\case
          NonZeroDigit_1 -> Just ()
          _ -> Nothing)

instance D2 NonZeroDigit where
  d2 =
    prism'
      (const NonZeroDigit_2)
      (\case
          NonZeroDigit_2 -> Just ()
          _ -> Nothing)

instance D3 NonZeroDigit where
  d3 =
    prism'
      (const NonZeroDigit_3)
      (\case
          NonZeroDigit_3 -> Just ()
          _ -> Nothing)

instance D4 NonZeroDigit where
  d4 =
    prism'
      (const NonZeroDigit_4)
      (\case
          NonZeroDigit_4 -> Just ()
          _ -> Nothing)

instance D5 NonZeroDigit where
  d5 =
    prism'
      (const NonZeroDigit_5)
      (\case
          NonZeroDigit_5 -> Just ()
          _ -> Nothing)

instance D6 NonZeroDigit where
  d6 =
    prism'
      (const NonZeroDigit_6)
      (\case
          NonZeroDigit_6 -> Just ()
          _ -> Nothing)

instance D7 NonZeroDigit where
  d7 =
    prism'
      (const NonZeroDigit_7)
      (\case
          NonZeroDigit_7 -> Just ()
          _ -> Nothing)

instance D8 NonZeroDigit where
  d8 =
    prism'
      (const NonZeroDigit_8)
      (\case
          NonZeroDigit_8 -> Just ()
          _ -> Nothing)

instance D9 NonZeroDigit where
  d9 =
    prism'
      (const NonZeroDigit_9)
      (\case
          NonZeroDigit_9 -> Just ()
          _ -> Nothing)

data OctDigit
  = OctDigit_0
  | OctDigit_1
  | OctDigit_2
  | OctDigit_3
  | OctDigit_4
  | OctDigit_5
  | OctDigit_6
  | OctDigit_7
  deriving (Eq, Show)

instance D0 OctDigit where
  d0 =
    prism'
      (const OctDigit_0)
      (\case
          OctDigit_0 -> Just ()
          _ -> Nothing)

instance D1 OctDigit where
  d1 =
    prism'
      (const OctDigit_1)
      (\case
          OctDigit_1 -> Just ()
          _ -> Nothing)

instance D2 OctDigit where
  d2 =
    prism'
      (const OctDigit_2)
      (\case
          OctDigit_2 -> Just ()
          _ -> Nothing)

instance D3 OctDigit where
  d3 =
    prism'
      (const OctDigit_3)
      (\case
          OctDigit_3 -> Just ()
          _ -> Nothing)

instance D4 OctDigit where
  d4 =
    prism'
      (const OctDigit_4)
      (\case
          OctDigit_4 -> Just ()
          _ -> Nothing)

instance D5 OctDigit where
  d5 =
    prism'
      (const OctDigit_5)
      (\case
          OctDigit_5 -> Just ()
          _ -> Nothing)

instance D6 OctDigit where
  d6 =
    prism'
      (const OctDigit_6)
      (\case
          OctDigit_6 -> Just ()
          _ -> Nothing)

instance D7 OctDigit where
  d7 =
    prism'
      (const OctDigit_7)
      (\case
          OctDigit_7 -> Just ()
          _ -> Nothing)

data HexDigit
  = HexDigit_0
  | HexDigit_1
  | HexDigit_2
  | HexDigit_3
  | HexDigit_4
  | HexDigit_5
  | HexDigit_6
  | HexDigit_7
  | HexDigit_8
  | HexDigit_9
  | HexDigit_a
  | HexDigit_A
  | HexDigit_b
  | HexDigit_B
  | HexDigit_c
  | HexDigit_C
  | HexDigit_d
  | HexDigit_D
  | HexDigit_e
  | HexDigit_E
  | HexDigit_f
  | HexDigit_F
  deriving (Eq, Show)

instance D0 HexDigit where
  d0 =
    prism'
      (const HexDigit_0)
      (\case
          HexDigit_0 -> Just ()
          _ -> Nothing)

instance D1 HexDigit where
  d1 =
    prism'
      (const HexDigit_1)
      (\case
          HexDigit_1 -> Just ()
          _ -> Nothing)

instance D2 HexDigit where
  d2 =
    prism'
      (const HexDigit_2)
      (\case
          HexDigit_2 -> Just ()
          _ -> Nothing)

instance D3 HexDigit where
  d3 =
    prism'
      (const HexDigit_3)
      (\case
          HexDigit_3 -> Just ()
          _ -> Nothing)

instance D4 HexDigit where
  d4 =
    prism'
      (const HexDigit_4)
      (\case
          HexDigit_4 -> Just ()
          _ -> Nothing)

instance D5 HexDigit where
  d5 =
    prism'
      (const HexDigit_5)
      (\case
          HexDigit_5 -> Just ()
          _ -> Nothing)

instance D6 HexDigit where
  d6 =
    prism'
      (const HexDigit_6)
      (\case
          HexDigit_6 -> Just ()
          _ -> Nothing)

instance D7 HexDigit where
  d7 =
    prism'
      (const HexDigit_7)
      (\case
          HexDigit_7 -> Just ()
          _ -> Nothing)

instance D8 HexDigit where
  d8 =
    prism'
      (const HexDigit_8)
      (\case
          HexDigit_8 -> Just ()
          _ -> Nothing)

instance D9 HexDigit where
  d9 =
    prism'
      (const HexDigit_9)
      (\case
          HexDigit_9 -> Just ()
          _ -> Nothing)

instance Da HexDigit where
  da =
    prism'
      (const HexDigit_a)
      (\case
          HexDigit_a -> Just ()
          _ -> Nothing)

instance DA HexDigit where
  dA =
    prism'
      (const HexDigit_A)
      (\case
          HexDigit_A -> Just ()
          _ -> Nothing)

instance Db HexDigit where
  db =
    prism'
      (const HexDigit_b)
      (\case
          HexDigit_b -> Just ()
          _ -> Nothing)

instance DB HexDigit where
  dB =
    prism'
      (const HexDigit_B)
      (\case
          HexDigit_B -> Just ()
          _ -> Nothing)

instance Dc HexDigit where
  dc =
    prism'
      (const HexDigit_c)
      (\case
          HexDigit_c -> Just ()
          _ -> Nothing)

instance DC HexDigit where
  dC =
    prism'
      (const HexDigit_C)
      (\case
          HexDigit_C -> Just ()
          _ -> Nothing)

instance Dd HexDigit where
  dd =
    prism'
      (const HexDigit_d)
      (\case
          HexDigit_d -> Just ()
          _ -> Nothing)

instance DD HexDigit where
  dD =
    prism'
      (const HexDigit_D)
      (\case
          HexDigit_D -> Just ()
          _ -> Nothing)

instance De HexDigit where
  de =
    prism'
      (const HexDigit_e)
      (\case
          HexDigit_e -> Just ()
          _ -> Nothing)

instance DE HexDigit where
  dE =
    prism'
      (const HexDigit_E)
      (\case
          HexDigit_E -> Just ()
          _ -> Nothing)

instance Df HexDigit where
  df =
    prism'
      (const HexDigit_f)
      (\case
          HexDigit_f -> Just ()
          _ -> Nothing)

instance DF HexDigit where
  dF =
    prism'
      (const HexDigit_F)
      (\case
          HexDigit_F -> Just ()
          _ -> Nothing)

data BinDigit = BinDigit_0 | BinDigit_1 deriving (Eq, Show)

instance D0 BinDigit where
  d0 =
    prism'
      (const BinDigit_0)
      (\case
          BinDigit_0 -> Just ()
          _ -> Nothing)

instance D1 BinDigit where
  d1 =
    prism'
      (const BinDigit_1)
      (\case
          BinDigit_1 -> Just ()
          _ -> Nothing)

printDigit :: Digit -> String
printDigit d =
  case d of
    Digit_0 -> "0"
    Digit_1 -> "1"
    Digit_2 -> "2"
    Digit_3 -> "3"
    Digit_4 -> "4"
    Digit_5 -> "5"
    Digit_6 -> "6"
    Digit_7 -> "7"
    Digit_8 -> "8"
    Digit_9 -> "9"

printNonZeroDigit :: NonZeroDigit -> String
printNonZeroDigit d =
  case d of
    NonZeroDigit_1 -> "1"
    NonZeroDigit_2 -> "2"
    NonZeroDigit_3 -> "3"
    NonZeroDigit_4 -> "4"
    NonZeroDigit_5 -> "5"
    NonZeroDigit_6 -> "6"
    NonZeroDigit_7 -> "7"
    NonZeroDigit_8 -> "8"
    NonZeroDigit_9 -> "9"

printOctDigit :: OctDigit -> String
printOctDigit d =
  case d of
    OctDigit_0 -> "0"
    OctDigit_1 -> "1"
    OctDigit_2 -> "2"
    OctDigit_3 -> "3"
    OctDigit_4 -> "4"
    OctDigit_5 -> "5"
    OctDigit_6 -> "6"
    OctDigit_7 -> "7"

printHexDigit :: HexDigit -> String
printHexDigit d =
  case d of
    HexDigit_0 -> "0"
    HexDigit_1 -> "1"
    HexDigit_2 -> "2"
    HexDigit_3 -> "3"
    HexDigit_4 -> "4"
    HexDigit_5 -> "5"
    HexDigit_6 -> "6"
    HexDigit_7 -> "7"
    HexDigit_8 -> "8"
    HexDigit_9 -> "9"
    HexDigit_a -> "a"
    HexDigit_A -> "A"
    HexDigit_b -> "b"
    HexDigit_B -> "B"
    HexDigit_c -> "c"
    HexDigit_C -> "C"
    HexDigit_d -> "d"
    HexDigit_D -> "D"
    HexDigit_e -> "e"
    HexDigit_E -> "E"
    HexDigit_f -> "f"
    HexDigit_F -> "F"

printBinDigit :: BinDigit -> String
printBinDigit d =
  case d of
    BinDigit_0 -> "0"
    BinDigit_1 -> "1"
