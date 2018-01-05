module Tree (
  Binop(..),
  Exp(..),
  Relop(..),
  Stm(..)
  ) where

import Temp (Label, Temp)

data Exp =
  EConst Int
  | EName Label
  | ETemp Temp
  | EBinop Binop Exp Exp
  | EMem Exp
  | ECall Exp [Exp]
  | ESeq Stm Exp
  deriving (Show)

data Stm =
  SMove Exp Exp
  | SExp Exp
  | SJump Exp [Label]
  | SCJump Relop Exp Exp Label Label
  | SSeq Stm Stm
  | SLabel Label
  deriving (Show)

data Binop =
  BPlus
  | BMinus
  | BMul
  | BDiv
  | BAnd
  | BOr
  | BLShift
  | BRShift
  | BARShift
  | BXor
  deriving (Show)

data Relop =
  REq
  | RNe
  | RLt
  | RGt
  | RLe
  | RGe
  | RULt
  | RULe
  | RUGt
  | RUGe
  deriving (Show)