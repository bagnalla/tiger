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
  SMove Exp Exp -- Intel syntax: destination then source
  | SExp Exp
  | SJump Exp [Label]
  | SCJump Relop Exp Exp Label Label -- true then false
  | SSeq Stm Stm
  | SLabel Label
  deriving (Show)

data Binop =
  BPlus
  | BMinus
  | BMul
  | BDiv
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
  deriving (Eq, Show)
