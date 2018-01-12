{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Tree (
  Binop(..),
  Exp(..),
  Relop(..),
  Stm(..)
  ) where

import Temp (Label, stringOfLabel, stringOfTemp, Temp)
import Sexp(Sexp(..), ToSexp(..))

data Exp =
  EConst Int
  | EName Label
  | ETemp Temp
  | EBinop Binop Exp Exp
  | EMem Exp
  | ECall Exp [Exp]
  | ESeq Stm Exp
  deriving (Eq, Show)

data Stm =
  SMove Exp Exp -- Intel syntax: destination then source
  | SExp Exp
  | SJump Exp [Label]
  | SCJump Relop Exp Exp Label Label -- true then false
  | SSeq Stm Stm
  | SLabel Label
  deriving (Eq, Show)

data Binop =
  BPlus
  | BMinus
  | BMul
  | BDiv
  | BLShift
  | BRShift
  | BARShift
  | BXor
  deriving (Eq, Show)

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

----------------------------------
-- Sexp instances for Exp and Stm

instance ToSexp Exp where
  toSexp (EConst i) =
    SList [SAtom "EConst", SAtom (show i)]
  toSexp (EName l) =
    SList [SAtom "EName", SAtom (stringOfLabel l)]
  toSexp (ETemp t) =
    SList [SAtom "ETemp", SAtom (stringOfTemp t)]
  toSexp (EBinop b e1 e2) =
    SList [SAtom "EBinop", SAtom (show b), toSexp e1, toSexp e2]
  toSexp (EMem e) = SList [SAtom "EMem", toSexp e]
  toSexp (ECall e es) =
    SList [SAtom "ECall", toSexp e, SList (map toSexp es)]
  toSexp (ESeq s e) =
    SList [SAtom "ESeq", toSexp s, toSexp e]

instance ToSexp Stm where
  toSexp (SMove e1 e2) =
    SList [SAtom "SMove", toSexp e1, toSexp e2]
  toSexp (SExp e) =
    SList [SAtom "SExp", toSexp e]
  toSexp (SJump e lbls) =
    SList [SAtom "SJump", toSexp e] -- omitting lbls
  toSexp (SCJump r e1 e2 lbl1 lbl2) =
    SList [SAtom "SCJump", SAtom (show r), toSexp e1, toSexp e2,
           SAtom (show lbl1), SAtom (show lbl2)]
  toSexp (SSeq s1 s2) =
    SList [SAtom "SSeq", toSexp s1, toSexp s2]
  toSexp (SLabel lbl) =
    SList [SAtom "SLabel", SAtom (stringOfLabel lbl)]
