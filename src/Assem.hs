module Assem (
  Instr(..),
  Oper(..),
  Reg
  ) where

import qualified Temp (Label, Temp)

type Reg = String
type Temp = Temp.Temp
type Label = Temp.Label

data Instr =
  IOper Oper
  | ILabel Lab
  | IMove Mov

data Oper = Oper
  { oper_assem :: String,
    oper_dst   :: [Temp],
    oper_src   :: [Temp],
    oper_jump  :: Maybe [Label] }

data Lab = Lab
  { lab_assem :: String,
    lab_lab   :: Label }

data Mov = Mov
  { mov_assem :: String,
    mov_dst   :: Temp,
    mov_src   :: Temp }

format :: (Temp -> String) -> Instr -> String
format f i = ""
