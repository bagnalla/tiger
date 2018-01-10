{-# LANGUAGE TypeFamilies #-}

module X64Frame (
  Access,
  X64Frame
  ) where

import Frame (Frame(..))
import Temp (Label(..), namedLabel, tempConst)
import Tree (Binop(..), Exp(..), Stm(..))

data X64Access =
  X64InFrame Int
  | X64InReg String
  deriving (Show)

data X64Instr =
  X64Mov X64Access X64Access
  | X64Push X64Access
  | X64Pop X64Access
  | X64Call String
  | X64Jmp String
  | X64Ret
  | X64Leave
  | X64Add Int X64Access
  | X64Sub Int X64Access
  | X64Nop
  deriving (Show)

data X64Frame =
  X64Frame { x64frame_formals   :: [X64Access],
             x64frame_viewshift :: [X64Instr],
             x64frame_numlocals :: Int,
             x64frame_label     :: Label }
  deriving (Show)

-- Save the current base pointer by pushing it onto the stack, then copy
-- the stack pointer value into the base pointer. At this point, the stack
-- pointer still needs to be decremented to allocate the activation frame.
x64ViewShift = [X64Push (X64InReg "bp"),
                X64Mov  (X64InReg "sp") (X64InReg "bp")]

x64fp = tempConst "fp"
x64rv = tempConst "rv"

x64WordSize = 8

x64Exp :: X64Access -> Exp -> Exp
x64Exp a e =
  case a of
    X64InFrame i -> EMem (EBinop BPlus e (EConst i))
    X64InReg   s -> ETemp (tempConst s)

x64ExternalCall :: String -> [Exp] -> Exp
x64ExternalCall s es = ECall (EName (namedLabel "s")) es

x64AllocLocal :: X64Frame -> Bool -> (X64Access, X64Frame)
x64AllocLocal f b =
  let n = x64frame_numlocals f
      f' = X64Frame { x64frame_formals   = x64frame_formals f,
                      x64frame_viewshift = x64frame_viewshift f,
                      x64frame_numlocals = n + 1,
                      x64frame_label     = x64frame_label f } in
  (X64InFrame (-(n + 1) * x64WordSize), f')

x64NewFrame :: Label -> [Bool] -> X64Frame
x64NewFrame lbl bs =
  let n = length bs
      offsets = reverse [i * x64WordSize | i <- take n [1..]]
      formals = [X64InFrame i | i <- offsets] in
  X64Frame { x64frame_formals   = formals,
             x64frame_viewshift = x64ViewShift,
             x64frame_numlocals = 0,
             x64frame_label     = lbl }

x64ProcEntryExit1 :: X64Frame -> Stm -> Stm
x64ProcEntryExit1 f s = s

instance Frame X64Frame where
  type Access X64Frame = X64Access

  newFrame       = x64NewFrame
  name           = x64frame_label
  formals        = x64frame_formals
  allocLocal     = x64AllocLocal
  fp _           = x64fp
  rv _           = x64rv
  wordSize _     = x64WordSize
  exp a          = x64Exp

  externalCall _ = x64ExternalCall

  procEntryExit1 a = x64ProcEntryExit1 a
