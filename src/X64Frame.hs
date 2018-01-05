{-# LANGUAGE TypeFamilies #-}

module X64Frame (
  Access,
  X64Frame
  ) where

import Frame (Frame(..))
import Temp (Label(..))

data X64Access =
  X64InFrame Int
  | X64InReg String

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

data X64Frame =
  X64Frame { x64frame_formals   :: [X64Access],
             x64frame_viewshift :: [X64Instr],
             x64frame_numlocals :: Int,
             x64frame_label     :: Label }

-- Save the current base pointer by pushing it onto the stack, then copy
-- the stack pointer value into the base pointer. At this point, the stack
-- pointer still needs to be decremented to allocate the activation frame.
x64ViewShift = [X64Push (X64InReg "bp"),
                X64Mov  (X64InReg "sp") (X64InReg "bp")]

x64WordSize = 8

instance Frame X64Frame where
  type Access X64Frame = X64Access
  newFrame lbl b =
    X64Frame { x64frame_formals = [],
               x64frame_viewshift = x64ViewShift,
               x64frame_numlocals = 0,
               x64frame_label = lbl }

  name           = x64frame_label
  formals        = x64frame_formals
  allocLocal f b = X64InFrame 0
  wordSize _     = x64WordSize