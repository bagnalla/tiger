module X64Gen (
  codeGen
  ) where

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer

import           Assem (Instr(..))
import           Frame (Frame)
import           Tree (Exp(..), Stm(..))

type CodeGenM =
  WriterT [Instr] (StateT Int Identity) ()

codeGen :: Frame a => a -> Stm -> [Instr]
codeGen frame stm = []


-- Given an integer for the gensym seed (so we don't collide with
-- symbols generated earlier in the compiler), run a CodeGenM
-- computation, returning both the list of Instrs and the modified
-- gensym integer.
runCanon :: CodeGenM -> Int -> ([Instr], Int)
runCanon r i =
  let (((), instrs), i') = runIdentity (runStateT (runWriterT r) i) in
    (instrs, i')
