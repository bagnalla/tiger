module Canon (
  basicBlocks, linearize, traceSchedule
  ) where

import Temp (Label)
import Tree (Stm(..))

linearize :: Stm -> [Stm]
linearize _ = []

basicBlocks :: [Stm] -> ([[Stm]], Label)
basicBlocks stm = basicBlocks stm

traceSchedule :: [[Stm]] -> Label -> [Stm]
traceSchedule _ _ = []
