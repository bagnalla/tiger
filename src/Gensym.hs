-- This module defines an infinite sequence of fresh variable names.

module Gensym (
  nextNum,
  nextSym
  ) where

import Control.Monad.State

-- Works for any string-valued state monad as long as its state is
-- numeric and showable.
nextSym :: (Num s, Show s, MonadState s m) => String -> m String
nextSym prefix = do
  i <- get
  put $ i + 1 -- IMPORTANT: i + 1, not 1 + i. See TransState in Semant.hs.
  return $ prefix ++ show i

nextNum :: (Num s, MonadState s m) => m s
nextNum = do
  i <- get
  put $ i + 1 -- IMPORTANT: i + 1, not 1 + i. See TransState in Semant.hs.
  return $ i
