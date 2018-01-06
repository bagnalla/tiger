{-# LANGUAGE TypeFamilies #-}

module Frame (
  Frame(..)
  ) where

import Temp (Label, Temp)
import Tree (Exp)

class Frame a where
  type Access a :: *

  newFrame   :: Label -> [Bool] -> a
  name       :: a -> Label
  formals    :: a -> [Access a]
  allocLocal :: a -> Bool -> Access a

  -- Surely there's a way to map the type 'a' to these values directly
  -- rather than requiring a frame to be passed in merely for instance
  -- resolution...
  fp         :: a -> Temp
  wordSize   :: a -> Int
  exp        :: a -> Access a -> Exp -> Exp
