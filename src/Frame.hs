{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Frame (
  Frag(..),
  Frame(..)
  ) where

import Temp (Label, Temp)
import Tree (Exp, Stm)

data Frag a where
  FProc   :: Frame a => Stm -> a -> Frag a
  FString :: Label -> String -> Frag a
deriving instance Show a => Show (Frag a)

class Frame a where
  type Access a :: *

  newFrame   :: Label -> [Bool] -> a
  name       :: a -> Label
  formals    :: a -> [Access a]
  allocLocal :: a -> Bool -> (Access a, a)

  -- Surely there's a way to map the type 'a' to these values directly
  -- rather than requiring a frame to be passed in merely for instance
  -- resolution...
  fp         :: a -> Temp
  wordSize   :: a -> Int
  exp        :: a -> Access a -> Exp -> Exp
  
  externalCall :: a -> String -> [Exp] -> Exp
