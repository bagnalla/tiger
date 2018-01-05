{-# LANGUAGE TypeFamilies #-}

module Frame (
  Frame(..)
  ) where

import Temp (Label)

class Frame a where
  type Access a :: *
  newFrame :: Label -> [Bool] -> a
  name :: a -> Label
  formals :: a -> [Access a]
  allocLocal :: a -> Bool -> Access a
  wordSize :: a -> Int
