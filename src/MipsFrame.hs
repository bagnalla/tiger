{-# LANGUAGE TypeFamilies #-}

module MipsFrame (
  Access,
  MipsFrame
  ) where

import Frame (Frame(..))
import Temp (Label(..))

data MipsFrame = MipsFrame

instance Frame MipsFrame where
  data Access MipsFrame = MipsAccess
  
  newFrame lbl b = MipsFrame
  
  name f = Label ""
  
  formals f = []

  allocLocal f b = MipsAccess
