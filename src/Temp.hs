module Temp (
  Label(..),
  makeString,
  namedLabel,
  newLabel,
  newTemp,
  Temp
  ) where

import Control.Monad.State

import Gensym (nextNum)
import Symtab (Id(..))

data Temp = Temp String
  deriving (Show)

-- newTemp :: () -> Temp
-- newTemp _ = Temp

newTemp :: (Num s, Show s, MonadState s m) => unit -> m Temp
newTemp _ = do
  n <- nextNum
  return $ Temp (show n)

makeString :: Temp -> String
makeString _ = ""

data Label = Label String
  deriving (Show)

newLabel :: (Num s, Show s, MonadState s m) => unit -> m Label
newLabel _ = do
  n <- nextNum
  return $ Label (show n)

namedLabel :: String -> Label
namedLabel s = Label s
