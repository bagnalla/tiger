-- This module defines a type for identifiers together with an ADT for
-- mapping them to values.

module Symtab (
  Id(..),
  Symtab,
  empty,
  add,
  addS,
  get,
  getS,
  exists,
  keys,
  fold, -- foldRWithKey
  -- Symtab.map, -- Is this necessary? Surely Map is a Functor.
  mapi -- mapWithKey
  ) where

-- Use Haskell's map data structure
import qualified Data.Map.Strict as Map

-- an Id is just a String
newtype Id = Id String
  deriving (Eq, Ord)

-- A Symtab maps Ids to values of some type
type Symtab a = Map.Map Id a

-- The empty Symtab
empty = Map.empty 

-- Add a binding to a Symtab
add k = Map.insert k

addS = add . Id

-- Get the value bound to an Id
get :: Id -> Symtab a -> Maybe a
get = Map.lookup

getS :: String -> Symtab a -> Maybe a
getS = get . Id

-- all :: (a -> Bool) -> Symtab a -> Bool
-- all pred = fold (\_ x acc -> pred x && acc) True

-- Check if an Id is bound in a Symtab
exists :: Id -> Symtab a -> Bool
exists = Map.member

-- Return list of Ids bound in a Symtab
keys :: Symtab a -> [Id]
keys = Map.keys

-- Fold over all key/value pairs
-- fold :: (a -> Id -> b -> a) -> a -> Symtab b -> a
-- fold = Map.foldlWithKey
fold :: (Id -> a -> b -> b) -> b -> Symtab a -> b
fold = Map.foldrWithKey

-- Map values
map :: (a -> b) -> Symtab a -> Symtab b
map = Map.map

-- Map where the function receives the Id as well as the value
mapi :: (Id -> a -> b) -> Symtab a -> Symtab b
mapi = Map.mapWithKey

----------------------
-- Typeclass instances

instance Show Id where
  show (Id s) = s
