-- TODO: Use GHC.Generics to automatically derive Sexp conversions for
-- datatypes.

module Sexp (
  Sexp(..),
  ToSexp(..),
  ) where

import Data.List (intercalate)

-- Basic S-expression datatype
data Sexp =
  SAtom String
  | SList [Sexp]

-- Typeclass for converting values to Sexps
class ToSexp a where
  toSexp :: a -> Sexp

-- Instance for Haskell lists
instance ToSexp a => ToSexp [a] where
  toSexp xs = SList (map toSexp xs)

-- Serialize an Sexp to a String.
instance Show Sexp where
  show (SAtom s) = s
  show (SList xs) = "(" ++ intercalate " " (map show xs) ++ ")"
