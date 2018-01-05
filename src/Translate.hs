{-# LANGUAGE GADTs #-}

module Translate (
  Access, Exp, Level, allocLocal, dummyExp, formals, newLevel, outermost
  ) where

import           Control.Monad.State

import           Frame (Frame, newFrame)
import qualified Frame as F (Access, allocLocal, formals)
import           Temp (Label, newLabel, newTemp)
import qualified Tree as T (Exp(..), Relop(..), Stm(..))

-- Is there an actual way to do this?
instance Show (a -> b) where
  show f = "<#function>"

data Exp =
  Ex T.Exp                       -- "expression"
  | Nx T.Stm                     -- "no result"
  | Cx ((Label, Label) -> T.Stm) -- "conditional"
  deriving (Show)

dummyExp :: Exp
dummyExp = Ex (T.EConst 0)

-- Convenience function for building sequences of Stms.
seqstm :: [T.Stm] -> T.Stm
seqstm (x : y : _ : xs) = T.SSeq x (seqstm (y : xs))
seqstm (x : y : _) = T.SSeq x y
seqstm _ = error "seqstm: requires at least two Stms"

-- Convert Exp to T.Exp
unEx :: (Num s, Show s, MonadState s m) => Exp -> m T.Exp
unEx (Ex exp) = return exp
unEx (Nx (T.SExp e)) = return e
-- unEx (Nx (T.SMove (e1, e2))) = return e2
-- unEx (Nx (T.SJump e _)) = return e
unEx (Nx _) = error "unEx: bad Stm argument"
unEx (Cx genstm) = do
  t <- newTemp ()
  lbl1 <- newLabel ()
  lbl2 <- newLabel ()
  let temp = T.ETemp t
  return $ T.ESeq (seqstm [T.SMove temp (T.EConst 1),
                            genstm (lbl1, lbl2),
                            T.SLabel lbl1,
                            T.SMove temp (T.EConst 0),
                            T.SLabel lbl2])
    temp

-- Convert Exp to T.Stm
unNx :: (Num s, Show s, MonadState s m) => Exp -> m T.Stm
unNx (Ex (T.ESeq stm _)) = return stm
unNx (Ex _) = error "unNx: bad Exp argument"
unNx (Nx stm) = return stm
-- unNx (Cx genstm) = do
--   e <- unEx (Cx genstm)
--   return $ T.SExp e
unNx (Cx genstm) = do
  -- e <- unEx (Cx genstm)
  return T.SExp <*> unEx (Cx genstm) -- applicative style


-- Convert Exp to a "genstm", a function from two labels to a
-- statement that jumps to one of them.
unCx :: Exp -> ((Label, Label) -> T.Stm)
unCx (Ex (T.EConst 0)) = \(t, _) -> T.SJump (T.EName t) [t]
unCx (Ex (T.EConst 1)) = \(_, f) -> T.SJump (T.EName f) [f]
unCx (Ex exp) = \(t, f) -> T.SCJump T.REq exp (T.EConst 0) f t
unCx (Nx _) = error "unCx: Nx constructor should never appear here"
unCx (Cx genstm) = genstm

-- data Level a =
--   LOutermost
--   | Level (Level a) a

-- The class constraint here may not be strictly necessary, but it's
-- probably a good idea to express the invariant explicitly so the
-- type system can enforce it (that this datatype is only instantiated
-- with instances of Frame for the type parameter).
data Level a where
  LOutermost :: Level a
  Level :: Frame a => Level a -> a -> Level a

frameOfLevel :: Frame a => Level a -> a
frameOfLevel (Level outer f) = f
frameOfLevel _ = error "frameOfLevel: no frame for outermost level"

-- Level plus a machine-specific Access
-- data Access a where
--   -- Access :: Frame a => Level a -> F.Access a -> Access a
--     Access :: Level a -> F.Access a -> Access a

-- This might not need a class constraint for the type system to
-- enforce our invariant. In order for "F.Access a" to typecheck, 'a'
-- must be an instance of Frame.
data Access a =
  Access (Level a) (F.Access a)

outermost :: Frame a => Level a
outermost = LOutermost

-- TODO: maybe change this to take a single record argument as is done
-- the book
newLevel :: Frame a =>
  Level a -> -- parent
  Label   -> -- name
  [Bool]  -> -- formals
  Level a
newLevel lvl lbl formals =
  -- Prepend an additional True for the static link since we treat it
  -- like a formal parameter.
  Level lvl (newFrame lbl (True : [True | _ <- formals]))

formals :: Frame a => Level a -> [Access a]
formals lvl@(Level _ frame) =
  -- Be sure not to return the first formal because it's the static
  -- link.
  [Access lvl x | x <- tail (F.formals frame)]

allocLocal :: Frame a => Level a -> Bool -> Access a
allocLocal lvl b =
  let frame = frameOfLevel lvl
      acc   = F.allocLocal frame b in
    Access lvl acc
