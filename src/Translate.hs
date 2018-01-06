{-# LANGUAGE GADTs #-}

module Translate (
  Access, Exp, Level, allocLocal, dummyExp, fieldVar, formals, newLevel,
  outermost, simpleVar, subscriptVar
  ) where

import           Control.Monad.State

import           Frame (Frame, name, newFrame)
import qualified Frame as F (Access, allocLocal, exp, formals, fp,
                             wordSize)
import           Temp (Label, newLabel, newTemp)
import           Tree (Binop(..), Relop(..), Stm(..))
import qualified Tree as T (Exp(..))

-- Is there an actual way to do this?
instance Show (a -> b) where
  show f = "<#function>"

data Exp =
  Ex T.Exp                     -- "expression"
  | Nx Stm                     -- "no result"
  | Cx ((Label, Label) -> Stm) -- "conditional"
  deriving (Show)

dummyExp :: Exp
dummyExp = Ex (T.EConst 0)

-- Convenience function for building sequence Stms.
seqstm :: [Stm] -> Stm
seqstm (x : y : _ : xs) = SSeq x (seqstm (y : xs))
seqstm (x : y : _) = SSeq x y
seqstm _ = error "seqstm: requires at least two Stms"

-- Convert Exp to T.Exp
unEx :: (Num s, Show s, MonadState s m) => Exp -> m T.Exp
unEx (Ex exp) = return exp
unEx (Nx (SExp e)) = return e
-- unEx (Nx (T.SMove (e1, e2))) = return e2
-- unEx (Nx (T.SJump e _)) = return e
unEx (Nx _) = error "unEx: bad Stm argument"
unEx (Cx genstm) = do
  t <- newTemp ()
  t_lbl <- newLabel ()
  f_lbl <- newLabel ()
  let temp = T.ETemp t
  return $ T.ESeq (seqstm [SMove temp (T.EConst 1),
                            genstm (t_lbl, f_lbl),
                            SLabel f_lbl,
                            SMove temp (T.EConst 0),
                            SLabel t_lbl])
    temp

-- Convert Exp to T.Stm
unNx :: (Num s, Show s, MonadState s m) => Exp -> m Stm
unNx (Ex (T.ESeq stm _)) = return stm
unNx (Ex _) = error "unNx: bad Exp argument"
unNx (Nx stm) = return stm
-- unNx (Cx genstm) = do
--   e <- unEx (Cx genstm)
--   return $ T.SExp e
unNx (Cx genstm) = do
  -- e <- unEx (Cx genstm)
  return SExp <*> unEx (Cx genstm) -- applicative style

-- Convert Exp to a "genstm", a function from two labels to a
-- statement that jumps to one of them.
unCx :: Exp -> ((Label, Label) -> Stm)
unCx (Ex (T.EConst 0)) = \(_, f) -> SJump (T.EName f) [f]
unCx (Ex (T.EConst 1)) = \(t, _) -> SJump (T.EName t) [t]
unCx (Ex exp) = \(t, f) -> SCJump REq exp (T.EConst 0) f t
unCx (Nx _) = error "unCx: Nx constructor should never appear here"
unCx (Cx genstm) = genstm

-- data Level a =
--   LOutermost
--   | Level (Level a) a

-- The class constraint here may not be strictly necessary, but it's
-- probably a good idea to express the invariant explicitly so the
-- type system can enforce it (that this datatype is only instantiated
-- with instances of Frame for the type parameter).
-- For now we just compare names of frames to test identity of levels.
data Level a where
  LOutermost :: Level a
  Level :: Frame a => Level a -> a -> Level a
instance Eq (Level a) where
  LOutermost == LOutermost = True
  Level _ f1 == Level _ f2 = name f1 == name f2

frameOfLevel :: Frame a => Level a -> a
frameOfLevel (Level _ f) = f
frameOfLevel _ = error "frameOfLevel: no frame for outermost level"

parentOfLevel :: Level a -> Level a
parentOfLevel (Level l _) = l
parentOfLevel _ = error "parentOfLevel: outermost level has no parent"

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
  -- Int     -> -- unique id
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

-- Given the declaration level and use level, produce a Tree.Exp that
-- walks the static links from the use level frame pointer to the
-- declaration level frame pointer. The result value is the address of
-- the frame pointer of the declaration level frame.
followLinks :: Frame a => Level a -> Level a -> T.Exp
followLinks lvlDec lvlUse =
  f lvlDec lvlUse (T.ETemp (F.fp (frameOfLevel lvlUse)))
  where f lvlDec lvlUse accum =
          if lvlDec == lvlUse then
            accum
          else
            let frame       = frameOfLevel lvlUse
                static_link = head (F.formals frame) in
              f lvlDec (parentOfLevel lvlUse)
              (F.exp frame static_link accum)

-- The Access argument gives the frame access and the level at which
-- the variable was declared, and the Level argument gives the level
-- at which the variable is being used.
-- First we use followLinks to obtain the correct frame pointer, and
-- then pass it to the Frame exp function to access the variable.
simpleVar :: Frame a => Access a -> Level a -> Exp
simpleVar (Access lvlDec acc) lvlUse =
  let frame = frameOfLevel lvlUse
      fp    = followLinks lvlDec lvlUse in
    Ex (F.exp frame acc fp)

-- MEM(MEM(e) + (i * CONST W))
-- where e is the array var, i is the index, and w is the word size.
subscriptVar :: (Num s, Show s, MonadState s m, Frame a) =>
  Level a -> Exp -> Exp -> m Exp
subscriptVar lvl arr i = do
  let frame = frameOfLevel lvl
      w     = (T.EConst (F.wordSize frame))
  arr' <- unEx arr
  i'   <- unEx i
  return . Ex . T.EMem $ T.EBinop BPlus arr' (T.EBinop BMul i' w)

-- Very similar to above since we just index into the record based on
-- the order of the fields, essentially treating it as an array. The
-- index is passed in from Semant.
fieldVar :: (Num s, Show s, MonadState s m, Frame a) =>
  Level a -> Exp -> Int -> m Exp
fieldVar lvl record i = do
  let frame = frameOfLevel lvl
      w     = (T.EConst (F.wordSize frame))
  record' <- unEx record
  return . Ex . T.EMem $ T.EBinop BPlus record'
    (T.EBinop BMul (T.EConst i) w)
