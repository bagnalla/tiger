{-# LANGUAGE GADTs #-}

module Translate (
  Access, Exp, Level, allocLocal, dummyExp, formals, newLevel, outermost,
  transArray, transAssign, transBreak, transCall, transFrags, transFieldVar,
  transFor, transIf, transInt, transNil, transOp, transRecord, transSeq,
  transSimpleVar, transString, transSubscriptVar, transWhile
  ) where

import           Control.Monad.State

import           Absyn (Oper(..))
import           Frame (Frag(..), Frame, name, newFrame)
import qualified Frame as F (Access, allocLocal, exp, externalCall,
                             formals, fp, wordSize)
import           Temp (Label, namedLabel, newLabel, newTemp)
import           Tree (Binop(..), Relop(..), Stm(..))
import qualified Tree as T (Exp(..))
import           Types (Ty(..))

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
seqstm [x] = x
seqstm _ = error "seqstm: requires at least one Stm"

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
unNx (Ex e) = return (SExp e)
unNx (Nx stm) = return stm
unNx (Cx genstm) = return SExp <*> unEx (Cx genstm)

-- Convert Exp to a "genstm", a function from two labels to a
-- statement that jumps to one of them.
unCx :: Exp -> ((Label, Label) -> Stm)
unCx (Ex (T.EConst 0)) = \(_, f) -> SJump (T.EName f) [f]
unCx (Ex (T.EConst 1)) = \(t, _) -> SJump (T.EName t) [t]
unCx (Ex exp) = \(t, f) -> SCJump REq exp (T.EConst 0) f t
unCx (Nx _) = error "unCx: Nx constructor should never appear here"
unCx (Cx genstm) = genstm

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
  _          == _          = False

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

levelOfAccess :: Access a -> Level a
levelOfAccess (Access lvl _) = lvl

outermost :: Frame a => Level a
outermost = LOutermost

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

allocLocal :: Frame a => Level a -> Bool -> (Access a, Level a)
allocLocal lvl b =
  let frame         = frameOfLevel lvl
      parent        = parentOfLevel lvl
      (acc, frame') = F.allocLocal frame b in
    (Access lvl acc, (Level parent frame'))

-----------------------
-- Tree IR translation.

-- The following functions are used by the typechecker to translate
-- the AST into tree IR.

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
transSimpleVar :: Frame a => Access a -> Level a -> Exp
transSimpleVar (Access lvlDec acc) lvlUse =
  let frame = frameOfLevel lvlUse
      fp    = followLinks lvlDec lvlUse in
    Ex (F.exp frame acc fp)

-- MEM(MEM(e) + (i * CONST W))
-- where e is the array var, i is the index, and w is the word size.
transSubscriptVar :: (Num s, Show s, MonadState s m, Frame a) =>
  Level a -> Exp -> Exp -> m Exp
transSubscriptVar lvl arr i = do
  let frame = frameOfLevel lvl
      w     = (T.EConst (F.wordSize frame))
  arr' <- unEx arr
  i'   <- unEx i
  return . Ex . T.EMem $ T.EBinop BPlus arr' (T.EBinop BMul i' w)

-- Very similar to above since we just index into the record based on
-- the order of the fields, essentially treating it as an array. The
-- index is passed in from Semant.
transFieldVar :: (Num s, Show s, MonadState s m, Frame a) =>
  Level a -> Exp -> Int -> m Exp
transFieldVar lvl record i = do
  let frame = frameOfLevel lvl
      w     = (T.EConst (F.wordSize frame))
  record' <- unEx record
  return . Ex . T.EMem $ T.EBinop BPlus record'
    (T.EBinop BMul (T.EConst i) w)

-------------
-- Constants

-- Not sure what this should be, using constant 0 for now.
transNil :: Exp
transNil = Ex (T.EConst 0)

transInt :: Int -> Exp
transInt i = Ex (T.EConst i)

-- TODO: also return fragment
transString :: (Num s, Show s, MonadState s m) => String -> m Exp
transString s = do
  lbl <- newLabel ()
  return $ Ex (T.EName lbl)

-------------------
-- Binops / Relops

transBinop :: Oper -> Binop
transBinop b =
  case b of
    PlusOp   -> BPlus
    MinusOp  -> BMinus
    TimesOp  -> BMul
    DivideOp -> BDiv
    _        -> error "transBinop: invalid Oper"

transRelop :: Oper -> Relop
transRelop b =
  case b of
    EqOp  -> REq
    NeqOp -> RNe
    LtOp  -> RLt
    LeOp  -> RLe
    GtOp  -> RGt
    GeOp  -> RGe
    _     -> error "transRelop: invalid Oper"    

transArith :: (Num s, Show s, MonadState s m) => Oper -> Exp -> Exp -> m Exp
transArith op e1 e2 = do
  let binop = transBinop op
  e1' <- unEx e1
  e2' <- unEx e2
  return $ Ex (T.EBinop binop e1' e2')
  -- Honestly I prefer the simple way above, at least in this case.
  -- return Ex `ap` (return (T.EBinop binop) `ap` (unEx e1) `ap` (unEx e2))

relopStringFun :: Relop -> (String, Bool)
relopStringFun r =
  if r `elem` [REq, RNe] then
    ("stringEqual", r == REq)
  else if r `elem` [RLt, RGt] then
    ("stringLessThan", r == RLt)
  else
    ("stringLessThanOrEqual", r == RLe)

relopStringCond :: Frame a => Level a -> Relop -> T.Exp -> T.Exp ->
  ((Label, Label) -> Stm)
relopStringCond lvl r arg1 arg2  =
  let (funName, b) = relopStringFun r
      extern = F.externalCall (frameOfLevel lvl)
      e = extern funName [arg1, arg2] in
    if b then
      \(t, f) -> SCJump REq e (T.EConst 0) f t
    else
      \(t, f) -> SCJump REq e (T.EConst 0) t f

-- We treat strings specially by delegating their comparison to
-- runtime functions.
transCond :: (Num s, Show s, MonadState s m, Frame a) =>
  Level a -> Ty -> Oper -> Exp -> Exp -> m Exp
transCond lvl ty op e1 e2 = do
  let relop = transRelop op
  e1' <- unEx e1
  e2' <- unEx e2
  return $
    if ty == StringTy then
      Cx (relopStringCond lvl relop e1' e2')
    else
      Cx (\(t, f) -> SCJump relop e1' e2' t f)

transOp :: (Num s, Show s, MonadState s m, Frame a) =>
  Level a -> Ty -> Oper -> Exp -> Exp -> m Exp
transOp lvl ty op e1 e2 =
  if op `elem` [PlusOp, MinusOp, TimesOp, DivideOp] then
    transArith op e1 e2
  else
    transCond lvl ty op e1 e2

----------------
-- Conditionals

transIf :: (Num s, Show s, MonadState s m) =>
  Exp -> Exp -> Maybe Exp -> m Exp
transIf e1 e2 e3 = do
  let e1' = unCx e1
  e2' <- unEx e2
  t <- newTemp ()
  t_lbl <- newLabel ()
  f_lbl <- newLabel ()
  let temp = T.ETemp t
  case e3 of
    Just e -> do
      e3' <- unEx e
      end_lbl <- newLabel ()
      return $ Ex $ T.ESeq (seqstm [e1' (t_lbl, f_lbl),
                                    SLabel t_lbl,
                                    SMove temp e2',
                                    SJump (T.EName end_lbl) [end_lbl],
                                    SLabel f_lbl,
                                    SMove temp e3',
                                    SLabel end_lbl
                                   ])
        temp
    Nothing -> do
      return $ Ex $ T.ESeq (seqstm [e1' (t_lbl, f_lbl),
                                    SLabel t_lbl,
                                    SMove temp e2',
                                    SLabel f_lbl
                                   ])
        temp

-----------
-- Records

-- Here we take a Level argument from Semant just so we can get the
-- word size from its frame.
transRecord :: (Num s, Show s, MonadState s m, Frame a) =>
  Level a -> [Exp] -> m Exp
transRecord lvl es = do
  t <- newTemp ()
  es' <- mapM unEx es
  let temp = T.ETemp t -- make new temp
      n = length es
      w = F.wordSize (frameOfLevel lvl)
      extern = F.externalCall (frameOfLevel lvl)
      alloc = SMove temp (extern "malloc" [T.EConst (n*w)]) -- allocate record
      inits = map (f w temp) (zip es' [0..]) -- initialize fields
  return $ Ex $ T.ESeq (seqstm (alloc : inits)) temp -- put it all together
  where f w temp (e, i) =
          SMove (T.EMem (T.EBinop BPlus temp (T.EConst (i*w)))) e

----------
-- Arrays

transArray :: (Num s, Show s, MonadState s m, Frame a) =>
  Level a -> Exp -> Exp -> m Exp
transArray lvl init size = do
  let extern = F.externalCall (frameOfLevel lvl)
  init' <- unEx init
  size' <- unEx size
  return . Ex $ extern "initArray" [init', size']

---------
-- While

transWhile :: (Num s, Show s, MonadState s m) =>
  Label -> Exp -> Exp -> m Exp
transWhile done_lbl test body = do
  test_lbl <- newLabel ()
  body_lbl <- newLabel ()
  test' <- unEx test
  body' <- unNx body
  return . Nx $
    seqstm [SLabel test_lbl,
            SCJump REq test' (T.EConst 0) done_lbl body_lbl,
            SLabel body_lbl,
            body',
            SJump (T.EName test_lbl) [test_lbl],
            SLabel done_lbl]

---------
-- Break

transBreak :: Label -> Exp
transBreak lbl = Nx $ SJump (T.EName lbl) [lbl]

-------
-- For

transFor :: (Num s, Show s, MonadState s m, Frame a) =>
  Access a -> Label -> Exp -> Exp -> Exp -> m Exp
transFor access done_lbl lo hi body = do
  let counter = transSimpleVar access (levelOfAccess access)
  counter' <- unEx counter
  body_lbl <- newLabel ()
  inc_lbl  <- newLabel ()
  lo'   <- unEx lo
  hi'   <- unEx hi
  body' <- unNx body
  return . Nx $
    seqstm [SCJump RLe lo' hi' body_lbl done_lbl,
            SLabel body_lbl,
            body',
            SCJump RLt lo' hi' inc_lbl done_lbl,
            SLabel inc_lbl,
            SMove counter' (T.EBinop BPlus counter' (T.EConst 1)),
            SJump (T.EName body_lbl) [body_lbl],
            SLabel done_lbl]

------------------
-- Function calls

transCall :: (Num s, Show s, MonadState s m, Frame a) =>
  Label -> [Exp] -> Level a -> Level a -> m Exp
transCall f es lvlDec lvlUse = do
  es' <- mapM unEx es
  let link = followLinks (parentOfLevel lvlDec) lvlUse
  return . Ex $ T.ECall (T.EName f) (link : es')

-------------
-- Sequences

transSeq :: (Num s, Show s, MonadState s m) => [Exp] -> m Exp
transSeq es = do
  es' <- mapM unNx es
  return . Nx $ seqstm es'

--------------
-- Assignment

transAssign :: (Num s, Show s, MonadState s m) => Exp -> Exp -> m Exp
transAssign dst src =
  -- Why not be a little weird
  (return Nx) `ap` (return SMove `ap` unEx dst `ap` unEx src)

--------------------
-- Create fragments

transFrags :: (Num s, Show s, MonadState s m, Frame a) =>
  [(Exp, Level a)] -> m [Frag a]
transFrags bodies_levels =
  mapM (\(body, lvl) -> do
           stm <- unNx body
           return $ FProc stm (frameOfLevel lvl))
  bodies_levels
