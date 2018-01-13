-- The Block type is just [Stm] and CanonM is the state monad type.

module Canon (
  CanonM, runCanon,

  -- Stm -> CanonM [Stm]
  -- Linearize a Stm, removing all SSeqs/ESeqs and making the direct
  -- parent of every ECall is not also an ECall.
  linearize,

  -- [Stm] -> CanonM ([Block], Label)
  -- Given a list of Stms produced from above, produce a list of basic
  -- blocks and an 'exit' label that is jumped to at the end.
  basicBlocks,

  -- [Block] -> Label -> CanonM [Stm]
  -- Given a list of basic blocks and 'exit' label produced from
  -- above, arrange the blocks such that every SCJump is immediately
  -- followed by its false label, and remove unnecessary SJumps where
  -- possible. Produces a flattened list of statements as the final
  -- result.
  traceSchedule
  ) where

import           Control.Monad.State
import           Data.Maybe (fromJust)
import           Symtab (add, empty, fold, Id(..), Symtab)
import qualified Symtab as S (get)
import           Temp (Label, newLabel, newTemp, stringOfLabel)
import           Tree (Exp(..), negateRelop, Stm(..))

nop :: Stm
nop = SExp (EConst 0)

type CanonM = State Int

-- Very conservative estimate of whether a statement and expression
-- safely commute.
commute :: Stm -> Exp -> Bool
commute (SExp (EConst _)) _                 = True
commute _                 (EName _)         = True
commute _                 (EConst _)        = True
commute _                 _                 = False

reorder_stm :: (Num s, Show s, MonadState s m) =>
  [Exp] -> ([Exp] -> Stm) -> m Stm
reorder_stm es build = do
  (stm, es') <- reorder es
  return $ SSeq stm (build es')

reorder_exp :: (Num s, Show s, MonadState s m) =>
  [Exp] -> ([Exp] -> Exp) -> m (Stm, Exp)
reorder_exp es build = do
  (stm, es') <- reorder es
  return (stm, build es')

reorder :: (Num s, Show s, MonadState s m) => [Exp] -> m (Stm, [Exp])
reorder (ECall f args : es) = do
  t <- newTemp ()
  let temp = ETemp t
  reorder (ESeq (SMove
                 temp (ECall f args)) temp : es)
reorder (e : es) = do
  (stm, e') <- do_exp e
  (stm', es') <- reorder es
  -- If stm' and e' commute, then we can just move stm' to the left.
  if commute stm' e' then
    return (SSeq stm stm', e' : es')
  -- Otherwise, we must introduce a new temporary and a SMove to do
  -- the evaluation of e' before stm', and use the temporary in place
  -- of e' everywhere on the right.
    else do
    t <- newTemp ()
    let temp = ETemp t
    return (SSeq stm (SSeq (SMove temp e') stm'), temp : es')
reorder [] =
  return (nop, [])

do_stm :: (Num s, Show s, MonadState s m) => Stm -> m Stm
do_stm (SMove (EMem e) (ECall f args)) =
  reorder_stm (e : f : args)
  (\es -> SMove (EMem (es!!0)) (ECall (es!!1) (drop 2 es)))
-- Since reorder rewrites ECalls into moves to temporaries, when an
-- ECall is already a direct subexpression of a SMove we avoid calling
-- reorder on it and reorder its subexpressions directly instead.
do_stm (SMove e (ECall f args)) =
  reorder_stm (e : f : args)
  (\es -> SMove (es!!0) (ECall (es!!1) (drop 2 es)))
-- TODO: Is this ok? We treat EMem specially here to prevent writing
-- it into a temporary when swapping with a statement, but exercise
-- 8.5 in the book seems to sugges that there are other reasons for
-- not treating the left operand as a subexpression.
do_stm (SMove (EMem e1) e2) =
  reorder_stm [e1, e2] (\es -> SMove (EMem (es!!0)) (es!!1))
do_stm (SMove e1 e2) =
  reorder_stm [e1, e2] (\es -> SMove (es!!0) (es!!1))
do_stm (SExp e) =
  reorder_stm [e] (\es -> SExp (head es))
do_stm (SJump e lbls) =
  reorder_stm [e] (\es -> SJump (head es) lbls)
do_stm (SCJump r e1 e2 lbl1 lbl2) =
  reorder_stm [e1, e2] (\es -> SCJump r (es!!0) (es!!1) lbl1 lbl2)
do_stm (SSeq s1 s2) =
  return SSeq `ap` do_stm s1 `ap` do_stm s2
do_stm stm = return stm

do_exp :: (Num s, Show s, MonadState s m) => Exp -> m (Stm, Exp)
do_exp (EBinop p a b) =
  reorder_exp [a, b] (\es -> EBinop p (es!!0) (es!!1))
do_exp (EMem e) =
  reorder_exp [e] (\es -> EMem (head es))
do_exp (ECall e es) =
  reorder_exp (e : es) (\es -> ECall (head es) (tail es))
do_exp (ESeq stm e) = do
  stm'        <- do_stm stm
  (stm'', e') <- do_exp e
  return (SSeq stm' stm'', e')
do_exp e = return (nop, e)

linearize :: Stm -> CanonM [Stm]
linearize stm = do
  stm' <- do_stm stm
  -- -- Remove nops. Might have to treat the case in which the very
  -- -- last statement is intended to be (SExp (EConst 0)) specially.
  return (filter (/= nop)) `ap` linear stm' []
  where linear (SSeq s1 s2) l = do
          l' <- linear s2 l
          linear s1 l'
        linear s l = return (s : l)
  -- Simpler but less efficient?
  -- linear stm'
  -- where linear (SSeq s1 s2) = do
  --         l1 <- linear s1
  --         l2 <- linear s2
  --         return (l1 ++ l2)
  --       linear s = return [s]

-- linearize_frag :: (Num s, Show s, MonadState s m) => Frag a -> m (Frag a)
-- linearize_frag frag =
--   case frag of
--     FProc stm frame -> do
--       stm' <- linearize stm
--       return $ FProc stm' frame
--     _ -> return frag

type Block = [Stm]
type Trace = [Block]

basicBlocks :: [Stm] -> CanonM ([Block], Label)
basicBlocks stms = do
  done <- newLabel ()
  blks <- blocks done stms [] []
  let blks' = filter (not . null) blks
  blks'' <- fix_blocks done blks'
  return (blks'', done)
  where
    is_label (SLabel _) = True
    is_label _          = False
    is_jump (SJump _ _) = True
    is_jump (SCJump _ _ _ _ _) = True
    is_jump _                  = False
    labelOfStm (SLabel lbl) = lbl

    -- Step through Stms building a list of blocks. Each block is
    -- constructed in reverse order by consing and then reversed when
    -- added to the list of blocks. The list of blocks is also
    -- constructed in reverse order and then reversed at the end.
    blocks done (stm : stms) cur_blk all_blks =
      if is_jump stm then
        blocks done stms [] (reverse (stm : cur_blk) : all_blks)
      else
        blocks done stms (stm : cur_blk) all_blks
    blocks _ [] cur_blk all_blks =
      return $ reverse ((reverse cur_blk) : all_blks)

    -- Make sure a block starts with a label and ends with a
    -- jump/branch.
    fix_block :: Block -> Label -> CanonM Block
    fix_block blk lbl = do
      let s1 = head blk
          s2 = last blk
      blk' <- if (not . is_label) s1 then do
        lbl <- newLabel ()
        return $ SLabel lbl : blk
        else
        return blk
      return $ if (not . is_jump) s2 then
                 blk' ++ [SJump (EName lbl) [lbl]]
               else
                 blk'

    -- Fix blocks in reverse order so that the next block always
    -- starts with a label that we can jump to if the current block is
    -- missing a jump/branch at the end.
    fix_blocks :: Label -> [Block] -> CanonM [Block]
    fix_blocks done (blk : blks) = do
      blks' <- fix_blocks done blks
      if null blks' then do
        blk' <- fix_block blk done
        return [blk']
        else do
        let next_lbl_stm = head (head blks')
            lbl = labelOfStm next_lbl_stm
        blk' <- fix_block blk lbl
        return $ blk' : blks'
    fix_blocks _ [] = return []

-- Take a list of basic blocks and an 'exit' label, and turn them back
-- into a big sequence of Stms but with every SCJump followed
-- immediately by its false label and SJumps removed when possible.
traceSchedule :: [Block] -> Label -> CanonM [Stm]
traceSchedule blks exit_lbl = do
  let tab = foldr (\blk acc ->
                     add (toId (lblOfBlk blk)) blk acc)
            empty blks
      marked = fold (\id _ acc -> add id False acc)
        (add (toId exit_lbl) True empty) tab
      traces = build_traces tab marked []
  -- Finishing touches...
  stms <- finish (concat (concat traces) ++
                   [SJump (EName exit_lbl) [exit_lbl]])
  -- return stms
  return (concat (concat traces))
  where
    lblOf stm =
      case stm of
        SLabel lbl -> lbl
        _ -> error "traceSchedule: expected label at front of block"
    lblOfBlk = lblOf . head 
    toId lbl = Id (stringOfLabel lbl)

    allMarked marked = fold (\_ x acc -> x && acc) True marked

    -- Choose an unmarked block, returning its label.
    select marked =
      fromJust $ fold (\id b acc ->
                         case acc of
                           Nothing   -> if b then Nothing else Just id
                           Just _    -> acc
                      ) Nothing marked

    -- Build traces until all blocks are marked.
    build_traces :: Symtab Block -> Symtab Bool -> [Trace] -> [Trace]
    build_traces tab marked accum =
      if allMarked marked then
        accum
      else
        let
          blk = fromJust (S.get (select marked) tab)
          (tr, marked') = build_trace blk tab marked in
          build_traces tab marked' (tr : accum)

    lblOfJump marked jmp =
      case jmp of
        SJump _ [lbl]      -> lbl
        SCJump _ _ _ t f ->
          if fromJust $ S.get (toId f) marked then t else f
        _ -> error "traceSchedule: ill-formed jump"

    -- Build a trace of unmarked blocks.
    build_trace :: Block -> Symtab Block -> Symtab Bool -> (Trace, Symtab Bool)
    build_trace blk tab marked =
      let lbl  = toId (lblOfBlk blk)
          lbl' = toId (lblOfJump marked (last blk))
          marked' = add lbl True marked in
        -- ([blk], marked')
        if fromJust $ S.get lbl' marked' then
          ([blk], marked')
        else
          let blk' = fromJust $ S.get lbl' tab
              (blks, marked'') = build_trace blk' tab marked' in
            (blk : blks, marked'')

    finish (stm@(SCJump r e1 e2 t f) : next : stms) = do
      stms' <- finish (next : stms)
      let lbl = lblOf next
      if lbl == t then
        return $ SCJump (negateRelop r) e1 e2 f t : stms'
        else if lbl /= f then do
        f' <- newLabel ()
        return $ SCJump r e1 e2 t f' : SLabel f' :
          SJump (EName f) [f] : stms'
        else
        return $ stm : stms'
    finish (SJump e [lbl1] : SLabel lbl2 : stms) =
      if lbl1 == lbl2 then
        finish stms
      else do
        stms' <- finish stms
        return $ SJump e [lbl1] : SLabel lbl2 : stms'
    finish (stm : stms) = pure (:) `ap` pure stm `ap` finish stms
    finish [] = return []


-- Run 
runCanon :: CanonM a -> Int -> (a, Int)
runCanon r i = runState r i
