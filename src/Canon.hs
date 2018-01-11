module Canon (
  basicBlocks, linearize, traceSchedule
  ) where

import           Control.Monad.State
import           Temp (Label, newTemp)
import           Tree (Exp(..), Stm(..))

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
  reorder (ESeq (SMove temp (ECall f args)) temp : es)
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

do_stm :: (Num s, Show s, MonadState s m) => Stm -> m Stm
-- Since reorder rewrites ECalls into moves to temporaries, when an
-- ECall is already a direct subexpression of a SMove we avoid calling
-- reorder on it and reorder its subexpressions directly instead.
do_stm (SMove e (ECall f args)) =
  reorder_stm (e : f : args)
  (\es -> SMove (es!!0) (ECall (es!!1) (drop 2 es)))
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
do_exp e = return (SExp (EConst 0), e)

linearize :: (Num s, Show s, MonadState s m) => Stm -> m [Stm]
linearize stm = do
  stm' <- do_stm stm
  linear stm' []
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

basicBlocks :: [Stm] -> ([[Stm]], Label)
basicBlocks stm = basicBlocks stm

traceSchedule :: [[Stm]] -> Label -> [Stm]
traceSchedule _ _ = []
