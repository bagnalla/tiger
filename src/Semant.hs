{-# LANGUAGE FlexibleInstances #-}

module Semant (
  ExpTy(..),
  runTrans,
  TransM,
  transProg
  ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.List (find, findIndex, nub)
import Data.Maybe (fromJust)

import qualified Absyn as A
import           Absyn (Array(..), Assign(..), Dec(..), Call(..), Exp(..),
                        Field(..), For(..), Fundec(..), If(..), Let(..),
                        Op(..), Oper(..), Pos, Record(..), Symbol, TyDec(..),
                        Var(..), VDec(..), While(..))
import           Env (base_tenv, base_venv, EnvEntry(..), FEntry(..),
                      getFunEntry, getVarAccess, getVarEntry, getVarTy,
                      TypeEnv, ValueEnv, VEntry(..))
import           Frame (Frag, Frame)
import           Gensym (nextNum)
import           Symtab as S (add, empty, get)
import           Temp (makeString, namedLabel, newLabel, Label)
import qualified Translate as T (Exp, outermost, formals)
import           Translate (Access, allocLocal, transFieldVar, Level,
                            newLevel, transArray, transAssign, transBreak,
                            transCall, transFrags, transFieldVar, transFor,
                            transIf, transInt, transNil, transOp,
                            transRecord, transSeq, transSimpleVar,
                            transString, transSubscriptVar, transWhile)
import           Types (fixTys, isNil, showTy, Ty(..))

data ExpTy = ExpTy
  { expty_exp :: T.Exp,
    expty_ty  :: Ty }
  deriving (Show)

-- The third element is a flag denoting whether a 'break' expression
-- is legal in the current context.
type Context b = (TypeEnv, ValueEnv b, Maybe Label, Level b)

-- Use newtype so we can define our own Show instance without overlap.
newtype TransState b = TransState (Int, [Frag b])

fragsOfTransState (TransState (_, frags)) = frags

-- Here we declare a Num instance for TransState, which allows us to
-- treat a TransState as a number. It just uses the internal Int and
-- doesn't change the rest of the state. For +, -, and *, the result
-- inherits the state of the LEFT operand, so it's important to do,
-- for example, st + 1 rather than 1 + st unless you want to reset the
-- state.
-- The whole point of this is to allow arbitrary state while being
-- compatible with the generic GenSym module, so it's a bit of a hack.
instance Num (TransState a) where
  TransState (a, x) + TransState (b, _) = TransState (a + b, x)
  TransState (a, x) - TransState (b, _) = TransState (a - b, x)
  TransState (a, x) * TransState (b, _) = TransState (a * b, x)
  abs (TransState (a, x))               = TransState (abs a, x)
  signum (TransState (a, x))            = TransState (signum a, x)
  fromInteger i                         = TransState (fromInteger i, [])
  negate (TransState (a, x))            = TransState (negate a, x)

instance Show (TransState a) where
  show (TransState (i, _)) = show i

-- This makes me realize.. we should just be using a writer monad for
-- fragments instead of messing with the state. I'll leave it like
-- this for now, though, since it's already done.
instance Monoid (TransState a) where
  mempty = TransState (0, [])
  TransState (i1, x1) `mappend` TransState (i2, x2) =
    TransState (i1 + i2, x1 ++ x2)

type TransM a b =
  ReaderT (Context b) (ExceptT String (StateT (TransState b) Identity)) a

raise :: String -> Pos -> TransM a b
raise str pos =
  throwError $ "Type error at " ++ show pos ++ ": " ++ str

getTy :: Symbol -> Pos -> TransM Ty b
getTy sym pos = do
  (tenv, venv, b, _) <- ask
  case S.get sym tenv of
    Just ty -> return ty
    Nothing -> raise "unbound type identifier" pos

-- Check if a loop index variable is assigned somewhere in an Exp
isVarAssigned :: Symbol -> Exp -> Bool
-- isVarAssigned sym (VarExp (SimpleVar sym' _)) =
--   if sym == sym' then True else False
isVarAssigned sym (RecordExp rcd) =
  foldl (||) False (map (\(_, exp, _) -> isVarAssigned sym exp)
                     (record_fields rcd))
  -- foldl (\acc (_, exp, _) -> acc || isVarAssigned sym exp) False (record_fields rcd)
isVarAssigned sym (SeqExp exps) =
  -- foldl (\acc ) False (map (isVarAssigned sym) (map fst exps))
  foldl (||) False [isVarAssigned sym (fst x) | x <- exps]
isVarAssigned sym (AssignExp a) =
  case assign_var a of
    SimpleVar sym' _ -> if sym == sym' then True else False
    _                -> False
isVarAssigned sym (IfExp ifE) =
  isVarAssigned sym (if_test ifE) || isVarAssigned sym (if_then ifE) ||
  case if_else ifE of
    Just e  -> isVarAssigned sym e
    Nothing -> False
isVarAssigned sym (WhileExp w) =
  isVarAssigned sym (while_test w) || isVarAssigned sym (while_body w)
isVarAssigned sym (ForExp f) =
  isVarAssigned sym (for_lo f) || isVarAssigned sym (for_hi f) ||
  isVarAssigned sym (for_body f)
isVarAssigned sym (LetExp l) =
  -- If the sym is rebound by one of the declarations then we stop here
  if foldl (\acc dec -> case dec of
                          VarDec vdec -> vdec_name vdec == sym
                          _           -> False)
     False (let_decs l) then
    False
  else
    isVarAssigned sym (let_body l)
isVarAssigned sym (ArrayExp a) =
  isVarAssigned sym (array_size a) || isVarAssigned sym (array_init a)
isVarAssigned _ _ = False

------------------
-- Typecheck Vars

transVar :: Frame b => Var -> TransM ExpTy b

transVar (SimpleVar sym pos) = do 
  (_, venv, _, lvl) <- ask
  case getVarEntry sym venv of
    Just ventry ->
      -- The ventry access contains the level of where the var is
      -- declared, and lvl is the level here where it's being
      -- used. simpleVar compares these to generate the correct IR
      -- code for accessing the var (following static links if
      -- necessary).
      return ExpTy { expty_exp = transSimpleVar (ventry_access ventry) lvl,
                     expty_ty = ventry_ty ventry }
    _       -> raise "unbound variable identifier" pos

transVar (FieldVar var sym pos) = do
  (_, _, _, lvl) <- ask
  var' <- transVar var
  case (expty_ty var') of
    RecordTy fieldTys _ ->
        case find (\x -> fst x == sym) fieldTys of
          Just (sym, ty) ->
            -- Get index of the field
            case findIndex ((== sym) . fst) fieldTys of
              Just i -> do
                exp <- transFieldVar lvl (expty_exp var') i
                return ExpTy { expty_exp = exp,
                               expty_ty = ty }
              _          -> raise "impossible" pos
          Nothing        -> raise "field doesn't exist" pos
    _ -> raise "expected record type" pos

transVar (SubscriptVar var i_exp pos) = do
  var'   <- transVar var -- The array variable
  i_exp' <- assertExpTy i_exp IntTy pos
  case (expty_ty var') of
    ArrayTy ty _ -> do
      (_, _, _, lvl) <- ask
      exp <- transSubscriptVar lvl (expty_exp var') (expty_exp i_exp')
      return ExpTy { expty_exp = exp,
                     expty_ty = ty }
    _ -> raise "expected array type" pos

-----------------
-- Typecheck Exps

assertExpTy :: Frame b => Exp -> Ty -> Pos -> TransM ExpTy b
assertExpTy exp ty pos = do
  exp' <- transExp exp
  if ty == (expty_ty exp') then return exp'
    else raise ("expected type " ++ showTy ty ++ ", found " ++
                showTy (expty_ty exp')) pos

transExp :: Frame b => Exp -> TransM ExpTy b

transExp (VarExp var) = transVar var

transExp NilExp =
  return ExpTy { expty_exp = transNil, expty_ty = NilTy }

transExp (IntExp i) =
  return ExpTy { expty_exp = transInt i, expty_ty = IntTy }

transExp (StringExp str pos) = do
  exp <- transString str
  return ExpTy { expty_exp = exp, expty_ty = StringTy }

transExp (CallExp c) = do
  (_, venv, _, lvl) <- ask
  case getFunEntry (call_func c) venv of
    Nothing -> raise "unbound function identifier" (call_pos c)
    Just f  ->
      if length (call_args c) /= length (fentry_formals f) then
        raise "wrong number of arguments to function" (call_pos c)
      else do
        args' <- mapM transExp (call_args c)
        foldM g () (zip (map expty_ty args') (fentry_formals f))
        exp <- transCall (fentry_label f) (map expty_exp args')
          (fentry_level f) lvl
        return ExpTy { expty_exp = exp,
                       expty_ty = fentry_result f }
          where g _ (arg_ty, formal_ty) =
                  if arg_ty == formal_ty then return ()
                  else raise ("expected " ++ showTy formal_ty ++ ", found " ++
                              showTy arg_ty) (call_pos c)

transExp (OpExp op) = do
  (_, _, _, lvl) <- ask
  case op_oper op of
    oper | oper `elem` [PlusOp, MinusOp, TimesOp, DivideOp] -> do
      left'  <- assertExpTy (op_left op) IntTy (op_pos op)
      right' <- assertExpTy (op_right op) IntTy (op_pos op)
      exp <- transOp lvl IntTy oper (expty_exp left') (expty_exp right')
      return ExpTy { expty_exp = exp, expty_ty = IntTy }
    oper | oper `elem` [LtOp, LeOp, GtOp, GeOp, EqOp, NeqOp] -> do
      left'  <- transExp (op_left op)
      let ty = expty_ty left'
      right' <- assertExpTy (op_right op) ty (op_pos op)
      exp <- transOp lvl ty oper (expty_exp left') (expty_exp right')
      return ExpTy { expty_exp = exp, expty_ty = ty }

transExp (RecordExp rcd) = do
  recordTy <- getTy (record_typ rcd) (record_pos rcd)
  case recordTy of
    RecordTy fieldTys _ -> do
      fields <- mapM f (zip (record_fields rcd) fieldTys)
      (_, _, _, lvl) <- ask
      exp <- transRecord lvl (map expty_exp fields)
      return ExpTy { expty_exp = exp, expty_ty = recordTy }
    -- NilTy ->
    --   if length (record_fields rcd) > 0 then
    --     raise "expected no fields for Nil record type" (record_pos rcd)
    --   else
    --     return ExpTy { expty_exp = (), expty_ty = NilTy }
    _ -> raise "expected record type" (record_pos rcd)
    where f ((sym1, exp, pos), (sym2, ty)) = do
            exp' <- transExp exp
            if sym1 == sym2 && (expty_ty exp') == ty then return exp'
              else raise ("expected field " ++ show sym2 ++ " :: " ++ showTy ty)
                   pos

transExp (SeqExp seq) = do
  es <- mapM f seq
  exp <- transSeq (map expty_exp es)
  return ExpTy { expty_exp = exp, expty_ty = UnitTy }
  where f (exp, pos) = transExp exp

transExp (AssignExp a) = do
  var' <- transVar (assign_var a)
  val <- assertExpTy (assign_exp a) (expty_ty var') (assign_pos a)
  exp <- transAssign (expty_exp var') (expty_exp val)
  return ExpTy { expty_exp = exp, expty_ty = UnitTy }

transExp (IfExp ifE) = do
  b <- assertExpTy (if_test ifE) IntTy (if_pos ifE)
  then' <- transExp (if_then ifE)
  case if_else ifE of
    Just exp -> do
      else' <- assertExpTy exp (expty_ty then') (if_pos ifE)
      exp <- transIf (expty_exp b) (expty_exp then')
        (Just (expty_exp else'))
      return ExpTy { expty_exp = exp, expty_ty = expty_ty then' }
    Nothing  ->
      if expty_ty then' /= UnitTy then
        raise "expected unit type for if-then" (if_pos ifE)
      else
        do
          exp <- transIf (expty_exp b) (expty_exp then') Nothing
          return ExpTy { expty_exp = exp,
                         expty_ty = expty_ty then' }

transExp (WhileExp w) = do
  test <- assertExpTy (while_test w) IntTy (while_pos w)
  done_lbl <- newLabel () -- pass this to translate function
  local (\(tenv, venv, _, lvl) -> (tenv, venv, Just done_lbl, lvl))
    (do
        body <- assertExpTy (while_body w) UnitTy (while_pos w)
        exp <- transWhile done_lbl (expty_exp test) (expty_exp body)
        return ExpTy { expty_exp = exp,
                       expty_ty = expty_ty body })

transExp (ForExp f) = do
  lo <- assertExpTy (for_lo f) IntTy (for_pos f)
  hi <- assertExpTy (for_hi f) IntTy (for_pos f)
  (_, _, _, lvl) <- ask
  let (access, lvl') = allocLocal lvl True
  done_lbl <- newLabel ()
  local (\(tenv, venv, _, lvl) ->
            (tenv, S.add (for_var f)
                   (VarEntry VEntry { ventry_access = access,
                                      ventry_ty     = IntTy })
                   venv, Just done_lbl, lvl'))
    (do
        body <- assertExpTy (for_body f) UnitTy (for_pos f)
        if isVarAssigned (for_var f) (for_body f) then
          raise "illegal assignment to loop index variable" (for_pos f)
          else do
          exp <- transFor access done_lbl (expty_exp lo)
            (expty_exp hi) (expty_exp body)
          return ExpTy { expty_exp = exp,
                         expty_ty = expty_ty body })

transExp (BreakExp pos) = do
  (_, _, b, _) <- ask
  case b of
    Just lbl -> return ExpTy { expty_exp = transBreak lbl,
                               expty_ty = UnitTy }
    Nothing  -> raise "illegal break" pos

transExp (LetExp l) = do
  ctx <- ask
  ctx' <- foldM f ctx (let_decs l)
  local (const ctx') (transExp (let_body l))
  where f ctx dec =
          local (const ctx) (transDec dec)

transExp (ArrayExp a) = do
  ty <- getTy (array_typ a) (array_pos a)
  case ty of
    ArrayTy ty tyID -> do
      init <- transExp (array_init a)
      if expty_ty init /= ty then
        raise ("expected type " ++ showTy ty ++ ", found " ++
                showTy (expty_ty init)) (array_pos a)
        else do
        size <- assertExpTy (array_size a) IntTy (array_pos a)
        (_, _, _, lvl) <- ask
        exp <- transArray lvl (expty_exp init) (expty_exp size)
        return ExpTy { expty_exp = exp,
                       expty_ty = ArrayTy ty tyID }
    _ -> raise "expected array type" (array_pos a)

-----------------
-- Typecheck Decs

transDec :: Frame b => Dec -> TransM (Context b) b

-- Not 100% sure that the level stuff is done right here ("keeping track
-- of levels" section at the end of chapter 6).
transDec (FunctionDec fundecs) =
  if not $ nub (map fun_name fundecs) == (map fun_name fundecs) then
    raise "duplicate function declarations in the same let batch"
    (fun_pos (fundecs!!0))
  else do
    (tenv, venv, b, lvl) <- ask
    labels <- mapM (\_ -> newLabel ()) fundecs
    let levels = map (\(f, lbl) -> mkLevel lvl lbl f)
          (zip fundecs labels)
    -- Build function entries
    headers <- mapM (\(f, lvl, lbl) -> mkHeader lvl lbl f)
      (zip3 fundecs levels labels)
    -- Extend venv with function entries
    let venv' = foldl (\acc (fentry, fdec) ->
                         S.add (fun_name fdec) fentry acc)
                venv (zip headers fundecs)
    -- Typecheck function bodies
    bodies_levels <- local (const (tenv, venv', b, lvl))
      (mapM (\(f, lvl) -> h f lvl) (zip fundecs levels))

    -- Add fragments to state
    frags <- transFrags (map (\(body, lvl) -> (expty_exp body, lvl))
                          bodies_levels)
    modify (\st -> foldr mappend st (map (\f -> TransState (0, [f])) frags))
           
    -- Return the extended context
    return (tenv, venv', b, lvl)

      -- Make header from a fundec
      where mkLevel lvl lbl fdec = do
              newLevel lvl lbl [True | _ <- fun_params fdec]
            mkHeader lvl lbl fdec = do
              param_tys <- mapM g (fun_params fdec)
              result_ty <- case (fun_result fdec) of
                             Just (sym, pos) -> getTy sym pos
                             Nothing         -> return UnitTy
              return $ FunEntry (FEntry { fentry_level = lvl,
                                          fentry_label = lbl,
                                          fentry_formals = param_tys,
                                          fentry_result  = result_ty })
            -- Get type of a field
            g fld =
              getTy (field_typ fld) (field_pos fld)
            -- Typecheck the body of a fundec
            h fdec lvl = do
              (tenv, venv, b, _) <- ask
              venv' <-
                foldM (\venv (formal, fld) -> do
                          ty <- getTy (field_typ fld) (field_pos fld)
                          let ventry = VEntry { ventry_access = formal,
                                                ventry_ty = ty }
                          return $ S.add (field_name fld)
                            (VarEntry ventry) venv)
                venv (zip (T.formals lvl) (fun_params fdec))
              local (const (tenv, venv', b, lvl))
                (case fun_result fdec of
                   Just (sym, pos) -> do
                     ty <- getTy sym pos
                     exp <- assertExpTy (fun_body fdec) ty pos
                     return (exp, lvl)
                   Nothing         -> do                     
                     exp <- assertExpTy (fun_body fdec) UnitTy
                       (fun_pos fdec)
                     return (exp, lvl))

transDec (VarDec v) = do
  exp' <- transExp (vdec_init v)
  (tenv, venv, b, lvl) <- ask
  case vdec_typ v of
    Just (sym, pos) ->
      case S.get sym tenv of
        Just ty ->
          if ty /= expty_ty exp' then
            raise ("expected type " ++ showTy ty ++ ", found " ++
                    showTy (expty_ty exp')) pos else return ()
        Nothing -> raise ("unbound type identifier " ++ show sym) pos
    Nothing -> if isNil (expty_ty exp') then
                 raise "missing record type annotation" (vdec_pos v)
               else return ()
  let (access, lvl') = allocLocal lvl True
  let ventry = VEntry { ventry_access = access,
                        ventry_ty = expty_ty exp' }
  return (tenv, add (vdec_name v) (VarEntry ventry) venv, b, lvl')

transDec (TypeDec tydecs) =
  if not $ nub (map tydec_name tydecs) == (map tydec_name tydecs) then
    raise "duplicate type declarations in the same let batch"
    (tydec_pos (tydecs!!0))
  else do
    (tenv, venv, b, lvl) <- ask
    let headers = map f tydecs
    local (\_ ->
              -- put type headers in tenv
              let tenv' = foldl (\acc (sym, ty) -> S.add sym ty acc)
                          tenv headers in
                (tenv', venv, b, lvl))
      (do
          checkForCycles tydecs
          tys <- mapM g tydecs -- translate to Tys
          let tys' = fixTys (map fst headers) tys -- tie recursion with fix
          tenv' <- foldM h tenv (zip (map fst headers) tys') -- build new tenv
          return (tenv', venv, b, lvl))
      where
        f t = ((tydec_name t), NameTy (tydec_name t))
        g t = transTy (tydec_ty t)
        h tenv (sym, ty) = return (S.add sym ty tenv)

checkForCycles :: [TyDec] ->TransM () b
checkForCycles tydecs = do
  foldM f () tydecs
  where f acc tydec =
          if traceCycle tydecs [tydec_name tydec] (tydec_ty tydec) then
            raise "illegal recursive cycle in type declaration"
            (tydec_pos tydec)
          else return ()

traceCycle :: [TyDec] -> [Symbol] -> A.Ty -> Bool
traceCycle tydecs seen (A.NameTy sym pos) =
  if sym `elem` seen then True
  else
    foldl (\acc tydec ->
             acc || if (tydec_name tydec) == sym then
                      traceCycle tydecs (sym : seen) (tydec_ty tydec)
                    else False)
    False tydecs
traceCycle _ _ _ = False

----------------
-- Translate Tys

transTy :: A.Ty -> TransM Ty b
transTy (A.NameTy sym pos) = getTy sym pos
transTy (A.RecordTy fields) = do
  TransState (tyId, _) <- nextNum
  fields' <- mapM f fields
  return (RecordTy fields' tyId)
    where f fld = do
            ty <- getTy (field_typ fld) (field_pos fld)
            return ((field_name fld), ty)
transTy (A.ArrayTy sym pos) = do
  TransState (tyId, _) <- nextNum
  ty <- getTy sym pos
  return (ArrayTy ty tyId)

----------------------
-- Translate a program

transProg :: Frame b => Exp -> TransM ExpTy b
transProg p = transExp p

----------------------------------
-- Run the translation computation

initContext :: Frame b => Context b
-- initContext = (base_tenv, base_venv, Nothing, T.outermost)
initContext = (base_tenv, base_venv, Nothing,
               newLevel T.outermost (namedLabel "main") [])

runTrans :: Frame b => TransM a b -> (Either String a, [Frag b])
runTrans t =
  let (res, st) = runIdentity (runStateT (runExceptT (runReaderT t
                                                      initContext)) 0) in
    (res, fragsOfTransState st)
