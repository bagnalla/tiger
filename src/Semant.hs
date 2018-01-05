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

import Data.List (find, nub)
import Data.Maybe (fromJust)

import qualified Absyn as A
import           Absyn (Array(..), Assign(..), Dec(..), Call(..), Exp(..),
                        Field(..), For(..), Fundec(..), If(..), Let(..),
                        Op(..), Oper(..), Pos, Record(..), Symbol, TyDec(..),
                        Var(..), VDec(..), While(..))
import           Env (base_tenv, base_venv, EnvEntry(..), FEntry(..),
                      getFunEntry, getVarTy, TypeEnv, ValueEnv, VEntry(..))
import           Frame (Frame)
import           Gensym (nextNum)
import           Symtab as S (add, empty, get)
import           Temp (makeString, newLabel, Label)
import qualified Translate as T (dummyExp, Exp, outermost)
import           Translate (Access, allocLocal, Level, newLevel)
-- import qualified Tree (Exp(..))
import           Types (fixTys, isNil, showTy, Ty(..))

data ExpTy = ExpTy
  { expty_exp :: T.Exp,
    expty_ty  :: Ty }
  deriving (Show)

-- The third element is a flag denoting whether a 'break' expression is
-- legal in the current context.
type Context b = (TypeEnv, ValueEnv b, Bool, Level b)

type TransM a b = ReaderT (Context b) (ExceptT String (StateT Int Identity)) a

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

transVar :: Var -> TransM ExpTy b

transVar (SimpleVar sym pos) = do 
  (_, venv, _, _) <- ask
  case getVarTy sym venv of
    Just ty -> return ExpTy { expty_exp = T.dummyExp, expty_ty = ty }
    _       -> raise "unbound variable identifier" pos

transVar (FieldVar var sym pos) = do
  var' <- transVar var
  case (expty_ty var') of
    RecordTy fieldTys _ ->
        case find (\x -> fst x == sym) fieldTys of
          Just (sym, ty) -> return ExpTy { expty_exp = T.dummyExp,
                                           expty_ty = ty }
          Nothing        -> raise "field doesn't exist" pos
    _ -> raise "expected record type" pos

transVar (SubscriptVar var exp pos) = do
  var' <- transVar var  
  case (expty_ty var') of
    ArrayTy ty _ ->
      return ExpTy { expty_exp = T.dummyExp,
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
  return ExpTy { expty_exp = T.dummyExp, expty_ty = NilTy }

transExp (IntExp i) =
  return ExpTy { expty_exp = T.dummyExp, expty_ty = IntTy }

transExp (StringExp str pos) =
  return ExpTy { expty_exp = T.dummyExp, expty_ty = StringTy }

transExp (CallExp c) = do
  (_, venv, _, _) <- ask
  case getFunEntry (call_func c) venv of
    Nothing -> raise "unbound function identifier" (call_pos c)
    Just f  ->
      if length (call_args c) /= length (fentry_formals f) then
        raise "wrong number of arguments to function" (call_pos c)
      else do
        args' <- mapM transExp (call_args c)
        foldM g () (zip (map expty_ty args') (fentry_formals f))
        return ExpTy { expty_exp = T.dummyExp,
                       expty_ty = fentry_result f }
          where g _ (arg_ty, formal_ty) =
                  if arg_ty == formal_ty then return ()
                  else raise ("expected " ++ showTy formal_ty ++ ", found " ++
                              showTy arg_ty) (call_pos c)

transExp (OpExp op) =
  case op_oper op of
    oper | oper `elem` [PlusOp, MinusOp, TimesOp, DivideOp,
                        LtOp, LeOp, GtOp, GeOp] -> do
      left'  <- assertExpTy (op_left op) IntTy (op_pos op)
      right' <- assertExpTy (op_right op) IntTy (op_pos op)
      return ExpTy { expty_exp = T.dummyExp, expty_ty = IntTy }
    oper | oper `elem` [EqOp, NeqOp] -> do
      left'  <- transExp (op_left op)
      let ty = expty_ty left'
      right' <- assertExpTy (op_right op) ty (op_pos op)
      return ExpTy { expty_exp = T.dummyExp, expty_ty = ty }

transExp (RecordExp rcd) = do
  recordTy <- getTy (record_typ rcd) (record_pos rcd)
  case recordTy of
    RecordTy fieldTys _ -> do
      foldM f () (zip (record_fields rcd) fieldTys)
      return ExpTy { expty_exp = T.dummyExp,
                     expty_ty = recordTy }
    -- NilTy ->
    --   if length (record_fields rcd) > 0 then
    --     raise "expected no fields for Nil record type" (record_pos rcd)
    --   else
    --     return ExpTy { expty_exp = (), expty_ty = NilTy }
    _ -> raise "expected record type" (record_pos rcd)
    where f _ ((sym1, exp, pos), (sym2, ty)) = do
            ty' <- transExp exp
            if sym1 == sym2 && (expty_ty ty') == ty then return ()
              else raise ("expected field " ++ show sym2 ++ " :: " ++ showTy ty)
                   pos

transExp (SeqExp seq) =
  foldM f x seq
  where f _ (exp, pos) = transExp exp
        x = ExpTy { expty_exp = T.dummyExp, expty_ty = UnitTy }

transExp (AssignExp a) = do
  var' <- transVar (assign_var a)
  assertExpTy (assign_exp a) (expty_ty var') (assign_pos a)
  return ExpTy { expty_exp = T.dummyExp, expty_ty = UnitTy }

transExp (IfExp ifE) = do
  assertExpTy (if_test ifE) IntTy (if_pos ifE)
  then' <- transExp (if_then ifE)
  case if_else ifE of
    Just exp -> do
      assertExpTy exp (expty_ty then') (if_pos ifE)
      return ()
    Nothing  -> if expty_ty then' /= UnitTy then
                  raise "expected unit type for if-then" (if_pos ifE)
                else return ()
  return ExpTy { expty_exp = T.dummyExp,
                 expty_ty = expty_ty then' }

transExp (WhileExp w) = do
  assertExpTy (while_test w) IntTy (while_pos w)
  local (\(tenv, venv, _, lvl) -> (tenv, venv, True, lvl))
    (do
        body' <- assertExpTy (while_body w) UnitTy (while_pos w)
        return ExpTy { expty_exp = T.dummyExp,
                       expty_ty = expty_ty body' })

transExp (ForExp f) = do
  assertExpTy (for_lo f) IntTy (for_pos f)
  assertExpTy (for_hi f) IntTy (for_pos f)
  (_, _, _, lvl) <- ask
  let access = allocLocal lvl True
  local (\(tenv, venv, _, lvl) ->
            (tenv, S.add (for_var f)
                   (VarEntry VEntry { ventry_access = access,
                                      ventry_ty     = IntTy })
                   venv, True, lvl))
    (do
        body' <- assertExpTy (for_body f) UnitTy (for_pos f)
        if isVarAssigned (for_var f) (for_body f) then
          raise "illegal assignment to loop index variable" (for_pos f)
        else
          return ExpTy { expty_exp = T.dummyExp,
                         expty_ty = expty_ty body' })

transExp (BreakExp pos) = do
  (_, _, b, _) <- ask
  if b then return ExpTy { expty_exp = T.dummyExp,
                           expty_ty = UnitTy }
    else raise "illegal break" pos

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
      ty' <- transExp (array_init a)
      if expty_ty ty' /= ty then
        raise ("expected type " ++ showTy ty ++ ", found " ++
               showTy (expty_ty ty')) (array_pos a)
        else return ExpTy { expty_exp = T.dummyExp,
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
    let levels = map (\(f, lbl) -> mkLevel lvl lbl f) (zip fundecs labels)
    -- Build function entries
    headers <- mapM (\(f, lvl, lbl) -> mkHeader lvl lbl f)
      (zip3 fundecs levels labels)
    -- Extend venv with function entries
    let venv' = foldl (\acc (fentry, fdec) ->
                         S.add (fun_name fdec) fentry acc)
                venv (zip headers fundecs)
    -- Typecheck function bodies
    local (const (tenv, venv', b, lvl))
      (mapM (\(f, lvl) -> h f lvl) (zip fundecs levels))
    -- Return the extended context
    return (tenv, venv', b, lvl)
    
      -- Make header from a fundec
      where mkLevel lvl lbl fdec = do
              newLevel lvl lbl
                [True | _ <- fun_params fdec]
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
              venv' <- foldM
                       (\acc fld -> do
                           ty <- getTy (field_typ fld) (field_pos fld)
                           let access = allocLocal lvl True
                           let ventry = VEntry { ventry_access = access,
                                                 ventry_ty = ty }
                           return $ S.add (field_name fld) (VarEntry ventry) acc)
                       venv (fun_params fdec)
              local (const (tenv, venv', b, lvl))
                (case fun_result fdec of
                   Just (sym, pos) -> do
                     ty <- getTy sym pos
                     assertExpTy (fun_body fdec) ty pos
                   Nothing         -> assertExpTy (fun_body fdec) UnitTy
                                      (fun_pos fdec))
              return ()

transDec (VarDec v) = do
  exp' <- transExp (vdec_init v)
  (tenv, venv, b, lvl) <- ask
  case vdec_typ v of
    Just (sym, pos) ->
      -- case getVarTy sym venv of
      case S.get sym tenv of
        Just ty ->
          if ty /= expty_ty exp' then
            raise ("expected type " ++ showTy ty ++ ", found " ++
                    showTy (expty_ty exp')) pos else return ()
        Nothing -> raise ("unbound type identifier " ++ show sym) pos
    Nothing -> if isNil (expty_ty exp') then
                 raise "missing record type annotation" (vdec_pos v)
               else return ()
  let access = allocLocal lvl True
  let ventry = VEntry { ventry_access = access,
                        ventry_ty = expty_ty exp' }
  return (tenv, add (vdec_name v) (VarEntry ventry) venv, b, lvl)

-- Who knows if this works at all
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
  tyId <- nextNum
  fields' <- mapM f fields
  return (RecordTy fields' tyId)
    where f fld = do
            ty <- getTy (field_typ fld) (field_pos fld)
            return ((field_name fld), ty)
transTy (A.ArrayTy sym pos) = do
  tyId <- nextNum
  ty <- getTy sym pos
  return (ArrayTy ty tyId)

----------------------
-- Translate a program

transProg :: Frame b => Exp -> TransM ExpTy b
transProg p = transExp p

----------------------------------
-- Run the translation computation

initContext :: Frame b => Context b
initContext = (base_tenv, base_venv, False, T.outermost)

runTrans :: Frame b => TransM a b -> Either String a
runTrans t = fst $ runIdentity (runStateT (runExceptT (runReaderT t
                                                        initContext)) 0)
