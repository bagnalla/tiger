module Env (
  base_tenv,
  base_venv,
  EnvEntry(..),
  FEntry(..),
  getFunEntry,
  getVarAccess,
  getVarEntry,
  getVarTy,
  TypeEnv,
  ValueEnv,
  VEntry(..)
  ) where

import Absyn (mkSymbol, Symbol)
import Frame (Frame)
import Symtab (add, empty, get, Symtab)
import Temp (Label)
import Translate (Access, Level)
import Types (Ty(..))

--------------------
-- Type environments

type TypeEnv = Symtab Ty

base_tenv :: TypeEnv
base_tenv = add (mkSymbol "string") StringTy (add (mkSymbol "int") IntTy empty)

---------------------
-- Value environments

data EnvEntry a =
  VarEntry (VEntry a)
  | FunEntry (FEntry a)

data VEntry a =
  VEntry { ventry_access :: Access a,
           ventry_ty :: Ty }

data FEntry a = FEntry
  { fentry_level :: Level a,
    fentry_label :: Label,
    fentry_formals :: [Ty],
    fentry_result  :: Ty }

type ValueEnv a = Symtab (EnvEntry a)

getVarTy :: Symbol -> ValueEnv a -> Maybe Ty
getVarTy sym venv =
  case Symtab.get sym venv of
    Just (VarEntry ventry) -> Just (ventry_ty ventry)
    _                      -> Nothing

getVarAccess :: Symbol -> ValueEnv a -> Maybe (Access a)
getVarAccess sym venv =
  case Symtab.get sym venv of
    Just (VarEntry ventry) -> Just (ventry_access ventry)
    _                      -> Nothing

getVarEntry :: Symbol -> ValueEnv a -> Maybe (VEntry a)
getVarEntry sym venv =
  case Symtab.get sym venv of
    Just (VarEntry v) -> Just v
    _                 -> Nothing

getFunEntry :: Symbol -> ValueEnv a -> Maybe (FEntry a)
getFunEntry sym venv =
  case Symtab.get sym venv of
    Just (FunEntry f) -> Just f
    _                 -> Nothing

base_venv :: (ValueEnv a)
base_venv = empty -- empty for now. will include intrinsic functions
