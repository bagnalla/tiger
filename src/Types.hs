module Types (
  approxTy,
  fixTys,
  isNil,
  showTy,
  TyId,
  Ty(..),
  test
  ) where

import Absyn (Symbol)
import Symtab (Id(..))

type TyId = Int

data Ty = 
  RecordTy [(Symbol, Ty)] TyId
  | NilTy
  | IntTy
  | StringTy
  | ArrayTy Ty TyId
  | NameTy Symbol
  | UnitTy
  deriving (Show)

instance Eq Ty where
  RecordTy _ tyId1 == RecordTy _ tyId2 = tyId1 == tyId2
  NilTy == NilTy = True
  NilTy == RecordTy _ _ = True
  RecordTy _ _ == NilTy = True
  IntTy == IntTy = True
  StringTy == StringTy = True
  ArrayTy _ tyId1 == ArrayTy _ tyId2 = tyId1 == tyId2
  NameTy sym1 == NameTy sym2 = sym1 == sym2
  UnitTy == UnitTy = True
  _ == _ = False

isNil :: Ty -> Bool
isNil NilTy = True
isNil _     = False

showTy :: Ty -> String
showTy ty = show (approxTy 1 ty)

tySubst :: Symbol -> Ty -> Ty -> Ty
tySubst sym ty (RecordTy fieldTys tyId) =
  RecordTy (map (\(s, t) -> (s, tySubst sym ty t)) fieldTys) tyId
tySubst sym ty (ArrayTy t tyId) =
  ArrayTy (tySubst sym ty t) tyId
tySubst sym ty (NameTy s) =
  if sym == s then ty else (NameTy s)
tySubst _ _ ty = ty

fix g = g (fix g)

abstractTy :: Symbol -> Ty -> (Ty -> Ty)
abstractTy sym ty = \s -> tySubst sym s ty

-- abstractTys :: [Symbol] -> Ty -> ([Ty] -> Ty)
-- abstractTys syms ty = \tys -> foldl f ty (zip syms tys)
--   where f acc (sym, ty) =
--           tySubst sym ty acc

abstractTys :: [Symbol] -> [Ty] -> ([Ty] -> [Ty])
abstractTys syms tys = \tys' -> map (\ty -> foldl f ty (zip syms tys')) tys
  where f acc (sym, ty) =
          tySubst sym ty acc

fixTy :: Symbol -> Ty -> Ty
fixTy sym ty = fix (abstractTy sym ty)

fixTys :: [Symbol] -> [Ty] -> [Ty]
fixTys syms tys =
  fix (abstractTys syms tys) 

test :: () -> IO Ty
test () =
  let ty = ArrayTy (NameTy (Id "asdf")) 0
      ty' = fixTy (Id "asdf") ty in
    return ty'

approxTy :: Int -> Ty -> Ty
approxTy n ty
  | n <= 0 = NilTy
  | n > 0  = case ty of
      RecordTy fieldTys tyId ->
        RecordTy (map (\(s, t) -> (s, approxTy (n-1) t)) fieldTys) tyId
      ArrayTy ty tyId -> ArrayTy (approxTy (n-1) ty) tyId
      _ -> ty
