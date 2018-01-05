module Absyn (
  Array(..), Assign(..), Call(..), Dec(..), Exp(..), Field(..), For(..),
  Fundec(..), If(..), Let(..), Op(..), Oper(..), Pos, Record(..),
  Symbol, Ty(..), TyDec(..), VDec(..), Var(..), While(..), mkSymbol
  ) where

import Lexer (AlexPosn)
import Symtab (Id(..))

type Pos = AlexPosn

type Symbol = Id

data Var =
  SimpleVar Symbol Pos
  | FieldVar Var Symbol Pos
  | SubscriptVar Var Exp Pos
  deriving (Eq, Show)

data Exp =
  VarExp Var
  | NilExp
  | IntExp Int
  | StringExp String Pos
  | CallExp Call
  | OpExp Op
  | RecordExp Record
  | SeqExp [(Exp, Pos)]
  | AssignExp Assign
  | IfExp If
  | WhileExp While
  | ForExp For
  | BreakExp Pos
  | LetExp Let
  | ArrayExp Array
  deriving (Eq, Show)

data Call = Call
  { call_func :: Symbol,
    call_args :: [Exp],
    call_pos  :: Pos }
  deriving (Eq, Show)

data Op = Op
  { op_left  :: Exp,
    op_oper  :: Oper,
    op_right :: Exp,
    op_pos   :: Pos }
  deriving (Eq, Show)

data Record = Record
  { record_fields :: [(Symbol, Exp, Pos)],
    record_typ    :: Symbol,
    record_pos    :: Pos}
  deriving (Eq, Show)

data Assign = Assign
  { assign_var :: Var,
    assign_exp :: Exp,
    assign_pos :: Pos }
  deriving (Eq, Show)

data If = If
  { if_test :: Exp,
    if_then :: Exp,
    if_else :: Maybe Exp,
    if_pos  :: Pos }
  deriving (Eq, Show)

data While = While
  { while_test :: Exp,
    while_body :: Exp,
    while_pos  :: Pos }
  deriving (Eq, Show)

data For = For
  { for_var  :: Symbol,
    for_lo   :: Exp,
    for_hi   :: Exp,
    for_body :: Exp,
    for_pos  :: Pos }
  deriving (Eq, Show)

data Let = Let
  { let_decs :: [Dec],
    let_body :: Exp,
    let_pos  :: Pos }
  deriving (Eq, Show)

data Array = Array
  { array_typ  :: Symbol,
    array_size :: Exp,
    array_init :: Exp,
    array_pos  :: Pos}
  deriving (Eq, Show)

data Dec =
  FunctionDec [Fundec]
  | VarDec VDec
  | TypeDec [TyDec]
  deriving (Eq, Show)

data VDec = VDec
  { vdec_name :: Symbol,
    vdec_typ  :: Maybe (Symbol, Pos),
    vdec_init :: Exp,
    vdec_pos  :: Pos }
  deriving (Eq, Show)

data TyDec = TyDec
  { tydec_name :: Symbol,
    tydec_ty   :: Ty,
    tydec_pos  :: Pos }
  deriving (Eq, Show)

data Ty =
  NameTy Symbol Pos
  | RecordTy [Field]
  | ArrayTy Symbol Pos
  deriving (Eq, Show)

data Oper =
  PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  deriving (Eq, Show)

data Field = Field
  { field_name :: Symbol,
    field_typ  :: Symbol,
    field_pos  :: Pos }
  deriving (Eq, Show)

data Fundec = Fundec
  { fun_name   :: Symbol,
    fun_params :: [Field],
    fun_result :: Maybe (Symbol, Pos),
    fun_body   :: Exp,
    fun_pos    :: Pos }
  deriving (Eq, Show)

-------
-- Misc

mkSymbol :: String -> Symbol
mkSymbol s = Id s
