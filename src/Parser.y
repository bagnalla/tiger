-- This grammar file is based on
-- 1) The template found here https://github.com/dagit/happy-plus-alex

{
{-# OPTIONS -w #-}
module Parser( parseProg ) where

import Data.Maybe (fromMaybe)

import Absyn
import Lexer
import Symtab (Id(..))
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%token
  type      { Token $$ TokenType }
  var       { Token $$ TokenVar }
  function  { Token $$ TokenFunction }
  break     { Token $$ TokenBreak }
  of        { Token $$ TokenOf }
  end       { Token $$ TokenEnd }
  in        { Token $$ TokenIn }
  nil       { Token $$ TokenNil }
  let       { Token $$ TokenLet }
  do        { Token $$ TokenDo }
  to        { Token $$ TokenTo }
  for       { Token $$ TokenFor }
  while     { Token $$ TokenWhile }
  else      { Token $$ TokenElse }
  then      { Token $$ TokenThen }
  if        { Token $$ TokenIf }
  array     { Token $$ TokenArray }
  ":="      { Token $$ TokenAssign }
  '|'       { Token $$ TokenOr }
  '&'       { Token $$ TokenAnd }
  ">="      { Token $$ TokenGe }
  '>'       { Token $$ TokenGt }
  "<="      { Token $$ TokenLe }
  '<'       { Token $$ TokenLt }
  "<>"      { Token $$ TokenNeq }
  '='       { Token $$ TokenEq }
  '/'       { Token $$ TokenDivide }
  '*'       { Token $$ TokenTimes }
  '-'       { Token $$ TokenMinus }
  '+'       { Token $$ TokenPlus }
  '.'       { Token $$ TokenDot }
  '}'       { Token $$ TokenRBrace }
  '{'       { Token $$ TokenLBrace }
  ']'       { Token $$ TokenRBrack }
  '['       { Token $$ TokenLBrack }
  ')'       { Token $$ TokenRParen }
  '('       { Token $$ TokenLParen }
  ';'       { Token $$ TokenSemicolon }
  ':'       { Token $$ TokenColon }
  ','       { Token $$ TokenComma }
  stringVal { Token _ (TokenString _) }
  intVal    { Token _ (TokenInt _) }
  id        { Token _ (TokenId _) }
--  eof       { Token $$ TokenEOF }

%right ';' ','
%left ITE
%nonassoc ']'
%nonassoc while do then in end let of
%nonassoc natVal id
%nonassoc '(' ')' '{' '}' '[' ']'
%nonassoc ':'
%nonassoc "<=" '<' ">=" '>' '=' "<>" ":="
%left '&' '|'
%left '+' '-'
%left '*' '/'
%nonassoc NEG
%%

Prog :
  Exp { $1 }

opt(p) :
  p { Just $1 }
  | { Nothing }

Decs :
  Dec { [$1] }
  | Dec Decs { case $1 of
                 TypeDec [tdec] ->
                   case $2 of
                     d:ds ->
                       case d of
                         TypeDec tdecs -> TypeDec (tdec:tdecs) : ds
                         _ -> $1 : $2
                     _ -> $1 : $2
                 FunctionDec [fdec] ->
                   case $2 of
                     f:fs ->
                       case f of
                         FunctionDec fdecs -> FunctionDec (fdec:fdecs) : fs
                         _ -> $1 : $2
                     _ -> $1 : $2
                 _ -> $1 : $2 }

Dec :
  Tydec { TypeDec [$1] }
  | Vardec { VarDec $1 }
  | Fundec { FunctionDec [$1] }

Tydec :
  type id '=' Ty { let (id, _) = extractId $2 in
                     TyDec { tydec_name = id, tydec_ty = $4, tydec_pos = $1 } }

Ty :
  id { let (id, pos) = extractId $1 in NameTy id pos }
  | '{' opt(Tyfields) '}' { case $2 of
                              Just fields -> RecordTy fields
                              Nothing     -> RecordTy [] }
  | array of id { let (id, pos) = extractId $3 in ArrayTy id pos }

Tyfields :
  Tyfields ',' Tyfields { $1 ++ $3 }
  | Tyfield { $1 }

Tyfield :
  id ':' id { let (id1, pos) = extractId $1 in
              let (id2, _)   = extractId $3 in
                [Field { field_name = id1, field_typ = id2, field_pos = pos }] }

Vardec :
  var id ":=" Exp
    { let (id, pos) = extractId $2 in
        VDec { vdec_name = id,
               vdec_typ = Nothing,
               vdec_init = $4,
               vdec_pos = pos } }
  | var id ':' id ":=" Exp
    { let (vid, pos1) = extractId $2 in
      let (tid, pos2) = extractId $4 in
      VDec { vdec_name = vid,
             vdec_typ = Just (tid, pos2),
             vdec_init = $6,
             vdec_pos = pos1 } }

Fundec :
  function id '(' Tyfields ')' '=' Exp
    { let (id, pos) = extractId $2 in
        Fundec { fun_name = id,
                 fun_params = $4,
                 fun_result = Nothing,
                 fun_body = $7,
                 fun_pos = pos } }
  | function id '(' Tyfields ')' ':' id '=' Exp
    { let (fid, pos1) = extractId $2 in
      let (tid, pos2) = extractId $7 in
        Fundec { fun_name = fid,
                 fun_params = $4,
                 fun_result = Just (tid, pos1),
                 fun_body = $9,
                 fun_pos = pos2 } }

Lvalue :
  id { let (id, pos) = extractId $1 in
         SimpleVar id pos }
  | Lvalue '.' id { let (id, pos) = extractId $3 in
                      FieldVar $1 id pos }
  | Lvalue '[' Exp ']' { SubscriptVar $1 $3 $2 }
  | id Subscript { let (id, pos) = extractId $1 in
                     SubscriptVar (SimpleVar id pos) $2 pos }

Subscript :
  '[' Exp ']' { $2 } 

Exps : 
  Exps ',' Exps { $1 ++ $3 }
  | Exp { [$1] }

Exp :
  Exp ';' Exp { let es1 = seqOfExp $2 $1 in
                let es2 = seqOfExp $2 $3 in
                SeqExp (es1 ++ es2) }
  | Lvalue { VarExp $1 }
  | nil { NilExp }
  | '(' ')' { SeqExp [] }
  | let Decs in opt(Exp) end { LetExp (Let { let_decs = $2,
                                             let_body = maybeToExp $4,
                                             let_pos = $1 }) }
  | intVal { case $1 of
               Token _ (TokenInt i) -> IntExp i }
  | stringVal { case $1 of
                  Token pos (TokenString s) -> StringExp s pos }
  | '-' Exp %prec NEG { mkOp PlusOp (IntExp 0) $2 $1 }
  | id '(' Exps ')' { let (id, pos) = extractId $1 in
                        CallExp (Call { call_func = id,
                                        call_args = $3,
                                        call_pos = pos }) }
  | Exp '+' Exp { mkOp PlusOp $1 $3 $2 }
  | Exp '-' Exp { mkOp MinusOp $1 $3 $2 }
  | Exp '*' Exp { mkOp TimesOp $1 $3 $2 }
  | Exp '/' Exp { mkOp DivideOp $1 $3 $2 }
  | Exp '=' Exp { mkOp EqOp $1 $3 $2 }
  | Exp "<=" Exp { mkOp LeOp $1 $3 $2 }
  | Exp '<' Exp { mkOp LtOp $1 $3 $2 }
  | Exp ">=" Exp { mkOp GeOp $1 $3 $2 }
  | Exp '>' Exp { mkOp GtOp $1 $3 $2 }
  | Exp "<>" Exp { mkOp NeqOp $1 $3 $2 }
  | Exp '&' Exp { IfExp (If { if_test = $1,
                              if_then = $3,
                              if_else = Just (IntExp 0),
                              if_pos = $2 }) }
  | Exp '|' Exp { IfExp (If { if_test = $1,
                              if_then = IntExp 1,
                              if_else = Just $3,
                              if_pos = $2 }) }
  | id '{' opt(Recfields) '}'
      { let (id, pos) = extractId $1 in
          RecordExp (Record { record_fields = fromMaybe [] $3,
                              record_typ = id,
                              record_pos = pos }) }
  | id Subscript of Exp
      { let (id, pos) = extractId $1 in
          ArrayExp (Array { array_typ = id,
                            array_size = $2,
                            array_init = $4,
                            array_pos = pos })}
  | Lvalue ":=" Exp
      { AssignExp (Assign { assign_var = $1,
                            assign_exp = $3,
                            assign_pos = $2 }) }
  | if Exp then Exp else Exp %prec ITE
      { IfExp (If { if_test = $2,
                    if_then = $4,
                    if_else = Just $6,
                    if_pos = $1 }) }
  | if Exp then Exp
      { IfExp (If { if_test = $2,
                    if_then = $4,
                    if_else = Nothing,
                    if_pos = $1 }) }
  | while Exp do Exp
      { WhileExp (While { while_test = $2,
                          while_body = $4,
                          while_pos = $1 }) }
  | for id ":=" Exp to Exp do Exp
      { let (id, pos) = extractId $2 in
          ForExp (For { for_var = id,
                        for_lo = $4,
                        for_hi = $6,
                        for_body = $8,
                        for_pos = $1 } ) }
  | break { BreakExp $1 }
  | '(' Exp ')' { $2 }

Recfields :
  Recfields ',' Recfields { $1 ++ $3 }
  | Recfield { [$1] }

Recfield :
  id '=' Exp { let (id, pos) = extractId $1 in (id, $3, pos) }

{
--toPos :: AlexPosn -> Int
--toPos p = case p of AlexPn i _ _ -> i

extractId :: Token -> (Symbol, Pos)
extractId t = case t of Token pos (TokenId id) -> (id, pos)

seqOfExp :: Pos -> Exp -> [(Exp, Pos)]
seqOfExp pos (SeqExp es) = es
seqOfExp pos e = [(e, pos)]

maybeToExp :: Maybe Exp -> Exp
maybeToExp Nothing = SeqExp []
maybeToExp (Just e) = e

mkOp :: Oper -> Exp -> Exp -> AlexPosn -> Exp
mkOp op e1 e2 pos = OpExp (Op { op_left = e1,
                                op_oper = op,
                                op_right = e2,
                                op_pos = pos })

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseProg :: FilePath -> String -> Either String Exp
parseProg = runAlex' parse
}
