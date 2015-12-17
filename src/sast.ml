open Ast

exception Invalid_use of string

type s_expr_value =
    S_Id of string
  | S_IntLit of int
  | S_FloatLit of float
  | S_StringLit of string
  | S_BoolLit of bool
  | S_Array of s_expr list
  | S_Access of s_expr * string
  | S_Index of string * s_expr
  | S_Range of s_expr * s_expr
  | S_Binop of s_expr * binop * s_expr
  | S_Unop of unop * s_expr
  | S_Postop of s_expr * postop
  | S_AssignOp of s_expr * asnop * s_expr
  | S_Assign of s_expr * s_expr
  | S_Cast of datatype * s_expr
  | S_CastFld of s_expr * string
  | S_CastTbl of datatype * datatype
  | S_FuncCall of s_expr * s_expr list
  | S_Tbl of s_expr list
  | S_Rec of s_expr list
  | S_RecRef of s_expr * s_expr
  | S_Fld of s_expr list * string
  | S_Noexpr
and s_expr = datatype * s_expr_value

type s_decl = datatype * id * s_expr

type s_stmt =
    S_Expr of s_expr
  | S_Return of s_expr
  | S_Block of s_stmt list
  | S_If of s_expr * s_stmt * s_stmt
  | S_For of s_expr * s_expr * s_expr * s_stmt
  | S_While of s_expr * s_stmt
  | S_VarDeclStmt of s_decl
  | S_Continue
  | S_Break
  | S_EmptyStmt

type func_sig = {
  sig_fname : id;
  sig_formals : s_decl list;
}

type s_func_decl = {
  s_fname : string;
  s_formals : s_decl list;
  s_body: s_stmt;
  s_return_type : datatype;
}

type s_program = {
  s_gdecls : s_decl list;
  s_fdecls : s_func_decl list;
}
