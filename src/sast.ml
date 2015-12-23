open Ast

type t_expr = {
  exp: expr;
  typ: s_var
}

and s_var = {
  s_ptype: datatype;
  s_dimension: t_expr list;
}

type s_decl = {
  s_vname: string;
  s_vtype: s_var;
  s_vinit: t_expr;
}

type s_stmt =
    S_Expr of t_expr
  | S_Return of t_expr
  | S_Block of s_stmt list
  | S_If of t_expr * s_stmt * s_stmt
  | S_For of t_expr * t_expr * t_expr * s_stmt
  | S_While of t_expr * s_stmt
  | S_VarDeclStmt of s_decl
  | S_Continue
  | S_Break
  | S_EmptyStmt

type s_func_decl = {
  s_fname : string;
  s_formals : s_decl list;
  s_body: s_stmt;
  s_return_type : s_var;
}

type s_program = {
  s_gdecls : s_decl list;
  s_fdecls : s_func_decl list;
}