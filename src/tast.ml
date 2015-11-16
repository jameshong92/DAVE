open Ast

exception Not_now of string

type i_datatype = Ivoid | Iint | Ifloat | Istring | Ibool
           | Iint_array | Ifloat_array | Istring_array

type i_var = {
  i_vtype : i_datatype;
  i_vname : string
}

type i_lvalue = 
    IId of string
  | IIndex of string * i_expr
  | IAccess of i_expr * string 
and i_expr = 
    IIntlit of int
  | IFloatlit of float
  | IStringlit of string
  | IBoollit of bool
  | IArray of i_expr list
  | IUnop of unop * iexpr
  | IPostop of i_expr * postop 
  | IAssgin of i_expr * i_expr
  | IAssignOp of i_expr * asnop * i_expr
  | IBinop of i_expr * binop * i_expr
  | ILval of i_lvalue
  | No_expr

type i_stmt = 
    IEmpty
  | IVarDec of i_var_decl
  | Iexpr of i_expr
  | IReturn of i_expr
  | IBreak
  | Icontinue
  | IIf of i_expr * i_stmt list * i_stmt list
  | IFor of i_stmt * i_expr * i_expr * i_stmt list
  | IWhile of i_expr * i_stmt list
  | IBlock of i_stmt
  | IDisp of i_expr (* cout << e << endl *)

type i_var_decl = 
  | IVardecl of i_datatype * i_expr
  | IArraydecl of i_datatype * i_expr * i_id

type i_struct_decl = {
	i_sname : string;
	i_vars : i_var list; 
}

type i_func_def = {
	i_fname : string;
	i_args : i_var list;
	i_body : i_stmt list;
	i_return : i_datatype;
}

type i_program = {
	i_var_decls : i_var_decl list;
	i_struct_decls : i_struct_decl list;
	i_func_defs : i_func_def list;
}
