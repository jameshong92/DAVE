open Ast

exception Not_now of string

type i_datatype = Ivoid | Iint | Idouble | Istring | Ibool
           | Iint_array | Idouble_array | Istring_array

type i_struct = Istruct

type i_id = Iid of string

type i_var = i_datatype * string

type i_lvalue = 
    IVar of id
  | IArray of i_id * i_expr
  | IAccess of i_expr * i_id 
and i_expr = 
    Iintlit of int
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
  | Noi_expr

type i_stmt = 
    IEmpty
  | IVarDec of i_var_decl
  | Iexpr of i_expr
  | IReturn of i_expr
  | IBreak
  | Icontinue
  | IIf of i_expr * i_stmt * i_stmt
  | IFor of i_stmt * i_expr * i_expr * i_stmt
  | IWhile of i_expr * i_stmt
  | IBlock of i_stmt list
  | IDisp of i_expr (* cout << e << endl *)

type i_var_decl = 
  | IVardecl of i_datatype * i_expr
  | IAccessdecl of i_datatype * i_id * i_expr
  | IArraydecl of i_datatype * i_expr * id

type i_struct_decls = {
	i_sname : i_struct * string;
	i_struct_vars : i_var list; 
}

type i_func_decls = {
	i_fname : string;
	i_args : i_var list;
	i_body : i_stmt list;
	i_return : i_datatype;
}

type i_program = {
	i_var_decls : i_var_decl list;
	i_struct_decls : i_struct_decl list;
	i_func_decls : i_fun list;
}
