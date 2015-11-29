open Printf
open Ast
open Sast

let string_of_binop = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Exp -> "^"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_asnop = function
    Asn -> "="
  | Addeq -> "+="
  | Subeq -> "-="
  | Muleq -> "*="
  | Diveq -> "/="
  | Modeq -> "%="

let rec string_of_expr = function
    S_IntLit(lit) -> string_of_int lit
  | S_FloatLit(lit) -> string_of_float lit
  | S_StringLit(lit) -> "\"" ^ lit ^ "\""
  | S_BoolLit(lit) -> string_of_bool lit
  | S_ArrayLit(exps) -> String.concat ", " (List.map string_of_expr exps)
  (*| Range(exp1, exp2) -> string_of_expr exp1 ^ " " ^ string_of_expr exp2
  *)
  | S_Binop(exp1, binop, exp2) -> string_of_expr exp1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr exp2
  | S_Unop(unop, exp) -> string_of_unop unop ^ " " ^ string_of_expr exp
  | S_Postop(lvalue, postop) -> string_of_lvalue lvalue ^ " " ^ string_postop potsop
  | S_AssignOp(lvalue, asnop, exp) -> string_of_lvalue lvalue ^ " " ^ string_of_asnop ^ " " ^ string_of_expr exp
  | S_Assign(lvalue, exp) -> string_of_lvalue lvalue ^ " " ^ string_of_expr exp
  | S_Lval(lvalue) -> string_of_lvalue lvalue
  (* Cast needs to differentiate primitives and structs *)
  | S_Cast(datatype, exp) -> "(" ^ string_of_datatype datatype ^ ") " ^ string_of_expr exp
  | S_FuncCall(id, exps) -> (string_of_func_call id exps)
  (* Need to implement structs to translate below *)
  | S_Rec(exps) -> "Rec{ " ^ String.concat ", " (List.map string_of_expr exps) ^ " }"
  | S_RecRef(id, exp) -> "RecRef( " ^ string_of_id id ^ ":" ^ string_of_expr exp ^ " )"
  | S_Fld(exps, lit) -> "Fld([" ^ String.concat "; " (List.map string_of_expr exps) ^ "], '" ^ lit ^ "' )"
  | S_None -> "NULL"
  | S_Noexpr -> ""

and string_of_func_call id exps = id ^ "(" ^ String.concat ", " (List.map string_of_expr exps) ^ " )"

let string_of_datatype = function
    Int -> "int"
  | Char -> "char"
  | Bool -> "bool"
  | String -> "string"
  | Tbl -> "tbl"
  | Rec -> "rec"
  | Fld -> "fld"
  | Void -> "void"
  | _ -> "unknown_type"

let string_of_decl = function
  	VarDecl(datatype, id) -> string_of_datatype datatype ^ " " ^ string_of_id id ^ ";"
  | AssignDecl(datatype, id, exp) -> string_of_datatype datatype ^ " " ^ string_of_id id ^ " = " ^ string_of_expr exp ^ ";"
  | ArrayDecl(datatype, exp, id) -> string_of_datatype datatype ^ " " ^ string_of_id id ^ "[" ^ string_of_expr exp ^ "];"

let rec string_of_stmt = function
    S_Expr(exp) -> "(" ^ string_of_expr exp ^ ");"
  | S_Return(exp) -> "return " ^ string_of_expr exp ^ ";"
  | S_Block(stmt_list) -> (String.concat "\n" (List.map string_of_stmt stmt_list)) ^ "\n"
  | S_If(exp, stmt1, stmt2) -> "if (" ^ string_of_expr exp ^ ") {\n" ^ string_of_stmt stmt1 ^ string_of_stmt stmt2 ^ "\n}"
  | S_For(init, test, after, stmt) -> "for (" ^ string_of_expr init ^ ", " ^ string_of_expr test ^ ", " ^ string_of_expr after ^ ") {\n" ^ string_of_stmt stmt ^ "\n}"
  | S_While(test, stmt) -> "while ( " ^ string_of_expr test ^ " ) {\n" ^ string_of_stmt stmt ^ "\n}"
  | S_VarDeclStmt(var) -> string_of_var_decl var
  | S_Continue -> "continue;"
  | S_Break -> "break;"

let string_of_func_decl_only funcdecl = string_of_var_type funcdecl.s_ret.s_ptype (List.length funcdecl.s_ret.s_dimension) ^ " " ^ funcdecl.s_fname ^ "("
  ^ (String.concat ", " (List.map string_of_param_var_decl funcdecl.s_formals)) ^ ");"

let string_of_func_decl_with_body global_arr_init funcdecl =
  let cast_away_const vardecl =
    if (List.length vardecl.s_vtype.s_dimension) > 0 || (vardecl.s_vtype.s_ptype == Graph || vardecl.s_vtype.s_ptype == Node || vardecl.s_vtype.s_ptype == Edge || vardecl.s_vtype.s_ptype == Str) then
      let nonconst_ref_type = (string_of_var_type vardecl.s_vtype.s_ptype (List.length vardecl.s_vtype.s_dimension)) in
        nonconst_ref_type ^ " &" ^ vardecl.s_vname ^ " = (" ^ nonconst_ref_type ^ " &)_" ^ vardecl.s_vname
    else
      ""
  in
  string_of_var_type funcdecl.s_ret.s_ptype (List.length funcdecl.s_ret.s_dimension) ^ " " ^ funcdecl.s_fname ^ "("
  ^ (String.concat ", " (List.map string_of_param_var_decl funcdecl.s_formals))
  ^ ") {\n" ^ (String.concat ";\n" (List.map cast_away_const
  funcdecl.s_formals)) ^ ";\n" ^ (if (String.compare funcdecl.s_fname "_main") == 0 then global_arr_init else "") ^ string_of_stmt funcdecl.s_body ^ "\n}"

let string_of_program prog =
  let global_var_tuple_list = (List.map string_of_global_var_decl prog.s_gdecls) in
  "#include <vector>\n#include \"libprint.h\"\n#include \"libstring.h\"\n#include \"libgraph.h\"\nusing namespace std;\n"
  ^ (String.concat ";\n" (List.map fst global_var_tuple_list)) ^ ";\n"
	^ (String.concat "\n" (List.map string_of_func_decl_only prog.s_fdecls)) ^ "\n"
  ^ (String.concat "\n" (List.map (fun fdecls -> string_of_func_decl_with_body (String.concat "\n" (List.map snd global_var_tuple_list)) fdecls) prog.s_fdecls)) ^ "\n"
  ^ "int main() { _main(); return 0;}"

let write_c_program filename program =
  let file = open_out filename in
    fprintf file "%s" (string_of_program program);
    close_out file
