open Ast
open Tast
open Printf

exception Not_done of string

let gen_datatype t = match t wtih
    Iint | Iint_array -> "int"
  | Idouble | Idouble_array -> "double"
  | Istring | Istring_array -> "string"
  | Ibool -> "bool"
  | Ivoid -> "void"

let gen_unop op = match op with 
    Neg -> "-" | Not -> "!" 

let gen_postop op = match op with 
    Inc -> "++" | Dec -> "--"

let gen_binop op = match op with 
    Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Leq -> "<=" | Gt -> ">" |
  | Geq -> ">=" | And -> "&&" | Or -> "||"

let gen_asnop op = match op with
    AddEq -> "+=" | Subeq -> "-=" | Muleq -> "*=" | Diveq -> "/=" | Modeq-> "%="

let gen_lval val = match val with
    IId x -> (" " ^ x ^ " ")
  | IIndex (s,e) -> (sprintf " (%s[%s]) " s (gen_expr e)) 
  | IAccess (e,s) -> (sprintf " %s\.%s" (gen_expr e) s)
let rec gen_expr exp = match exp with
    IIntlit x -> (sprintf " %d " x)
  | IFloatlit x -> (sprintf " %f " x)
  | IStringlit x -> (" string(\"" ^ x ^ "\") ")
  | IBoolval x -> if x then " true " else " false "
  | IBinop (e1, op, e2) -> (sprintf "( %s %s %s )" (gen_expr e1) (gen_binop op) (gen_expr e2))
  | IAssign (e1, e2) -> (sprintf "( %s = %s )" (gen_expr e1) (gen_binop op) (gen_expr e2))
  | IAssignOp (e1, op, e2) -> (sprintf "( %s %s %s )" (gen_expr e1) (gen_asnop op) (gen_expr e2))
  | IUnop (op, e) -> (sprintf "( %s %s )" (gen_unop op) (gen_expr e))
  | IPostop (e, op) -> (sprintf "( %s %s )" (gen_expr e) (gen_postop op))
  | ILval x -> (gen_lval x)
  | IArray el -> (sprintf "%s" (gen_array "," el))
  | Icall (s, el) -> (sprintf "( %s(%s) )" s (gen_array "," el))
and gen_array op el = match el with
    [] -> ""
  | [e] -> gen_expr e
  | e1::e2::tl -> (sprintf "%s %s %s" (gen_expr e1) op (gen_array op (e2::tl))

let rec gen_var_decls vars = match vars with
    [] -> []
  | hd::tl -> (gen_var_decl hd) :: (gen_var_decls tl)
and gen_var_decl var = 
  let t, s, e = var in match t with
    Iint_array | Ifloat_array | Istring_array -> sprintf "%s %s[] = %s;" (gen_datatype t) s (gen_expr e)
    | t -> sprintf "%s %s = %s;" (gen_datatype t) s (gen_expr e)

let rec gen_stmt stmt = match stmt with
    IEmpty -> ";"
  | IExpr e -> ((gen_expr e) ^ ";")
  | IReturn e -> (sprintf "return %s ;" (gen_expr e))
  | IBreak -> "break;"
  | IContinue -> "continue;"
  | IDisp e -> (sprintf "cout << %s << endl;" (gen_expr e))
  | IVarDec(t, s, e) -> (gen_var_decl (t,s,e))
  | IIf(e1, e2, e3) -> (sprintf "if (%s) {%s} else { %s }" (gen_expr e1) (gen_stmt e2) (gen_stmt e3))
  | IFor(e1, e2, e3, e4) -> (sprintf "for (%s %s; %s) { %s }" (gen_stmt e1) (gen_expr e2) (gen_expr e3) (gen_stmts e4))
  | IBlock(e) -> sprintf "{ %s }" (gen_stmts e)
  | IWhile (e1,e2) -> (sprintf "while (%s) { %s }" (gen_expr e1) (gen_stmt e2))
and gen_stmts stmts = match stmts with
    [] -> []
  | hd::tl -> (gen_stmt hd) :: (gen_stmts tl)

let rec gen_args op args = match args with
    [] -> ""
  | [a] -> sprintf "%s %s" (gen_datatype a.i_vtype) (a.i_vname)
  | a::b::tl -> (sprintf "%s %s%s" (gen_datatype a.i_vtype) (a.i_vname) ^ (gen_args op (b::tl))
let rec gen_func_defs fundefs = match fundefs with
    [] -> []
  | hd :: tl -> (if hd.i_body != [] then
                    ([sprintf "%s %s(%s) {" (gen_datatype hd.i_return) hd.i_fname (gen_vars "," hd.i_args)] 
                     @(gen_stmts hd.i_body)@ ["}"])
                 else ([sprintf "extern %s %s(%s);" (gen_datatype hd.i_return) hd.i_fname (gen_args "," hd.i_args)])
                )@(gen_func_defs tl)

let rec gen_i_struct_vars s_vars = match s_vars with
    [] -> ""
  | hd :: tl -> (sprintf "%s %s ; " (gen_datatype hd.i_vtype) (hd.i_vname) ^ (gen_i_struct_vars tl))
let rec gen_struct_decls structs = match structs with
    [] -> []
  | hd :: tl -> ([sprintf "struct %s {" hd.i_sname] @(gen_i_struct_vars hd.i_struct_vars)@ ["} %s" hd.i_sname])

let compile oc prg = 
    let head_lines = 
      ["#include \"DAVE.hpp\""]
    in 
    let var_lines = 
      let vars = prg.i_var_decls in
      gen_var_decls vars
    in
    let struct_lines = 
      let structs = prg.i_struct_decls in 
      gen_struct_decls structs
    in
    let func_lines =
      let funs = prg.i_func_defs in
      gen_func_defs funs
    in 
    let all = head_lines @ var_lines @ struct_lines @ func_lines in
    List.iter (fun line -> fprintf oc "%s\n" line) all
