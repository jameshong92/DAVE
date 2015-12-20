open Ast
open Sast
open Printf

let rec string_of_datatype = function
  Int -> "int"
| Float -> "float"
| Bool -> "bool"
| String -> "string"
| Tbl -> "tbl"
| Rec -> "rec"
| Fld -> "fld"
| Void -> "void"

let string_of_unop = function
  Neg -> "-"
| Not -> "!"

let string_of_postop = function
  Inc -> "++"
| Dec -> "--"

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
| Or -> "||"
| And -> "&&"

let string_of_asnop = function
  Asn -> "="
| Addeq -> "+="
| Subeq -> "-="
| Muleq -> "*="
| Diveq -> "/="
| Modeq -> "%="

(* let string_of_id = function
  Id(id) -> id *)

let rec list_of_dim v = match v with
  [] -> ""
  | hd::tl -> let exp_string = string_of_expr hd.exp in
              if exp_string = "0" then
                "[]"
              else
                "[" ^ exp_string ^ "]"

and string_of_expr = function
  Range(exp1, exp2) -> "range(" ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2 ^ ")"
| IntLit(lit) -> string_of_int lit
| FloatLit(lit) -> string_of_float lit
| StringLit(lit) -> "\"" ^ lit ^ "\""
| BoolLit(lit) -> string_of_bool lit
| ArrayLit(exps) -> "{" ^ String.concat ", " (List.map string_of_expr exps) ^ "}"
| Binop(exp1, binop, exp2) -> "(" ^ string_of_expr exp1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr exp2 ^ ")"
| Unop(unop, exp) -> "(" ^ string_of_unop unop ^ string_of_expr exp ^ ")"
| Postop(lvalue, postop) -> "(" ^ string_of_expr lvalue ^ string_of_postop postop ^ ")"
| AssignOp(lvalue, asnop, exp) -> "(" ^ string_of_expr lvalue ^ " " ^ string_of_asnop asnop ^ " " ^ string_of_expr exp ^ ")"
| Cast(datatype, exp) -> "((" ^ string_of_datatype datatype.ptype ^ ")" ^ string_of_expr exp ^ ")"
| FuncCall(id, exps) -> string_of_func_call id exps
| Tbl(exps) -> "Tbl(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"
| Rec(exps) -> "Rec(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"
| RecRef(id, exp) -> "RecRef(" ^ id ^ ", " ^ string_of_expr exp ^ ")"
| Fld(exps, lit) -> "Fld([" ^ String.concat ", " (List.map string_of_expr exps) ^ "], \"" ^ lit ^ "\")"
| Lval(lvalue) -> string_of_expr lvalue
| Noexpr -> ""
| None -> "null"
| Var(exp) -> exp
| Array(exp1, exp2) -> string_of_array exp1 exp2
| Access(exp1, exp2) -> "(" ^ string_of_expr exp1 ^ "." ^ exp2 ^ ")"

and string_of_array id exp2 = match exp2 with
  | Range(id1, id2) -> "slice_array(" ^ id ^ ", sizeof(" ^ id ^ "), " ^ string_of_expr id1 ^ ", " ^ string_of_expr id2 ^ ")"
  | _ -> "(" ^ id ^ "[" ^ string_of_expr exp2 ^ "])"

and string_of_func_call id exps = id ^ "(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"

and string_of_vtype v =
(*   if v.s_dimension == [] then
    string_of_datatype v.s_ptype
  else string_of_datatype v.s_ptype ^ list_of_dim (List.rev v.s_dimension) *)
let dimension = v.s_dimension in
  match dimension with
  [] -> string_of_datatype v.s_ptype
  | _ -> string_of_datatype v.s_ptype ^ list_of_dim (List.rev v.s_dimension)

and string_of_var v id =
(*   if v.s_dimension == [] then
    string_of_datatype v.s_ptype ^ " " ^ id
  else
    string_of_datatype v.s_ptype ^ " " ^ id ^ list_of_dim (List.rev v.s_dimension) *)
let dimension = v.s_dimension in
  match dimension with
  [] -> string_of_datatype v.s_ptype ^ " " ^ id
  | _ -> string_of_datatype v.s_ptype ^ " " ^ id ^ list_of_dim (List.rev v.s_dimension)

let string_of_decl vdecl =
  let init = vdecl.s_vinit.exp in
  match init with
    Noexpr ->
      (string_of_var vdecl.s_vtype vdecl.s_vname) ^ ";"
    | _ ->
      (string_of_var vdecl.s_vtype vdecl.s_vname) ^ " = " ^ string_of_expr vdecl.s_vinit.exp ^ ";"

let rec gen_stmt = function
  S_Expr(exp) -> "(" ^ (string_of_expr exp.exp) ^ ");"
| S_Return(exp) -> "return " ^ string_of_expr exp.exp ^ ";"
| S_Block(stmt_list) -> "{\n" ^ (String.concat "\n" (List.map gen_stmt stmt_list)) ^ "\n}\n"
| S_If(exp, stmt1, stmt2) -> (if stmt2 == S_EmptyStmt then
  "if (" ^ (string_of_expr exp.exp) ^ ")\n" ^ (gen_stmt stmt1) else
  "if (" ^ (string_of_expr exp.exp) ^ ")\n" ^ (gen_stmt stmt1) ^ "else " ^ (gen_stmt stmt2))
| S_For(init, test, after, stmt) -> "for (" ^ string_of_expr init.exp ^ "; " ^ string_of_expr test.exp ^ "; " ^ string_of_expr after.exp ^ ") " ^ gen_stmt stmt
| S_While(test, stmt) -> "while (" ^ (string_of_expr test.exp) ^ ") " ^ (gen_stmt stmt)
| S_VarDeclStmt(decl) -> string_of_decl decl
| S_Continue -> "continue;"
| S_Break -> "break;"
| S_EmptyStmt -> ";"

let string_of_func_decl funcdecl =
  string_of_vtype funcdecl.s_return_type ^ " "
  ^ funcdecl.s_fname ^ "("
  ^ (String.concat ", " (List.map string_of_decl funcdecl.s_formals))
  ^ ") {\n"
  ^ gen_stmt funcdecl.s_body ^ "\n}"

let string_of_program prg =
  "#include \"dave.h\"\nusing namespace std;\n"
  ^ (String.concat "\n" (List.map string_of_decl prg.s_gdecls)) ^ "\n"
  ^ (String.concat "\n" (List.map string_of_func_decl prg.s_fdecls)) ^ "\n"

let compile oc prg =
  let out_file = open_out oc in
  fprintf out_file "%s" (string_of_program prg);
  close_out out_file
