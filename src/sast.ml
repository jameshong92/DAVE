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
  s_sdecls : s_stmt list;
}

(* printing for debugging purposes *)
(* let rec string_of_datatype = function
  Int -> "int"
| Float -> "float"
| Bool -> "bool"
| String -> "string"
| Tbl -> "tbl"
| Rec -> "rec"
| Fld -> "fld"
| Void -> "void"
| ArrayType(datatype) -> string_of_datatype datatype

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
| Modeq -> "%=" *)
(* 
let string_of_id = function
  Id(id) -> id

let rec string_of_expr = function
  Range(exp1, exp2) -> "range(" ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2 ^ ")"
| IntLit(lit) -> string_of_int lit
| FloatLit(lit) -> string_of_float lit
| StringLit(lit) -> "\"" ^ lit ^ "\""
| BoolLit(lit) -> string_of_bool lit
| ArrayLit(exps) -> "{" ^ String.concat ", " (List.map string_of_expr exps) ^ "}"
| Binop(exp1, binop, exp2) -> "(" ^ string_of_expr exp1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr exp2 ^ ")"
| Unop(unop, exp) -> "(" ^ string_of_unop unop ^ string_of_expr exp ^ ")"
| Postop(lvalue, postop) -> "(" ^ string_of_lvalue lvalue ^ string_of_postop postop ^ ")"
| AssignOp(lvalue, asnop, exp) -> "(" ^ string_of_lvalue lvalue ^ " " ^ string_of_asnop asnop ^ " " ^ string_of_expr exp ^ ")"
| Assign(lvalue, exp) -> "(" ^ string_of_lvalue lvalue ^ " = " ^ string_of_expr exp ^ ")"
| Cast(datatype, exp) -> "((" ^ string_of_datatype datatype ^ ")" ^ string_of_expr exp ^ ")"
| FuncCall(id, exps) -> string_of_func_call id exps
| Tbl(exps) -> "Tbl(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"
| Rec(exps) -> "Rec(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"
| RecRef(id, exp) -> "RecRef(" ^ string_of_id id ^ ", " ^ string_of_expr exp ^ ")"
| Fld(exps, lit) -> "Fld([" ^ String.concat ", " (List.map string_of_expr exps) ^ "], \"" ^ lit ^ "\")"
| Lval(lvalue) -> string_of_lvalue lvalue
| Noexpr -> ""
| None -> "NULL"

and string_of_func_call id exps = string_of_id id ^ "(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"

and string_of_lvalue = function
  Var(exp) -> string_of_id exp
| Array(exp1, exp2) -> "(" ^ string_of_id exp1 ^ "[" ^ string_of_expr exp2 ^ "])"
| Access(exp1, exp2) -> "(" ^ string_of_expr exp1 ^ "." ^ string_of_id exp2 ^ ")"


let string_of_decl = function
  VarDecl(dtype, id) -> (match dtype with
  ArrayType(t) -> (string_of_datatype t) ^ string_of_id id ^ "[];"
  | _ -> (string_of_datatype dtype) ^ " " ^ string_of_id id ^ ";")
| AssignDecl(dtype, id, exp) -> (match dtype with
  ArrayType(t) -> string_of_datatype t ^ " " ^ string_of_id id ^ "[] = " ^ string_of_expr exp ^ ";"
  | _ -> string_of_datatype dtype ^ " " ^ string_of_id id ^ " = " ^ string_of_expr exp ^ ";")
| ArrayDecl(dtype, exp, id) -> string_of_datatype dtype ^ " " ^ string_of_id id ^ "[" ^ string_of_expr exp ^ "];"


let rec gen_stmt = function
  S_Expr(exp) -> "(" ^ (string_of_expr exp) ^ ");"
| S_Return(exp) -> "return " ^ string_of_expr exp ^ ";"
| S_Block(stmt_list) -> "{\n" ^ (String.concat "\n" (List.map gen_stmt stmt_list)) ^ "\n}\n"
| S_If(exp, stmt1, stmt2) -> (if stmt2 == EmptyStmt then
  "if (" ^ (string_of_expr exp) ^ ")\n" ^ (gen_stmt stmt1) ^ "\n" else
  "if (" ^ (string_of_expr exp) ^ ")\n" ^ (gen_stmt stmt1) ^ "\nelse " ^ (gen_stmt stmt2))
| S_For(init, test, after, stmt) -> "for (" ^ string_of_expr init ^ "; " ^ string_of_expr test ^ "; " ^ string_of_expr after ^ ") " ^ gen_stmt stmt
| S_While(test, stmt) -> "while (" ^ (string_of_expr test) ^ ") " ^ (gen_stmt stmt)
| S_VarDeclStmt(decl) -> string_of_decl decl
| S_Continue -> "continue;"
| S_Break -> "break;"
| S_EmptyStmt -> ";"

let string_of_func_decl funcdecl = 
  string_of_datatype funcdecl.return_type ^ " "
  ^ string_of_id funcdecl.fname ^ "("
  ^ (String.concat ", " (List.map string_of_decl funcdecl.formals))
  ^ ") {\n"
  ^ gen_stmt funcdecl.body ^ "\n}"

let string_of_program prg =
  "#include \"dave.h\"\nusing namespace std;\n"
  ^ (String.concat "\n" (List.map string_of_decl prg.gdecls)) ^ "\n" 
  ^ (String.concat "\n" (List.map string_of_func_decl prg.fdecls)) ^ "\n"

let print_program oc prg = 
  let out_file = open_out oc in
  fprintf out_file "%s" (string_of_program prg);
  close_out out_file *)