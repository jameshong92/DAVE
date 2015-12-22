open Ast
open Sast
open SemanticExceptions
open Printf

let rec string_of_datatype = function
  Int -> "int"
| Float -> "double"
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

let rec list_of_dim v = match v with
  [] -> ""
  | hd::tl -> 
    let exp_string = string_of_expr hd.exp in
      if exp_string = "0" then "" else "(" ^ exp_string ^ ")"

and string_of_expr = function
  Range(exp1, exp2) -> "range(" ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2 ^ ")"
| IntLit(lit) -> string_of_int lit
| FloatLit(lit) -> string_of_float lit
| StringLit(lit) -> "\"" ^ lit ^ "\""
| BoolLit(lit) -> string_of_bool lit
| ArrayLit(exps) -> "{" ^ String.concat ", " (List.map string_of_expr exps) ^ "}"
| Binop(exp1, binop, exp2) -> (
  match exp1, exp2 with
    StringLit(lit1), StringLit(lit2) -> "append(\"" ^ lit1 ^ "\",\"" ^ lit2 ^ "\")"
(*     | Tbl(_), Tbl(_) -> (
      match binop with
        Add -> "plus(" ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2 ^ ")"
        | Sub -> "minus(" ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2 ^ ")"
        | Mul -> "mul(" ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2 ^ ")"
        | Div -> "div(" ^ string_of_expr exp1 ^ ", " ^ string_of_expr exp2 ^ ")"
        | _ -> "(" ^ string_of_expr exp1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr exp2 ^ ")"
    ) *)
    | _,_ -> "(" ^ string_of_expr exp1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr exp2 ^ ")"
  )
| Unop(unop, exp) -> "(" ^ string_of_unop unop ^ string_of_expr exp ^ ")"
| Postop(lvalue, postop) -> "(" ^ string_of_expr lvalue ^ string_of_postop postop ^ ")"
| AssignOp(lvalue, asnop, exp) -> "(" ^ string_of_expr lvalue ^ " " ^ string_of_asnop asnop ^ " " ^ string_of_expr exp ^ ")"
| Cast(datatype, exp) -> "((" ^ string_of_datatype datatype.ptype ^ ")" ^ string_of_expr exp ^ ")"
| FuncCall(id, exps) -> string_of_func_call id exps
| Tbl(exps) -> "tbl(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"
| Rec(exps) -> "rec(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"
| RecRef(id, exp) -> "tuple(" ^ string_of_expr exp ^ ", \"" ^ id ^ "\")"
| Fld(exp, lit) -> "fld(" ^ string_of_expr exp ^ ", \"" ^ lit ^ "\")"
| Lval(lvalue) -> string_of_expr lvalue
| Noexpr -> ""
| None -> "NULL"
| Var(exp) -> exp
| Array(exp1, exp2) -> string_of_array exp1 exp2
| Access(exp1, exp2) -> "(" ^ string_of_expr exp1 ^ "." ^ exp2 ^ ")"

and string_of_array id exp2 = match exp2 with
  | Range(id1, id2) -> 
    if string_of_expr id2 = "0" then
      "slice_array(" ^ id ^ ", " ^ string_of_expr id1 ^ ", getArrayLen(" ^ id ^ "))"
    else
      "slice_array(" ^ id ^ ", " ^ string_of_expr id1 ^ ", " ^ string_of_expr id2 ^ ")"
  | _ -> "(" ^ id ^ "[" ^ string_of_expr exp2 ^ "])"

and string_of_func_call id exps = id ^ "(" ^ String.concat ", " (List.map string_of_expr exps) ^ ")"

and string_of_vtype v =
  let dimension = v.s_dimension in
    match dimension with
    [] -> string_of_datatype v.s_ptype
    | _ -> "vector<" ^ string_of_datatype v.s_ptype ^ "> " ^ list_of_dim (List.rev v.s_dimension)

and string_of_var v id =
  let dimension = v.s_dimension in
    match dimension with
    [] -> string_of_datatype v.s_ptype ^ " " ^ id
    | _ -> "vector<" ^ string_of_datatype v.s_ptype ^ "> " ^ id ^ list_of_dim (List.rev v.s_dimension)

let string_of_decl vdecl =
  let init = vdecl.s_vinit.exp in
  match init with
    Noexpr -> ((string_of_var vdecl.s_vtype vdecl.s_vname))
    | Lval(exp) -> (match exp with
        Array(id, exp1) -> (match exp1 with
            Range(id1, id2) -> string_of_var vdecl.s_vtype vdecl.s_vname ^ " = _slice_array(" ^ id ^ ", " ^ string_of_expr id1 ^ ", " ^ string_of_expr id2 ^ ")"
            | _ -> (string_of_var vdecl.s_vtype vdecl.s_vname) ^ " = " ^ string_of_expr vdecl.s_vinit.exp
          )
        | _ -> (string_of_var vdecl.s_vtype vdecl.s_vname) ^ " = " ^ string_of_expr vdecl.s_vinit.exp
      )
    | Rec(exprs) -> "tuple _" ^ vdecl.s_vname ^ "[] = {" ^ (String.concat ", " (List.map string_of_expr exprs)) ^ "};\n" ^ 
                    (string_of_var vdecl.s_vtype vdecl.s_vname) ^ " = rec(_" ^ vdecl.s_vname ^ ", getArrayLen(_" ^ vdecl.s_vname ^ "))"
    | Fld(expr, id) -> 
      (match expr with
        ArrayLit(array_expr) -> (match array_expr with
          _ ->  let array_type = (match (List.hd array_expr) with StringLit(_) -> "string" | BoolLit(_) -> "bool" | FloatLit(_) -> "double" | _ -> "int") in
                array_type ^ " __" ^ vdecl.s_vname ^ "[] = " ^ string_of_expr expr ^ ";\n" ^
                "vector<" ^ array_type ^ "> _" ^ vdecl.s_vname ^ " = to_vector(__" ^ vdecl.s_vname ^ ", getArrayLen(__" ^ vdecl.s_vname ^ "));\n" ^ 
                "fld " ^ vdecl.s_vname ^ " = fld(&_" ^ vdecl.s_vname ^ "[0],\"" ^ id ^ "\", _" ^ vdecl.s_vname ^".size())"
        )
        | _ -> "fld " ^ vdecl.s_vname ^ " = fld(&" ^ string_of_expr expr ^ "[0], \"" ^ id ^ "\", " ^ string_of_expr expr ^".size())"
      )
    | Tbl(exprs) ->
(*       let tbl_elem_type = (match type_of_expr (List.hd exprs).exp with Fld -> "fld" | _ -> "rec") in
      if List.length exprs > 1 then
        let base_string =
          tbl_elem_type ^ " __" ^ vdecl.s_vname ^ "[] = {" ^ String.concat ", " (List.map (fun x -> string_of_expr x) exprs) ^ "};\n" ^
          "vector<" ^ tbl_elem_type ^ "> _" ^ vdecl.s_vname ^ " = to_vector(__" ^ vdecl.s_vname ^ ", getArrayLen(__" ^ vdecl.s_vname ^ "));\n" in (
        if tbl_elem_type == "fld" then 
          base_string ^ "tbl " ^ vdecl.s_vname ^ " = tbl(&_" ^ vdecl.s_vname ^ "[0], _" ^ vdecl.s_vname ^ "[0].length, _" ^ vdecl.s_vname ^".size())"
        else if tbl_elem_type == "rec" then 
          base_string ^ "tbl " ^ vdecl.s_vname ^ " = tbl(&_" ^ vdecl.s_vname ^ "[0], _" ^ vdecl.s_vname ^ ".size(), _" ^ vdecl.s_vname ^ "[0].length)"
        else raise Not_implemented_err)
      else *)
        let tbl_element = string_of_expr (List.hd exprs) in
            "tbl " ^ vdecl.s_vname ^ " = tbl(&" ^ tbl_element ^ "[0], " ^ tbl_element ^ ".size(), " ^ tbl_element ^ "[0].length)"
    | ArrayLit(exprs) ->
      let array_type = (match vdecl.s_vinit.typ.s_ptype with Rec -> "rec" | Fld -> "fld" | String -> "string" | Bool -> "bool" | Float -> "double" | _ -> "int") in
        array_type ^ " _" ^ vdecl.s_vname ^ "[] = {" ^ (String.concat ", " (List.map string_of_expr exprs)) ^ "};\n" ^
        "vector<" ^ array_type ^ "> " ^ vdecl.s_vname ^ " = to_vector(_" ^ vdecl.s_vname ^ ", getArrayLen(_" ^ vdecl.s_vname ^ "))"
    | _ -> (string_of_var vdecl.s_vtype vdecl.s_vname) ^ " = " ^ string_of_expr vdecl.s_vinit.exp

let rec gen_stmt = function
  S_Expr(exp) -> "(" ^ (string_of_expr exp.exp) ^ ");"
| S_Return(exp) -> "return " ^ string_of_expr exp.exp ^ ";"
| S_Block(stmt_list) -> "{\n" ^ (String.concat "\n" (List.map gen_stmt stmt_list)) ^ "\n}\n"
| S_If(exp, stmt1, stmt2) -> (if stmt2 == S_EmptyStmt then
  "if (" ^ (string_of_expr exp.exp) ^ ")\n" ^ (gen_stmt stmt1) else
  "if (" ^ (string_of_expr exp.exp) ^ ")\n" ^ (gen_stmt stmt1) ^ "else " ^ (gen_stmt stmt2))
| S_For(init, test, after, stmt) -> "for (" ^ string_of_expr init.exp ^ "; " ^ string_of_expr test.exp ^ "; " ^ string_of_expr after.exp ^ ") " ^ gen_stmt stmt
| S_While(test, stmt) -> "while (" ^ (string_of_expr test.exp) ^ ") " ^ (gen_stmt stmt)
| S_VarDeclStmt(decl) -> string_of_decl decl ^ ";"
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
  (String.concat "\n" (List.map string_of_decl prg.s_gdecls)) ^ "\n"
  ^ (String.concat "\n" (List.map string_of_func_decl prg.s_fdecls)) ^ "\n"

let compile oc prg =
  let out_file = open_out oc in
  fprintf out_file "#include \"dave.h\"\n";
  fprintf out_file "%s" (string_of_program (List.hd prg));
  fprintf out_file "%s" (string_of_program (List.nth prg 1));
  close_out out_file
