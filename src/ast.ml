(*ast.ml*)
exception Invalid_type of string
(*Binary Operators (In Order): +, -, *, /, %, ^, ==, !=, <, >, >=, <=, &&, ||*)
type binop = Add | Sub | Mul | Div | Mod | Exp | Eq | Neq | Lt | Gt | Leq | Geq | And | Or
(*Unary Operators (Before Operand) (In Order): !, -*)
type unop = Not | Neg
(*Unary Operators (After Operand) (In Order): ++, --*)
type postop = Inc | Dec
(*Assignment Operators = += -= *= /= %=*)
type asnop = Asn | Addeq | Subeq | Muleq | Diveq | Modeq
(*Supported Datatypes*)
type datatype = Int | Float | String | Bool | Fld | Tbl | Rec | Void | ArrayType of datatype
(*Arguments*)
type arg = datatype * string
(*Types of References*)
(* type ref = ARef | RecRef | FldRef | TblRef *)


(*Expression*)
(*Critical: Refer to the Issue List Before Proceed*)
type id = Id of string

type lvalue =
	Var of id
	| Array of id * expr
	| Access of expr * id
and expr =
	IntLit of int
	| FloatLit of float
	| StringLit of string
	| BoolLit of bool
	| ArrayLit of expr list
	| Range of expr * expr
	| Binop of expr * binop * expr
	| Unop of unop * expr
	| Postop of lvalue * postop
	| AssignOp of lvalue * asnop * expr
	| Assign of lvalue * expr
	| Lval of lvalue
	| Cast of datatype * expr
	(*| CastFld of expr * string
	| CastTbl of datatype * datatype *)
	| FuncCall of id * expr list
	(* | Ref of id * ref * expr list *)
	(*Tbl = a list of Rec | a list of Fld*)
	| Tbl of expr list
	(*Rec = list of id (key) * Value of Each Component*)
	(*Supplemental: Store Values of Each Component in the Format of String When Coding the Parser*)
	| Rec of expr list
	| RecRef of id * expr
	(*Fld = Values of the List * Name of the Field*)
	| Fld of expr list * string
	| Noexpr
	| None

type decl =
	VarDecl of datatype * id
	| AssignDecl of datatype * id * expr
	| ArrayDecl of datatype * expr * id

(*Statement*)
type stmt =
	Expr of expr
	| Return of expr
	| Block of stmt list
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt
	| VarDeclStmt of decl
	| Continue
	| Break

(*Function Declaration*)
type func_decl = {
	fname : id;
	formals : decl list;
	body: stmt;
	return_type : datatype;
}

(*Programs*)
type program = {
	gdecls : decl list;
	fdecls : func_decl list;
}


(*Printing AST*)
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
| Gt -> ">"
| Leq -> "<="
| Geq -> "<="
| And -> "&&"
| Or -> "||"

let string_of_asnop = function
	Asn -> "="
| Addeq -> "+="
| Subeq -> "-="
| Muleq -> "*="
| Diveq -> "/="
| Modeq -> "%="

let string_of_unop = function
	Not -> "!"
| Neg -> "-"

let string_of_postop = function
	Inc -> "++"
| Dec -> "--"

let rec string_of_datatype = function
	Int -> "int"
| Float -> "float"
| Bool -> "bool"
| String -> "str"
| Tbl -> "tbl"
| Rec -> "rec"
| Fld -> "fld"
| Void -> "void"
| ArrayType(datatype) -> "ArrayType( " ^ string_of_datatype datatype ^ " )"

let string_of_id = function
	Id(id) -> "Id( " ^ id ^ " )"

let rec string_of_expr = function
	IntLit(lit) -> "IntLit( " ^ string_of_int lit ^ " )"
| FloatLit(lit) -> "FloatLit( " ^ string_of_float lit ^ " )"
| StringLit(lit) -> "StringLit( " ^ lit ^ " )"
| BoolLit(lit) -> "BoolLit( " ^ string_of_bool lit ^ " )"
| ArrayLit(exps) -> "ArrayLit( " ^ String.concat ", " (List.map string_of_expr exps) ^ " )"
| Range(exp1, exp2) -> "Range( " ^ string_of_expr exp1 ^ " " ^ string_of_expr exp2 ^ " )"
| Binop(exp1, binop, exp2) -> "Binop( " ^ string_of_expr exp1 ^ " " ^ string_of_binop binop ^ " " ^ string_of_expr exp2 ^ " )"
| Unop(unop, exp) -> "Unop( " ^ string_of_unop unop ^ " " ^ string_of_expr exp ^ " )"
| Postop(lvalue, postop) -> "Postop( " ^ string_of_lvalue lvalue ^ " " ^ string_of_postop postop ^ " )"
| AssignOp(lvalue, asnop, exp) -> "AssignOp( " ^ string_of_lvalue lvalue ^ " " ^ string_of_asnop asnop ^ " " ^ string_of_expr exp ^ " )"
| Assign(lvalue, exp) -> "Assign( " ^ string_of_lvalue lvalue ^ " " ^ string_of_expr exp ^ " )"
| Lval(lvalue) -> "Lval( " ^ string_of_lvalue lvalue ^ " )"
| Cast(datatype, exp) -> "Cast( " ^ string_of_datatype datatype ^ " " ^ string_of_expr exp ^ " )"
| FuncCall(id, exps) -> "FuncCall( " ^ string_of_id id ^ ", " ^ String.concat "; " (List.map string_of_expr exps) ^ " )"
| Tbl(exps) -> "Tbl( " ^ String.concat "; " (List.map string_of_expr exps) ^ " )"
| Rec(exps) -> "Rec{ " ^ String.concat ", " (List.map string_of_expr exps) ^ " }"
| RecRef(id, exp) -> "RecRef( " ^ string_of_id id ^ ":" ^ string_of_expr exp ^ " )"
| Fld(exps, lit) -> "Fld([" ^ String.concat "; " (List.map string_of_expr exps) ^ "], '" ^ lit ^ "' )"
| Noexpr -> "Noexpr"
| None -> "NULL"

and string_of_lvalue = function
	Var(lit) -> "Var( " ^ string_of_id lit ^ " )"
| Array(id, index) -> "Array( " ^ string_of_id id ^ "[" ^ string_of_expr index ^ "] ) "
| Access(exp, id) -> "Access( " ^ string_of_expr exp ^ " " ^ string_of_id id ^ " )"

let string_of_decl = function
	VarDecl(datatype, id) -> "VarDecl( " ^ string_of_datatype datatype ^ " " ^ string_of_id id ^ " )"
| AssignDecl(datatype, id, exp) -> "AssignDecl( " ^ string_of_datatype datatype ^ " " ^ string_of_id id ^ " = " ^ string_of_expr exp ^ " )"
| ArrayDecl(datatype, exp, id) -> "ArrayDecl( " ^ string_of_datatype datatype ^ "[ " ^ string_of_expr exp ^ " ]" ^ string_of_id id ^ " )"

let rec string_of_stmt = function
	Expr(exp) -> "Expr( " ^ string_of_expr exp ^ " )"
| Return(exp) -> "Return( " ^ string_of_expr exp ^ ")"
| Block(stmt_list) -> (String.concat "\n" (List.map string_of_stmt stmt_list)) ^ "\n"
| If(exp, stmt1, stmt2) -> "if (" ^ string_of_expr exp ^ ") {\n" ^ string_of_stmt stmt1 ^ string_of_stmt stmt2 ^ "\n}"
| For(init, test, after, stmt) -> "for (" ^ string_of_expr init ^ ", " ^ string_of_expr test ^ ", " ^ string_of_expr after ^ ") {\n" ^ string_of_stmt stmt ^ "\n}"
| While(test, stmt) -> "while( " ^ string_of_expr test ^ " ) {\n" ^ string_of_stmt stmt ^ "\n}"
| VarDeclStmt(var) -> "VarDeclStmt( " ^ (string_of_decl var) ^ " )"
| Continue -> "Continue;\n"
| Break -> "Break\n"

let string_of_func_decl funcdecl = "Function( return type: (" 
	^ string_of_datatype funcdecl.return_type ^ ") name: \"" 
	^ string_of_id funcdecl.fname ^ "\" formals: (" 
  ^ (String.concat ", " (List.map string_of_decl funcdecl.formals))
  ^ ") {\n" 
	^ string_of_stmt funcdecl.body ^ "\n}"

let string_of_program prgm = "Program( " 
	^ (String.concat "\n" (List.map string_of_decl prgm.gdecls)) ^ "\n\n" 
	^ (String.concat "\n\n" (List.map string_of_func_decl prgm.fdecls)) ^ "\n)\n"

let type_of_string = function
	"int" -> Int
	| "float" -> Float
	| "bool" -> Bool
	| "str" -> String
	| "fld" -> Fld
	| "rec" -> Rec
	| "tbl" -> Tbl
	| "void" -> Void
	| dtype -> raise (Invalid_type dtype)
