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
type ref = ARef | RecRef | FldRef | TblRef


(*Expression*)
(*Critical: Refer to the Issue List Before Proceed*)
type id = Id of string

type literal =
	IntLit of int
	| FloatLit of float
	| StringLit of string
	| BoolLit of bool

type lvalue =
	Var of id
	| Array of id * expr
	| Access of expr * id
and expr =
	Range of expr * expr
	| Binop of expr * binop * expr
	| Unop of unop * expr
	| Postop of lvalue * postop
	| AssignOp of lvalue * asnop * expr
	| Assign of lvalue * expr
	| Lval of lvalue
	| Cast of datatype * expr
	| CastFld of expr * string
	| CastTbl of datatype * datatype
	| FuncCall of id * expr list
	(* | Ref of id * ref * expr list *)
	(*Tbl = a list of Rec | a list of Fld*)
	| Tbl of expr list
	(*Rec = list of id (key) * Value of Each Component*)
	(*Supplemental: Store Values of Each Component in the Format of String When Coding the Parser*)
	| Rec of id * literal list
	(*Fld = Values of the List * Name of the Field*)
	| Fld of expr * string
	| Noexpr

type decl =
	VarDecl of datatype * expr
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
	fname : string;
	formals : arg list;
	body: stmt list;
	return_type : datatype;
}

(*Programs*)
type program = {
	gdecls : stmt list;
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
