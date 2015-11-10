(*ast.ml*)
(*Binary Operators (In Order): +, -, *, /, %, ^, ==, !=, <, >, >=, <=, &&, ||*)
type binop = Add | Sub | Mul | Div | Mod | Exp | Equal | Neq | Lt | Gt | Leq | Geq | And | Or
(*Unary Operators (Before Operand) (In Order): !, -*)
type unop = Not | Neg
(*Unary Operators (After Operand) (In Order): ++, --*)
type postop = Inc | Dec
(*Assignment Operators = += -= *= /= %=*)
type asnop = Asn | Addeq | Subeq | Muleq | Diveq | Modeq
(*Supported Datatypes*)
type datatype = Int | Float | String | Bool | Rec | Tbl | Fld | Void
(*Arguments*)
type arg = datatype * string
(*Types of References*)
type ref = ARef | RecRef | FldRef | TblRef


(*Expression*)
(*Critical: Refer to the Issue List Before Proceed*)
type id = Id of string

type lvalue =
	Var of id
	| ArrayElem of id * expr list
and expr = 
	IntLit of int
	| FloatLit of float
	| StringLit of string
	| BoolLit of bool
	| Binop of expr * binop * expr
	| AssignOp of lvalue * binop * expr
	| Unop of uop * expr
	| Postop of lvalue * postop
	| ArrayRange of expr * expr
	| Ref of id * ref * expr list
	| Assign of lvalue * expr
	| FuncCall of id * expr list
	(*Tbl = a list of Rec | a list of Fld*)
	| Tbl of expr list
	(*Rec = Name of the Rec * (Datatype of Each Component * Name of Each Component) * Value of Each Component*)
	(*Supplemental: Store Values of Each Component in the Format of String When Coding the Parser*)
	| Rec of datatype * expr list
	(*Fld = Name of the Field * Datatype of the Field * Values of the List*)
	| Fld of id * datatype * expr list
	| Lval of lvalue
	| Noexpr

and var_decl = VarDecl of datatype * expr

(*Statement*)
type stmt = 
	Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt
	| Localvar of var_decl

(*Function Declaration*)
type func_decl = {
	fname : string;
	formals : arg list;
	body: stmt list;
	return_type : datatype;
}

(*Programs*)
type program = stmt list * func_decl list


(*Printing AST*)
let string_of_binop = function
	Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "%"
| Exp -> "^"
| Equal -> "=="
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















