(*ast.ml*)
(*Binary Operators (In Order): +, -, *, /, %, ^, ==, !=, <, >, >=, <=, &&, ||*)
type op = Add | Sub | Multi | Div | Mod | Exp | Equal | Neq | Lt | Gt | Leq | Geq | And | Or
(*Unary Operators (Before Operand) (In Order): ++, --, !, -*)
type preunop = Incbef | Decbef | Not | Neg
(*Unary Operators (After Operand) (In Order): ++, --*)
type sufunop = Incaft | Decaft
(*Supported Datatypes*)
type datatype = Int | Float | String | Bool
(*Arguments*)
type arg = datatype * string
(*Types of References*)
type ref = ARef | RRef | FRef | TRef


(*Expression*)
(*Critical: Refer to the Issue List Before Proceed*)
type expr = 
	Id of string
	| IntLit of int
	| FloatLit of float
	| StringLit of string
	| BoolLit of bool
	| Var of datatype * expr
	| Binop of expr * op * expr
	| PrefixUnop of preunop * expr
	| SuffixUnop of expr * sufunop
	| Array of string * datatype * expr list
	| Ref of string * ref * expr list
	| Assign of string * expr
	| FuncCall of string * expr list
	(*Tbl = a list of Rec | a list of Fld*)
	| Tbl of expr list
	(*Rec = Name of the Rec * (Datatype of Each Component * Name of Each Component) * Value of Each Component*)
	(*Supplemental: Store Values of Each Component in the Format of String When Coding the Parser*)
	| Rec of string * reccompoent list * expr list
	(*Fld = Name of the Field * Datatype of the Field * Values of the List*)
	| Fld of string * datatype * expr list
	| Noexpr

(*Statement*)
type stmt = 
	Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt

(*Function Declaration*)
type func_decl = {
	fname : string;
	return_type : datatype;
	formals : arg list;
	body: stmt list;
}

(*Programs*)
type program = stmt list * func_decl list






















