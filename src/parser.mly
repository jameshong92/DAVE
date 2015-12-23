%{ open Ast %}
%token NEW
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMICOL COMMA DOT COLON
%token ADDEQ SUBEQ MULEQ DIVEQ MODEQ
%token PLUS MINUS TIMES DIVIDE MOD INC DEC EXP
%token ASN AND OR NOT
%token EQ NEQ LT LEQ GT GEQ
%token CONTINUE BREAK IF ELSE FOR WHILE RETURN
%token FLD TBL REC
%token VOID STR BOOL INT FLOAT FLD TBL REC
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> STR_LIT ID
%token NONE
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASN INC DEC ADDEQ SUBEQ MULEQ DIVEQ MODEQ
%right DOT
%left LBRACK
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD EXP
%right NOT UNMINUS

%start program
%type <Ast.program> program

%%

/* Expressions */

/* str int float bool rec fld tbl str[] int[] float[] bool[] fld[] rec[] */
datatype:
	INT 												{
																{
																	ptype = Int;
																	dimension = []
																}
															}
| STR {
																{
																	ptype = String;
																	dimension = []
																}
															}
| FLOAT {
																{
																	ptype = Float;
																	dimension = []
																}
															}
| BOOL {
																{
																	ptype = Bool;
																	dimension = []
																}
															}
| FLD {
																{
																	ptype = Fld;
																	dimension = []
																}
															}
| REC {
																{
																	ptype = Rec;
																	dimension = []
																}
															}
| TBL {
																{
																	ptype = Tbl;
																	dimension = []
																}
															}
| VOID 												{
																{
																	ptype = Void;
																	dimension = []
																}
															}
| datatype LBRACK expr_opt RBRACK 
															{
																{
																	ptype = $1.ptype;
																	dimension = [(if $3 == Noexpr then IntLit(0) else $3)]
																}
															}

/* id[index_list], expr.id */
lvalue:
	ID 													{ Var($1) }
	| ID LBRACK index_list RBRACK
															{ Array($1, $3) }

index_list:
	expr 									 			{ $1 }
	| COLON expr 					 			{ Range(Noexpr, $2) }
	| expr COLON					 			{ Range($1, Noexpr) }
	| expr COLON expr 		 			{ Range($1, $3) }

expr:
	| expr PLUS expr 						{ Binop($1, Add, $3) }
	| expr MINUS expr 					{ Binop($1, Sub, $3) }
	| expr TIMES expr 					{ Binop($1, Mul, $3) }
	| expr DIVIDE expr 					{ Binop($1, Div, $3) }
	| expr MOD expr 						{ Binop($1, Mod, $3) }
	| expr EXP expr 						{ Binop($1, Exp, $3) }
	| expr EQ expr 							{ Binop($1, Eq, $3) }
	| expr NEQ expr 						{ Binop($1, Neq, $3) }
	| expr LT expr 							{ Binop($1, Lt, $3) }
	| expr GT expr 							{ Binop($1, Gt, $3) }
	| expr LEQ expr 						{ Binop($1, Leq, $3) }
	| expr GEQ expr 						{ Binop($1, Geq, $3) }
	| expr AND expr 						{ Binop($1, And, $3) }
	| expr OR expr 							{ Binop($1, Or, $3) }

	| NOT expr 									{ Unop(Not, $2) }
	| MINUS expr %prec UNMINUS	{ Unop(Neg, $2) }
	| NONE											{ None }

	| lvalue INC 								{ Postop($1, Inc) }
	| lvalue DEC 								{ Postop($1, Dec) }

	| lvalue ADDEQ expr 				{ AssignOp($1, Addeq, $3) }
	| lvalue SUBEQ expr 				{ AssignOp($1, Subeq, $3) }
	| lvalue MULEQ expr 				{ AssignOp($1, Muleq, $3) }
	| lvalue DIVEQ expr 				{ AssignOp($1, Diveq, $3) }
	| lvalue MODEQ expr 				{ AssignOp($1, Modeq, $3) }
	| lvalue ASN expr 					{ AssignOp($1, Asn, $3) }
	| expr DOT ID 							{ Access($1, $3) }
	| lvalue 										{ Lval($1) }

	| LPAREN expr RPAREN 				{ $2 }

	| datatype LPAREN expr RPAREN
															{ Cast($1, $3) }
	| literal 									{ $1 }
	| tbl_lit 									{ Tbl($1) }

	| ID LPAREN actuals_opt RPAREN
															{ FuncCall($1, $3) }

literal:
	INT_LIT 										{ IntLit($1) }
	| FLOAT_LIT 								{ FloatLit($1) }
	| STR_LIT 									{ StringLit($1) }
	| BOOL_LIT 									{ BoolLit($1) }
	| fld_lit 									{ $1 }
	| rec_lit 									{ $1 }
	| LBRACK actuals_list RBRACK { ArrayLit(List.rev $2) }

tbl_lit:
	NEW TBL LPAREN actuals_list RPAREN
															{ List.rev $4 }

rec_lit:
	NEW REC LPAREN rec_init RPAREN 
															{ Rec(List.rev $4) }

rec_init:
	ID COLON literal 						{ [RecRef($1, $3)] }
	| rec_init COMMA ID COLON literal
															{ RecRef($3, $5) :: $1 }

fld_lit:
	NEW FLD LPAREN expr COMMA STR_LIT RPAREN
															{ Fld($4, $6) }

actuals_opt:
	/* nothing */ 							{ [] }
	| actuals_list 							{ List.rev $1 }

actuals_list:
	expr 												{ [$1] }
	| actuals_list COMMA expr 	{ $3 :: $1 }

/* Declarations */
program:
  /* nothing */ 							{ { gdecls = []; fdecls = [] } }
  | program vdecl 						{ { gdecls = $2 :: $1.gdecls; fdecls = $1.fdecls } }
  | program fdecl 						{ { gdecls = $1.gdecls; fdecls = $2 :: $1.fdecls } }

fdecl:
	datatype ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
															{ 
																{
																	fname = $2;
																	formals = $4;
																	body = Block(List.rev $7);
																	return_type = $1
																} 
															}

formals_opt:
	/* nothing */ 							{ [] }
	| formal_list 							{ List.rev $1 }

formal_list:
	datatype ID 								{[
																{
																	vname = $2;
																	vtype = $1;
																	vinit = Noexpr;
																}]
															}
	| formal_list COMMA datatype ID
															{ ({
																	vname = $4;
																	vtype = $3;
																	vinit = Noexpr;
																}) :: $1
															}

vdecl:
	datatype ID SEMICOL 				{
																{
																	vname = $2;
																	vtype = $1;
																	vinit = Noexpr;
																}
															}
	| datatype ID ASN expr SEMICOL
															{
																{
																	vname = $2;
																	vtype = $1;
																	vinit = $4;
																}
															}

expr_opt:
	/* nothing */ 							{ Noexpr }
	| expr 											{ $1 }

stmt_list:
	/* nothing */ 							{ [] }
	| stmt_list stmt 						{ $2 :: $1 }

stmt:
	expr SEMICOL 								{ Expr($1) }
	| RETURN expr SEMICOL 			{ Return($2) }
	| LBRACE stmt_list RBRACE 	{ Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt ELSE stmt
															{ If($3, $5, $7) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE
															{ If($3, $5, EmptyStmt) }
	| FOR LPAREN expr_opt SEMICOL expr_opt SEMICOL expr_opt RPAREN stmt
															{ For($3, $5, $7, $9) }
	| WHILE LPAREN expr RPAREN stmt
															{ While($3, $5) }
	| CONTINUE SEMICOL					{ Continue }
	| BREAK SEMICOL							{ Break }
	| vdecl 										{ VarDeclStmt($1) }
